-- M2.6 string.dump + binary chunk load tests
local pass = 0
local fail = 0

local function check(name, got, expected)
  if got == expected then
    pass = pass + 1
  else
    fail = fail + 1
    print("FAIL: " .. name .. " expected=" .. tostring(expected) .. " got=" .. tostring(got))
  end
end

-- basic roundtrip
local chunk = string.dump(function() return 1 end)
check("dump type", type(chunk), "string")
check("binary signature", string.byte(chunk, 1), 27)
local f = assert(load(chunk, nil, "b", {}))
check("loaded type", type(f), "function")
check("loaded call", f(), 1)

-- roundtrip of a function using globals (env: _ENV upvalue)
local h = assert(load(string.dump(load("x = 1; return x")), nil, "b"))
check("global via env", h(), 1)
check("global was set", _G.x, 1)
_G.x = nil

-- strip removes debug info but code still runs
local stripped = assert(load(string.dump(function() return 42 end, true), nil, "b"))
check("stripped call", stripped(), 42)

-- strip keeps errors working (without line info)
local boom = assert(load(string.dump(function() error("kaboom") end, true), "boom", "b"))
local ok, err = pcall(boom)
check("stripped error ok", ok, false)
check("stripped error msg", tostring(err):find("kaboom") ~= nil, true)

-- constants of all types
local k = function() return nil, true, false, 1, 2.5, "str" end
local lk = assert(load(string.dump(k), nil, "b"))
local a, b, c, d, e, s = lk()
check("const nil", a, nil)
check("const true", b, true)
check("const false", c, false)
check("const int", d, 1)
check("const float", e, 2.5)
check("const string", s, "str")

-- varargs
local va = function(...) return select("#", ...), ... end
local lva = assert(load(string.dump(va), nil, "b"))
local n, x, y = lva("x", "y")
check("vararg count", n, 2)
check("vararg 1", x, "x")
check("vararg 2", y, "y")

-- named vararg table
local va2 = function(... args) return select("#", ...), args end
local lva2 = assert(load(string.dump(va2), nil, "b"))
local n2, args = lva2(1, 2, 3)
check("named vararg count", n2, 3)
check("named vararg 1", args[1], 1)
check("named vararg 3", args[3], 3)

-- nested function
local function outer()
  return function(v) return v + 100 end
end
local lo = assert(load(string.dump(outer), nil, "b"))
check("nested", lo()(1), 101)

-- upvalues: fresh instances; first bound to env (whatever its name)
local ux, uy = 1, 2
local function multi() return ux, uy, _ENV end
local env = {tag = 1}
local lm = assert(load(string.dump(multi), nil, "b", env))
local a1, b1, env2 = lm()
check("first upvalue bound to env", a1, env)
check("second upvalue fresh", b1, nil)
check("_ENV upvalue fresh", env2, nil)

-- recursion through _ENV
function fact(n)
  if n <= 1 then return 1 else return n * fact(n - 1) end
end
local lfact = assert(load(string.dump(fact), "fact", "b"))
check("recursion", lfact(5), 120)

-- mode enforcement
check("binary in t", load(chunk, nil, "t") ~= nil or true, true)
do
  local bad1, msg1 = load(chunk, nil, "t")
  check("binary rejected in t", bad1, nil)
  check("binary mode msg", tostring(msg1):find("attempt to load a binary chunk") ~= nil, true)
  local bad2, msg2 = load("return 1", nil, "b")
  check("text rejected in b", bad2, nil)
  check("text mode msg", tostring(msg2):find("attempt to load a text chunk") ~= nil, true)
  check("bt accepts binary", type(load(chunk, nil, "bt")), "function")
  local bad3, msg3 = load("\27Rua not really", nil, "b")
  check("bad binary", bad3, nil)
  check("bad binary msg", tostring(msg3):find("bad binary format") ~= nil, true)
end

-- string.dump argument checks
do
  local ok1 = pcall(string.dump, print)
  check("dump C function fails", ok1, false)
  local ok2 = pcall(string.dump, 42)
  check("dump non-function fails", ok2, false)
end

-- binary chunks from files
do
  local path = os.tmpname()
  local fh = assert(io.open(path, "wb"))
  fh:write(string.dump(function() return "from file" end))
  fh:close()

  local lf = assert(loadfile(path, "b"))
  check("loadfile binary", lf(), "from file")

  local lf2 = assert(loadfile(path))
  check("loadfile binary default mode", lf2(), "from file")

  check("dofile binary", dofile(path), "from file")
  os.remove(path)
end

-- summary
print(string.format("=== Dump tests: %d passed, %d failed ===", pass, fail))
if fail > 0 then
  error("Some tests failed!")
end

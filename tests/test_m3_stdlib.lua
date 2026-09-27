-- M3: loader/VM/stdlib completion regression tests
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

local function check_match(name, got, pat)
  if type(got) == "string" and got:find(pat, 1, true) then
    pass = pass + 1
  else
    fail = fail + 1
    print("FAIL: " .. name .. " expected match " .. pat .. " got " .. tostring(got))
  end
end

local function check_error(name, pat, f, ...)
  local ok, err = pcall(f, ...)
  if ok or not (type(err) == "string" and err:find(pat, 1, true)) then
    fail = fail + 1
    print("FAIL: " .. name .. " expected error " .. pat .. " got " .. tostring(err))
  else
    pass = pass + 1
  end
end

-- ── return pcall/xpcall ─────────────────────────────────────────
do
  local function f() return pcall(function() return 1, 2, 3 end) end
  local a, b, c, d = f()
  check("return pcall", a, true)
  check("return pcall 2", b, 1)
  check("return pcall 4", d, 3)

  local function g() return xpcall(function() error("x", 0) end, function(e) return e end) end
  local ok, err = g()
  check("return xpcall", ok, false)
  check_match("return xpcall msg", err, "x")
end

-- ── load reader ─────────────────────────────────────────────────
do
  local pieces = {"return ", "10 + ", "32"}
  local i = 0
  local f = assert(load(function()
    i = i + 1
    return pieces[i]
  end))
  check("load reader", f(), 42)

  local f2 = assert(load(function() return nil end))
  check("load reader empty", f2() == nil, true)

  check("load reader non-string", load(function() return true end), nil)

  local ok, err = load(function() error("reader boom") end)
  check("load reader error nil", ok, nil)
  check_match("load reader error msg", err, "reader boom")
end

-- ── error objects ───────────────────────────────────────────────
do
  local ok, err = pcall(error)
  check("error() object", err, "<no error object>")

  local ok2, err2 = pcall(function() local x = nil; return x.y end)
  check_match("runtime error position", err2, "attempt to index a nil value")
  check("runerror has position", err2:find(":") ~= nil, true)

  local ok3, err3 = pcall(function() error("raw", 0) end)
  check("error level 0", err3, "raw")
end

-- ── table.sort ──────────────────────────────────────────────────
do
  local t = {5, 3, 8, 1}
  table.sort(t)
  check("sort default", table.concat(t, ","), "1,3,5,8")
  table.sort(t, function(a, b) return a > b end)
  check("sort comparator", table.concat(t, ","), "8,5,3,1")

  table.sort({}, error) -- no comparisons, no error
  check("sort empty+error", true, true)

  local b = setmetatable({}, {__len = function() return -1 end})
  table.sort(b, error)
  check("sort negative len", true, true)

  local c = setmetatable({}, {__len = function() return math.maxinteger end})
  check_error("sort too big", "too big", table.sort, c)

  check_error("sort invalid order", "invalid order function",
              table.sort, {1, 2, 3, 4, 5}, function() return true end)

  check_error("sort non-function", "'table.sort'",
              table.sort, {1, 2, 3}, table.sort)

  -- __lt metamethod for default comparator
  local mt = {__lt = function(x, y) return x.v < y.v end}
  local arr = {}
  for i = 1, 6 do arr[i] = setmetatable({v = 7 - i}, mt) end
  table.sort(arr)
  check("sort __lt", arr[1].v, 1)
  check("sort __lt 2", arr[6].v, 6)
end

-- ── warn ────────────────────────────────────────────────────────
do
  check_error("warn no args", "warn", warn)
  check_error("warn bad arg", "string expected", warn, 1, {})
  warn("@on")
  warn("checked warning")
  warn("@off")
  warn("@store")
  warn("stored ")
  warn(42)
  warn("@normal")
  check("warn store", _WARN, "stored 42")
  _WARN = nil
end

-- ── os.setlocale ────────────────────────────────────────────────
do
  check("setlocale C", os.setlocale("C"), "C")
  check("setlocale query", os.setlocale(), "C")
  check("setlocale category", os.setlocale(nil, "numeric"), "C")
  check("setlocale unavailable", os.setlocale("pt_BR"), nil)
  check_error("setlocale bad category", "invalid option",
              os.setlocale, "C", "bogus")
end

-- ── io defaults ─────────────────────────────────────────────────
do
  local path = os.tmpname()
  local f = assert(io.open(path, "w"))
  f:write("alpha\nbeta\n")
  f:close()

  local old_in = io.input()
  io.input(path)
  check("io.input switch", io.read("l"), "alpha")
  io.input():close()
  io.input(old_in)

  io.input(path)
  local lines = {}
  for l in io.lines() do lines[#lines + 1] = l end
  check("io.lines default", #lines, 2)
  check("io.lines default 2", lines[2], "beta")
  io.input():close()
  io.input(old_in)

  local out = os.tmpname()
  local old_out = io.output()
  io.output(out)
  local wh = io.write("w", 1)
  check("io.write returns handle", io.type(wh), "file")
  io.output():close()
  io.output(old_out)
  local rf = assert(io.open(out))
  check("io.output switch", rf:read("a"), "w1")
  rf:close()
  os.remove(path)
  os.remove(out)
end

-- ── io.popen ────────────────────────────────────────────────────
do
  local p = assert(io.popen("echo popen-hello"))
  check("popen read", p:read("l"), "popen-hello")
  local ok, what, code = p:close()
  check("popen close ok", ok, true)
  check("popen close what", what, "exit")
  check("popen close code", code, 0)

  local z = assert(io.popen("exit 4"))
  local a2, b2, c2 = z:close()
  check("popen nonzero ok", a2, nil)
  check("popen nonzero code", c2, 4)

  check("popen bad mode", pcall(io.popen, "true", "x"), false)
end

-- ── file setvbuf / __name ───────────────────────────────────────
do
  local f = assert(io.open(os.tmpname(), "w"))
  check("setvbuf no", f:setvbuf("no"), f)
  check("setvbuf full", f:setvbuf("full"), f)
  check("setvbuf line", f:setvbuf("line"), f)
  check_error("setvbuf bad", "invalid option", f.setvbuf, f, "nope")
  f:close()
  check("file __name", getmetatable(io.stdout).__name, "FILE*")
end

print(string.format("=== M3 stdlib tests: %d passed, %d failed ===", pass, fail))
if fail > 0 then
  error("Some tests failed!")
end

-- Tests for the `debug` library.
-- Run from the workspace root: rua tests/debug_test.lua

-- ── the library is registered ─────────────────────────────────────
assert(type(debug) == "table", "debug library should be a table")

-- ── debug.traceback ───────────────────────────────────────────────
do
  -- nil/no message → returns a string
  local t = debug.traceback()
  assert(type(t) == "string", "traceback() should return a string")

  -- with a string message → message is preserved somewhere in the trace
  local m = debug.traceback("oops")
  assert(type(m) == "string", "traceback('msg') should return a string")
  assert(m:find("oops", 1, true) ~= nil, "traceback should contain the message")

  -- non-string / non-nil message → returned as-is (matches stock Lua)
  local obj = {}
  local r = debug.traceback(obj)
  assert(r == obj, "traceback on a non-string message returns it unchanged")
end

-- ── debug.getinfo ─────────────────────────────────────────────────
do
  local function foo() return debug.getinfo(1) end
  local info = foo()
  assert(type(info) == "table", "getinfo(1) should return a table")
  -- Default what = "flnStu" → these fields should be present
  assert(type(info.source) == "string", "info.source should be string")
  assert(type(info.short_src) == "string", "info.short_src should be string")
  assert(type(info.what) == "string", "info.what should be string")
  assert(type(info.currentline) == "number", "info.currentline should be number")
  assert(info.isvararg == false, "foo is not vararg")
  assert(info.nparams == 0, "foo has 0 params")
  assert(type(info.nups) == "number", "info.nups should be number")
end

do
  -- getinfo on a function value
  local function bar(a, b, ...) return a + b end
  local info = debug.getinfo(bar, "Su")
  assert(type(info) == "table")
  assert(info.nparams == 2, "bar has 2 params, got " .. tostring(info.nparams))
  assert(info.isvararg == true, "bar is vararg")
  assert(info.what == "Lua", "bar is a Lua function")
end

do
  -- getinfo on a C (native) function
  local info = debug.getinfo(print, "S")
  assert(type(info) == "table")
  assert(info.what == "C", "print should be a C function, got " .. tostring(info.what))
  assert(info.source == "=[C]", "C source should be '=[C]', got " .. tostring(info.source))
end

do
  -- invalid/out-of-range level → nil
  assert(debug.getinfo(999) == nil, "out-of-range level should return nil")
end

-- ── debug.getlocal / debug.setlocal ───────────────────────────────
do
  local function f(x, y)
    local z = x + y
    -- At this point, locals are x(1), y(2), z(3).
    local n1, v1 = debug.getlocal(1, 1)
    local n2, v2 = debug.getlocal(1, 2)
    local n3, v3 = debug.getlocal(1, 3)
    assert(n1 == "x" and v1 == x, "local 1 should be x")
    assert(n2 == "y" and v2 == y, "local 2 should be y")
    assert(n3 == "z" and v3 == z, "local 3 should be z")
    -- Beyond the active set → nil
    local nN = debug.getlocal(1, 99)
    assert(nN == nil, "getlocal past end should be nil")
    return z
  end
  assert(f(3, 4) == 7)
end

do
  -- getlocal on a function value returns parameter names only
  local function g(aa, bb) return aa + bb end
  local n1 = debug.getlocal(g, 1)
  local n2 = debug.getlocal(g, 2)
  assert(n1 == "aa", "param 1 name should be 'aa', got " .. tostring(n1))
  assert(n2 == "bb", "param 2 name should be 'bb', got " .. tostring(n2))
end

do
  -- setlocal mutates the named local of a running frame
  local function h()
    local a = 1
    local b = 2
    -- Overwrite a from within h itself (level 1).
    local name = debug.setlocal(1, 1, 99)
    assert(name == "a", "setlocal should return the local's name, got " .. tostring(name))
    return a, b
  end
  local a, b = h()
  assert(a == 99, "setlocal should have changed a, got " .. tostring(a))
  assert(b == 2, "b should be untouched")
end

-- ── debug.getupvalue / setupvalue / upvalueid / upvaluejoin ───────
-- Helper: scan a closure's upvalues and return the 1-based index of
-- the one named `want`. We don't hardcode the index because the
-- compiler may also attach `_ENV` as an upvalue of inner closures.
local function find_upvalue(f, want)
  local i = 1
  while true do
    local n = debug.getupvalue(f, i)
    if n == nil then return nil end
    if n == want then return i end
    i = i + 1
  end
end

do
  local function make_counter(start)
    return function() start = start + 1; return start end
  end
  local c1 = make_counter(10)
  local c2 = make_counter(100)

  local idx = find_upvalue(c1, "start")
  assert(idx ~= nil, "inner closure should have a 'start' upvalue")

  -- getupvalue: returns (name, value)
  local n, v = debug.getupvalue(c1, idx)
  assert(n == "start")
  assert(v == 10, "expected 10, got " .. tostring(v))

  -- setupvalue: returns name; value is updated
  local set_name = debug.setupvalue(c1, idx, 50)
  assert(set_name == "start")
  assert(c1() == 51, "after setupvalue, counter should resume from 50")

  -- Out-of-range index → nil
  assert(debug.getupvalue(c1, 99) == nil)
  assert(debug.setupvalue(c1, 99, 0) == nil)

  -- upvalueid: same closure's same slot → same id;
  --           different closures' `start` upvalues → different ids.
  local idx2 = find_upvalue(c2, "start")
  local id_a = debug.upvalueid(c1, idx)
  local id_b = debug.upvalueid(c1, idx)
  local id_c = debug.upvalueid(c2, idx2)
  assert(id_a == id_b, "same upvalue should have same id")
  assert(id_a ~= id_c, "different closures should have different upvalue ids")

  -- upvaluejoin: make c1 and c2 share c2's `start`.
  debug.upvaluejoin(c1, idx, c2, idx2)
  assert(debug.upvalueid(c1, idx) == debug.upvalueid(c2, idx2),
         "after join, ids should match")
  c2()             -- start -> 101
  local v2 = c2()  -- start -> 102
  assert(v2 == 102)
  local v1 = c1()  -- start -> 103
  assert(v1 == 103, "c1 should see c2's upvalue after join, got " .. tostring(v1))
end

-- ── debug.getmetatable / debug.setmetatable ───────────────────────
do
  -- On a table it works like normal setmetatable but without any
  -- __metatable protection checks.
  local mt = { __tostring = function(x) return "t" end }
  local t = {}
  debug.setmetatable(t, mt)
  assert(debug.getmetatable(t) == mt, "metatable should be readable back")

  -- Ignores __metatable protection (that's the whole point of debug.*).
  local prot_mt = { __metatable = "locked" }
  local t2 = setmetatable({}, prot_mt)
  -- Plain getmetatable returns the "locked" sentinel...
  assert(getmetatable(t2) == "locked")
  -- ...but debug.getmetatable bypasses it.
  assert(debug.getmetatable(t2) == prot_mt,
         "debug.getmetatable should bypass __metatable")

  -- Passing nil as mt clears the metatable.
  debug.setmetatable(t, nil)
  assert(debug.getmetatable(t) == nil, "metatable should have been cleared")
end

-- ── debug.sethook / gethook (stubs, should at least not error) ────
do
  debug.sethook()
  local h = debug.gethook()
  assert(h == nil, "gethook stub should return nil")
end

print("All debug library tests passed!")

-- Basic coroutine tests

-- Test 1: coroutine.create and resume with simple body
local co = coroutine.create(function(a, b)
    return a + b
end)
local ok, result = coroutine.resume(co, 10, 20)
assert(ok == true, "resume should succeed")
assert(result == 30, "result should be 30, got " .. tostring(result))

-- Test 2: coroutine.status
local co2 = coroutine.create(function() end)
assert(coroutine.status(co2) == "suspended", "new coroutine should be suspended")
coroutine.resume(co2)
assert(coroutine.status(co2) == "dead", "finished coroutine should be dead")

-- Test 3: yield and resume
local co3 = coroutine.create(function(x)
    local y = coroutine.yield(x + 1)
    return y + 2
end)
local ok1, v1 = coroutine.resume(co3, 10)
assert(ok1 == true)
assert(v1 == 11, "yield value should be 11, got " .. tostring(v1))
assert(coroutine.status(co3) == "suspended")

local ok2, v2 = coroutine.resume(co3, 100)
assert(ok2 == true)
assert(v2 == 102, "return value should be 102, got " .. tostring(v2))
assert(coroutine.status(co3) == "dead")

-- Test 4: multiple yields
local co4 = coroutine.create(function()
    coroutine.yield(1)
    coroutine.yield(2)
    coroutine.yield(3)
    return 4
end)
local _, r1 = coroutine.resume(co4)
local _, r2 = coroutine.resume(co4)
local _, r3 = coroutine.resume(co4)
local _, r4 = coroutine.resume(co4)
assert(r1 == 1)
assert(r2 == 2)
assert(r3 == 3)
assert(r4 == 4)

-- Test 5: resume dead coroutine fails
local ok5, msg5 = coroutine.resume(co4)
assert(ok5 == false)
assert(type(msg5) == "string")

-- Test 6: coroutine.wrap
local gen = coroutine.wrap(function()
    coroutine.yield(10)
    coroutine.yield(20)
    return 30
end)
assert(gen() == 10)
assert(gen() == 20)
assert(gen() == 30)

-- Test 7: coroutine.running
local main_thread, is_main = coroutine.running()
assert(is_main == true, "main thread should report is_main=true")
assert(type(main_thread) == "thread")

local co7 = coroutine.create(function()
    local t, m = coroutine.running()
    assert(m == false, "inside coroutine, is_main should be false")
    assert(type(t) == "thread")
    return t
end)
local ok7, inner_thread = coroutine.resume(co7)
assert(ok7 == true)
assert(inner_thread == co7, "running() should return the coroutine itself")

-- Test 8: coroutine.isyieldable
assert(coroutine.isyieldable() == false, "main thread is not yieldable")
local co8 = coroutine.create(function()
    assert(coroutine.isyieldable() == true, "inside coroutine should be yieldable")
    coroutine.yield()
end)
coroutine.resume(co8)

-- Test 9: coroutine.close
local co9 = coroutine.create(function()
    coroutine.yield()
end)
coroutine.resume(co9) -- suspend it
local ok9 = coroutine.close(co9)
assert(ok9 == true)
assert(coroutine.status(co9) == "dead")

-- Test 10: error in coroutine
local co10 = coroutine.create(function()
    error("oops")
end)
local ok10, err10 = coroutine.resume(co10)
assert(ok10 == false)
assert(coroutine.status(co10) == "dead")

-- Test 11: yield from inside pcall
local co11 = coroutine.create(function()
    local ok = pcall(function()
        coroutine.yield(42)
    end)
    return ok, "done"
end)
local ok11a, v11a = coroutine.resume(co11)
assert(ok11a == true)
assert(v11a == 42, "yield through pcall should work, got " .. tostring(v11a))
local ok11b, v11b, v11c = coroutine.resume(co11)
assert(ok11b == true)
assert(v11b == true, "pcall should return true")
assert(v11c == "done")

-- Test 12: wrap error propagation
local wrapErr = coroutine.wrap(function()
    error("wrap error")
end)
local ok12, err12 = pcall(wrapErr)
assert(ok12 == false, "wrap should propagate error")

-- Test 13: for loop with wrap
local results = {}
local gen13 = coroutine.wrap(function()
    for i = 1, 5 do
        coroutine.yield(i)
    end
end)
for v in gen13 do
    results[#results + 1] = v
end
assert(#results == 5)
assert(results[1] == 1)
assert(results[5] == 5)

-- Test 14: multiple return values from yield
local co14 = coroutine.create(function()
    coroutine.yield(1, 2, 3)
    return 4, 5
end)
local ok14, a, b, c = coroutine.resume(co14)
assert(ok14 and a == 1 and b == 2 and c == 3)
local ok14b, d, e = coroutine.resume(co14)
assert(ok14b and d == 4 and e == 5)

print("All coroutine tests passed!")

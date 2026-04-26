-- M2.4: Vararg & Multi-return Tests
print("=== M2.4 Vararg & Multi-return Tests ===")

-- Basic vararg
print("testing basic vararg...")
local function f(...)
    return ...
end
local a, b, c = f(10, 20, 30)
assert(a == 10 and b == 20 and c == 30, "basic vararg forwarding")

-- Vararg count
local function count(...)
    return select('#', ...)
end
assert(count() == 0, "count 0")
assert(count(1) == 1, "count 1")
assert(count(1,2,3) == 3, "count 3")
assert(count(nil, nil) == 2, "count nil nil")

-- select with numeric index
local function sel(n, ...)
    return select(n, ...)
end
local x, y = sel(2, 'a', 'b', 'c')
assert(x == 'b' and y == 'c', "select 2")
local x2 = sel(3, 'a', 'b', 'c')
assert(x2 == 'c', "select 3")

-- Named vararg table
print("testing named vararg table...")
local function g(... args)
    assert(args.n == 3, "named vararg n field")
    assert(args[1] == 10, "named vararg [1]")
    assert(args[2] == 20, "named vararg [2]")
    assert(args[3] == 30, "named vararg [3]")
end
g(10, 20, 30)

local function g2(... args)
    assert(args.n == 0, "empty named vararg")
end
g2()

-- Named vararg with regular params
local function h(a, b, ... rest)
    assert(a == 1, "param a")
    assert(b == 2, "param b")
    assert(rest.n == 2, "rest n")
    assert(rest[1] == 3, "rest[1]")
    assert(rest[2] == 4, "rest[2]")
end
h(1, 2, 3, 4)

-- ... expression still works with named vararg
local function h2(... args)
    local a, b, c = ...
    assert(a == 1 and b == 2 and c == 3, "... with named vararg")
end
h2(1, 2, 3)

-- Vararg in table constructor
print("testing vararg in table constructor...")
local function pack(...)
    return {...}
end
local t = pack(10, 20, 30)
assert(t[1] == 10, "pack [1]")
assert(t[2] == 20, "pack [2]")
assert(t[3] == 30, "pack [3]")
assert(#t == 3, "pack len")

-- Multi-return in assignment
print("testing multi-return in assignment...")
local function multi() return 1, 2, 3 end
local a1, b1, c1 = multi()
assert(a1 == 1 and b1 == 2 and c1 == 3, "multi-return in local")

a1, b1, c1 = multi()
assert(a1 == 1 and b1 == 2 and c1 == 3, "multi-return in assign")

-- Multi-return truncated when not last
local a2, b2 = multi(), "x"
assert(a2 == 1 and b2 == "x", "multi-return truncated")

-- Multi-return in table constructor
print("testing multi-return in table constructor...")
local t2 = {multi()}
assert(t2[1] == 1 and t2[2] == 2 and t2[3] == 3, "multi-return in table ctor")

local t3 = {multi(), "x"}
assert(t3[1] == 1 and t3[2] == "x", "multi-return truncated in ctor")

-- Multi-return in function call args
print("testing multi-return in function args...")
local function sum3(a, b, c) return a + b + c end
assert(sum3(multi()) == 6, "multi-return as call args")

-- Generic for with multi-return iterator
print("testing generic for...")
local sum = 0
for i, v in ipairs({10, 20, 30}) do
    sum = sum + v
end
assert(sum == 60, "ipairs sum")

-- pairs
local keys = {}
local vals = {}
local pt = {a = 1, b = 2}
for k, v in pairs(pt) do
    keys[#keys + 1] = k
    vals[#vals + 1] = v
end
assert(#keys == 2, "pairs keys count")

-- Vararg forwarding through multiple functions
print("testing vararg forwarding...")
local function wrap(...)
    return f(...)
end
local r1, r2, r3 = wrap(100, 200, 300)
assert(r1 == 100 and r2 == 200 and r3 == 300, "vararg forwarding")

-- Vararg in return
print("testing vararg in return...")
local function retvar(...)
    return ...
end
local x1, x2, x3 = retvar(7, 8, 9)
assert(x1 == 7 and x2 == 8 and x3 == 9, "vararg return")

-- Vararg with excess adjustment
local function adj(a, b, ...)
    return a, b, ...
end
local p, q, r = adj(1, 2, 3, 4)
assert(p == 1 and q == 2 and r == 3, "vararg adjust")

print("=== All M2.4 tests passed! ===")

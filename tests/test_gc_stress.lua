-- GC stress test: allocate lots of temporary tables and strings
-- to force multiple collection cycles.

-- Create many temporary tables in a loop (should be collected)
local sum = 0
for i = 1, 10000 do
    local t = {i, i*2, i*3}
    sum = sum + t[1] + t[2] + t[3]
end
print("table stress sum:", sum)
assert(sum == 300030000, "table stress failed")

-- String concatenation stress (creates many intermediate strings)
local s = ""
for i = 1, 100 do
    s = s .. "x"
end
print("string length:", #s)
assert(#s == 100, "string stress failed")

-- Closure allocation stress
local function make_counter(start)
    local n = start
    return function()
        n = n + 1
        return n
    end
end

local total = 0
for i = 1, 1000 do
    local c = make_counter(i)
    total = total + c() + c() + c()
end
print("closure stress total:", total)

-- Nested table stress: tables referencing other tables
local root = {}
for i = 1, 100 do
    root[i] = {value = i, children = {}}
    if i > 1 then
        root[i].children[1] = root[i-1]
    end
end
print("nested table root[100].value:", root[100].value)
assert(root[100].value == 100)
assert(root[100].children[1].value == 99)

print("=== GC stress test passed! ===")

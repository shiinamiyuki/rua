-- M2.7 Table Library Tests
local pass = 0
local fail = 0

local function check(name, got, expected)
  if tostring(got) == tostring(expected) then
    pass = pass + 1
  else
    fail = fail + 1
    print("FAIL: " .. name .. " expected=" .. tostring(expected) .. " got=" .. tostring(got))
  end
end

-- table.insert (append)
do
  local t = {1, 2, 3}
  table.insert(t, 4)
  check("insert append", #t, 4)
  check("insert append val", t[4], 4)
end

-- table.insert (at position)
do
  local t = {1, 2, 3}
  table.insert(t, 2, 10)
  check("insert pos len", #t, 4)
  check("insert pos [1]", t[1], 1)
  check("insert pos [2]", t[2], 10)
  check("insert pos [3]", t[3], 2)
  check("insert pos [4]", t[4], 3)
end

-- table.remove (last)
do
  local t = {10, 20, 30}
  local r = table.remove(t)
  check("remove last val", r, 30)
  check("remove last len", #t, 2)
end

-- table.remove (at position)
do
  local t = {10, 20, 30}
  local r = table.remove(t, 2)
  check("remove pos val", r, 20)
  check("remove pos len", #t, 2)
  check("remove pos [1]", t[1], 10)
  check("remove pos [2]", t[2], 30)
end

-- table.concat
do
  local t = {"hello", "world", "foo"}
  check("concat", table.concat(t, ", "), "hello, world, foo")
  check("concat no sep", table.concat(t), "helloworldfoo")
  check("concat range", table.concat(t, "-", 2, 3), "world-foo")
end

-- table.concat with numbers
do
  local t = {1, 2, 3}
  check("concat nums", table.concat(t, "+"), "1+2+3")
end

-- table.move
do
  local a = {1, 2, 3, 4, 5}
  table.move(a, 3, 5, 1)
  check("move [1]", a[1], 3)
  check("move [2]", a[2], 4)
  check("move [3]", a[3], 5)
end

-- table.move between tables
do
  local a = {1, 2, 3}
  local b = {10, 20, 30}
  table.move(a, 1, 3, 2, b)
  check("move to b [1]", b[1], 10)
  check("move to b [2]", b[2], 1)
  check("move to b [3]", b[3], 2)
  check("move to b [4]", b[4], 3)
end

-- table.pack
do
  local t = table.pack(10, 20, 30)
  check("pack n", t.n, 3)
  check("pack [1]", t[1], 10)
  check("pack [2]", t[2], 20)
  check("pack [3]", t[3], 30)
end

-- table.unpack
do
  local t = {10, 20, 30}
  local a, b, c = table.unpack(t)
  check("unpack a", a, 10)
  check("unpack b", b, 20)
  check("unpack c", c, 30)
end

-- table.unpack with range
do
  local t = {10, 20, 30, 40, 50}
  local a, b, c = table.unpack(t, 2, 4)
  check("unpack range a", a, 20)
  check("unpack range b", b, 30)
  check("unpack range c", c, 40)
end

-- table.sort (numbers)
do
  local t = {3, 1, 4, 1, 5, 9, 2, 6}
  table.sort(t)
  check("sort [1]", t[1], 1)
  check("sort [2]", t[2], 1)
  check("sort [3]", t[3], 2)
  check("sort [4]", t[4], 3)
  check("sort [5]", t[5], 4)
  check("sort [6]", t[6], 5)
  check("sort [7]", t[7], 6)
  check("sort [8]", t[8], 9)
end

-- table.sort (strings)
do
  local t = {"banana", "apple", "cherry"}
  table.sort(t)
  check("sort str [1]", t[1], "apple")
  check("sort str [2]", t[2], "banana")
  check("sort str [3]", t[3], "cherry")
end

-- table.create
do
  local t = table.create(10, 5)
  check("create type", type(t), "table")
  check("create len", #t, 0)
end

-- Summary
print(string.format("=== Table tests: %d passed, %d failed ===", pass, fail))
if fail > 0 then
  error("Some tests failed!")
end

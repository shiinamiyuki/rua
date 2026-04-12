-- Rua test: basic features
print("=== Hello from Rua! ===")

-- Arithmetic
print("1 + 2 =", 1 + 2)
print("10 - 3 =", 10 - 3)
print("6 * 7 =", 6 * 7)
print("10 / 3 =", 10 / 3)
print("10 // 3 =", 10 // 3)
print("10 % 3 =", 10 % 3)
print("2 ^ 10 =", 2 ^ 10)

-- Variables and control flow
local x = 42
print("x =", x)

if x > 40 then
    print("x is big")
else
    print("x is small")
end

-- While loop
local sum = 0
local i = 1
while i <= 10 do
    sum = sum + i
    i = i + 1
end
print("sum 1..10 =", sum)

-- Numeric for loop
print("counting:")
for i = 1, 5 do
    print("  i =", i)
end

-- Functions
function factorial(n)
    if n <= 1 then
        return 1
    end
    return n * factorial(n - 1)
end
print("5! =", factorial(5))
print("10! =", factorial(10))

-- Fibonacci
local function fib(n)
    if n <= 1 then return n end
    return fib(n - 1) + fib(n - 2)
end
print("fib(10) =", fib(10))
print("fib(20) =", fib(20))

-- Closures
function make_counter()
    local count = 0
    return function()
        count = count + 1
        return count
    end
end

local c = make_counter()
print("counter:", c(), c(), c())

-- String concatenation
local greeting = "Hello" .. ", " .. "World" .. "!"
print(greeting)

-- Tables
local t = {10, 20, 30, 40, 50}
print("table:", t[1], t[2], t[3], t[4], t[5])
print("length:", #t)

-- Type checking
print("type(42) =", type(42))
print("type(3.14) =", type(3.14))
print("type('hi') =", type("hi"))
print("type(true) =", type(true))
print("type(nil) =", type(nil))
print("type({}) =", type({}))
print("type(print) =", type(print))

-- Multiple return values
function multi()
    return 1, 2, 3
end
local a, b, c = multi()
print("multi:", a, b, c)

-- Boolean logic
print("true and false =", true and false)
print("true or false =", true or false)
print("not true =", not true)

print("=== All tests passed! ===")

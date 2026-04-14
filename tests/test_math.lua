-- M2.8: Math Library Tests
print("=== M2.8 Math Library Tests ===")

-- Constants
print("testing constants...")
assert(type(math.pi) == "number", "pi type")
assert(math.pi > 3.14 and math.pi < 3.15, "pi value")
assert(math.huge == 1/0, "huge")
assert(math.maxinteger > 0, "maxinteger positive")
assert(math.mininteger < 0, "mininteger negative")

-- abs
print("testing abs...")
assert(math.abs(-5) == 5, "abs(-5)")
assert(math.abs(5) == 5, "abs(5)")
assert(math.abs(0) == 0, "abs(0)")
assert(math.abs(-3.5) == 3.5, "abs(-3.5)")
-- integer/float preservation
assert(type(math.abs(-5)) == "number", "abs int type")

-- floor/ceil
print("testing floor/ceil...")
assert(math.floor(3.7) == 3, "floor(3.7)")
assert(math.floor(-3.7) == -4, "floor(-3.7)")
assert(math.floor(3) == 3, "floor(3)")
assert(math.ceil(3.2) == 4, "ceil(3.2)")
assert(math.ceil(-3.2) == -3, "ceil(-3.2)")
assert(math.ceil(3) == 3, "ceil(3)")

-- sqrt
print("testing sqrt...")
assert(math.sqrt(4) == 2.0, "sqrt(4)")
assert(math.sqrt(9) == 3.0, "sqrt(9)")
assert(math.sqrt(0) == 0.0, "sqrt(0)")

-- sin/cos/tan
print("testing trig...")
assert(math.abs(math.sin(0)) < 1e-10, "sin(0)")
assert(math.abs(math.cos(0) - 1) < 1e-10, "cos(0)")
assert(math.abs(math.tan(0)) < 1e-10, "tan(0)")
assert(math.abs(math.sin(math.pi/2) - 1) < 1e-10, "sin(pi/2)")

-- asin/acos/atan
print("testing inverse trig...")
assert(math.abs(math.asin(1) - math.pi/2) < 1e-10, "asin(1)")
assert(math.abs(math.acos(1) - 0) < 1e-10, "acos(1)")
assert(math.abs(math.atan(1) - math.pi/4) < 1e-10, "atan(1)")
assert(math.abs(math.atan(1, 0) - math.pi/2) < 1e-10, "atan(1,0)")

-- deg/rad
print("testing deg/rad...")
assert(math.abs(math.deg(math.pi) - 180) < 1e-10, "deg(pi)")
assert(math.abs(math.rad(180) - math.pi) < 1e-10, "rad(180)")

-- exp/log
print("testing exp/log...")
assert(math.abs(math.exp(0) - 1) < 1e-10, "exp(0)")
assert(math.abs(math.exp(1) - 2.718281828459045) < 1e-10, "exp(1)")
assert(math.abs(math.log(1) - 0) < 1e-10, "log(1)")
assert(math.abs(math.log(math.exp(1)) - 1) < 1e-10, "log(e)")
assert(math.abs(math.log(100, 10) - 2) < 1e-10, "log(100,10)")

-- max/min
print("testing max/min...")
assert(math.max(1, 2, 3) == 3, "max(1,2,3)")
assert(math.max(-1, -2, -3) == -1, "max(-1,-2,-3)")
assert(math.min(1, 2, 3) == 1, "min(1,2,3)")
assert(math.min(-1, -2, -3) == -3, "min(-1,-2,-3)")

-- modf
print("testing modf...")
local i, f = math.modf(3.75)
assert(i == 3, "modf int part")
assert(math.abs(f - 0.75) < 1e-10, "modf frac part")
local i2, f2 = math.modf(-3.75)
assert(i2 == -3, "modf neg int")
assert(math.abs(f2 + 0.75) < 1e-10, "modf neg frac")

-- fmod
print("testing fmod...")
assert(math.fmod(7, 3) == 1, "fmod(7,3)")

-- tointeger
print("testing tointeger...")
assert(math.tointeger(5) == 5, "tointeger(5)")
assert(math.tointeger(5.0) == 5, "tointeger(5.0)")
assert(math.tointeger(5.5) == nil, "tointeger(5.5)")
assert(math.tointeger("x") == nil, "tointeger(string)")

-- math.type
print("testing math.type...")
assert(math.type(1) == "integer", "math.type(1)")
assert(math.type(1.0) == "float", "math.type(1.0)")
assert(math.type("x") == nil, "math.type(string)")

-- ult
print("testing ult...")
assert(math.ult(1, 2) == true, "ult(1,2)")
assert(math.ult(2, 1) == false, "ult(2,1)")
assert(math.ult(-1, 1) == false, "ult(-1,1) unsigned")

-- random
print("testing random...")
math.randomseed(42)
local r1 = math.random()
assert(r1 >= 0 and r1 < 1, "random [0,1)")
local r2 = math.random(10)
assert(r2 >= 1 and r2 <= 10, "random(10)")
local r3 = math.random(5, 10)
assert(r3 >= 5 and r3 <= 10, "random(5,10)")

-- frexp/ldexp
print("testing frexp/ldexp...")
local m, e = math.frexp(8.0)
assert(math.abs(m - 0.5) < 1e-10, "frexp mantissa")
assert(e == 4, "frexp exponent")
assert(math.abs(math.ldexp(m, e) - 8.0) < 1e-10, "ldexp roundtrip")

print("=== All M2.8 Math tests passed! ===")

-- M2.6 string.pack / string.unpack / string.packsize tests
local pack = string.pack
local unpack = string.unpack
local packsize = string.packsize

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

local function checkerror(name, msg, f, ...)
  local ok, err = pcall(f, ...)
  if ok or not tostring(err):find(msg) then
    fail = fail + 1
    print("FAIL: " .. name .. " expected error matching " .. msg .. " got " .. tostring(err))
  else
    pass = pass + 1
  end
end

-- integer formats
check("B", unpack("B", pack("B", 0xff)), 0xff)
check("b max", unpack("b", pack("b", 0x7f)), 0x7f)
check("b min", unpack("b", pack("b", -0x80)), -0x80)
check("H", unpack("H", pack("H", 0xffff)), 0xffff)
check("h min", unpack("h", pack("h", -0x8000)), -0x8000)
check("j", unpack("j", pack("j", math.maxinteger)), math.maxinteger)
check("J sign", unpack("<J", pack("<j", -1)), -1)

-- sized integers, both endians, 1..16 bytes
local sizeLI = packsize("j")
for i = 1, 16 do
  local s = string.rep("\xff", i)
  check("i" .. i .. " -1", pack("i" .. i, -1), s)
  check("i" .. i .. " size", packsize("i" .. i), #s)
  check("i" .. i .. " unpack", unpack("i" .. i, s), -1)

  s = "\xAA" .. string.rep("\0", i - 1)
  check("<I" .. i, pack("<I" .. i, 0xAA), s)
  check("<I" .. i .. " unpack", unpack("<I" .. i, s), 0xAA)
  check(">I" .. i, pack(">I" .. i, 0xAA), s:reverse())
  check(">I" .. i .. " unpack", unpack(">I" .. i, s:reverse()), 0xAA)
end

-- sign extension / oversized integers
check("sign ext", unpack("<i2", "\xf0\xff"), -16)
check("oversize i9", unpack("<i9", pack("<j", -1) .. "\xff"), -1)
check("oversize I9", unpack("<I9", pack("<j", -1) .. "\0"), -1)
checkerror("i16 overflow", "16%-byte integer", unpack, "i16", string.rep("\3", 16))
checkerror("unsigned overflow", "does not fit", unpack, "<I9", ("\x00"):rep(8) .. "\1")

-- pack overflow checks
checkerror("pack signed overflow", "integer overflow", pack, ">i1", 128)
checkerror("pack unsigned overflow", "unsigned overflow", pack, "<I1", -1)
checkerror("pack unsigned overflow 2", "unsigned overflow", pack, ">I1", 256)

-- mixed endianness
check("mixed", pack(">i2 <i2", 10, 20), "\0\10\20\0")
local a, b = unpack("<i2 >i2", "\10\0\0\20")
check("mixed a", a, 10)
check("mixed b", b, 20)
check("native =", pack("=i4", 2001), pack("i4", 2001))

-- floats
for _, n in ipairs{0, -1.1, 1.9, 1e20, -1e20, 0.1, 2000.7} do
  check("n " .. tostring(n), unpack("n", pack("n", n)), n)
  check("f reverse " .. tostring(n), pack("<f", n), pack(">f", n):reverse())
  check("d reverse " .. tostring(n), pack(">d", n), pack("<d", n):reverse())
end
check("f roundtrip", unpack("<f", pack("<f", 2000.25)), 2000.25)
check("inf", unpack("d", pack("d", 1/0)), 1/0)

-- strings
do
  local s = string.rep("abc", 1000)
  check("zB", pack("zB", s, 247), s .. "\0\xF7")
  local s1, b1 = unpack("zB", s .. "\0\xF9")
  check("zB unpack str", s1, s)
  check("zB unpack byte", b1, 249)
  check("s default", unpack("s", pack("s", s)), s)
  for i = 2, 16 do
    local p = pack("s" .. i, s)
    check("s" .. i .. " roundtrip", unpack("s" .. i, p), s)
    check("s" .. i .. " size", #p, #s + i)
  end
  checkerror("s1 too small", "does not fit", pack, "s1", s)
  checkerror("z zeros", "contains zeros", pack, "z", "alo\0")
  checkerror("z unfinished", "unfinished string", unpack, "zc10000000", "alo")
  checkerror("s too short", "too short", unpack, "s", pack("s", "alo"):sub(1, -2))
  checkerror("c too short", "too short", unpack, "c5", "abcd")
  checkerror("c longer", "longer than", pack, "c3", "1234")
end

-- fixed-size strings
check("c0", pack("c0", ""), "")
check("csize0", packsize("c0"), 0)
check("c0 unpack", (unpack("c0", "")), "")
check("c pad", pack("c8", "123456"), "123456\0\0")
check("c exact", pack("c3", "123"), "123")

-- alignment
check("no align", pack(" < i1 i2 ", 2, 3), "\2\3\0")
check("align size", packsize("!xXi16"), 8)
check("align 8", packsize("!8 xXi8"), 8)
check("align 2", packsize("!2 xXi8"), 2)
check("align 16", packsize("!16 xXi16"), 16)
do
  local x = pack(">!8 b Xh i4 i8 c1 Xi8", -12, 100, 200, "\xEC")
  check("align pack", x,
        "\xf4" .. "\0\0\0" ..
        "\0\0\0\100" ..
        "\0\0\0\0\0\0\0\xC8" ..
        "\xEC" .. "\0\0\0\0\0\0\0")
  local a1, b2, c2, d2, pos = unpack(">!8 c1 Xh i4 i8 b Xi8 XI XH", x)
  check("align unpack a", a1, "\xF4")
  check("align unpack b", b2, 100)
  check("align unpack c", c2, 200)
  check("align unpack d", d2, -20)
  check("align unpack pos", pos - 1, #x)
end
checkerror("X invalid", "invalid next option", pack, "X")
checkerror("X invalid 2", "invalid next option", unpack, "XXi", "")
checkerror("X invalid 3", "invalid next option", pack, "Xc1")
checkerror("not power of 2", "not power of 2", pack, "!4i3", 0)

-- invalid formats
checkerror("i0", "out of limits", pack, "i0", 0)
checkerror("i17", "out of limits", pack, "i17", 0)
checkerror("!17", "out of limits", pack, "!17", 0)
checkerror("Xi17", "out of limits", pack, "Xi17", 0)
checkerror("bad option", "invalid format option 'r'", pack, "i3r", 0)
checkerror("missing c size", "missing size", pack, "c", "")
checkerror("packsize s", "variable%-length format", packsize, "s")
checkerror("packsize z", "variable%-length format", packsize, "z")
checkerror("s100", "out of limits", pack, "s100", "alo")

-- initial position (1-based, negative from end)
do
  local x = pack("i4i4i4i4", 1, 2, 3, 4)
  for pos = 1, 16, 4 do
    local i, p = unpack("i4", x, pos)
    check("pos " .. pos, i, pos // 4 + 1)
    check("pos return " .. pos, p, pos + 4)
  end
  local i4, p4 = unpack("!4 i4", x, -4)
  check("neg pos", i4, 4)
  check("neg pos return", p4, 17)
  for i = 1, #x + 1 do
    check("c0 pos " .. i, (unpack("c0", x, i)), "")
  end
  checkerror("pos out of string", "out of string", unpack, "c0", x, #x + 2)
end

-- multiple types in sequence
do
  local x = pack("<b h b f d f n i", 1, 2, 3, 4, 5, 6, 7, 8)
  check("sequence size", #x, packsize("<b h b f d f n i"))
  local a1, b1, c1, d1, e1, f1, g1, h1 = unpack("<b h b f d f n i", x)
  check("sequence 1", a1, 1)
  check("sequence 2", b1, 2)
  check("sequence 3", c1, 3)
  check("sequence 4", d1, 4)
  check("sequence 5", e1, 5)
  check("sequence 6", f1, 6)
  check("sequence 7", g1, 7)
  check("sequence 8", h1, 8)
end

-- Regression: calls inside a concat chain must place their arguments
-- immediately above the call base (used to corrupt arg registers).
check("concat call 1", "a" .. string.rep("b", 2) .. "c", "abbc")
check("concat call 2", string.rep("b", 2) .. "c", "bbc")
check("concat call 3", string.rep("b", 2) .. string.rep("c", 2), "bbcc")
check("concat call 4", "x" .. string.sub("hello", 2) .. "y", "xelloy")

-- summary
print(string.format("=== Pack tests: %d passed, %d failed ===", pass, fail))
if fail > 0 then
  error("Some tests failed!")
end

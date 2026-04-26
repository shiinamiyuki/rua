-- M2.6 String Library Tests
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

-- string.len
check("len", string.len("hello"), 5)
check("len empty", string.len(""), 0)
check("len method", ("abc"):len(), 3)

-- string.upper / string.lower
check("upper", string.upper("hello"), "HELLO")
check("lower", string.lower("WORLD"), "world")
check("upper method", ("abc"):upper(), "ABC")

-- string.sub
check("sub 1,3", string.sub("hello", 1, 3), "hel")
check("sub 2", string.sub("hello", 2), "ello")
check("sub -3", string.sub("hello", -3), "llo")
check("sub neg", string.sub("hello", -3, -1), "llo")

-- string.byte / string.char
check("byte", string.byte("A"), 65)
check("byte 2", string.byte("ABC", 2), 66)
check("char", string.char(65, 66, 67), "ABC")
check("char single", string.char(48), "0")

-- string.rep
check("rep", string.rep("ab", 3), "ababab")
check("rep sep", string.rep("ab", 3, "-"), "ab-ab-ab")
check("rep 0", string.rep("x", 0), "")
check("rep 1", string.rep("xy", 1), "xy")

-- string.reverse
check("reverse", string.reverse("hello"), "olleh")
check("reverse empty", string.reverse(""), "")

-- string.format
check("fmt %d", string.format("%d", 42), "42")
check("fmt %s", string.format("%s", "hi"), "hi")
check("fmt %f", string.format("%.2f", 3.14159), "3.14")
check("fmt %x", string.format("%x", 255), "ff")
check("fmt %X", string.format("%X", 255), "FF")
check("fmt %o", string.format("%o", 8), "10")
check("fmt %%", string.format("%%"), "%")
check("fmt %q", string.format("%q", 'she said "hi"'), '"she said \\"hi\\""')
check("fmt %c", string.format("%c", 65), "A")
check("fmt multi", string.format("%d + %d = %d", 1, 2, 3), "1 + 2 = 3")
check("fmt neg", string.format("%d", -5), "-5")

-- string.find (plain)
do
  local s, e = string.find("hello world", "world", 1, true)
  check("find plain start", s, 7)
  check("find plain end", e, 11)
end
do
  local r = string.find("hello", "xyz", 1, true)
  check("find plain nil", r, nil)
end

-- string.find (pattern)
do
  local s, e = string.find("hello world", "%a+")
  check("find pat start", s, 1)
  check("find pat end", e, 5)
end
do
  local s, e = string.find("hello world", "(%a+)", 7)
  check("find pat init start", s, 7)
  check("find pat init end", e, 11)
end

-- string.match
check("match word", string.match("hello world", "%a+"), "hello")
do
  local a, b = string.match("hello world", "(%a+)%s+(%a+)")
  check("match cap1", a, "hello")
  check("match cap2", b, "world")
end
check("match digits", string.match("abc123def", "%d+"), "123")
check("match nil", string.match("hello", "%d+"), nil)

-- string.gmatch
do
  local words = {}
  for w in string.gmatch("hello world foo", "%a+") do
    words[#words + 1] = w
  end
  check("gmatch count", #words, 3)
  check("gmatch 1", words[1], "hello")
  check("gmatch 2", words[2], "world")
  check("gmatch 3", words[3], "foo")
end

-- string.gsub
do
  local r, n = string.gsub("hello world", "(%a+)", "%1-%1")
  check("gsub result", r, "hello-hello world-world")
  check("gsub count", n, 2)
end
do
  local r, n = string.gsub("hello world", "%a+", "X", 1)
  check("gsub max", r, "X world")
  check("gsub max count", n, 1)
end

-- Pattern character classes
check("pat %d", string.match("abc123", "%d+"), "123")
check("pat %a", string.match("123abc", "%a+"), "abc")
check("pat %w", string.match("!@hello123!!", "%w+"), "hello123")
check("pat %s", string.match("hello  world", "%s+"), "  ")
check("pat %l", string.match("HELLOworld", "%l+"), "world")
check("pat %u", string.match("helloWORLD", "%u+"), "WORLD")
check("pat %p", string.match("hello!@#world", "%p+"), "!@#")

-- Pattern anchors
check("pat ^", string.match("hello", "^hel"), "hel")
check("pat ^ fail", string.match("hello", "^ell"), nil)
check("pat $", string.match("hello", "llo$"), "llo")

-- Character sets
check("set [aeiou]", string.match("hello", "[aeiou]+"), "e")
check("set [^aeiou]", string.match("hello", "[^aeiou]+"), "h")
check("set range", string.match("abc123", "[0-9]+"), "123")

-- Quantifiers
check("quant *", string.match("aabbb", "b*"), "")  -- matches empty at start
check("quant +", string.match("aabbb", "b+"), "bbb")
check("quant ?", string.match("colour", "colou?r"), "colour")
check("quant -", string.match("  hello  ", "^%s-(%a+)"), "hello")

-- Method syntax
check("method find", ("hello"):find("llo"), 3)
check("method upper", ("hello"):upper(), "HELLO")
check("method sub", ("hello"):sub(2, 4), "ell")

-- Summary
print(string.format("=== String tests: %d passed, %d failed ===", pass, fail))
if fail > 0 then
  error("Some tests failed!")
end

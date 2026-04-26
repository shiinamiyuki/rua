-- Tests for io and os libraries

-- ── os.clock ───────────────────────────────────────────────────────
do
  local t = os.clock()
  assert(type(t) == "number")
  assert(t >= 0)
end

-- ── os.time ────────────────────────────────────────────────────────
do
  local t = os.time()
  assert(type(t) == "number")
  assert(t > 1000000000) -- after 2001
end

-- ── os.difftime ────────────────────────────────────────────────────
do
  local t1 = os.time()
  local t2 = t1 + 100
  assert(os.difftime(t2, t1) == 100.0)
end

-- ── os.date ────────────────────────────────────────────────────────
do
  local d = os.date("!%Y")
  assert(type(d) == "string")
  -- Year should be 4 digits
  assert(#d == 4)
  local yr = tonumber(d)
  assert(yr >= 2024)
end

-- ── os.date("*t") ─────────────────────────────────────────────────
do
  local t = os.date("!*t")
  assert(type(t) == "table")
  assert(t.year >= 2024)
  assert(t.month >= 1 and t.month <= 12)
  assert(t.day >= 1 and t.day <= 31)
  assert(t.hour >= 0 and t.hour <= 23)
  assert(t.min >= 0 and t.min <= 59)
  assert(t.sec >= 0 and t.sec <= 60)
  assert(t.wday >= 1 and t.wday <= 7)
  assert(t.yday >= 1 and t.yday <= 366)
end

-- ── os.getenv ──────────────────────────────────────────────────────
do
  -- PATH should always be set
  local p = os.getenv("PATH")
  assert(p ~= nil)
  -- Non-existent variable
  local q = os.getenv("THIS_SHOULD_NOT_EXIST_12345")
  assert(q == nil)
end

-- ── os.tmpname ─────────────────────────────────────────────────────
do
  local name = os.tmpname()
  assert(type(name) == "string")
  assert(#name > 0)
end

-- ── os.setlocale ───────────────────────────────────────────────────
do
  local loc = os.setlocale()
  assert(type(loc) == "string")
end

-- ── io.type ────────────────────────────────────────────────────────
do
  assert(io.type(io.stdin) == "file")
  assert(io.type(io.stdout) == "file")
  assert(io.type(io.stderr) == "file")
  assert(io.type(42) == false)
  assert(io.type("hello") == false)
  assert(io.type(nil) == false)
end

-- ── io.open / file:write / file:read / file:close ──────────────────
do
  local tmpfile = os.tmpname() .. "_rua_test.txt"

  -- Write
  local f = io.open(tmpfile, "w")
  assert(f ~= nil)
  f:write("hello\n")
  f:write("world\n")
  f:close()

  -- Read lines
  local f2 = io.open(tmpfile, "r")
  assert(f2 ~= nil)
  local line1 = f2:read("l")
  assert(line1 == "hello", "expected 'hello', got '" .. tostring(line1) .. "'")
  local line2 = f2:read("l")
  assert(line2 == "world", "expected 'world', got '" .. tostring(line2) .. "'")
  local line3 = f2:read("l")
  assert(line3 == nil) -- EOF
  f2:close()

  -- Read with *L (keep newline)
  local f3 = io.open(tmpfile, "r")
  local line = f3:read("L")
  assert(line == "hello\n", "expected 'hello\\n', got '" .. tostring(line) .. "'")
  f3:close()

  -- Read all
  local f4 = io.open(tmpfile, "r")
  local all = f4:read("a")
  assert(all == "hello\nworld\n", "expected full content, got '" .. tostring(all) .. "'")
  f4:close()

  -- Read bytes
  local f5 = io.open(tmpfile, "r")
  local bytes = f5:read(5)
  assert(bytes == "hello", "expected 'hello', got '" .. tostring(bytes) .. "'")
  f5:close()

  -- Clean up
  os.remove(tmpfile)
end

-- ── io.type on closed file ─────────────────────────────────────────
do
  local tmpfile = os.tmpname() .. "_rua_closed.txt"
  local f = io.open(tmpfile, "w")
  f:write("test")
  f:close()
  assert(io.type(f) == "closed file")
  os.remove(tmpfile)
end

-- ── file:seek ──────────────────────────────────────────────────────
do
  local tmpfile = os.tmpname() .. "_rua_seek.txt"
  local f = io.open(tmpfile, "w")
  f:write("abcdefghij")
  f:close()

  local f2 = io.open(tmpfile, "r")
  -- Default seek: "cur", 0
  local pos = f2:seek()
  assert(pos == 0, "expected 0, got " .. tostring(pos))
  -- Seek to offset 5
  pos = f2:seek("set", 5)
  assert(pos == 5)
  local data = f2:read(3)
  assert(data == "fgh", "expected 'fgh', got '" .. tostring(data) .. "'")
  -- Seek from end
  pos = f2:seek("end", -3)
  assert(pos == 7)
  data = f2:read(3)
  assert(data == "hij", "expected 'hij', got '" .. tostring(data) .. "'")
  f2:close()
  os.remove(tmpfile)
end

-- ── file:flush ─────────────────────────────────────────────────────
do
  local tmpfile = os.tmpname() .. "_rua_flush.txt"
  local f = io.open(tmpfile, "w")
  f:write("flush test")
  f:flush()
  f:close()
  local f2 = io.open(tmpfile, "r")
  local content = f2:read("a")
  assert(content == "flush test")
  f2:close()
  os.remove(tmpfile)
end

-- ── io.lines(filename) ─────────────────────────────────────────────
do
  local tmpfile = os.tmpname() .. "_rua_lines.txt"
  local f = io.open(tmpfile, "w")
  f:write("alpha\nbeta\ngamma\n")
  f:close()

  local lines = {}
  for line in io.lines(tmpfile) do
    lines[#lines + 1] = line
  end
  assert(#lines == 3, "expected 3 lines, got " .. #lines)
  assert(lines[1] == "alpha")
  assert(lines[2] == "beta")
  assert(lines[3] == "gamma")
  os.remove(tmpfile)
end

-- ── file:lines() ───────────────────────────────────────────────────
do
  local tmpfile = os.tmpname() .. "_rua_flines.txt"
  local f = io.open(tmpfile, "w")
  f:write("one\ntwo\nthree\n")
  f:close()

  local f2 = io.open(tmpfile, "r")
  local lines = {}
  for line in f2:lines() do
    lines[#lines + 1] = line
  end
  assert(#lines == 3)
  assert(lines[1] == "one")
  assert(lines[2] == "two")
  assert(lines[3] == "three")
  f2:close()
  os.remove(tmpfile)
end

-- ── os.rename ──────────────────────────────────────────────────────
do
  local tmpfile1 = os.tmpname() .. "_rua_rename1.txt"
  local tmpfile2 = os.tmpname() .. "_rua_rename2.txt"
  local f = io.open(tmpfile1, "w")
  f:write("rename test")
  f:close()

  local ok = os.rename(tmpfile1, tmpfile2)
  assert(ok == true)

  local f2 = io.open(tmpfile2, "r")
  local content = f2:read("a")
  assert(content == "rename test")
  f2:close()
  os.remove(tmpfile2)
end

-- ── io.open error ──────────────────────────────────────────────────
do
  local ok, err = pcall(io.open, "/nonexistent/path/file.txt", "r")
  assert(not ok or err == nil) -- Should either error or return nil
end

-- ── Multiple reads ─────────────────────────────────────────────────
do
  local tmpfile = os.tmpname() .. "_rua_multi.txt"
  local f = io.open(tmpfile, "w")
  f:write("line1\nline2\n")
  f:close()

  local f2 = io.open(tmpfile, "r")
  local a, b = f2:read("l", "l")
  assert(a == "line1", "expected 'line1', got '" .. tostring(a) .. "'")
  assert(b == "line2", "expected 'line2', got '" .. tostring(b) .. "'")
  f2:close()
  os.remove(tmpfile)
end

-- ── Write and append ───────────────────────────────────────────────
do
  local tmpfile = os.tmpname() .. "_rua_append.txt"

  -- Write
  local f = io.open(tmpfile, "w")
  f:write("first")
  f:close()

  -- Append
  local f2 = io.open(tmpfile, "a")
  f2:write("second")
  f2:close()

  -- Verify
  local f3 = io.open(tmpfile, "r")
  local content = f3:read("a")
  assert(content == "firstsecond", "expected 'firstsecond', got '" .. tostring(content) .. "'")
  f3:close()
  os.remove(tmpfile)
end

-- ── io.write to stdout ─────────────────────────────────────────────
do
  -- Just verify it doesn't error
  io.write("io.write test output\n")
  io.flush()
end

-- ── os.execute ─────────────────────────────────────────────────────
do
  -- Just check shell availability
  local ok = os.execute()
  assert(ok == true)
end

print("All io/os tests passed!")

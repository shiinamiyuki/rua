-- Test suite for M3.2: Error Handling
local passed = 0
local failed = 0

local function check(name, got, expected)
    if got == expected then
        passed = passed + 1
    else
        failed = failed + 1
        print("FAIL: " .. name)
        print("  expected: " .. tostring(expected))
        print("  got:      " .. tostring(got))
    end
end

local function check_match(name, got, pattern)
    if type(got) == "string" and string.find(got, pattern) then
        passed = passed + 1
    else
        failed = failed + 1
        print("FAIL: " .. name)
        print("  expected pattern: " .. pattern)
        print("  got:              " .. tostring(got))
    end
end

-- ============================================================
-- error() basic
-- ============================================================

-- error with string message
do
    local ok, err = pcall(error, "hello")
    check("error string", ok, false)
    -- error(msg, 1) should annotate with source:line
    check_match("error annotated", err, "test_errors%.lua:%d+: hello")
end

-- error with non-string (no annotation)
do
    local ok, err = pcall(error, 42)
    check("error integer", ok, false)
    check("error integer value", err, 42)
end

-- error with nil
do
    local ok, err = pcall(error, nil)
    check("error nil", ok, false)
    check("error nil value", err, nil)
end

-- error with table
do
    local t = {msg = "oops"}
    local ok, err = pcall(error, t)
    check("error table", ok, false)
    check("error table same", err, t)
end

-- error with boolean
do
    local ok, err = pcall(error, false)
    check("error boolean", ok, false)
    check("error boolean value", err, false)
end

-- error with level 0 (no annotation)
do
    local ok, err = pcall(error, "raw message", 0)
    check("error level 0", ok, false)
    check("error level 0 no annotation", err, "raw message")
end

-- error with level 2 (caller's caller)
do
    local function inner()
        error("from inner", 2)
    end
    local function outer()
        inner()
    end
    local ok, err = pcall(outer)
    check("error level 2", ok, false)
    -- The error should point to the line where outer() calls inner(),
    -- not where inner() calls error()
    check_match("error level 2 annotated", err, "test_errors%.lua:%d+: from inner")
end

-- ============================================================
-- xpcall basic
-- ============================================================

-- xpcall success
do
    local function handler(err)
        return "handled: " .. tostring(err)
    end
    local ok, result = xpcall(function() return 42 end, handler)
    check("xpcall success ok", ok, true)
    check("xpcall success result", result, 42)
end

-- xpcall with error
do
    local function handler(err)
        return "handled: " .. tostring(err)
    end
    local ok, result = xpcall(function() error("boom", 0) end, handler)
    check("xpcall error ok", ok, false)
    check("xpcall error handled", result, "handled: boom")
end

-- xpcall with args
do
    local function handler(err)
        return "caught"
    end
    local ok, a, b = xpcall(function(x, y) return x + y, x * y end, handler, 3, 4)
    check("xpcall args ok", ok, true)
    check("xpcall args a", a, 7)
    check("xpcall args b", b, 12)
end

-- xpcall handler receives error object
do
    local received = nil
    local function handler(err)
        received = err
        return "got it"
    end
    local ok, result = xpcall(function() error(42) end, handler)
    check("xpcall handler receives", ok, false)
    check("xpcall handler got value", received, 42)
    check("xpcall handler result", result, "got it")
end

-- xpcall handler itself errors — returns original error
do
    local function bad_handler(err)
        error("handler broke")
    end
    local ok, result = xpcall(function() error("original", 0) end, bad_handler)
    check("xpcall bad handler ok", ok, false)
    check("xpcall bad handler result", result, "original")
end

-- xpcall with multiple returns on success
do
    local function handler(err) return err end
    local ok, a, b, c = xpcall(function() return 1, 2, 3 end, handler)
    check("xpcall multi ok", ok, true)
    check("xpcall multi a", a, 1)
    check("xpcall multi b", b, 2)
    check("xpcall multi c", c, 3)
end

-- ============================================================
-- Nested pcall/xpcall
-- ============================================================

-- pcall inside pcall
do
    local ok2, err
    local ok1 = pcall(function()
        ok2, err = pcall(function()
            error("inner", 0)
        end)
    end)
    check("nested pcall outer ok", ok1, true)
    check("nested pcall inner ok", ok2, false)
    check("nested pcall inner err", err, "inner")
end

-- xpcall inside pcall
do
    local function handler(err)
        return "handled: " .. tostring(err)
    end
    local ok2, result
    local ok1 = pcall(function()
        ok2, result = xpcall(function()
            error("nested", 0)
        end, handler)
    end)
    check("xpcall in pcall outer ok", ok1, true)
    check("xpcall in pcall inner ok", ok2, false)
    check("xpcall in pcall result", result, "handled: nested")
end

-- pcall inside xpcall
do
    local function handler(err)
        return "outer: " .. tostring(err)
    end
    local ok, result = xpcall(function()
        local ok2, err = pcall(function()
            error("inner pcall", 0)
        end)
        -- inner pcall catches it, outer should succeed
        return ok2, err
    end, handler)
    check("pcall in xpcall ok", ok, true)
    check("pcall in xpcall result", result, false)
end

-- ============================================================
-- error position annotation
-- ============================================================

-- error in a function shows correct source
do
    local function fail()
        error("test fail")
    end
    local ok, err = pcall(fail)
    check("error position ok", ok, false)
    -- Should contain file name and a line number
    check_match("error position format", err, "test_errors%.lua:%d+: test fail")
end

-- assert error message
do
    local ok, err = pcall(assert, false, "custom assert")
    check("assert error ok", ok, false)
    check("assert error msg", err, "custom assert")
end

-- ============================================================
-- Edge cases
-- ============================================================

-- error with no arguments
do
    local ok, err = pcall(error)
    check("error no args ok", ok, false)
    check("error no args nil", err, nil)
end

-- pcall with no function (pcall(nil) should return false + error)
do
    local ok, err = pcall(nil)
    check("pcall no func ok", ok, false)
    check_match("pcall no func err", err, "attempt to call a nil value")
end

-- xpcall with non-function handler
do
    local ok, err = xpcall(function() error("test", 0) end, "not a function")
    check("xpcall bad handler type ok", ok, false)
    -- When handler fails, return original error
    check("xpcall bad handler type err", err, "test")
end

-- ============================================================
-- Summary
-- ============================================================
print("=== Error handling tests: " .. passed .. " passed, " .. failed .. " failed ===")

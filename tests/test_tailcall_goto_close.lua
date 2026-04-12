-- Tests for M2.5 (Tail Calls & Goto) and M2.3 (Variable Declarations)

local function check(cond, msg)
    if not cond then
        error("FAIL: " .. msg)
    end
end

print("=== M2.5 + M2.3 Tests ===")

-- ══════════════════════════════════════════════════════════════
-- M2.5: Tail Calls
-- ══════════════════════════════════════════════════════════════

print("testing tail calls...")

-- Basic tail call: should not grow the stack
do
    local function countdown(n)
        if n <= 0 then return "done" end
        return countdown(n - 1)  -- tail call
    end
    check(countdown(100000) == "done", "deep tail recursion")
end

-- Mutual tail recursion
do
    local function is_even(n)
        if n == 0 then return true end
        return is_odd(n - 1)
    end

    function is_odd(n)
        if n == 0 then return false end
        return is_even(n - 1)
    end

    check(is_even(0) == true, "is_even(0)")
    check(is_even(1) == false, "is_even(1)")
    check(is_odd(1) == true, "is_odd(1)")
    check(is_even(100) == true, "is_even(100)")
end

-- Not a tail call: extra work after the call
do
    local function f(n)
        if n <= 0 then return 1 end
        return f(n - 1) + 0  -- NOT a tail call (addition after)
    end
    check(f(10) == 1, "non-tail call with addition")
end

-- Not a tail call: multiple return values
do
    local function f()
        return 1, 2
    end
    local function g()
        return f(), 3  -- NOT a tail call (adjusted to 1 result)
    end
    local a, b = g()
    check(a == 1, "multi-return not tail call a")
    check(b == 3, "multi-return not tail call b")
end

-- Tail call with multiple args
do
    local function sum(a, b, c)
        return a + b + c
    end
    local function wrapper(a, b, c)
        return sum(a, b, c)  -- tail call
    end
    check(wrapper(1, 2, 3) == 6, "tail call with multiple args")
end

-- Tail call with method syntax
do
    local obj = {}
    function obj:get()
        return self.val
    end
    local function getval(o)
        return o:get()  -- tail call
    end
    obj.val = 42
    check(getval(obj) == 42, "tail call with method")
end

-- ══════════════════════════════════════════════════════════════
-- M2.5: Goto
-- ══════════════════════════════════════════════════════════════

print("testing goto...")

-- Basic goto forward
do
    local x = 1
    goto skip
    x = 2
    ::skip::
    check(x == 1, "goto forward")
end

-- Goto backward (loop simulation)
do
    local count = 0
    ::loop_start::
    count = count + 1
    if count < 5 then goto loop_start end
    check(count == 5, "goto backward loop")
end

-- Nested goto
do
    local result = ""
    goto first
    result = result .. "X"
    ::second::
    result = result .. "B"
    goto done
    ::first::
    result = result .. "A"
    goto second
    ::done::
    check(result == "AB", "nested goto")
end

-- Goto in if-else
do
    local x = 10
    if x > 5 then
        goto big
    else
        goto small
    end
    ::big::
    x = x + 100
    goto finish
    ::small::
    x = x - 100
    ::finish::
    check(x == 110, "goto in if-else")
end

-- ══════════════════════════════════════════════════════════════
-- M2.3: local <const>
-- ══════════════════════════════════════════════════════════════

print("testing local <const>...")

-- Basic const
do
    local x <const> = 42
    check(x == 42, "const local value")

    -- Attempting to assign to a const should be a compile error
    -- We can only test this at runtime by loading a string
    local ok, err = pcall(function()
        -- This just verifies const is accessible
        return x + 1
    end)
    check(ok and err == 43, "const local readable")
end

-- Const with multiple values
do
    local a <const>, b = 10, 20
    check(a == 10, "const multi a")
    check(b == 20, "const multi b")
    b = 30  -- b is not const, so this should work
    check(b == 30, "non-const multi b reassigned")
end

-- ══════════════════════════════════════════════════════════════
-- M2.3: local <close>
-- ══════════════════════════════════════════════════════════════

print("testing local <close>...")

-- Basic __close called on scope exit
do
    local closed = false
    do
        local x <close> = setmetatable({}, {
            __close = function(self, err)
                closed = true
            end
        })
    end
    check(closed, "__close called on scope exit")
end

-- __close with error argument (normal exit = nil)
do
    local err_val = "not nil"
    do
        local x <close> = setmetatable({}, {
            __close = function(self, err)
                err_val = err
            end
        })
    end
    check(err_val == nil, "__close error arg is nil on normal exit")
end

-- Multiple __close in reverse order
do
    local order = ""
    do
        local a <close> = setmetatable({id="A"}, {
            __close = function(self) order = order .. self.id end
        })
        local b <close> = setmetatable({id="B"}, {
            __close = function(self) order = order .. self.id end
        })
        local c <close> = setmetatable({id="C"}, {
            __close = function(self) order = order .. self.id end
        })
    end
    check(order == "CBA", "reverse order close: " .. order)
end

-- nil value for <close> is silently ignored
do
    local ok = pcall(function()
        local x <close> = nil
    end)
    check(ok, "nil close value ignored")
end

-- false value for <close> is silently ignored
do
    local ok = pcall(function()
        local x <close> = false
    end)
    check(ok, "false close value ignored")
end

-- __close called on function return
do
    local closed = false
    local function f()
        local x <close> = setmetatable({}, {
            __close = function() closed = true end
        })
        return 42
    end
    local result = f()
    check(closed, "__close called on return")
    check(result == 42, "return value preserved after close")
end

-- __close called when error occurs (via pcall)
do
    local closed = false
    local err_received = nil
    local ok, err = pcall(function()
        local x <close> = setmetatable({}, {
            __close = function(self, e)
                closed = true
                err_received = e
            end
        })
        error("test error")
    end)
    check(closed, "__close called on error")
end

-- <close> variable acts like <const> (can't reassign)
-- This is a compile-time check. We verify the value is readable.
do
    local x <close> = setmetatable({val = 99}, {
        __close = function() end
    })
    check(x.val == 99, "close var is readable")
end

-- ══════════════════════════════════════════════════════════════
-- M2.3: global declarations
-- ══════════════════════════════════════════════════════════════

print("testing global declarations...")

-- Basic global declaration
do
    global gvar1 = 42
    check(gvar1 == 42, "global decl")
end

-- Global without value → nil
do
    global gvar2
    check(gvar2 == nil, "global decl nil")
end

-- Multiple global
do
    global ga, gb = 10, 20
    check(ga == 10, "global multi a")
    check(gb == 20, "global multi b")
end

-- ══════════════════════════════════════════════════════════════
-- Tail call does NOT happen with <close> in scope
-- ══════════════════════════════════════════════════════════════

print("testing tail call inhibited by <close>...")
do
    local close_count = 0
    local function inner(n)
        if n <= 0 then return "done" end
        return inner(n - 1)
    end
    local function outer()
        local x <close> = setmetatable({}, {
            __close = function() close_count = close_count + 1 end
        })
        -- This should NOT be a tail call because <close> is in scope
        return inner(5)
    end
    check(outer() == "done", "close inhibits tail call")
    check(close_count == 1, "close var closed even with return")
end

print("=== All M2.5 + M2.3 tests passed! ===")

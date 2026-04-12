-- Comprehensive metamethod tests

-- Helper
local function check(cond, msg)
    if not cond then
        error("FAIL: " .. msg)
    end
end

print("=== Metamethod Tests ===")

-- ── __index (function) ────────────────────────────────────────
print("testing __index...")
do
    local t = setmetatable({}, {
        __index = function(self, key)
            return key .. "!"
        end
    })
    check(t.hello == "hello!", "__index function")
    check(t.world == "world!", "__index function 2")

    -- Existing keys bypass __index
    t.x = 42
    check(t.x == 42, "__index skipped for existing key")
end

-- ── __index (table chain) ─────────────────────────────────────
do
    local base = { foo = 10, bar = 20 }
    local derived = setmetatable({}, { __index = base })
    check(derived.foo == 10, "__index table chain")
    check(derived.bar == 20, "__index table chain 2")
    check(derived.baz == nil, "__index table chain nil")
end

-- ── __newindex (function) ─────────────────────────────────────
print("testing __newindex...")
do
    local log = {}
    local t = setmetatable({}, {
        __newindex = function(self, key, value)
            log[#log + 1] = key .. "=" .. tostring(value)
            rawset(self, key, value)
        end
    })
    t.x = 10
    t.y = 20
    check(log[1] == "x=10", "__newindex log 1")
    check(log[2] == "y=20", "__newindex log 2")
    -- Existing key: __newindex not called
    t.x = 30
    check(#log == 2, "__newindex not called for existing key")
    check(t.x == 30, "__newindex existing key updated")
end

-- Helper to extract val from tables or return raw value
local function getval(x)
    if type(x) == "table" then return rawget(x, "val") or 0 end
    return x
end

-- ── __add ─────────────────────────────────────────────────────
print("testing __add...")
do
    local mt = {
        __add = function(a, b) return getval(a) + getval(b) end
    }
    local a = setmetatable({val = 10}, mt)
    local b = setmetatable({val = 20}, mt)
    check(a + b == 30, "__add table + table")
    check(a + 5 == 15, "__add table + number")
    check(5 + a == 15, "__add number + table")
end

-- ── __sub ─────────────────────────────────────────────────────
print("testing __sub...")
do
    local mt = { __sub = function(a, b) return getval(a) - getval(b) end }
    local a = setmetatable({val = 30}, mt)
    check(a - 10 == 20, "__sub")
end

-- ── __mul ─────────────────────────────────────────────────────
print("testing __mul...")
do
    local mt = { __mul = function(a, b) return getval(a) * getval(b) end }
    local a = setmetatable({val = 6}, mt)
    check(a * 7 == 42, "__mul")
end

-- ── __div ─────────────────────────────────────────────────────
print("testing __div...")
do
    local mt = { __div = function(a, b) return getval(a) / getval(b) end }
    local a = setmetatable({val = 10}, mt)
    check(a / 2 == 5.0, "__div")
end

-- ── __mod ─────────────────────────────────────────────────────
print("testing __mod...")
do
    local mt = { __mod = function(a, b) return getval(a) % getval(b) end }
    local a = setmetatable({val = 10}, mt)
    check(a % 3 == 1, "__mod")
end

-- ── __pow ─────────────────────────────────────────────────────
print("testing __pow...")
do
    local mt = { __pow = function(a, b) return getval(a) ^ getval(b) end }
    local a = setmetatable({val = 2}, mt)
    check(a ^ 10 == 1024.0, "__pow")
end

-- ── __unm ─────────────────────────────────────────────────────
print("testing __unm...")
do
    local mt = {
        __unm = function(a)
            return -a.val
        end
    }
    local a = setmetatable({val = 42}, mt)
    check(-a == -42, "__unm")
end

-- ── __idiv ────────────────────────────────────────────────────
print("testing __idiv...")
do
    local mt = { __idiv = function(a, b) return getval(a) // getval(b) end }
    local a = setmetatable({val = 10}, mt)
    check(a // 3 == 3, "__idiv")
end

-- ── __band, __bor, __bxor, __bnot, __shl, __shr ──────────────
print("testing bitwise metamethods...")
do
    local mt = {
        __band = function(a, b) return getval(a) & getval(b) end,
        __bor  = function(a, b) return getval(a) | getval(b) end,
        __bxor = function(a, b) return getval(a) ~ getval(b) end,
        __bnot = function(a) return ~a.val end,
        __shl  = function(a, b) return getval(a) << getval(b) end,
        __shr  = function(a, b) return getval(a) >> getval(b) end,
    }
    local a = setmetatable({val = 0xFF}, mt)
    check((a & 0x0F) == 0x0F, "__band")
    check((a | 0x100) == 0x1FF, "__bor")
    check((a ~ 0xFF) == 0, "__bxor")
    check((~a) == ~0xFF, "__bnot")
    check((a << 4) == 0xFF0, "__shl")
    check((a >> 4) == 0x0F, "__shr")
end

-- ── __concat ──────────────────────────────────────────────────
print("testing __concat...")
do
    local mt = {
        __concat = function(a, b)
            local sa = type(a) == "table" and a.val or a
            local sb = type(b) == "table" and b.val or b
            return sa .. sb
        end
    }
    local a = setmetatable({val = "hello"}, mt)
    check(a .. " world" == "hello world", "__concat table .. string")
    check("say " .. a == "say hello", "__concat string .. table")
end

-- ── __len ─────────────────────────────────────────────────────
print("testing __len...")
do
    local mt = {
        __len = function(a)
            return 42
        end
    }
    local a = setmetatable({1, 2, 3}, mt)
    check(#a == 42, "__len")
end

-- ── __eq ──────────────────────────────────────────────────────
print("testing __eq...")
do
    local mt = {
        __eq = function(a, b)
            return a.val == b.val
        end
    }
    local a = setmetatable({val = 10}, mt)
    local b = setmetatable({val = 10}, mt)
    local c = setmetatable({val = 20}, mt)
    check(a == b, "__eq true")
    check(not (a == c), "__eq false")
    check(a ~= c, "__eq ~= true")
end

-- ── __lt and __le ─────────────────────────────────────────────
print("testing __lt and __le...")
do
    local mt = {
        __lt = function(a, b)
            return a.val < b.val
        end,
        __le = function(a, b)
            return a.val <= b.val
        end
    }
    local a = setmetatable({val = 10}, mt)
    local b = setmetatable({val = 20}, mt)
    local c = setmetatable({val = 10}, mt)
    check(a < b, "__lt true")
    check(not (b < a), "__lt false")
    check(a <= c, "__le true (equal)")
    check(a <= b, "__le true (less)")
    check(not (b <= a), "__le false")
end

-- ── __call ────────────────────────────────────────────────────
print("testing __call...")
do
    local mt = {
        __call = function(self, x, y)
            return x + y
        end
    }
    local a = setmetatable({}, mt)
    check(a(3, 4) == 7, "__call")
end

-- ── __metatable ───────────────────────────────────────────────
print("testing __metatable...")
do
    local mt = { __metatable = "protected" }
    local t = setmetatable({}, mt)
    check(getmetatable(t) == "protected", "__metatable returned by getmetatable")
    local ok, err = pcall(setmetatable, t, {})
    check(not ok, "__metatable prevents setmetatable")
end

-- ── String-to-number coercion ─────────────────────────────────
print("testing string-to-number coercion...")
do
    check("10" + 5 == 15, "string + number")
    check(5 + "10" == 15, "number + string")
    check("10" + "20" == 30, "string + string -> number")
    check("10" * 3 == 30, "string * number")
    check("10.5" + 1 == 11.5, "float string + number")
    check(-"5" == -5, "unary minus on string")
end

-- ── Bitwise coercion ──────────────────────────────────────────
print("testing bitwise coercion...")
do
    check("0xFF" & 0x0F == 0x0F, "string & number (hex)")
    check(3.0 & 1 == 1, "float & int")
end

-- ── __index chain (multi-level inheritance) ───────────────────
print("testing __index chain...")
do
    local base = { x = 1 }
    local mid = setmetatable({ y = 2 }, { __index = base })
    local top = setmetatable({ z = 3 }, { __index = mid })
    check(top.x == 1, "3-level __index chain x")
    check(top.y == 2, "3-level __index chain y")
    check(top.z == 3, "3-level __index chain z")
end

-- ── __newindex with table delegation ──────────────────────────
print("testing __newindex table delegation...")
do
    local storage = {}
    local proxy = setmetatable({}, { __newindex = storage })
    proxy.hello = "world"
    check(storage.hello == "world", "__newindex table delegation")
    check(rawget(proxy, "hello") == nil, "__newindex proxy untouched")
end

print("=== All metamethod tests passed! ===")

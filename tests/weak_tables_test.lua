-- M3.3: weak tables tests
-- Run from the workspace root: rua tests/weak_tables_test.lua
--
-- All "drop the local then GC" patterns are wrapped in helper functions
-- so the locals leave their frame entirely. (Slots inside a `do...end`
-- block are still rooted by our register-based VM until the enclosing
-- function returns -- this matches stock Lua behavior on similar VMs.)

-- ── weak values: collected when nothing else holds them ────────────
do
  local t = setmetatable({}, { __mode = "v" })
  local function populate()
    local v = {}
    t[1] = v
    return v  -- caller's choice whether to keep it alive
  end
  local v = populate()
  assert(t[1] == v, "weak value should be live while local holds it")
  v = nil
  collectgarbage("collect")
  assert(t[1] == nil, "weak value should be collected, got " .. tostring(t[1]))
end

-- ── weak values stay if held strongly ──────────────────────────────
do
  local t = setmetatable({}, { __mode = "v" })
  local strong = {}
  t[1] = strong
  collectgarbage("collect")
  assert(t[1] == strong, "weak value held strongly should survive GC")
  strong = nil
  collectgarbage("collect")
  assert(t[1] == nil, "weak value should now be collected")
end

-- ── weak keys: collected when nothing else holds them ─────────────
do
  local t = setmetatable({}, { __mode = "k" })
  local function populate()
    local k1 = {}
    local k2 = {}
    t[k1] = "a"
    t[k2] = "b"
  end
  populate()
  collectgarbage("collect")
  local count = 0
  for _ in pairs(t) do count = count + 1 end
  assert(count == 0, "expected 0 weak-key entries after GC, got " .. count)
end

-- ── weak keys: surviving keys keep their values (ephemeron) ────────
do
  local t = setmetatable({}, { __mode = "k" })
  local k_alive = {}
  local v_alive = {}
  local function populate()
    local k_dead = {}
    t[k_alive] = v_alive
    t[k_dead] = "dies with k_dead"
  end
  populate()
  collectgarbage("collect")
  assert(t[k_alive] == v_alive, "alive key should still map to its value")
  local n = 0
  for _ in pairs(t) do n = n + 1 end
  assert(n == 1, "expected 1 surviving entry, got " .. n)
end

-- ── full weak: kv ──────────────────────────────────────────────────
do
  local t = setmetatable({}, { __mode = "kv" })
  local function populate()
    local k, v = {}, {}
    t[k] = v
  end
  populate()
  collectgarbage("collect")
  local n = 0
  for _ in pairs(t) do n = n + 1 end
  assert(n == 0, "fully-weak table should drop all entries, got " .. n)
end

-- ── primitive (non-heap) values are not affected by weakness ──────
-- Strings in Lua *are* GC-managed, so they can be collected from weak
-- tables if no strong reference exists — only truly value-typed entries
-- (integers, floats, booleans) are guaranteed to persist.
do
  local t = setmetatable({}, { __mode = "v" })
  t[1] = 42
  t[2] = 3.14
  t[3] = true
  collectgarbage("collect")
  assert(t[1] == 42, "integer value should not be cleared")
  assert(t[2] == 3.14, "float value should not be cleared")
  assert(t[3] == true, "boolean value should not be cleared")
end

-- ── changing __mode after the fact takes effect on next GC ─────────
do
  local t = {}
  local function populate()
    local v = {}
    t[1] = v
  end
  populate()
  collectgarbage("collect")
  assert(type(t[1]) == "table", "value should still be alive in strong table")
  setmetatable(t, { __mode = "v" })
  collectgarbage("collect")
  assert(t[1] == nil, "value should be collected after switching to weak")
end

-- ── ephemeron value-references-key cycle is collected ─────────────
-- t = { [k] = v } with v keeping a reference back to k via t.
do
  local t = setmetatable({}, { __mode = "k" })
  local function populate()
    local k = {}
    local v = { ref = k }  -- value holds key
    t[k] = v
  end
  populate()
  collectgarbage("collect")
  local n = 0
  for _ in pairs(t) do n = n + 1 end
  assert(n == 0, "ephemeron self-cycle should still be collected, got " .. n)
end

print("M3.3 weak tables tests: OK")

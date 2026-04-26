-- M3.3: __gc finalizer tests
-- Run from the workspace root: rua tests/finalizers_test.lua

-- ── basic: __gc runs exactly once when object becomes unreachable ──
do
  local called = 0
  local function make()
    local t = setmetatable({}, {
      __gc = function() called = called + 1 end,
    })
    return t
  end
  local obj = make()
  collectgarbage("collect")
  assert(called == 0, "finalizer must not run while object is reachable")
  obj = nil
  collectgarbage("collect")
  assert(called == 1, "finalizer should have run exactly once, got " .. called)
  collectgarbage("collect")
  assert(called == 1, "finalizer must not run twice, got " .. called)
end

-- ── finalizer receives the object being collected ─────────────────
do
  local seen_self = nil
  local seen_field = nil
  local function make()
    local t = { marker = "hi" }
    setmetatable(t, {
      __gc = function(self)
        seen_self = type(self)
        seen_field = self.marker
      end,
    })
  end
  make()
  collectgarbage("collect")
  assert(seen_self == "table", "expected table, got " .. tostring(seen_self))
  assert(seen_field == "hi", "expected 'hi', got " .. tostring(seen_field))
end

-- ── errors in finalizers don't kill the VM ────────────────────────
do
  local after = false
  local function make_bad()
    setmetatable({}, {
      __gc = function() error("oops") end,
    })
  end
  make_bad()
  collectgarbage("collect")
  after = true
  assert(after, "VM should keep running after a finalizer error")
end

-- ── finalizers run in reverse order of registration (LIFO) ────────
do
  local order = {}
  local function make(label)
    setmetatable({}, {
      __gc = function() order[#order + 1] = label end,
    })
  end
  make("a")
  make("b")
  make("c")
  collectgarbage("collect")
  -- Lua reference manual guarantees reverse registration order.
  assert(#order == 3, "expected 3 finalizers, got " .. #order)
  assert(order[1] == "c" and order[2] == "b" and order[3] == "a",
         "expected c,b,a; got " .. table.concat(order, ","))
end

-- ── resurrection: a finalizer may store a reference to self ───────
do
  local saved
  local runs = 0
  local function make()
    setmetatable({ id = 42 }, {
      __gc = function(self)
        runs = runs + 1
        saved = self  -- resurrect
      end,
    })
  end
  make()
  collectgarbage("collect")
  assert(runs == 1, "finalizer should have fired once")
  assert(type(saved) == "table" and saved.id == 42,
         "resurrected object should be usable")
  -- Drop the resurrection reference: the finalizer has already fired
  -- once and must NOT fire again on the next collection.
  saved = nil
  collectgarbage("collect")
  assert(runs == 1, "finalizer must not fire twice, got " .. runs)
end

-- ── collectgarbage("count") returns a number ──────────────────────
do
  local n = collectgarbage("count")
  assert(type(n) == "number" and n >= 0, "count should be a non-negative number")
end

print("M3.3 finalizer tests: OK")

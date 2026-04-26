-- M3.6: package / require tests
-- Run from the workspace root: rua tests/require_test.lua

-- ── package table basics ───────────────────────────────────────────
assert(type(package) == "table", "package table missing")
assert(type(package.loaded) == "table", "package.loaded missing")
assert(type(package.preload) == "table", "package.preload missing")
assert(type(package.path) == "string", "package.path missing")
assert(type(package.searchers) == "table", "package.searchers missing")
assert(type(package.searchpath) == "function", "package.searchpath missing")
assert(type(package.config) == "string", "package.config missing")

-- package.config has 5 lines
local cfg_lines = 0
for _ in string.gmatch(package.config, "[^\n]+") do cfg_lines = cfg_lines + 1 end
assert(cfg_lines == 5, "package.config should have 5 lines, got " .. cfg_lines)

-- ── require: file searcher ─────────────────────────────────────────
package.path = "./tests/require_test_modules/?.lua;./tests/require_test_modules/?/init.lua"

local m = require("mymod")
assert(type(m) == "table", "require did not return a table")
assert(m.value == 42, "module value mismatch")
assert(m.greet("world") == "hello world", "module function failed")

-- module is cached
local m2 = require("mymod")
assert(m == m2, "require should return cached module")

-- dotted module names: foo.bar -> foo/bar.lua
local sub = require("sub.inner")
assert(sub.name == "sub.inner", "dotted require failed: got " .. tostring(sub.name))

-- package.loaded is populated
assert(package.loaded["mymod"] == m, "package.loaded[mymod] missing")
assert(package.loaded["sub.inner"] == sub, "package.loaded[sub.inner] missing")

-- ── require: preload searcher ──────────────────────────────────────
package.preload["mypreload"] = function(name)
  return { from = "preload", name = name }
end

local p = require("mypreload")
assert(p.from == "preload", "preload module result wrong")
assert(p.name == "mypreload", "preload module did not get its name")
assert(package.loaded["mypreload"] == p, "preload module not cached")

-- ── package.searchpath ─────────────────────────────────────────────
local file, msg = package.searchpath("mymod", package.path)
assert(file ~= nil, "searchpath failed: " .. tostring(msg))
assert(string.find(file, "mymod%.lua$"), "searchpath wrong file: " .. file)

local missing, err = package.searchpath("does.not.exist", package.path)
assert(missing == nil, "searchpath should fail for missing module")
assert(type(err) == "string" and string.find(err, "no file"), "searchpath err message bad")

-- ── load ───────────────────────────────────────────────────────────
local f, e = load("return 1 + 2")
assert(f and not e, "load failed: " .. tostring(e))
assert(f() == 3, "loaded chunk returned wrong value")

-- load with custom env
local env = { x = 10 }
local f2 = load("return x + 5", "chunk", "t", env)
assert(f2() == 15, "loaded chunk with env failed")

-- load with syntax error
local bad, err = load("local 1 = 2")
assert(bad == nil, "bad chunk should not load")
assert(type(err) == "string", "load error should be a string")

-- load multiple values
local f3 = load("return 1, 2, 3")
local a, b, c = f3()
assert(a == 1 and b == 2 and c == 3, "multi-return load broken")

-- ── require: nonexistent module raises ─────────────────────────────
local ok, err = pcall(require, "no.such.module")
assert(not ok, "require should fail for missing module")
assert(string.find(err, "module 'no.such.module' not found"), "missing-module error wrong: " .. tostring(err))

-- ── _G is wired up ─────────────────────────────────────────────────
assert(_G == _G, "_G missing")
assert(_G.print == print, "_G.print mismatch")

print("M3.6 require/package tests: OK")

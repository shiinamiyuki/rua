//! Virtual machine: register-based bytecode execution.

use std::cell::RefCell;
use std::rc::Rc;

use crate::bytecode::*;
use crate::closure::*;
use crate::coroutine::{Coroutine, CoroutineStatus};
use crate::error::LuaError;
use crate::gc::{Gc, GcObjectKind, GcRef};
use crate::table::Table;
use crate::value::Value;

// ── Metamethod tag names ───────────────────────────────────────────

const MM_ADD: &[u8] = b"__add";
const MM_SUB: &[u8] = b"__sub";
const MM_MUL: &[u8] = b"__mul";
const MM_DIV: &[u8] = b"__div";
const MM_MOD: &[u8] = b"__mod";
const MM_POW: &[u8] = b"__pow";
const MM_UNM: &[u8] = b"__unm";
const MM_IDIV: &[u8] = b"__idiv";
const MM_BAND: &[u8] = b"__band";
const MM_BOR: &[u8] = b"__bor";
const MM_BXOR: &[u8] = b"__bxor";
const MM_BNOT: &[u8] = b"__bnot";
const MM_SHL: &[u8] = b"__shl";
const MM_SHR: &[u8] = b"__shr";
const MM_CONCAT: &[u8] = b"__concat";
const MM_LEN: &[u8] = b"__len";
const MM_EQ: &[u8] = b"__eq";
const MM_LT: &[u8] = b"__lt";
const MM_LE: &[u8] = b"__le";
const MM_INDEX: &[u8] = b"__index";
const MM_NEWINDEX: &[u8] = b"__newindex";
const MM_CALL: &[u8] = b"__call";
const MM_TOSTRING: &[u8] = b"__tostring";
const MM_METATABLE: &[u8] = b"__metatable";
const MM_CLOSE: &[u8] = b"__close";
const MM_GC: &[u8] = b"__gc";
const MM_MODE: &[u8] = b"__mode";

// ── Call frame ─────────────────────────────────────────────────────

/// A single activation record on the call stack.
pub(crate) struct CallFrame {
    /// GcRef to the closure being executed.
    pub(crate) closure: GcRef,
    /// Cached prototype (Rc clone, avoids going through GcRef each instruction).
    pub(crate) proto: Rc<Proto>,
    /// Cached upvalues from the closure.
    pub(crate) upvalues: Vec<UpvalueRef>,
    /// Base register index in the shared stack.
    pub(crate) base: usize,
    /// Program counter (index into proto.code).
    pub(crate) pc: usize,
    /// Where to place results in the caller's stack (absolute index).
    pub(crate) result_base: usize,
    /// Number of results the caller expects (-1 = variable).
    pub(crate) num_results: i32,
    /// Vararg values for this frame.
    pub(crate) varargs: Vec<Value>,
    /// Absolute stack index one past the highest currently in-use register.
    /// Updated by instructions that stage arguments/operands in temporaries
    /// above the active-locals region (notably Call and Concat). The GC
    /// uses this to include in-flight temps as roots.
    pub(crate) runtime_top: usize,
}

/// Saved pcall/xpcall context for yield-across-pcall support.
/// When yield happens inside a pcall, we save the pcall context here
/// so it can be restored when the coroutine resumes.
#[derive(Clone)]
pub(crate) struct PcallGuard {
    /// Frame depth when pcall started (= saved_depth).
    pub(crate) frame_depth: usize,
    /// Number of open upvalues when pcall started.
    pub(crate) open_uv_len: usize,
    /// Where to place the pcall boolean result.
    pub(crate) result_base: usize,
    /// Number of results the caller expects.
    pub(crate) num_results: i32,
    /// true for xpcall (has a message handler).
    pub(crate) is_xpcall: bool,
    /// Message handler function (for xpcall).
    pub(crate) handler: Value,
}

// ── VM state ───────────────────────────────────────────────────────

/// The Lua virtual machine.
pub struct Vm {
    /// Garbage collector / object allocator.
    pub gc: Gc,
    /// Shared register stack.
    stack: Vec<Value>,
    /// Call stack.
    frames: Vec<CallFrame>,
    /// Open upvalues (sorted by stack index, ascending).
    open_upvalues: Vec<UpvalueRef>,
    /// Stack top for variable-length arg/result lists.
    top: usize,
    /// GcRef to the pcall closure (for special-case detection in CALL).
    pcall_ref: Option<GcRef>,
    /// GcRef to the xpcall closure (for special-case detection in CALL).
    xpcall_ref: Option<GcRef>,
    /// GcRef to the error closure (for special-case detection in CALL).
    error_ref: Option<GcRef>,
    /// Stack indices of to-be-closed variables (sorted ascending).
    tbc_slots: Vec<usize>,
    /// Shared metatable for all string values.
    string_metatable: Option<GcRef>,

    // ── Coroutine support ──────────────────────────────────────────
    /// GcRef to the main thread coroutine object.
    main_thread: Option<GcRef>,
    /// GcRef to the currently running coroutine (None = main thread).
    current_thread: Option<GcRef>,
    /// Yield flag: set by coroutine.yield, cleared by resume.
    /// Contains the yielded values.
    yielded: Option<Vec<Value>>,
    /// Return values from a top-level RETURN (used by resume to collect results).
    last_return_values: Vec<Value>,
    /// GcRef identity markers for coroutine library functions.
    coro_resume_ref: Option<GcRef>,
    coro_yield_ref: Option<GcRef>,
    coro_wrap_ref: Option<GcRef>,
    coro_running_ref: Option<GcRef>,
    coro_isyieldable_ref: Option<GcRef>,
    coro_close_ref: Option<GcRef>,
    /// Pcall/xpcall guard stack for yield-across-pcall support.
    pcall_guards: Vec<PcallGuard>,

    // ── Debug library support ──────────────────────────────────────
    debug_traceback_ref: Option<GcRef>,
    debug_getinfo_ref: Option<GcRef>,
    debug_getlocal_ref: Option<GcRef>,
    debug_setlocal_ref: Option<GcRef>,
    debug_getupvalue_ref: Option<GcRef>,
    debug_setupvalue_ref: Option<GcRef>,
    debug_upvalueid_ref: Option<GcRef>,
    debug_upvaluejoin_ref: Option<GcRef>,

    // ── Package / require support ──────────────────────────────────
    require_ref: Option<GcRef>,
    load_ref: Option<GcRef>,
    loadfile_ref: Option<GcRef>,
    dofile_ref: Option<GcRef>,
    preload_searcher_ref: Option<GcRef>,
    file_searcher_ref: Option<GcRef>,
    /// The `package` library table (for quick access to loaded/preload/searchers/path).
    package_ref: Option<GcRef>,
    /// The global environment `_ENV` table.
    globals_ref: Option<GcRef>,

    /// `collectgarbage` (VM-special, runs a real mark-and-sweep).
    collectgarbage_ref: Option<GcRef>,

    /// Re-entrancy guard for `__gc` finalizer dispatch.
    in_finalizer: bool,
}

impl Vm {
    /// Create a new VM.
    pub fn new() -> Self {
        Vm {
            gc: Gc::new(),
            stack: vec![Value::Nil; 256],
            frames: Vec::new(),
            open_upvalues: Vec::new(),
            top: 0,
            pcall_ref: None,
            xpcall_ref: None,
            error_ref: None,
            tbc_slots: Vec::new(),
            string_metatable: None,
            main_thread: None,
            current_thread: None,
            yielded: None,
            last_return_values: Vec::new(),
            coro_resume_ref: None,
            coro_yield_ref: None,
            coro_wrap_ref: None,
            coro_running_ref: None,
            coro_isyieldable_ref: None,
            coro_close_ref: None,
            pcall_guards: Vec::new(),
            debug_traceback_ref: None,
            debug_getinfo_ref: None,
            debug_getlocal_ref: None,
            debug_setlocal_ref: None,
            debug_getupvalue_ref: None,
            debug_setupvalue_ref: None,
            debug_upvalueid_ref: None,
            debug_upvaluejoin_ref: None,
            require_ref: None,
            load_ref: None,
            loadfile_ref: None,
            dofile_ref: None,
            preload_searcher_ref: None,
            file_searcher_ref: None,
            package_ref: None,
            globals_ref: None,
            collectgarbage_ref: None,
            in_finalizer: false,
        }
    }

    /// Load and execute a compiled top-level chunk.
    pub fn execute_main(&mut self, proto: Proto) -> Result<(), LuaError> {
        // Create the global environment table (_ENV)
        let env_table = self.create_global_env();

        // Create the main thread (coroutine representing the main execution)
        let main_coro = Coroutine {
            status: CoroutineStatus::Running,
            stack: Vec::new(),
            frames: Vec::new(),
            open_upvalues: Vec::new(),
            tbc_slots: Vec::new(),
            top: 0,
            body: None,
            yield_result_base: 0,
            yield_num_results: 0,
            is_main: true,
            pcall_guards: Vec::new(),
        };
        let main_thread_ref = self.gc.new_thread(main_coro);
        self.main_thread = Some(main_thread_ref);

        // Create the main closure from the proto
        let proto_rc = Rc::new(proto);
        let env_upvalue = Rc::new(RefCell::new(Upvalue::Closed(Value::Object(env_table))));
        let main_closure =
            Closure::new_lua(Rc::clone(&proto_rc), vec![Rc::clone(&env_upvalue)]);
        let main_ref = self.gc.new_closure(main_closure);

        // Push the main call frame
        let base = 0;
        self.ensure_stack(base + proto_rc.max_stack_size as usize);
        self.frames.push(CallFrame {
            closure: main_ref,
            proto: proto_rc,
            upvalues: vec![env_upvalue],
            base,
            pc: 0,
            result_base: 0,
            num_results: 0,
            varargs: Vec::new(),
            runtime_top: base,
        });

        self.execute()
    }

    /// Create the global environment with standard library functions.
    fn create_global_env(&mut self) -> GcRef {
        let mut env = Table::new();

        // Pre-intern all metamethod name strings so find_string() always finds them.
        for name in &[
            MM_ADD, MM_SUB, MM_MUL, MM_DIV, MM_MOD, MM_POW, MM_UNM, MM_IDIV,
            MM_BAND, MM_BOR, MM_BXOR, MM_BNOT, MM_SHL, MM_SHR,
            MM_CONCAT, MM_LEN, MM_EQ, MM_LT, MM_LE,
            MM_INDEX, MM_NEWINDEX, MM_CALL, MM_TOSTRING, MM_METATABLE,
            MM_CLOSE, MM_GC, MM_MODE,
        ] {
            self.gc.new_string(name);
        }

        // Register built-in functions
        self.register_native(&mut env, "print", crate::stdlib::lua_print);
        self.register_native(&mut env, "type", crate::stdlib::lua_type);
        self.register_native(&mut env, "tostring", crate::stdlib::lua_tostring);
        self.register_native(&mut env, "tonumber", crate::stdlib::lua_tonumber);
        self.register_native(&mut env, "assert", crate::stdlib::lua_assert);

        // error is special: handled by the VM to add source:line info.
        {
            let error_closure = Closure::new_native("error", |_, _| Ok(vec![]));
            let error_gc = self.gc.new_closure(error_closure);
            self.error_ref = Some(error_gc);
            let key = self.gc.new_string(b"error");
            env.raw_set(Value::Object(key), Value::Object(error_gc));
        }

        // collectgarbage is special: it triggers a real mark-and-sweep cycle
        // which needs full VM context (stack, frames) for roots.
        {
            let cg_closure = Closure::new_native("collectgarbage", |_, _| Ok(vec![]));
            let cg_gc = self.gc.new_closure(cg_closure);
            self.collectgarbage_ref = Some(cg_gc);
            let key = self.gc.new_string(b"collectgarbage");
            env.raw_set(Value::Object(key), Value::Object(cg_gc));
        }

        // pcall is special: handled by the VM directly, not as a regular native call.
        {
            let pcall_closure = Closure::new_native("pcall", |_, _| Ok(vec![]));
            let pcall_gc = self.gc.new_closure(pcall_closure);
            self.pcall_ref = Some(pcall_gc);
            let key = self.gc.new_string(b"pcall");
            env.raw_set(Value::Object(key), Value::Object(pcall_gc));
        }

        // xpcall is special: handled by the VM directly, like pcall.
        {
            let xpcall_closure = Closure::new_native("xpcall", |_, _| Ok(vec![]));
            let xpcall_gc = self.gc.new_closure(xpcall_closure);
            self.xpcall_ref = Some(xpcall_gc);
            let key = self.gc.new_string(b"xpcall");
            env.raw_set(Value::Object(key), Value::Object(xpcall_gc));
        }

        self.register_native(&mut env, "ipairs", crate::stdlib::lua_ipairs);
        self.register_native(&mut env, "pairs", crate::stdlib::lua_pairs);
        self.register_native(&mut env, "rawget", crate::stdlib::lua_rawget);
        self.register_native(&mut env, "rawset", crate::stdlib::lua_rawset);
        self.register_native(&mut env, "rawlen", crate::stdlib::lua_rawlen);
        self.register_native(&mut env, "rawequal", crate::stdlib::lua_rawequal);
        self.register_native(&mut env, "select", crate::stdlib::lua_select);
        self.register_native(&mut env, "setmetatable", crate::stdlib::lua_setmetatable);
        self.register_native(&mut env, "getmetatable", crate::stdlib::lua_getmetatable);

        // _VERSION
        let version_str = self.gc.new_string(b"Lua 5.5");
        let version_key = self.gc.new_string(b"_VERSION");
        env.raw_set(Value::Object(version_key), Value::Object(version_str));

        // math library
        let mut math_table = Table::new();
        for (name, func) in crate::stdlib::math::math_functions() {
            self.register_native(&mut math_table, name, func);
        }
        for (name, val) in crate::stdlib::math::math_constants() {
            let key = self.gc.new_string(name.as_bytes());
            math_table.raw_set(Value::Object(key), val);
        }
        let math_ref = self.gc.new_table(math_table);
        let math_key = self.gc.new_string(b"math");
        env.raw_set(Value::Object(math_key), Value::Object(math_ref));

        // string library
        let mut string_table = Table::new();
        for (name, func) in crate::stdlib::string::string_functions() {
            self.register_native(&mut string_table, name, func);
        }
        let string_ref = self.gc.new_table(string_table);
        let string_key = self.gc.new_string(b"string");
        env.raw_set(Value::Object(string_key), Value::Object(string_ref));

        // Set up string metatable: { __index = string }
        let mut string_mt = Table::new();
        let index_key = self.gc.new_string(MM_INDEX);
        string_mt.raw_set(Value::Object(index_key), Value::Object(string_ref));
        let string_mt_ref = self.gc.new_table(string_mt);
        self.string_metatable = Some(string_mt_ref);

        // table library
        let mut table_table = Table::new();
        for (name, func) in crate::stdlib::table::table_functions() {
            self.register_native(&mut table_table, name, func);
        }
        let table_ref = self.gc.new_table(table_table);
        let table_key = self.gc.new_string(b"table");
        env.raw_set(Value::Object(table_key), Value::Object(table_ref));

        // coroutine library
        let mut coro_table = Table::new();

        // coroutine.create(f) — regular native
        self.register_native(&mut coro_table, "create", crate::stdlib::coroutine::lua_coroutine_create);
        // coroutine.status(co) — regular native
        self.register_native(&mut coro_table, "status", crate::stdlib::coroutine::lua_coroutine_status);
        // coroutine.wrap(f) — regular native
        self.register_native(&mut coro_table, "wrap", crate::stdlib::coroutine::lua_coroutine_wrap);

        // coroutine.resume — special: handled by VM
        {
            let c = Closure::new_native("resume", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.coro_resume_ref = Some(r);
            let k = self.gc.new_string(b"resume");
            coro_table.raw_set(Value::Object(k), Value::Object(r));
        }
        // coroutine.yield — special: handled by VM
        {
            let c = Closure::new_native("yield", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.coro_yield_ref = Some(r);
            let k = self.gc.new_string(b"yield");
            coro_table.raw_set(Value::Object(k), Value::Object(r));
        }
        // coroutine.running — special: handled by VM
        {
            let c = Closure::new_native("running", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.coro_running_ref = Some(r);
            let k = self.gc.new_string(b"running");
            coro_table.raw_set(Value::Object(k), Value::Object(r));
        }
        // coroutine.isyieldable — special: handled by VM
        {
            let c = Closure::new_native("isyieldable", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.coro_isyieldable_ref = Some(r);
            let k = self.gc.new_string(b"isyieldable");
            coro_table.raw_set(Value::Object(k), Value::Object(r));
        }
        // coroutine.close — special: handled by VM
        {
            let c = Closure::new_native("close", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.coro_close_ref = Some(r);
            let k = self.gc.new_string(b"close");
            coro_table.raw_set(Value::Object(k), Value::Object(r));
        }

        let coro_ref = self.gc.new_table(coro_table);
        let coro_key = self.gc.new_string(b"coroutine");
        env.raw_set(Value::Object(coro_key), Value::Object(coro_ref));

        // ── io library ─────────────────────────────────────────────

        // Build file method table (used as __index for file handles)
        let mut file_method_table = Table::new();
        for (name, func) in crate::stdlib::io::file_methods() {
            self.register_native(&mut file_method_table, name, func);
        }
        let file_method_ref = self.gc.new_table(file_method_table);

        // Build file metatable: { __index = method_table, __close = close_fn }
        let mut file_mt = Table::new();
        let index_key2 = self.gc.new_string(MM_INDEX);
        file_mt.raw_set(Value::Object(index_key2), Value::Object(file_method_ref));
        let close_key = self.gc.new_string(MM_CLOSE);
        let close_closure = Closure::new_native("file.__close", crate::stdlib::io::file_gc_close);
        let close_ref = self.gc.new_closure(close_closure);
        file_mt.raw_set(Value::Object(close_key), Value::Object(close_ref));
        let file_mt_ref = self.gc.new_table(file_mt);
        self.gc.file_metatable = Some(file_mt_ref);

        // Build io table with library functions
        let mut io_table = Table::new();
        for (name, func) in crate::stdlib::io::io_functions() {
            self.register_native(&mut io_table, name, func);
        }

        // io.stdin, io.stdout, io.stderr file handles
        let mt = self.gc.file_metatable;
        let stdin_ud = self.gc.new_userdata(
            Box::new(crate::stdlib::io::LuaFile::stdin()), mt,
        );
        let stdout_ud = self.gc.new_userdata(
            Box::new(crate::stdlib::io::LuaFile::stdout()), mt,
        );
        let stderr_ud = self.gc.new_userdata(
            Box::new(crate::stdlib::io::LuaFile::stderr()), mt,
        );
        let stdin_key = self.gc.new_string(b"stdin");
        io_table.raw_set(Value::Object(stdin_key), Value::Object(stdin_ud));
        let stdout_key = self.gc.new_string(b"stdout");
        io_table.raw_set(Value::Object(stdout_key), Value::Object(stdout_ud));
        let stderr_key = self.gc.new_string(b"stderr");
        io_table.raw_set(Value::Object(stderr_key), Value::Object(stderr_ud));

        let io_ref = self.gc.new_table(io_table);
        let io_key = self.gc.new_string(b"io");
        env.raw_set(Value::Object(io_key), Value::Object(io_ref));

        // ── os library ─────────────────────────────────────────────

        let mut os_table = Table::new();
        for (name, func) in crate::stdlib::os::os_functions() {
            self.register_native(&mut os_table, name, func);
        }
        let os_ref = self.gc.new_table(os_table);
        let os_key = self.gc.new_string(b"os");
        env.raw_set(Value::Object(os_key), Value::Object(os_ref));

        // ── utf8 library ───────────────────────────────────────────

        let mut utf8_table = Table::new();
        for (name, func) in crate::stdlib::utf8::utf8_functions() {
            self.register_native(&mut utf8_table, name, func);
        }
        // utf8.charpattern
        let cp_key = self.gc.new_string(b"charpattern");
        let cp_val = self.gc.new_string(crate::stdlib::utf8::UTF8_CHARPATTERN);
        utf8_table.raw_set(Value::Object(cp_key), Value::Object(cp_val));
        let utf8_ref = self.gc.new_table(utf8_table);
        let utf8_key = self.gc.new_string(b"utf8");
        env.raw_set(Value::Object(utf8_key), Value::Object(utf8_ref));

        // ── debug library ──────────────────────────────────────────

        let mut debug_table = Table::new();

        // Native functions (no VM access needed)
        for (name, func) in crate::stdlib::debug::debug_native_functions() {
            self.register_native(&mut debug_table, name, func);
        }

        // VM-special functions
        {
            let c = Closure::new_native("traceback", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.debug_traceback_ref = Some(r);
            let k = self.gc.new_string(b"traceback");
            debug_table.raw_set(Value::Object(k), Value::Object(r));
        }
        {
            let c = Closure::new_native("getinfo", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.debug_getinfo_ref = Some(r);
            let k = self.gc.new_string(b"getinfo");
            debug_table.raw_set(Value::Object(k), Value::Object(r));
        }
        {
            let c = Closure::new_native("getlocal", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.debug_getlocal_ref = Some(r);
            let k = self.gc.new_string(b"getlocal");
            debug_table.raw_set(Value::Object(k), Value::Object(r));
        }
        {
            let c = Closure::new_native("setlocal", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.debug_setlocal_ref = Some(r);
            let k = self.gc.new_string(b"setlocal");
            debug_table.raw_set(Value::Object(k), Value::Object(r));
        }
        {
            let c = Closure::new_native("getupvalue", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.debug_getupvalue_ref = Some(r);
            let k = self.gc.new_string(b"getupvalue");
            debug_table.raw_set(Value::Object(k), Value::Object(r));
        }
        {
            let c = Closure::new_native("setupvalue", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.debug_setupvalue_ref = Some(r);
            let k = self.gc.new_string(b"setupvalue");
            debug_table.raw_set(Value::Object(k), Value::Object(r));
        }
        {
            let c = Closure::new_native("upvalueid", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.debug_upvalueid_ref = Some(r);
            let k = self.gc.new_string(b"upvalueid");
            debug_table.raw_set(Value::Object(k), Value::Object(r));
        }
        {
            let c = Closure::new_native("upvaluejoin", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.debug_upvaluejoin_ref = Some(r);
            let k = self.gc.new_string(b"upvaluejoin");
            debug_table.raw_set(Value::Object(k), Value::Object(r));
        }

        let debug_ref = self.gc.new_table(debug_table);
        let debug_key = self.gc.new_string(b"debug");
        env.raw_set(Value::Object(debug_key), Value::Object(debug_ref));

        // ── package library ────────────────────────────────────────

        // Stubs for VM-special functions (dispatch by GcRef identity).
        let preload_searcher_stub = Closure::new_native("preload_searcher", |_, _| Ok(vec![]));
        let preload_searcher_ref = self.gc.new_closure(preload_searcher_stub);
        self.preload_searcher_ref = Some(preload_searcher_ref);

        let file_searcher_stub = Closure::new_native("lua_searcher", |_, _| Ok(vec![]));
        let file_searcher_ref = self.gc.new_closure(file_searcher_stub);
        self.file_searcher_ref = Some(file_searcher_ref);

        // package.loaded, package.preload
        let loaded_ref = self.gc.new_table(Table::new());
        let preload_ref = self.gc.new_table(Table::new());

        // package.searchers = { preload_searcher, file_searcher }
        let mut searchers_tbl = Table::new();
        searchers_tbl.raw_set(Value::Integer(1), Value::Object(preload_searcher_ref));
        searchers_tbl.raw_set(Value::Integer(2), Value::Object(file_searcher_ref));
        let searchers_ref = self.gc.new_table(searchers_tbl);

        // package.searchpath (pure native)
        let searchpath_closure =
            Closure::new_native("searchpath", crate::stdlib::package::lua_searchpath);
        let searchpath_ref = self.gc.new_closure(searchpath_closure);

        // Build the package table itself
        let mut package_tbl = Table::new();
        {
            let k = self.gc.new_string(b"loaded");
            package_tbl.raw_set(Value::Object(k), Value::Object(loaded_ref));
        }
        {
            let k = self.gc.new_string(b"preload");
            package_tbl.raw_set(Value::Object(k), Value::Object(preload_ref));
        }
        {
            let k = self.gc.new_string(b"searchers");
            package_tbl.raw_set(Value::Object(k), Value::Object(searchers_ref));
        }
        {
            let k = self.gc.new_string(b"searchpath");
            package_tbl.raw_set(Value::Object(k), Value::Object(searchpath_ref));
        }
        {
            let k = self.gc.new_string(b"path");
            let path_str = crate::stdlib::package::default_path();
            let v = self.gc.new_string(path_str.as_bytes());
            package_tbl.raw_set(Value::Object(k), Value::Object(v));
        }
        {
            let k = self.gc.new_string(b"config");
            let v = self.gc.new_string(crate::stdlib::package::PACKAGE_CONFIG.as_bytes());
            package_tbl.raw_set(Value::Object(k), Value::Object(v));
        }
        let package_ref = self.gc.new_table(package_tbl);
        self.package_ref = Some(package_ref);
        let package_key = self.gc.new_string(b"package");
        env.raw_set(Value::Object(package_key), Value::Object(package_ref));

        // require, load, loadfile, dofile — VM-special by GcRef identity.
        {
            let c = Closure::new_native("require", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.require_ref = Some(r);
            let k = self.gc.new_string(b"require");
            env.raw_set(Value::Object(k), Value::Object(r));
        }
        {
            let c = Closure::new_native("load", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.load_ref = Some(r);
            let k = self.gc.new_string(b"load");
            env.raw_set(Value::Object(k), Value::Object(r));
        }
        {
            let c = Closure::new_native("loadfile", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.loadfile_ref = Some(r);
            let k = self.gc.new_string(b"loadfile");
            env.raw_set(Value::Object(k), Value::Object(r));
        }
        {
            let c = Closure::new_native("dofile", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.dofile_ref = Some(r);
            let k = self.gc.new_string(b"dofile");
            env.raw_set(Value::Object(k), Value::Object(r));
        }

        let env_ref = self.gc.new_table(env);
        self.globals_ref = Some(env_ref);

        // Expose globals as `_G` (bound to the same table).
        {
            let g_key = self.gc.new_string(b"_G");
            env_ref
                .as_object_mut()
                .as_table_mut()
                .unwrap()
                .raw_set(Value::Object(g_key), Value::Object(env_ref));
        }

        env_ref
    }

    /// Register a native function in a table.
    fn register_native(&mut self, table: &mut Table, name: &'static str, func: NativeFn) {
        let closure = Closure::new_native(name, func);
        let gc_ref = self.gc.new_closure(closure);
        let key = self.gc.new_string(name.as_bytes());
        table.raw_set(Value::Object(key), Value::Object(gc_ref));
    }

    /// Ensure the stack has at least `size` slots.
    fn ensure_stack(&mut self, size: usize) {
        if self.stack.len() <= size {
            self.stack.resize(size + 64, Value::Nil);
        }
    }

    // ── Register accessors ─────────────────────────────────────────

    #[inline]
    fn reg(&self, base: usize, idx: usize) -> Value {
        self.stack[base + idx]
    }

    #[inline]
    fn set_reg(&mut self, base: usize, idx: usize, val: Value) {
        self.stack[base + idx] = val;
    }

    // ── Upvalue helpers ────────────────────────────────────────────

    fn get_upvalue_val(&self, upvalues: &[UpvalueRef], idx: usize) -> Value {
        match *upvalues[idx].borrow() {
            Upvalue::Open(stack_idx) => self.stack[stack_idx],
            Upvalue::Closed(val) => val,
        }
    }

    /// Find an existing open upvalue for the given stack index, or create one.
    fn find_or_create_upvalue(&mut self, stack_idx: usize) -> UpvalueRef {
        for uv in &self.open_upvalues {
            if let Upvalue::Open(idx) = *uv.borrow() {
                if idx == stack_idx {
                    return Rc::clone(uv);
                }
            }
        }
        let uv = Rc::new(RefCell::new(Upvalue::Open(stack_idx)));
        self.open_upvalues.push(Rc::clone(&uv));
        uv
    }

    /// Close all open upvalues with stack index >= `from`.
    fn close_upvalues(&mut self, from: usize) {
        for uv in &self.open_upvalues {
            let should_close = match *uv.borrow() {
                Upvalue::Open(idx) => idx >= from,
                Upvalue::Closed(_) => false,
            };
            if should_close {
                let val = match *uv.borrow() {
                    Upvalue::Open(idx) => self.stack[idx],
                    _ => unreachable!(),
                };
                *uv.borrow_mut() = Upvalue::Closed(val);
            }
        }
        self.open_upvalues
            .retain(|uv| matches!(*uv.borrow(), Upvalue::Open(_)));
    }

    /// Close all to-be-closed variables with stack index >= `from`.
    /// Calls `__close` metamethod in reverse order. `err_obj` is the error
    /// object (if any) that caused the scope exit.
    fn close_tbc_vars(&mut self, from: usize, err_obj: Option<Value>) -> Result<(), LuaError> {
        // Collect TBC slots at or above `from` in reverse order
        let mut to_close: Vec<usize> = Vec::new();
        while let Some(&slot) = self.tbc_slots.last() {
            if slot >= from {
                self.tbc_slots.pop();
                to_close.push(slot);
            } else {
                break;
            }
        }

        let mut first_err: Option<LuaError> = None;
        for slot in to_close {
            let val = self.stack[slot];
            // nil and false are silently ignored
            if val == Value::Nil || val == Value::Boolean(false) {
                continue;
            }
            if let Some(mm) = self.get_metamethod(val, MM_CLOSE) {
                let err_arg = err_obj.unwrap_or(Value::Nil);
                let result = self.call_value(mm, &[val, err_arg]);
                if let Err(e) = result {
                    if first_err.is_none() {
                        first_err = Some(e);
                    }
                }
            }
            // Per spec: value must have __close or be nil/false
            // If it has neither, that's an error (already validated at TBC time)
        }

        if let Some(e) = first_err {
            return Err(e);
        }
        Ok(())
    }

    // ── Metamethod infrastructure ─────────────────────────────────

    /// Get the metatable of a value (if any).
    fn get_metatable(&self, val: Value) -> Option<GcRef> {
        match val {
            Value::Object(r) => match &r.as_object().kind {
                GcObjectKind::Table(t) => t.metatable,
                GcObjectKind::Closure(_) => None,
                GcObjectKind::String(_) => self.string_metatable,
                GcObjectKind::Thread(_) => None,
                GcObjectKind::Userdata(ud) => ud.metatable,
            },
            _ => None,
        }
    }

    /// Look up a metamethod by name in a value's metatable.
    /// Returns the metamethod value, or None if not found.
    fn get_metamethod(&self, val: Value, name: &[u8]) -> Option<Value> {
        let mt_ref = self.get_metatable(val)?;
        let mt = mt_ref.as_object().as_table()?;
        let key_ref = self.gc.find_string(name)?;
        let result = mt.raw_get(&Value::Object(key_ref));
        if result.is_nil() { None } else { Some(result) }
    }

    /// Look up a metamethod from the first operand, then the second.
    fn get_binop_metamethod(&self, a: Value, b: Value, name: &[u8]) -> Option<Value> {
        self.get_metamethod(a, name).or_else(|| self.get_metamethod(b, name))
    }

    /// Call a metamethod with the given arguments and return one result.
    fn call_metamethod(&mut self, mm: Value, args: &[Value]) -> Result<Value, LuaError> {
        let results = self.call_value(mm, args)?;
        Ok(results.into_iter().next().unwrap_or(Value::Nil))
    }

    /// Call a metamethod and return all results.
    fn call_metamethod_multi(&mut self, mm: Value, args: &[Value]) -> Result<Vec<Value>, LuaError> {
        self.call_value(mm, args)
    }

    /// Call a value (function or callable via __call) with args and return results.
    /// This handles synchronous native calls and pushes frames for Lua calls.
    fn call_value(&mut self, func: Value, args: &[Value]) -> Result<Vec<Value>, LuaError> {
        // Find a place on the stack for this call
        let call_base = self.find_call_base();
        self.ensure_stack(call_base + args.len() + 2);
        self.stack[call_base] = func;
        for (i, &arg) in args.iter().enumerate() {
            self.stack[call_base + 1 + i] = arg;
        }

        let saved_depth = self.frames.len();

        // Handle __call chain
        let mut actual_func = func;
        let mut call_limit = 16;
        loop {
            match actual_func {
                Value::Object(r) if r.as_object().as_closure().is_some() => break,
                _ => {
                    call_limit -= 1;
                    if call_limit == 0 {
                        return Err(LuaError::new("'__call' chain too long"));
                    }
                    match self.get_metamethod(actual_func, MM_CALL) {
                        Some(mm) => {
                            // Shift args: prepend the original value
                            let mut new_args = Vec::with_capacity(args.len() + 1);
                            new_args.push(actual_func);
                            new_args.extend_from_slice(args);
                            // Recurse with the metamethod as the function
                            return self.call_value(mm, &new_args);
                        }
                        None => {
                            return Err(LuaError::new(format!(
                                "attempt to call a {} value",
                                actual_func.type_name()
                            )));
                        }
                    }
                }
            }
        }

        let gc_ref = actual_func.as_gc_ref().unwrap();
        let is_native = matches!(gc_ref.as_object().as_closure().unwrap(), Closure::Native(_) | Closure::NativeDyn(_) | Closure::WrapIterator(_));

        if is_native {
            // Intercept error() to add source:line annotation
            if self.error_ref == Some(gc_ref) {
                self.handle_error(args)?;
                unreachable!();
            }
            return match gc_ref.as_object().as_closure().unwrap() {
                Closure::Native(nc) => (nc.func)(args, &mut self.gc),
                Closure::NativeDyn(nc) => (nc.func)(args, &mut self.gc),
                Closure::WrapIterator(_) | Closure::Lua(_) => unreachable!(),
            };
        }

        // Lua function: push frame and execute
        let result_base = call_base;
        self.do_call(actual_func, call_base, args, result_base, -1)?;

        if self.frames.len() > saved_depth {
            self.execute_to_depth(saved_depth)?;
        }

        // Collect results from result_base..self.top
        let results: Vec<Value> = self.stack[result_base..self.top].to_vec();
        Ok(results)
    }

    /// Find a safe call_base for internal metamethod calls (above all active frames).
    fn find_call_base(&self) -> usize {
        let stack_top = if self.frames.is_empty() {
            self.top
        } else {
            let last = &self.frames[self.frames.len() - 1];
            let frame_top = last.base + last.proto.max_stack_size as usize;
            self.top.max(frame_top)
        };
        stack_top + 2
    }

    // ── String-to-number coercion (M2.2) ───────────────────────────

    /// Try to coerce a value to a number for arithmetic.
    /// Strings are converted to numbers if they represent valid numerals.
    fn coerce_to_number(v: Value) -> Option<Value> {
        match v {
            Value::Integer(_) | Value::Float(_) => Some(v),
            Value::Object(r) => {
                let s = r.as_object().as_string()?;
                let text = std::str::from_utf8(s.as_bytes()).ok()?;
                let text = text.trim();
                // Try integer first
                if let Ok(n) = text.parse::<i64>() {
                    return Some(Value::Integer(n));
                }
                // Try hex integer
                if let Some(hex) = text.strip_prefix("0x").or_else(|| text.strip_prefix("0X")) {
                    if let Ok(n) = i64::from_str_radix(hex, 16) {
                        return Some(Value::Integer(n));
                    }
                }
                // Try float
                if let Ok(n) = text.parse::<f64>() {
                    return Some(Value::Float(n));
                }
                None
            }
            _ => None,
        }
    }

    /// Try to coerce a value to an integer for bitwise ops.
    /// Strings are converted, floats with exact integer representation are converted.
    fn coerce_to_integer(v: Value) -> Option<i64> {
        match v {
            Value::Integer(n) => Some(n),
            Value::Float(f) => {
                let i = f as i64;
                if i as f64 == f { Some(i) } else { None }
            }
            Value::Object(r) => {
                let s = r.as_object().as_string()?;
                let text = std::str::from_utf8(s.as_bytes()).ok()?;
                let text = text.trim();
                if let Ok(n) = text.parse::<i64>() {
                    return Some(n);
                }
                if let Some(hex) = text.strip_prefix("0x").or_else(|| text.strip_prefix("0X")) {
                    if let Ok(n) = i64::from_str_radix(hex, 16) {
                        return Some(n);
                    }
                }
                // Try float string → integer
                if let Ok(f) = text.parse::<f64>() {
                    let i = f as i64;
                    if i as f64 == f {
                        return Some(i);
                    }
                }
                None
            }
            _ => None,
        }
    }

    // ── Arithmetic helpers ─────────────────────────────────────────

    /// Try raw arithmetic (including string-to-number coercion). Returns None if
    /// operands are not numeric/coercible (metamethod needed).
    fn try_arith_add(a: Value, b: Value) -> Option<Value> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => Some(Value::Integer(x.wrapping_add(y))),
            (Value::Float(x), Value::Float(y)) => Some(Value::Float(x + y)),
            (Value::Integer(x), Value::Float(y)) => Some(Value::Float(x as f64 + y)),
            (Value::Float(x), Value::Integer(y)) => Some(Value::Float(x + y as f64)),
            _ => {
                // Try string-to-number coercion
                let a2 = Self::coerce_to_number(a)?;
                let b2 = Self::coerce_to_number(b)?;
                Self::try_arith_add(a2, b2)
            }
        }
    }

    fn try_arith_sub(a: Value, b: Value) -> Option<Value> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => Some(Value::Integer(x.wrapping_sub(y))),
            (Value::Float(x), Value::Float(y)) => Some(Value::Float(x - y)),
            (Value::Integer(x), Value::Float(y)) => Some(Value::Float(x as f64 - y)),
            (Value::Float(x), Value::Integer(y)) => Some(Value::Float(x - y as f64)),
            _ => {
                let a2 = Self::coerce_to_number(a)?;
                let b2 = Self::coerce_to_number(b)?;
                Self::try_arith_sub(a2, b2)
            }
        }
    }

    fn try_arith_mul(a: Value, b: Value) -> Option<Value> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => Some(Value::Integer(x.wrapping_mul(y))),
            (Value::Float(x), Value::Float(y)) => Some(Value::Float(x * y)),
            (Value::Integer(x), Value::Float(y)) => Some(Value::Float(x as f64 * y)),
            (Value::Float(x), Value::Integer(y)) => Some(Value::Float(x * y as f64)),
            _ => {
                let a2 = Self::coerce_to_number(a)?;
                let b2 = Self::coerce_to_number(b)?;
                Self::try_arith_mul(a2, b2)
            }
        }
    }

    fn try_arith_div(a: Value, b: Value) -> Option<Value> {
        let x = match a {
            Value::Integer(n) => n as f64,
            Value::Float(n) => n,
            _ => {
                let a2 = Self::coerce_to_number(a)?;
                let b2 = Self::coerce_to_number(b)?;
                return Self::try_arith_div(a2, b2);
            }
        };
        let y = match b {
            Value::Integer(n) => n as f64,
            Value::Float(n) => n,
            _ => {
                let b2 = Self::coerce_to_number(b)?;
                return Self::try_arith_div(Value::Float(x), b2);
            }
        };
        Some(Value::Float(x / y))
    }

    fn try_arith_idiv(a: Value, b: Value) -> Result<Option<Value>, LuaError> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => {
                if y == 0 {
                    return Err(LuaError::new("attempt to perform 'n//0'"));
                }
                Ok(Some(Value::Integer(lua_idiv(x, y))))
            }
            (Value::Float(x), Value::Float(y)) => Ok(Some(Value::Float((x / y).floor()))),
            (Value::Integer(x), Value::Float(y)) => Ok(Some(Value::Float((x as f64 / y).floor()))),
            (Value::Float(x), Value::Integer(y)) => Ok(Some(Value::Float((x / y as f64).floor()))),
            _ => {
                let a2 = Self::coerce_to_number(a);
                let b2 = Self::coerce_to_number(b);
                if let (Some(a2), Some(b2)) = (a2, b2) {
                    Self::try_arith_idiv(a2, b2)
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn try_arith_mod(a: Value, b: Value) -> Result<Option<Value>, LuaError> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => {
                if y == 0 {
                    return Err(LuaError::new("attempt to perform 'n%0'"));
                }
                Ok(Some(Value::Integer(lua_imod(x, y))))
            }
            (Value::Float(x), Value::Float(y)) => Ok(Some(Value::Float(lua_fmod(x, y)))),
            (Value::Integer(x), Value::Float(y)) => Ok(Some(Value::Float(lua_fmod(x as f64, y)))),
            (Value::Float(x), Value::Integer(y)) => Ok(Some(Value::Float(lua_fmod(x, y as f64)))),
            _ => {
                let a2 = Self::coerce_to_number(a);
                let b2 = Self::coerce_to_number(b);
                if let (Some(a2), Some(b2)) = (a2, b2) {
                    Self::try_arith_mod(a2, b2)
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn try_arith_pow(a: Value, b: Value) -> Option<Value> {
        let x = match a {
            Value::Integer(n) => n as f64,
            Value::Float(n) => n,
            _ => {
                let a2 = Self::coerce_to_number(a)?;
                let b2 = Self::coerce_to_number(b)?;
                return Self::try_arith_pow(a2, b2);
            }
        };
        let y = match b {
            Value::Integer(n) => n as f64,
            Value::Float(n) => n,
            _ => {
                let b2 = Self::coerce_to_number(b)?;
                return Self::try_arith_pow(Value::Float(x), b2);
            }
        };
        Some(Value::Float(x.powf(y)))
    }

    fn try_arith_unm(a: Value) -> Option<Value> {
        match a {
            Value::Integer(x) => Some(Value::Integer(x.wrapping_neg())),
            Value::Float(x) => Some(Value::Float(-x)),
            _ => {
                let a2 = Self::coerce_to_number(a)?;
                Self::try_arith_unm(a2)
            }
        }
    }

    /// Perform a binary arithmetic operation: try raw, then metamethod.
    fn arith_binop(
        &mut self,
        a: Value,
        b: Value,
        try_fn: fn(Value, Value) -> Option<Value>,
        mm_name: &[u8],
    ) -> Result<Value, LuaError> {
        if let Some(result) = try_fn(a, b) {
            return Ok(result);
        }
        if let Some(mm) = self.get_binop_metamethod(a, b, mm_name) {
            return self.call_metamethod(mm, &[a, b]);
        }
        Err(LuaError::new(format!(
            "attempt to perform arithmetic on a {} value",
            if !a.is_number() && Self::coerce_to_number(a).is_none() { a.type_name() } else { b.type_name() }
        )))
    }

    /// Perform a binary arithmetic op that can error (idiv, mod) - try raw, then metamethod.
    fn arith_binop_err(
        &mut self,
        a: Value,
        b: Value,
        try_fn: fn(Value, Value) -> Result<Option<Value>, LuaError>,
        mm_name: &[u8],
    ) -> Result<Value, LuaError> {
        match try_fn(a, b)? {
            Some(result) => Ok(result),
            None => {
                if let Some(mm) = self.get_binop_metamethod(a, b, mm_name) {
                    return self.call_metamethod(mm, &[a, b]);
                }
                Err(LuaError::new(format!(
                    "attempt to perform arithmetic on a {} value",
                    if !a.is_number() && Self::coerce_to_number(a).is_none() { a.type_name() } else { b.type_name() }
                )))
            }
        }
    }

    /// Perform unary minus: try raw, then metamethod.
    fn arith_unm(&mut self, a: Value) -> Result<Value, LuaError> {
        if let Some(result) = Self::try_arith_unm(a) {
            return Ok(result);
        }
        if let Some(mm) = self.get_metamethod(a, MM_UNM) {
            // Unary ops get a dummy second argument equal to the first
            return self.call_metamethod(mm, &[a, a]);
        }
        Err(LuaError::new(format!(
            "attempt to perform arithmetic on a {} value",
            a.type_name()
        )))
    }

    // ── Bitwise helpers ────────────────────────────────────────────

    /// Perform binary bitwise op with coercion + metamethod fallback.
    fn bitwise_binop(
        &mut self,
        a: Value,
        b: Value,
        raw_fn: fn(i64, i64) -> i64,
        mm_name: &[u8],
    ) -> Result<Value, LuaError> {
        if let (Some(x), Some(y)) = (Self::coerce_to_integer(a), Self::coerce_to_integer(b)) {
            return Ok(Value::Integer(raw_fn(x, y)));
        }
        if let Some(mm) = self.get_binop_metamethod(a, b, mm_name) {
            return self.call_metamethod(mm, &[a, b]);
        }
        Err(LuaError::new(format!(
            "attempt to perform bitwise operation on a {} value",
            if Self::coerce_to_integer(a).is_none() { a.type_name() } else { b.type_name() }
        )))
    }

    /// Perform unary bitwise NOT with coercion + metamethod fallback.
    fn bitwise_bnot(&mut self, a: Value) -> Result<Value, LuaError> {
        if let Some(x) = Self::coerce_to_integer(a) {
            return Ok(Value::Integer(!x));
        }
        if let Some(mm) = self.get_metamethod(a, MM_BNOT) {
            return self.call_metamethod(mm, &[a, a]);
        }
        Err(LuaError::new(format!(
            "attempt to perform bitwise operation on a {} value",
            a.type_name()
        )))
    }

    // ── Comparison helpers ─────────────────────────────────────────

    /// Raw equality (no metamethods). Used by PartialEq on Value.
    fn compare_eq_raw(a: Value, b: Value) -> bool {
        a == b
    }

    /// Equality with __eq metamethod support.
    fn compare_eq(&mut self, a: Value, b: Value) -> Result<bool, LuaError> {
        // Primitive equality first
        if a == b {
            return Ok(true);
        }
        // __eq is only tried when both are tables or both are full userdata
        // and they are not primitively equal
        let both_tables = a.is_table() && b.is_table();
        if !both_tables {
            return Ok(false);
        }
        if let Some(mm) = self.get_binop_metamethod(a, b, MM_EQ) {
            let result = self.call_metamethod(mm, &[a, b])?;
            return Ok(result.is_truthy());
        }
        Ok(false)
    }

    fn try_compare_lt(a: Value, b: Value) -> Option<bool> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => Some(x < y),
            (Value::Float(x), Value::Float(y)) => Some(x < y),
            (Value::Integer(x), Value::Float(y)) => Some((x as f64) < y),
            (Value::Float(x), Value::Integer(y)) => Some(x < (y as f64)),
            (Value::Object(ra), Value::Object(rb)) => {
                match (&ra.as_object().kind, &rb.as_object().kind) {
                    (GcObjectKind::String(sa), GcObjectKind::String(sb)) => {
                        Some(sa.as_bytes() < sb.as_bytes())
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn compare_lt(&mut self, a: Value, b: Value) -> Result<bool, LuaError> {
        if let Some(result) = Self::try_compare_lt(a, b) {
            return Ok(result);
        }
        if let Some(mm) = self.get_binop_metamethod(a, b, MM_LT) {
            let result = self.call_metamethod(mm, &[a, b])?;
            return Ok(result.is_truthy());
        }
        Err(LuaError::new(format!(
            "attempt to compare {} with {}",
            a.type_name(),
            b.type_name()
        )))
    }

    fn try_compare_le(a: Value, b: Value) -> Option<bool> {
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => Some(x <= y),
            (Value::Float(x), Value::Float(y)) => Some(x <= y),
            (Value::Integer(x), Value::Float(y)) => Some((x as f64) <= y),
            (Value::Float(x), Value::Integer(y)) => Some(x <= (y as f64)),
            (Value::Object(ra), Value::Object(rb)) => {
                match (&ra.as_object().kind, &rb.as_object().kind) {
                    (GcObjectKind::String(sa), GcObjectKind::String(sb)) => {
                        Some(sa.as_bytes() <= sb.as_bytes())
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn compare_le(&mut self, a: Value, b: Value) -> Result<bool, LuaError> {
        if let Some(result) = Self::try_compare_le(a, b) {
            return Ok(result);
        }
        if let Some(mm) = self.get_binop_metamethod(a, b, MM_LE) {
            let result = self.call_metamethod(mm, &[a, b])?;
            return Ok(result.is_truthy());
        }
        Err(LuaError::new(format!(
            "attempt to compare {} with {}",
            a.type_name(),
            b.type_name()
        )))
    }

    // ── String concatenation helper ────────────────────────────────

    /// Check if a value can be raw-concatenated (string or number).
    fn is_concat_raw(v: Value) -> bool {
        v.is_string() || v.is_number()
    }

    fn concat_values(&mut self, base: usize, from: usize, to: usize) -> Result<Value, LuaError> {
        // Check if all values can be raw-concatenated
        let all_raw = (from..=to).all(|i| Self::is_concat_raw(self.stack[base + i]));

        if all_raw {
            let mut result = Vec::new();
            for i in from..=to {
                let v = self.stack[base + i];
                match v {
                    Value::Object(r) if r.as_object().as_string().is_some() => {
                        result.extend_from_slice(r.as_object().as_string().unwrap().as_bytes());
                    }
                    Value::Integer(n) => {
                        result.extend_from_slice(format!("{n}").as_bytes());
                    }
                    Value::Float(n) => {
                        result.extend_from_slice(format!("{n}").as_bytes());
                    }
                    _ => unreachable!(),
                }
            }
            return Ok(Value::Object(self.gc.new_string(&result)));
        }

        // Need metamethods: fold right-to-left
        let mut acc = self.stack[base + to];
        for i in (from..to).rev() {
            let lhs = self.stack[base + i];
            if Self::is_concat_raw(lhs) && Self::is_concat_raw(acc) {
                // Raw concat of these two
                let mut buf = Vec::new();
                Self::append_to_buf(lhs, &mut buf);
                Self::append_to_buf(acc, &mut buf);
                acc = Value::Object(self.gc.new_string(&buf));
            } else {
                // Try __concat metamethod
                if let Some(mm) = self.get_binop_metamethod(lhs, acc, MM_CONCAT) {
                    acc = self.call_metamethod(mm, &[lhs, acc])?;
                } else {
                    return Err(LuaError::new(format!(
                        "attempt to concatenate a {} value",
                        if !Self::is_concat_raw(lhs) { lhs.type_name() } else { acc.type_name() }
                    )));
                }
            }
        }
        Ok(acc)
    }

    fn append_to_buf(v: Value, buf: &mut Vec<u8>) {
        match v {
            Value::Object(r) if r.as_object().as_string().is_some() => {
                buf.extend_from_slice(r.as_object().as_string().unwrap().as_bytes());
            }
            Value::Integer(n) => buf.extend_from_slice(format!("{n}").as_bytes()),
            Value::Float(n) => buf.extend_from_slice(format!("{n}").as_bytes()),
            _ => {}
        }
    }

    // ── Table access helpers ───────────────────────────────────────

    /// Table get with __index metamethod support.
    fn table_get(&mut self, table_val: Value, key: Value) -> Result<Value, LuaError> {
        let mut current = table_val;
        let mut limit = 16;
        loop {
            limit -= 1;
            if limit == 0 {
                return Err(LuaError::new("'__index' chain too long; possible loop"));
            }

            match current {
                Value::Object(r) if r.as_object().as_table().is_some() => {
                    let raw = r.as_object().as_table().unwrap().raw_get(&key);
                    if !raw.is_nil() {
                        return Ok(raw);
                    }
                    // Key not found: check __index metamethod
                    match self.get_metamethod(current, MM_INDEX) {
                        Some(mm) if mm.is_function() => {
                            return self.call_metamethod(mm, &[current, key]);
                        }
                        Some(mm) => {
                            // __index is a table (or other value with __index)
                            current = mm;
                            continue;
                        }
                        None => return Ok(Value::Nil),
                    }
                }
                _ => {
                    // Not a table: check __index metamethod
                    match self.get_metamethod(current, MM_INDEX) {
                        Some(mm) if mm.is_function() => {
                            return self.call_metamethod(mm, &[current, key]);
                        }
                        Some(mm) => {
                            current = mm;
                            continue;
                        }
                        None => {
                            return Err(LuaError::new(format!(
                                "attempt to index a {} value",
                                table_val.type_name()
                            )));
                        }
                    }
                }
            }
        }
    }

    /// Table set with __newindex metamethod support.
    fn table_set(&mut self, table_val: Value, key: Value, val: Value) -> Result<(), LuaError> {
        let mut current = table_val;
        let mut limit = 16;
        loop {
            limit -= 1;
            if limit == 0 {
                return Err(LuaError::new("'__newindex' chain too long; possible loop"));
            }

            match current {
                Value::Object(r) if r.as_object().as_table().is_some() => {
                    // Check if key already exists (raw)
                    let exists = !r.as_object().as_table().unwrap().raw_get(&key).is_nil();
                    if exists {
                        // Existing key: always do raw set (no __newindex)
                        r.as_object().as_table().unwrap(); // validate
                        // Need mutable access
                        let mut r2 = r;
                        r2.as_object_mut().as_table_mut().unwrap().raw_set(key, val);
                        return Ok(());
                    }
                    // New key: check __newindex
                    match self.get_metamethod(current, MM_NEWINDEX) {
                        Some(mm) if mm.is_function() => {
                            self.call_metamethod(mm, &[current, key, val])?;
                            return Ok(());
                        }
                        Some(mm) => {
                            current = mm;
                            continue;
                        }
                        None => {
                            // No metamethod: raw set
                            let mut r2 = r;
                            r2.as_object_mut().as_table_mut().unwrap().raw_set(key, val);
                            return Ok(());
                        }
                    }
                }
                _ => {
                    match self.get_metamethod(current, MM_NEWINDEX) {
                        Some(mm) if mm.is_function() => {
                            self.call_metamethod(mm, &[current, key, val])?;
                            return Ok(());
                        }
                        Some(mm) => {
                            current = mm;
                            continue;
                        }
                        None => {
                            return Err(LuaError::new(format!(
                                "attempt to index a {} value",
                                table_val.type_name()
                            )));
                        }
                    }
                }
            }
        }
    }

    // ── Garbage collection ───────────────────────────────────────────

    /// Gather all GC roots from the VM state and run a collection cycle.
    fn collect_garbage(&mut self) {
        let mut roots = Vec::new();

        // Root: stack values. For each frame we scan only the *active*
        // register window — from `base` through `base + nactvar_at(pc)`.
        // Beyond that are temporaries that the compiler has already "freed"
        // (even though the slots still hold their last values); treating
        // them as roots would defeat weak-table collection. Duplicate roots
        // are harmless because `mark_object` is idempotent.
        for frame in &self.frames {
            roots.push(frame.closure);
            for val in &frame.varargs {
                if let Value::Object(r) = val {
                    roots.push(*r);
                }
            }
            for uv in &frame.upvalues {
                if let Upvalue::Closed(Value::Object(r)) = &*uv.borrow() {
                    roots.push(*r);
                }
            }

            let nactvar = frame.proto.nactvar_at(frame.pc as u32) as usize;
            let start = frame.base.min(self.stack.len());
            let by_nactvar = (frame.base + nactvar).min(self.stack.len());
            let by_runtime = frame.runtime_top.min(self.stack.len());
            let end = by_nactvar.max(by_runtime);
            for val in &self.stack[start..end] {
                if let Value::Object(r) = val {
                    roots.push(*r);
                }
            }
        }
        // Note: we deliberately do NOT scan [0..self.top] here. `self.top`
        // is a stale high-water mark set by Vararg / Call / etc. and is not
        // reset between instructions, so including it would spuriously root
        // temporaries from long-ago operations — defeating weak-table and
        // finalizer semantics. Per-frame `runtime_top` (widened by Call and
        // Concat before GC points) already covers legitimate in-flight
        // temporaries.

        // Root: closed values in open_upvalues (open ones point into stack, already covered)
        for uv in &self.open_upvalues {
            if let Upvalue::Closed(Value::Object(r)) = &*uv.borrow() {
                roots.push(*r);
            }
        }

        // Root: coroutine objects (main thread, current thread)
        if let Some(mt) = self.main_thread {
            roots.push(mt);
        }
        if let Some(ct) = self.current_thread {
            roots.push(ct);
        }

        // Root: special coroutine function refs
        for r in [
            self.coro_resume_ref, self.coro_yield_ref, self.coro_wrap_ref,
            self.coro_running_ref, self.coro_isyieldable_ref, self.coro_close_ref,
        ] {
            if let Some(r) = r {
                roots.push(r);
            }
        }

        // Root: debug library special function refs
        for r in [
            self.debug_traceback_ref, self.debug_getinfo_ref,
            self.debug_getlocal_ref, self.debug_setlocal_ref,
            self.debug_getupvalue_ref, self.debug_setupvalue_ref,
            self.debug_upvalueid_ref, self.debug_upvaluejoin_ref,
        ] {
            if let Some(r) = r {
                roots.push(r);
            }
        }

        // Root: pcall guard handler values (for xpcall)
        for guard in &self.pcall_guards {
            if let Value::Object(r) = guard.handler {
                roots.push(r);
            }
        }

        // Root: metatables stored on VM and GC
        if let Some(r) = self.string_metatable {
            roots.push(r);
        }
        if let Some(r) = self.gc.file_metatable {
            roots.push(r);
        }

        // Root: package / require special refs, package table, and globals.
        for r in [
            self.require_ref,
            self.load_ref,
            self.loadfile_ref,
            self.dofile_ref,
            self.preload_searcher_ref,
            self.file_searcher_ref,
            self.package_ref,
            self.globals_ref,
            self.collectgarbage_ref,
        ] {
            if let Some(r) = r {
                roots.push(r);
            }
        }

        self.gc.collect(&roots);
        self.run_pending_finalizers();
    }

    /// Drain `__gc` finalizers queued by the most recent collection. Each
    /// finalizer is called with the object as its sole argument; errors are
    /// reported to stderr and do not propagate (matches Lua's warning
    /// semantics for finalizers). Finalizers run LIFO: the most-recently
    /// registered runs first.
    fn run_pending_finalizers(&mut self) {
        // Re-entrancy guard: if a finalizer triggers another collect that
        // queues new finalizers, the outer loop will pick them up.
        if self.in_finalizer {
            return;
        }
        self.in_finalizer = true;
        while let Some(obj_ref) = self.gc.pop_pending_finalizer() {
            let val = Value::Object(obj_ref);
            // The obj_ref is no longer rooted via gc.pending_finalizers, but
            // we pass it as an argument to call_value (placed on the VM stack)
            // so it stays alive across any GC triggered by the finalizer.
            let mm = match obj_ref.as_object().kind {
                GcObjectKind::Table(_) | GcObjectKind::Userdata(_) => {
                    self.get_metamethod(val, MM_GC)
                }
                _ => None,
            };
            if let Some(mm) = mm {
                if let Err(e) = self.call_value(mm, &[val]) {
                    eprintln!("Lua warning: error in __gc finalizer: {e}");
                }
            }
        }
        self.in_finalizer = false;
    }

    /// Check if GC should run and trigger it if so.
    #[inline]
    fn maybe_collect(&mut self) {
        if self.gc.should_collect() {
            self.collect_garbage();
        }
    }

    // ── Main dispatch loop ─────────────────────────────────────────

    fn execute(&mut self) -> Result<(), LuaError> {
        self.execute_to_depth(0)
    }

    fn  execute_to_depth(&mut self, min_depth: usize) -> Result<(), LuaError> {
        loop {
            if self.frames.len() <= min_depth {
                return Ok(());
            }

            // If a coroutine has yielded, stop executing and bubble up.
            if self.yielded.is_some() {
                return Ok(());
            }

            let fi = self.frames.len() - 1;
            let pc = self.frames[fi].pc;
            let base = self.frames[fi].base;
            let inst = self.frames[fi].proto.code[pc];
            self.frames[fi].pc += 1;
            // Between instructions, no temporaries are "in flight" — the
            // active-locals region is the only live root in this frame.
            // Individual opcodes that spill values into temps above locals
            // (Call, Concat) will widen `runtime_top` before a GC point.
            self.frames[fi].runtime_top = base;

            let op = OpCode::from_u8(decode_op(inst))
                .ok_or_else(|| LuaError::new(format!("invalid opcode: {}", decode_op(inst))))?;
            let a = decode_a(inst) as usize;
            let b = decode_b(inst) as usize;
            let c = decode_c(inst) as usize;
            let bx = decode_bx(inst) as usize;
            let sbx = decode_sbx(inst);

            match op {
                // ── Loading ────────────────────────────────────────
                OpCode::Move => {
                    let val = self.reg(base, b);
                    self.set_reg(base, a, val);
                }

                OpCode::LoadI => {
                    self.set_reg(base, a, Value::Integer(sbx as i64));
                }

                OpCode::LoadK => {
                    let val = self.frames[fi].proto.constants[bx].to_value(&mut self.gc);
                    self.set_reg(base, a, val);
                }

                OpCode::LoadKX => {
                    let next_inst = self.frames[fi].proto.code[self.frames[fi].pc];
                    self.frames[fi].pc += 1;
                    let ax = decode_ax(next_inst) as usize;
                    let val = self.frames[fi].proto.constants[ax].to_value(&mut self.gc);
                    self.set_reg(base, a, val);
                }

                OpCode::LoadBool => {
                    self.set_reg(base, a, Value::Boolean(b != 0));
                    if c != 0 {
                        self.frames[fi].pc += 1;
                    }
                }

                OpCode::LoadNil => {
                    for i in a..=a + b {
                        self.set_reg(base, i, Value::Nil);
                    }
                }

                // ── Upvalues ───────────────────────────────────────
                OpCode::GetUpval => {
                    let upvalues = &self.frames[fi].upvalues;
                    let val = self.get_upvalue_val(upvalues, b);
                    self.set_reg(base, a, val);
                }

                OpCode::SetUpval => {
                    let val = self.reg(base, a);
                    let uv = Rc::clone(&self.frames[fi].upvalues[b]);
                    match &mut *uv.borrow_mut() {
                        Upvalue::Open(stack_idx) => self.stack[*stack_idx] = val,
                        Upvalue::Closed(v) => *v = val,
                    }
                }

                OpCode::GetTabUp => {
                    let upvalues = &self.frames[fi].upvalues;
                    let table_val = self.get_upvalue_val(upvalues, b);
                    let key = self.frames[fi].proto.constants[c].to_value(&mut self.gc);
                    let result = self.table_get(table_val, key)?;
                    self.set_reg(base, a, result);
                }

                OpCode::SetTabUp => {
                    let upvalues = &self.frames[fi].upvalues;
                    let table_val = self.get_upvalue_val(upvalues, a);
                    let key = self.frames[fi].proto.constants[b].to_value(&mut self.gc);
                    let val = self.reg(base, c);
                    self.table_set(table_val, key, val)?;
                }

                // ── Tables ─────────────────────────────────────────
                OpCode::NewTable => {
                    self.maybe_collect();
                    let table = Table::with_capacity(b, c);
                    let gc_ref = self.gc.new_table(table);
                    self.set_reg(base, a, Value::Object(gc_ref));
                }

                OpCode::GetTable => {
                    let table_val = self.reg(base, b);
                    let key = self.reg(base, c);
                    let result = self.table_get(table_val, key)?;
                    self.set_reg(base, a, result);
                }

                OpCode::SetTable => {
                    let table_val = self.reg(base, a);
                    let key = self.reg(base, b);
                    let val = self.reg(base, c);
                    self.table_set(table_val, key, val)?;
                }

                OpCode::SetList => {
                    let table_val = self.reg(base, a);
                    let num = if b > 0 {
                        b
                    } else {
                        self.top - (base + a) - 1
                    };
                    let offset = (c as u32 - 1) * FIELDS_PER_FLUSH;

                    if let Value::Object(mut r) = table_val {
                        let obj = r.as_object_mut();
                        if let GcObjectKind::Table(t) = &mut obj.kind {
                            for i in 1..=num {
                                let val = self.stack[base + a + i];
                                let key = offset as i64 + i as i64;
                                while t.array.len() < key as usize {
                                    t.array.push(Value::Nil);
                                }
                                t.array[key as usize - 1] = val;
                            }
                        }
                    }
                }

                // ── Arithmetic ─────────────────────────────────────
                OpCode::Add => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.arith_binop(rb, rc, Self::try_arith_add, MM_ADD)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Sub => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.arith_binop(rb, rc, Self::try_arith_sub, MM_SUB)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Mul => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.arith_binop(rb, rc, Self::try_arith_mul, MM_MUL)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Div => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.arith_binop(rb, rc, Self::try_arith_div, MM_DIV)?;
                    self.set_reg(base, a, result);
                }

                OpCode::IDiv => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.arith_binop_err(rb, rc, Self::try_arith_idiv, MM_IDIV)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Mod => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.arith_binop_err(rb, rc, Self::try_arith_mod, MM_MOD)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Pow => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.arith_binop(rb, rc, Self::try_arith_pow, MM_POW)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Unm => {
                    let rb = self.reg(base, b);
                    let result = self.arith_unm(rb)?;
                    self.set_reg(base, a, result);
                }

                // ── Bitwise ────────────────────────────────────────
                OpCode::BAnd => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.bitwise_binop(rb, rc, |x, y| x & y, MM_BAND)?;
                    self.set_reg(base, a, result);
                }

                OpCode::BOr => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.bitwise_binop(rb, rc, |x, y| x | y, MM_BOR)?;
                    self.set_reg(base, a, result);
                }

                OpCode::BXor => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.bitwise_binop(rb, rc, |x, y| x ^ y, MM_BXOR)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Shl => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.bitwise_binop(rb, rc, lua_shl, MM_SHL)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Shr => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.bitwise_binop(rb, rc, lua_shr, MM_SHR)?;
                    self.set_reg(base, a, result);
                }

                OpCode::BNot => {
                    let rb = self.reg(base, b);
                    let result = self.bitwise_bnot(rb)?;
                    self.set_reg(base, a, result);
                }

                // ── Logic ──────────────────────────────────────────
                OpCode::Not => {
                    let rb = self.reg(base, b);
                    self.set_reg(base, a, Value::Boolean(!rb.is_truthy()));
                }

                // ── String / Length ────────────────────────────────
                OpCode::Concat => {
                    // Concat operands live in [b..=c], which may be above
                    // the active-locals window. Root them before GC.
                    self.frames[fi].runtime_top = base + c as usize + 1;
                    self.maybe_collect();
                    let result = self.concat_values(base, b, c)?;
                    self.set_reg(base, a, result);
                }

                OpCode::Len => {
                    let rb = self.reg(base, b);
                    // __len is checked first for tables (if metatable exists), always for other types
                    let has_mm = match rb {
                        Value::Object(r) if r.as_object().as_string().is_some() => false,
                        _ => self.get_metamethod(rb, MM_LEN).is_some(),
                    };
                    if has_mm {
                        let mm = self.get_metamethod(rb, MM_LEN).unwrap();
                        let result = self.call_metamethod(mm, &[rb, rb])?;
                        self.set_reg(base, a, result);
                    } else {
                        match rb {
                            Value::Object(r) => match &r.as_object().kind {
                                GcObjectKind::String(s) => {
                                    self.set_reg(base, a, Value::Integer(s.len() as i64));
                                }
                                GcObjectKind::Table(t) => {
                                    self.set_reg(base, a, Value::Integer(t.length() as i64));
                                }
                                _ => {
                                    return Err(LuaError::new(format!(
                                        "attempt to get length of a {} value",
                                        rb.type_name()
                                    )))
                                }
                            },
                            _ => {
                                return Err(LuaError::new(format!(
                                    "attempt to get length of a {} value",
                                    rb.type_name()
                                )))
                            }
                        }
                    }
                }

                // ── Comparison & Conditional ───────────────────────
                OpCode::Eq => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.compare_eq(rb, rc)?;
                    if result != (a != 0) {
                        self.frames[fi].pc += 1;
                    }
                }

                OpCode::Lt => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.compare_lt(rb, rc)?;
                    if result != (a != 0) {
                        self.frames[fi].pc += 1;
                    }
                }

                OpCode::Le => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.compare_le(rb, rc)?;
                    if result != (a != 0) {
                        self.frames[fi].pc += 1;
                    }
                }

                OpCode::Test => {
                    let ra = self.reg(base, a);
                    let cond = !ra.is_truthy();
                    if cond == (c != 0) {
                        self.frames[fi].pc += 1;
                    }
                }

                OpCode::TestSet => {
                    let rb = self.reg(base, b);
                    let cond = !rb.is_truthy();
                    if cond == (c != 0) {
                        self.frames[fi].pc += 1;
                    } else {
                        self.set_reg(base, a, rb);
                    }
                }

                // ── Control flow ───────────────────────────────────
                OpCode::Jmp => {
                    self.frames[fi].pc = (self.frames[fi].pc as i64 + sbx as i64) as usize;
                }

                OpCode::ForPrep => {
                    let init = self.reg(base, a);
                    let limit = self.reg(base, a + 1);
                    let step = self.reg(base, a + 2);
                    self.for_prep_validate(init, limit, step)?;
                    // Pre-subtract step so that ForLoop's first increment yields init
                    let adjusted = Self::try_arith_sub(init, step)
                        .ok_or_else(|| LuaError::new("'for' initial value must be a number"))?;
                    self.set_reg(base, a, adjusted);
                    // Jump forward to FORLOOP
                    self.frames[fi].pc =
                        (self.frames[fi].pc as i64 + sbx as i64) as usize;
                }

                OpCode::ForLoop => {
                    let index = self.reg(base, a);
                    let limit = self.reg(base, a + 1);
                    let step = self.reg(base, a + 2);

                    let new_index = Self::try_arith_add(index, step)
                        .ok_or_else(|| LuaError::new("'for' step must be a number"))?;
                    self.set_reg(base, a, new_index);

                    if self.for_loop_check(new_index, limit, step) {
                        self.set_reg(base, a + 3, new_index);
                        self.frames[fi].pc =
                            (self.frames[fi].pc as i64 + sbx as i64) as usize;
                    }
                }

                OpCode::TForPrep => {
                    self.frames[fi].pc =
                        (self.frames[fi].pc as i64 + sbx as i64) as usize;
                }

                OpCode::TForLoop => {
                    // A = base register, B = backward jump offset, C = number of loop variables
                    let iter = self.reg(base, a);
                    let state = self.reg(base, a + 1);
                    let control = self.reg(base, a + 2);

                    // Set up call: place function and args in temp registers
                    let call_base = base + a + 4;
                    self.ensure_stack(call_base + 3);
                    self.stack[call_base] = iter;
                    self.stack[call_base + 1] = state;
                    self.stack[call_base + 2] = control;

                    // Call with 2 args, C results (C = num_vars)
                    self.call_function(call_base, 3, c as i32)?;

                    // Check first result
                    let first_result = self.reg(base, a + 4);
                    if !first_result.is_nil() {
                        self.set_reg(base, a + 2, first_result);
                        // Jump back using B (backward offset)
                        let jump_offset = -(b as i64);
                        self.frames[fi].pc =
                            (self.frames[fi].pc as i64 + jump_offset) as usize;
                    }
                }

                // ── Functions ──────────────────────────────────────
                OpCode::Closure => {
                    self.maybe_collect();
                    let child_proto = {
                        let parent_proto = &self.frames[fi].proto;
                        Rc::new(parent_proto.protos[bx].clone())
                    };

                    let mut new_upvalues = Vec::new();
                    for uv_desc in &child_proto.upvalues {
                        if uv_desc.in_stack {
                            let stack_idx = base + uv_desc.index as usize;
                            new_upvalues.push(self.find_or_create_upvalue(stack_idx));
                        } else {
                            let parent_uv =
                                Rc::clone(&self.frames[fi].upvalues[uv_desc.index as usize]);
                            new_upvalues.push(parent_uv);
                        }
                    }

                    let closure = Closure::new_lua(child_proto, new_upvalues);
                    let gc_ref = self.gc.new_closure(closure);
                    self.set_reg(base, a, Value::Object(gc_ref));
                }

                OpCode::Call => {
                    let func_val = self.stack[base + a];
                    // Detect VM-special functions by GcRef identity
                    let special = if let Value::Object(r) = func_val {
                        if self.pcall_ref == Some(r) { 1 }
                        else if self.xpcall_ref == Some(r) { 2 }
                        else if self.error_ref == Some(r) { 3 }
                        else if self.coro_resume_ref == Some(r) { 4 }
                        else if self.coro_yield_ref == Some(r) { 5 }
                        else if self.coro_running_ref == Some(r) { 6 }
                        else if self.coro_isyieldable_ref == Some(r) { 7 }
                        else if self.coro_close_ref == Some(r) { 8 }
                        else if r.as_object().as_closure().map_or(false, |c| matches!(c, Closure::WrapIterator(_))) { 9 }
                        else if self.debug_traceback_ref == Some(r) { 10 }
                        else if self.debug_getinfo_ref == Some(r) { 11 }
                        else if self.debug_getlocal_ref == Some(r) { 12 }
                        else if self.debug_setlocal_ref == Some(r) { 13 }
                        else if self.debug_getupvalue_ref == Some(r) { 14 }
                        else if self.debug_setupvalue_ref == Some(r) { 15 }
                        else if self.debug_upvalueid_ref == Some(r) { 16 }
                        else if self.debug_upvaluejoin_ref == Some(r) { 17 }
                        else if self.require_ref == Some(r) { 18 }
                        else if self.load_ref == Some(r) { 19 }
                        else if self.loadfile_ref == Some(r) { 20 }
                        else if self.dofile_ref == Some(r) { 21 }
                        else if self.collectgarbage_ref == Some(r) { 22 }
                        else { 0 }
                    } else { 0 };

                    let num_results = if c == 0 { -1 } else { c as i32 - 1 };
                    let num_args = if b > 0 { b - 1 } else { self.top - (base + a) - 1 };

                    // Keep the function and its arguments as roots while the
                    // call is in flight (a native may trigger GC, and these
                    // slots are above the caller's active-locals window).
                    self.frames[fi].runtime_top = base + a + num_args + 1;

                    match special {
                        1 => { // pcall
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_pcall(&args, base + a, num_results)?;
                        }
                        2 => { // xpcall
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_xpcall(&args, base + a, num_results)?;
                        }
                        3 => { // error
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_error(&args)?;
                        }
                        4 => { // coroutine.resume
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_resume(&args, base + a, num_results)?;
                        }
                        5 => { // coroutine.yield
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_yield(&args, base + a, num_results)?;
                        }
                        6 => { // coroutine.running
                            self.handle_running(base + a, num_results);
                        }
                        7 => { // coroutine.isyieldable
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_isyieldable(&args, base + a, num_results);
                        }
                        8 => { // coroutine.close
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_close(&args, base + a, num_results)?;
                        }
                        9 => { // wrap iterator
                            let co_ref = match func_val {
                                Value::Object(r) => match r.as_object().as_closure().unwrap() {
                                    Closure::WrapIterator(co) => *co,
                                    _ => unreachable!(),
                                },
                                _ => unreachable!(),
                            };
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_wrap_call(co_ref, &args, base + a, num_results)?;
                        }
                        10 => { // debug.traceback
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_debug_traceback(&args, base + a, num_results);
                        }
                        11 => { // debug.getinfo
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_debug_getinfo(&args, base + a, num_results)?;
                        }
                        12 => { // debug.getlocal
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_debug_getlocal(&args, base + a, num_results)?;
                        }
                        13 => { // debug.setlocal
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_debug_setlocal(&args, base + a, num_results)?;
                        }
                        14 => { // debug.getupvalue
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_debug_getupvalue(&args, base + a, num_results);
                        }
                        15 => { // debug.setupvalue
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_debug_setupvalue(&args, base + a, num_results);
                        }
                        16 => { // debug.upvalueid
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_debug_upvalueid(&args, base + a, num_results);
                        }
                        17 => { // debug.upvaluejoin
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_debug_upvaluejoin(&args, base + a, num_results)?;
                        }
                        18 => { // require
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_require(&args, base + a, num_results)?;
                        }
                        19 => { // load
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_load(&args, base + a, num_results)?;
                        }
                        20 => { // loadfile
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_loadfile(&args, base + a, num_results)?;
                        }
                        21 => { // dofile
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_dofile(&args, base + a, num_results)?;
                        }
                        22 => { // collectgarbage
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_collectgarbage(&args, base + a, num_results)?;
                        }
                        _ => {
                            self.call_function(base + a, b, num_results)?;
                        }
                    }
                }

                OpCode::TailCall => {
                    let func_val = self.reg(base, a);

                    // Intercept special functions in tail position
                    if let Value::Object(r) = func_val {
                        if self.error_ref == Some(r) {
                            let num_args = if b > 0 { b - 1 } else { self.top - (base + a) - 1 };
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_error(&args)?;
                            unreachable!();
                        }
                        if self.coro_yield_ref == Some(r) {
                            let num_args = if b > 0 { b - 1 } else { self.top - (base + a) - 1 };
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_yield(&args, base + a, self.frames[fi].num_results)?;
                            continue;
                        }
                    }

                    let num_args = if b > 0 { b - 1 } else { self.top - (base + a) - 1 };
                    let args: Vec<Value> = (0..num_args)
                        .map(|i| self.stack[base + a + 1 + i])
                        .collect();

                    self.frames[fi].runtime_top = base + a + num_args + 1;

                    // Check if the target is a Lua function for tail call optimization
                    let is_lua_closure = matches!(func_val,
                        Value::Object(r) if r.as_object().as_closure()
                            .map_or(false, |c| matches!(c, Closure::Lua(_)))
                    );

                    self.close_tbc_vars(base, None)?;
                    self.close_upvalues(base);

                    let result_base = self.frames[fi].result_base;
                    let num_results = self.frames[fi].num_results;

                    if is_lua_closure {
                        // Tail call optimization: pop current frame, reuse slot
                        self.frames.pop();

                        // Place arguments at the base for the new frame
                        for (i, &arg) in args.iter().enumerate() {
                            self.stack[base + 1 + i] = arg;
                        }
                        self.stack[base] = func_val;

                        self.do_call(func_val, base, &args, result_base, num_results)?;
                    } else {
                        // C/native function: keep current frame on stack during the call
                        // so debug functions can see the correct call stack, then pop after
                        self.do_call(func_val, base + a, &args, result_base, num_results)?;
                        self.frames.pop();
                    }
                }

                OpCode::Return => {
                    let num_ret = if b > 0 {
                        b - 1
                    } else {
                        self.top.saturating_sub(base + a)
                    };
                    let results: Vec<Value> =
                        (0..num_ret).map(|i| self.stack[base + a + i]).collect();

                    self.close_tbc_vars(base, None)?;
                    self.close_upvalues(base);

                    let result_base = self.frames[fi].result_base;
                    let num_results = self.frames[fi].num_results;
                    self.frames.pop();

                    if self.frames.is_empty() {
                        self.last_return_values = results;
                        return Ok(());
                    }

                    self.place_results(result_base, num_results, &results);

                    // Check if we just returned from a pcall-guarded frame.
                    // If so, write the `true` prefix that pcall didn't get to write
                    // because it returned early on yield.
                    if let Some(guard) = self.pcall_guards.last() {
                        if self.frames.len() == guard.frame_depth {
                            let guard = self.pcall_guards.pop().unwrap();
                            self.stack[guard.result_base] = Value::Boolean(true);
                        }
                    }
                }

                OpCode::VarArg => {
                    let varargs = self.frames[fi].varargs.clone();
                    if c == 0 {
                        for (i, &val) in varargs.iter().enumerate() {
                            self.ensure_stack(base + a + i);
                            self.stack[base + a + i] = val;
                        }
                        self.top = base + a + varargs.len();
                    } else {
                        let n = c - 1;
                        for i in 0..n {
                            self.ensure_stack(base + a + i);
                            self.stack[base + a + i] =
                                varargs.get(i).copied().unwrap_or(Value::Nil);
                        }
                    }
                }

                OpCode::VarArgPrep => {
                    // Top-level: no-op. Varargs are set up by the caller.
                }

                // ── Scope & cleanup ────────────────────────────────
                OpCode::Close => {
                    self.close_tbc_vars(base + a, None)?;
                    self.close_upvalues(base + a);
                }

                OpCode::Tbc => {
                    // Validate: value must have __close or be nil/false
                    let val = self.reg(base, a);
                    if val != Value::Nil && val != Value::Boolean(false) {
                        if self.get_metamethod(val, MM_CLOSE).is_none() {
                            return Err(LuaError::new(
                                "variable is not closable (no __close metamethod)",
                            ));
                        }
                    }
                    self.tbc_slots.push(base + a);
                }

                OpCode::ExtraArg => {
                    return Err(LuaError::new("unexpected EXTRAARG instruction"));
                }
            }
        }
    }

    // ── Function call implementation ───────────────────────────────

    /// Call a function at stack[func_idx] with arguments.
    fn call_function(
        &mut self,
        func_idx: usize,
        arg_count: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let func_val = self.stack[func_idx];
        let num_args = if arg_count > 0 {
            arg_count - 1
        } else {
            self.top - func_idx - 1
        };
        let args: Vec<Value> = (0..num_args)
            .map(|i| self.stack[func_idx + 1 + i])
            .collect();

        self.do_call(func_val, func_idx, &args, func_idx, num_results)
    }

    /// Internal call dispatch: handles both Lua and native closures, plus __call.
    fn do_call(
        &mut self,
        func_val: Value,
        call_base: usize,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        // Resolve __call chain
        let mut actual_func = func_val;
        let mut actual_args = args.to_vec();
        let mut call_limit = 16;
        loop {
            let gc_ref = match actual_func {
                Value::Object(r) if r.as_object().as_closure().is_some() => r,
                _ => {
                    call_limit -= 1;
                    if call_limit == 0 {
                        return Err(LuaError::new("'__call' chain too long"));
                    }
                    match self.get_metamethod(actual_func, MM_CALL) {
                        Some(mm) => {
                            // Prepend the original value as first arg
                            let mut new_args = Vec::with_capacity(actual_args.len() + 1);
                            new_args.push(actual_func);
                            new_args.extend(actual_args);
                            actual_func = mm;
                            actual_args = new_args;
                            continue;
                        }
                        None => {
                            return Err(LuaError::new(format!(
                                "attempt to call a {} value",
                                func_val.type_name()
                            )));
                        }
                    }
                }
            };

            let is_native = matches!(
                gc_ref.as_object().as_closure().unwrap(),
                Closure::Native(_) | Closure::NativeDyn(_) | Closure::WrapIterator(_)
            );

            if is_native {
                // Intercept error() to add source:line annotation
                if self.error_ref == Some(gc_ref) {
                    return self.handle_error(&actual_args);
                }
                // Intercept coroutine special functions
                if self.coro_resume_ref == Some(gc_ref) {
                    return self.handle_resume(&actual_args, result_base, num_results);
                }
                if self.coro_yield_ref == Some(gc_ref) {
                    return self.handle_yield(&actual_args, result_base, num_results);
                }
                // Intercept debug VM-special functions
                if self.debug_traceback_ref == Some(gc_ref) {
                    self.handle_debug_traceback(&actual_args, result_base, num_results);
                    return Ok(());
                }
                if self.debug_getinfo_ref == Some(gc_ref) {
                    return self.handle_debug_getinfo(&actual_args, result_base, num_results);
                }
                if self.debug_getlocal_ref == Some(gc_ref) {
                    return self.handle_debug_getlocal(&actual_args, result_base, num_results);
                }
                if self.debug_setlocal_ref == Some(gc_ref) {
                    return self.handle_debug_setlocal(&actual_args, result_base, num_results);
                }
                if self.debug_getupvalue_ref == Some(gc_ref) {
                    self.handle_debug_getupvalue(&actual_args, result_base, num_results);
                    return Ok(());
                }
                if self.debug_setupvalue_ref == Some(gc_ref) {
                    self.handle_debug_setupvalue(&actual_args, result_base, num_results);
                    return Ok(());
                }
                if self.debug_upvalueid_ref == Some(gc_ref) {
                    self.handle_debug_upvalueid(&actual_args, result_base, num_results);
                    return Ok(());
                }
                if self.debug_upvaluejoin_ref == Some(gc_ref) {
                    return self.handle_debug_upvaluejoin(&actual_args, result_base, num_results);
                }
                if self.require_ref == Some(gc_ref) {
                    return self.handle_require(&actual_args, result_base, num_results);
                }
                if self.load_ref == Some(gc_ref) {
                    return self.handle_load(&actual_args, result_base, num_results);
                }
                if self.loadfile_ref == Some(gc_ref) {
                    return self.handle_loadfile(&actual_args, result_base, num_results);
                }
                if self.dofile_ref == Some(gc_ref) {
                    return self.handle_dofile(&actual_args, result_base, num_results);
                }
                if self.collectgarbage_ref == Some(gc_ref) {
                    return self.handle_collectgarbage(&actual_args, result_base, num_results);
                }
                let results = match gc_ref.as_object().as_closure().unwrap() {
                    Closure::Native(nc) => (nc.func)(&actual_args, &mut self.gc)?,
                    Closure::NativeDyn(nc) => (nc.func)(&actual_args, &mut self.gc)?,
                    Closure::WrapIterator(co) => {
                        let co = *co;
                        return self.handle_wrap_call(co, &actual_args, result_base, num_results);
                    }
                    _ => unreachable!(),
                };
                self.place_results(result_base, num_results, &results);
            } else {
                let (proto, upvalues) = match gc_ref.as_object().as_closure().unwrap() {
                    Closure::Lua(lc) => (Rc::clone(&lc.proto), lc.upvalues.clone()),
                    _ => unreachable!(),
                };

                let new_base = call_base + 1;
                let num_params = proto.num_params as usize;
                let is_vararg = proto.is_vararg;
                let max_stack = proto.max_stack_size as usize;

                self.ensure_stack(new_base + max_stack);

                // Place arguments into registers
                for i in 0..num_params.min(actual_args.len()) {
                    self.stack[new_base + i] = actual_args[i];
                }
                for i in actual_args.len()..num_params {
                    self.stack[new_base + i] = Value::Nil;
                }

                let varargs = if is_vararg && actual_args.len() > num_params {
                    actual_args[num_params..].to_vec()
                } else {
                    Vec::new()
                };

                self.frames.push(CallFrame {
                    closure: gc_ref,
                    proto: Rc::clone(&proto),
                    upvalues,
                    base: new_base,
                    pc: 0,
                    result_base,
                    num_results,
                    varargs: varargs.clone(),
                    runtime_top: new_base,
                });

                // Named vararg table: create table from varargs and store in register
                if let Some(va_reg) = proto.vararg_name_reg {
                    let mut t = Table::new();
                    for (i, &val) in varargs.iter().enumerate() {
                        t.raw_set(Value::Integer(i as i64 + 1), val);
                    }
                    let n_key = Value::Object(self.gc.new_string(b"n"));
                    t.raw_set(n_key, Value::Integer(varargs.len() as i64));
                    let tref = self.gc.new_table(t);
                    self.stack[new_base + va_reg as usize] = Value::Object(tref);
                }
            }

            return Ok(());
        }
    }

    /// Place call results at the given position.
    fn place_results(&mut self, result_base: usize, num_results: i32, results: &[Value]) {
        if num_results < 0 {
            for (i, &val) in results.iter().enumerate() {
                self.ensure_stack(result_base + i);
                self.stack[result_base + i] = val;
            }
            self.top = result_base + results.len();
        } else {
            let nr = num_results as usize;
            for i in 0..nr {
                self.ensure_stack(result_base + i);
                self.stack[result_base + i] = results.get(i).copied().unwrap_or(Value::Nil);
            }
        }
    }

    // ── Protected call (pcall) ─────────────────────────────────────

    /// Handle pcall(f, ...).
    ///
    /// pcall_args: the arguments passed to pcall itself (f, arg1, arg2, ...)
    /// result_base: where to place (true, results...) or (false, err)
    /// num_results: how many results the caller expects
    fn handle_pcall(
        &mut self,
        pcall_args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let func = pcall_args.first().copied().unwrap_or(Value::Nil);
        let call_args: Vec<Value> = if pcall_args.len() > 1 {
            pcall_args[1..].to_vec()
        } else {
            vec![]
        };

        let saved_depth = self.frames.len();
        let saved_open_uv_len = self.open_upvalues.len();

        // Inner call: results go to result_base+1 to leave room for the boolean
        let inner_result_base = result_base + 1;
        let inner_num_results = if num_results < 0 {
            -1
        } else if num_results > 1 {
            num_results - 1
        } else {
            0
        };

        // Set up the call on the stack
        self.ensure_stack(inner_result_base + call_args.len() + 1);
        self.stack[inner_result_base] = func;
        for (i, arg) in call_args.iter().enumerate() {
            self.stack[inner_result_base + 1 + i] = *arg;
        }

        let call_result = self.do_call(
            func,
            inner_result_base,
            &call_args,
            inner_result_base,
            inner_num_results,
        );

        match call_result {
            Ok(()) => {
                if self.frames.len() > saved_depth {
                    // Lua function: a frame was pushed, execute it protectedly
                    match self.execute_to_depth(saved_depth) {
                        Ok(()) => {
                            // If a yield happened inside pcall, save a guard so
                            // resume can provide error protection and true-prefix.
                            if self.yielded.is_some() {
                                self.pcall_guards.push(PcallGuard {
                                    frame_depth: saved_depth,
                                    open_uv_len: saved_open_uv_len,
                                    result_base,
                                    num_results,
                                    is_xpcall: false,
                                    handler: Value::Nil,
                                });
                                return Ok(());
                            }
                            // Success: prepend true
                            self.stack[result_base] = Value::Boolean(true);
                            if num_results < 0 {
                                // self.top was set by the inner Return; it points
                                // past the last inner result. That's already correct
                                // since results sit at inner_result_base..self.top
                                // and we wrote true at result_base = inner_result_base - 1.
                            }
                        }
                        Err(e) => {
                            let err_val = e.to_value(&mut self.gc);
                            self.recover_from_error(saved_depth, saved_open_uv_len, Some(err_val));
                            self.place_results(
                                result_base,
                                num_results,
                                &[Value::Boolean(false), err_val],
                            );
                        }
                    }
                } else {
                    // Native function completed synchronously
                    // do_call already placed results at inner_result_base
                    self.stack[result_base] = Value::Boolean(true);
                    if num_results < 0 {
                        // top was set by place_results inside do_call. The results
                        // are at inner_result_base..self.top. Result_base has true.
                    }
                }
            }
            Err(e) => {
                // The call itself failed (e.g. calling a non-function)
                let err_val = e.to_value(&mut self.gc);
                self.recover_from_error(saved_depth, saved_open_uv_len, Some(err_val));
                self.place_results(
                    result_base,
                    num_results,
                    &[Value::Boolean(false), err_val],
                );
            }
        }
        Ok(())
    }

    /// Unwind frames and upvalues back to a saved checkpoint after an error.
    fn recover_from_error(&mut self, saved_depth: usize, saved_open_uv_len: usize, err_obj: Option<Value>) {
        // Close TBC vars for all frames being unwound
        if self.frames.len() > saved_depth {
            let from = self.frames[saved_depth].base;
            let _ = self.close_tbc_vars(from, err_obj);
        }
        while self.frames.len() > saved_depth {
            let frame_base = self.frames.last().unwrap().base;
            self.close_upvalues(frame_base);
            self.frames.pop();
        }
        // Trim any open upvalues that were created inside the failed call
        self.open_upvalues.truncate(saved_open_uv_len);
    }

    // ── Coroutine operations ───────────────────────────────────────

    /// Save the currently running thread's execution state into its Coroutine object.
    fn save_vm_to_thread(&mut self, thread_ref: GcRef) {
        let co = thread_ref.as_object_mut().as_coroutine_mut().unwrap();
        co.stack = std::mem::replace(&mut self.stack, vec![Value::Nil; 256]);
        co.frames = std::mem::take(&mut self.frames);
        co.open_upvalues = std::mem::take(&mut self.open_upvalues);
        co.tbc_slots = std::mem::take(&mut self.tbc_slots);
        co.pcall_guards = std::mem::take(&mut self.pcall_guards);
        co.top = self.top;
        self.top = 0;
    }

    /// Load a coroutine's execution state into the VM.
    fn load_thread_to_vm(&mut self, thread_ref: GcRef) {
        let co = thread_ref.as_object_mut().as_coroutine_mut().unwrap();
        self.stack = std::mem::replace(&mut co.stack, Vec::new());
        self.frames = std::mem::take(&mut co.frames);
        self.open_upvalues = std::mem::take(&mut co.open_upvalues);
        self.tbc_slots = std::mem::take(&mut co.tbc_slots);
        self.pcall_guards = std::mem::take(&mut co.pcall_guards);
        self.top = co.top;
        co.top = 0;
    }

    /// Get the GcRef of the currently running thread.
    fn running_thread(&self) -> GcRef {
        self.current_thread.unwrap_or_else(|| self.main_thread.unwrap())
    }

    /// Handle coroutine.resume(co [, val1, ...]).
    fn handle_resume(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let co_val = args.first().copied().unwrap_or(Value::Nil);
        let co_ref = match co_val {
            Value::Object(r) if r.as_object().as_coroutine().is_some() => r,
            _ => {
                let err_str = self.gc.new_string(b"value is not a thread");
                self.place_results(
                    result_base,
                    num_results,
                    &[Value::Boolean(false), Value::Object(err_str)],
                );
                return Ok(());
            }
        };

        let resume_args: Vec<Value> = if args.len() > 1 { args[1..].to_vec() } else { vec![] };

        // Check status
        let status = co_ref.as_object().as_coroutine().unwrap().status;
        if status != CoroutineStatus::Suspended {
            let msg = match status {
                CoroutineStatus::Dead => "cannot resume dead coroutine",
                CoroutineStatus::Running => "cannot resume running coroutine",
                CoroutineStatus::Normal => "cannot resume normal coroutine",
                _ => unreachable!(),
            };
            let err_str = self.gc.new_string(msg.as_bytes());
            self.place_results(
                result_base,
                num_results,
                &[Value::Boolean(false), Value::Object(err_str)],
            );
            return Ok(());
        }

        let is_first_resume = co_ref.as_object().as_coroutine().unwrap().body.is_some();

        // Save the current (resumer) thread state
        let resumer_ref = self.running_thread();
        self.save_vm_to_thread(resumer_ref);

        // Set resumer to Normal
        resumer_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Normal;
        // Save where to place resume results when we come back
        resumer_ref.as_object_mut().as_coroutine_mut().unwrap().yield_result_base = result_base;
        resumer_ref.as_object_mut().as_coroutine_mut().unwrap().yield_num_results = num_results;

        // Load the target coroutine state
        self.load_thread_to_vm(co_ref);
        co_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Running;
        self.current_thread = if co_ref == self.main_thread.unwrap() { None } else { Some(co_ref) };

        if is_first_resume {
            let body = co_ref.as_object_mut().as_coroutine_mut().unwrap().body.take().unwrap();
            // Set up the initial call
            let call_base = 0;
            self.ensure_stack(call_base + resume_args.len() + 2);
            self.stack[call_base] = Value::Object(body);
            for (i, &arg) in resume_args.iter().enumerate() {
                self.stack[call_base + 1 + i] = arg;
            }
            self.do_call(Value::Object(body), call_base, &resume_args, call_base, -1)?;
        } else {
            // Resumed after yield: deliver resume args as yield's return values
            let yr_base = co_ref.as_object().as_coroutine().unwrap().yield_result_base;
            let yr_num = co_ref.as_object().as_coroutine().unwrap().yield_num_results;
            self.place_results(yr_base, yr_num, &resume_args);
        }

        // Execute the coroutine (with pcall guard retry loop).
        let exec_result = loop {
            match self.execute() {
                Ok(()) => break Ok(()),
                Err(e) => {
                    // Check if a pcall guard can catch this error.
                    if let Some(guard) = self.pcall_guards.last() {
                        if guard.frame_depth <= self.frames.len() {
                            let guard = self.pcall_guards.pop().unwrap();
                            let err_val = e.to_value(&mut self.gc);
                            let handled = if guard.is_xpcall {
                                self.call_message_handler(guard.handler, err_val)
                            } else {
                                err_val
                            };
                            self.recover_from_error(guard.frame_depth, guard.open_uv_len, Some(err_val));
                            self.place_results(
                                guard.result_base,
                                guard.num_results,
                                &[Value::Boolean(false), handled],
                            );
                            continue; // Re-enter execution
                        }
                    }
                    break Err(e);
                }
            }
        };

        // Determine outcome
        if let Err(e) = exec_result {
            // Error: coroutine is now dead
            let err_val = e.to_value(&mut self.gc);
            co_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Dead;
            self.save_vm_to_thread(co_ref);

            // Restore resumer
            self.load_thread_to_vm(resumer_ref);
            resumer_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Running;
            self.current_thread = if resumer_ref == self.main_thread.unwrap() { None } else { Some(resumer_ref) };

            let rb = resumer_ref.as_object().as_coroutine().unwrap().yield_result_base;
            let nr = resumer_ref.as_object().as_coroutine().unwrap().yield_num_results;
            self.place_results(rb, nr, &[Value::Boolean(false), err_val]);
        } else if let Some(yield_vals) = self.yielded.take() {
            // Yield: coroutine suspended
            co_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Suspended;
            self.save_vm_to_thread(co_ref);

            // Restore resumer
            self.load_thread_to_vm(resumer_ref);
            resumer_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Running;
            self.current_thread = if resumer_ref == self.main_thread.unwrap() { None } else { Some(resumer_ref) };

            let rb = resumer_ref.as_object().as_coroutine().unwrap().yield_result_base;
            let nr = resumer_ref.as_object().as_coroutine().unwrap().yield_num_results;
            let mut results = Vec::with_capacity(1 + yield_vals.len());
            results.push(Value::Boolean(true));
            results.extend(yield_vals);
            self.place_results(rb, nr, &results);
        } else {
            // Normal return: coroutine's body finished
            let return_vals = std::mem::take(&mut self.last_return_values);
            co_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Dead;
            self.save_vm_to_thread(co_ref);

            // Restore resumer
            self.load_thread_to_vm(resumer_ref);
            resumer_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Running;
            self.current_thread = if resumer_ref == self.main_thread.unwrap() { None } else { Some(resumer_ref) };

            let rb = resumer_ref.as_object().as_coroutine().unwrap().yield_result_base;
            let nr = resumer_ref.as_object().as_coroutine().unwrap().yield_num_results;
            let mut results = Vec::with_capacity(1 + return_vals.len());
            results.push(Value::Boolean(true));
            results.extend(return_vals);
            self.place_results(rb, nr, &results);
        }

        Ok(())
    }

    /// Handle coroutine.yield(...).
    fn handle_yield(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        if self.current_thread.is_none() {
            return Err(LuaError::new("cannot yield from main thread"));
        }

        // Save where to deliver resume arguments when this coroutine is resumed
        let co_ref = self.current_thread.unwrap();
        co_ref.as_object_mut().as_coroutine_mut().unwrap().yield_result_base = result_base;
        co_ref.as_object_mut().as_coroutine_mut().unwrap().yield_num_results = num_results;

        // Set the yield flag — execute_to_depth will stop
        self.yielded = Some(args.to_vec());
        Ok(())
    }

    /// Handle coroutine.wrap(f) iterator call.
    fn handle_wrap_call(
        &mut self,
        co_ref: GcRef,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        // Build pseudo-args for resume: [co, args...]
        let mut resume_args = Vec::with_capacity(1 + args.len());
        resume_args.push(Value::Object(co_ref));
        resume_args.extend_from_slice(args);

        // Save result placement so handle_resume can use it
        // We call handle_resume which will place results at result_base

        // Instead of calling handle_resume, we do a direct resume that strips the boolean prefix
        let status = co_ref.as_object().as_coroutine().unwrap().status;
        if status != CoroutineStatus::Suspended {
            let msg = if status == CoroutineStatus::Dead {
                "cannot resume dead coroutine"
            } else {
                "cannot resume running coroutine"
            };
            return Err(LuaError::new(msg));
        }

        let is_first = co_ref.as_object().as_coroutine().unwrap().body.is_some();
        let resumer_ref = self.running_thread();
        self.save_vm_to_thread(resumer_ref);
        resumer_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Normal;
        resumer_ref.as_object_mut().as_coroutine_mut().unwrap().yield_result_base = result_base;
        resumer_ref.as_object_mut().as_coroutine_mut().unwrap().yield_num_results = num_results;

        self.load_thread_to_vm(co_ref);
        co_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Running;
        self.current_thread = if co_ref == self.main_thread.unwrap() { None } else { Some(co_ref) };

        if is_first {
            let body = co_ref.as_object_mut().as_coroutine_mut().unwrap().body.take().unwrap();
            let call_base = 0;
            self.ensure_stack(call_base + args.len() + 2);
            self.stack[call_base] = Value::Object(body);
            for (i, &arg) in args.iter().enumerate() {
                self.stack[call_base + 1 + i] = arg;
            }
            if let Err(e) = self.do_call(Value::Object(body), call_base, args, call_base, -1) {
                co_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Dead;
                self.save_vm_to_thread(co_ref);
                self.load_thread_to_vm(resumer_ref);
                resumer_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Running;
                self.current_thread = if resumer_ref == self.main_thread.unwrap() { None } else { Some(resumer_ref) };
                return Err(e);
            }
        } else {
            let yr_base = co_ref.as_object().as_coroutine().unwrap().yield_result_base;
            let yr_num = co_ref.as_object().as_coroutine().unwrap().yield_num_results;
            self.place_results(yr_base, yr_num, args);
        }

        let exec_result = loop {
            match self.execute() {
                Ok(()) => break Ok(()),
                Err(e) => {
                    if let Some(guard) = self.pcall_guards.last() {
                        if guard.frame_depth <= self.frames.len() {
                            let guard = self.pcall_guards.pop().unwrap();
                            let err_val = e.to_value(&mut self.gc);
                            let handled = if guard.is_xpcall {
                                self.call_message_handler(guard.handler, err_val)
                            } else {
                                err_val
                            };
                            self.recover_from_error(guard.frame_depth, guard.open_uv_len, Some(err_val));
                            self.place_results(
                                guard.result_base,
                                guard.num_results,
                                &[Value::Boolean(false), handled],
                            );
                            continue;
                        }
                    }
                    break Err(e);
                }
            }
        };

        if let Err(e) = exec_result {
            co_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Dead;
            self.save_vm_to_thread(co_ref);
            self.load_thread_to_vm(resumer_ref);
            resumer_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Running;
            self.current_thread = if resumer_ref == self.main_thread.unwrap() { None } else { Some(resumer_ref) };
            return Err(e);
        } else if let Some(yield_vals) = self.yielded.take() {
            co_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Suspended;
            self.save_vm_to_thread(co_ref);
            self.load_thread_to_vm(resumer_ref);
            resumer_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Running;
            self.current_thread = if resumer_ref == self.main_thread.unwrap() { None } else { Some(resumer_ref) };

            let rb = resumer_ref.as_object().as_coroutine().unwrap().yield_result_base;
            let nr = resumer_ref.as_object().as_coroutine().unwrap().yield_num_results;
            // wrap: no boolean prefix, just the values
            self.place_results(rb, nr, &yield_vals);
        } else {
            let return_vals = std::mem::take(&mut self.last_return_values);
            co_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Dead;
            self.save_vm_to_thread(co_ref);
            self.load_thread_to_vm(resumer_ref);
            resumer_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Running;
            self.current_thread = if resumer_ref == self.main_thread.unwrap() { None } else { Some(resumer_ref) };

            let rb = resumer_ref.as_object().as_coroutine().unwrap().yield_result_base;
            let nr = resumer_ref.as_object().as_coroutine().unwrap().yield_num_results;
            self.place_results(rb, nr, &return_vals);
        }

        Ok(())
    }

    /// Handle coroutine.running().
    fn handle_running(&mut self, result_base: usize, num_results: i32) {
        let thread_ref = self.running_thread();
        let is_main = thread_ref.as_object().as_coroutine().unwrap().is_main;
        self.place_results(
            result_base,
            num_results,
            &[Value::Object(thread_ref), Value::Boolean(is_main)],
        );
    }

    /// Handle coroutine.isyieldable([co]).
    fn handle_isyieldable(&mut self, args: &[Value], result_base: usize, num_results: i32) {
        let co_ref = match args.first() {
            Some(Value::Object(r)) if r.as_object().as_coroutine().is_some() => *r,
            _ => self.running_thread(),
        };
        let is_main = co_ref.as_object().as_coroutine().unwrap().is_main;
        self.place_results(result_base, num_results, &[Value::Boolean(!is_main)]);
    }

    /// Handle coroutine.close([co]).
    fn handle_close(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let co_ref = match args.first() {
            Some(Value::Object(r)) if r.as_object().as_coroutine().is_some() => *r,
            _ => {
                // Default = running coroutine. For now: cannot close running.
                return Err(LuaError::new("cannot close a running coroutine"));
            }
        };

        let status = co_ref.as_object().as_coroutine().unwrap().status;
        match status {
            CoroutineStatus::Dead => {
                self.place_results(result_base, num_results, &[Value::Boolean(true)]);
            }
            CoroutineStatus::Suspended => {
                // Close TBC variables in the suspended coroutine
                // We need to temporarily load the coroutine's state to close its TBC vars
                let resumer_ref = self.running_thread();
                self.save_vm_to_thread(resumer_ref);

                self.load_thread_to_vm(co_ref);
                // Close all TBC vars and upvalues
                if !self.frames.is_empty() {
                    let _ = self.close_tbc_vars(0, None);
                    self.close_upvalues(0);
                }
                self.frames.clear();
                self.save_vm_to_thread(co_ref);
                co_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Dead;

                self.load_thread_to_vm(resumer_ref);
                self.place_results(result_base, num_results, &[Value::Boolean(true)]);
            }
            _ => {
                let msg = format!("cannot close a {} coroutine", status.as_str());
                let err_str = self.gc.new_string(msg.as_bytes());
                self.place_results(
                    result_base,
                    num_results,
                    &[Value::Boolean(false), Value::Object(err_str)],
                );
            }
        }
        Ok(())
    }

    // ── Source location & error annotation ─────────────────────────

    /// Get the source:line string for a given call stack level.
    /// Level 0 = current frame, level 1 = caller, etc.
    /// Returns None if the level is out of range or the frame is a native call.
    fn get_source_line(&self, level: usize) -> Option<String> {
        let num_frames = self.frames.len();
        if level >= num_frames {
            return None;
        }
        let frame = &self.frames[num_frames - 1 - level];
        let pc = if frame.pc > 0 { frame.pc - 1 } else { 0 };
        let line = frame.proto.line_info.get(pc).copied().unwrap_or(0);
        let source = frame.proto.source.as_deref().unwrap_or("?");
        // Strip leading '@' from source name (Lua convention for file names)
        let source = source.strip_prefix('@').unwrap_or(source);
        Some(format!("{}:{}", source, line))
    }

    /// Annotate a string error value with source:line prefix.
    /// If the value is not a string, return it unchanged.
    fn annotate_error(&mut self, err: Value, level: usize) -> Value {
        // Only annotate string errors
        if let Value::Object(r) = err {
            if r.as_object().as_string().is_some() {
                if level > 0 {
                    // level 1 = where error() was called (frame below error's caller)
                    // The current frame is inside the Call dispatch, so we need to
                    // look at frames. Level 1 = the frame that called error().
                    if let Some(loc) = self.get_source_line(level - 1) {
                        let msg = r.as_object().as_string().unwrap();
                        let msg_str = std::str::from_utf8(msg.as_bytes()).unwrap_or("?");
                        let annotated = format!("{}: {}", loc, msg_str);
                        let s = self.gc.new_string(annotated.as_bytes());
                        return Value::Object(s);
                    }
                }
            }
        }
        err
    }

    /// Handle error(msg [, level]).
    /// Raises an error, annotating string messages with source:line based on level.
    fn handle_error(&mut self, args: &[Value]) -> Result<(), LuaError> {
        let msg = args.first().copied().unwrap_or(Value::Nil);
        let level = match args.get(1) {
            Some(Value::Integer(n)) => *n as usize,
            Some(Value::Float(f)) => *f as usize,
            None => 1,
            _ => 0, // non-number level means no annotation
        };

        let annotated = self.annotate_error(msg, level);
        Err(LuaError::with_value(annotated))
    }

    // ── xpcall ─────────────────────────────────────────────────────

    /// Handle xpcall(f, msgh [, arg1, ...]).
    fn handle_xpcall(
        &mut self,
        xpcall_args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let func = xpcall_args.first().copied().unwrap_or(Value::Nil);
        let msgh = xpcall_args.get(1).copied().unwrap_or(Value::Nil);
        let call_args: Vec<Value> = if xpcall_args.len() > 2 {
            xpcall_args[2..].to_vec()
        } else {
            vec![]
        };

        let saved_depth = self.frames.len();
        let saved_open_uv_len = self.open_upvalues.len();

        let inner_result_base = result_base + 1;
        let inner_num_results = if num_results < 0 {
            -1
        } else if num_results > 1 {
            num_results - 1
        } else {
            0
        };

        self.ensure_stack(inner_result_base + call_args.len() + 1);
        self.stack[inner_result_base] = func;
        for (i, arg) in call_args.iter().enumerate() {
            self.stack[inner_result_base + 1 + i] = *arg;
        }

        let call_result = self.do_call(
            func,
            inner_result_base,
            &call_args,
            inner_result_base,
            inner_num_results,
        );

        match call_result {
            Ok(()) => {
                if self.frames.len() > saved_depth {
                    match self.execute_to_depth(saved_depth) {
                        Ok(()) => {
                            // If a yield happened inside xpcall, save a guard.
                            if self.yielded.is_some() {
                                self.pcall_guards.push(PcallGuard {
                                    frame_depth: saved_depth,
                                    open_uv_len: saved_open_uv_len,
                                    result_base,
                                    num_results,
                                    is_xpcall: true,
                                    handler: msgh,
                                });
                                return Ok(());
                            }
                            self.stack[result_base] = Value::Boolean(true);
                        }
                        Err(e) => {
                            let err_val = e.to_value(&mut self.gc);
                            // Call message handler before unwinding
                            let handled = self.call_message_handler(msgh, err_val);
                            self.recover_from_error(saved_depth, saved_open_uv_len, Some(err_val));
                            self.place_results(
                                result_base,
                                num_results,
                                &[Value::Boolean(false), handled],
                            );
                        }
                    }
                } else {
                    self.stack[result_base] = Value::Boolean(true);
                }
            }
            Err(e) => {
                let err_val = e.to_value(&mut self.gc);
                let handled = self.call_message_handler(msgh, err_val);
                self.recover_from_error(saved_depth, saved_open_uv_len, Some(err_val));
                self.place_results(
                    result_base,
                    num_results,
                    &[Value::Boolean(false), handled],
                );
            }
        }
        Ok(())
    }

    /// Call a message handler for xpcall. If the handler itself errors,
    /// return the original error.
    fn call_message_handler(&mut self, msgh: Value, err_val: Value) -> Value {
        // Try to call the handler; if it fails, return original error
        match self.call_value(msgh, &[err_val]) {
            Ok(results) => results.into_iter().next().unwrap_or(Value::Nil),
            Err(_) => err_val, // handler failed, return original error
        }
    }

    // ── Stack traceback ────────────────────────────────────────────

    /// Build a stack traceback string.
    fn traceback(&self, msg: Option<&str>, level: usize) -> String {
        let mut result = String::new();
        if let Some(msg) = msg {
            result.push_str(msg);
            result.push('\n');
        }
        result.push_str("stack traceback:");

        let num_frames = self.frames.len();
        let start = if level < num_frames { num_frames - level } else { 0 };

        for i in (0..start).rev() {
            let frame = &self.frames[i];
            let pc = if frame.pc > 0 { frame.pc - 1 } else { 0 };
            let line = frame.proto.line_info.get(pc).copied().unwrap_or(0);
            let source = frame.proto.source.as_deref().unwrap_or("?");
            let source = source.strip_prefix('@').unwrap_or(source);

            result.push_str("\n\t");
            result.push_str(source);
            result.push(':');
            result.push_str(&line.to_string());
            result.push_str(": in ");

            // Try to find function name
            let closure = frame.closure.as_object().as_closure().unwrap();
            match closure {
                Closure::Lua(_) => {
                    if i == 0 {
                        result.push_str("main chunk");
                    } else {
                        result.push_str("local function");
                    }
                }
                Closure::Native(nc) => {
                    result.push_str("function '");
                    result.push_str(&nc.name);
                    result.push('\'');
                }
                Closure::NativeDyn(nc) => {
                    result.push_str("function '");
                    result.push_str(&nc.name);
                    result.push('\'');
                }
                Closure::WrapIterator(_) => {
                    result.push_str("function 'wrap_iterator'");
                }
            }
        }
        result
    }

    // ── Debug library handlers ─────────────────────────────────────

    /// Handle debug.traceback([message [, level]])
    fn handle_debug_traceback(&mut self, args: &[Value], result_base: usize, num_results: i32) {
        // If message is not a string and not nil, return it directly
        let msg_arg = args.first().copied().unwrap_or(Value::Nil);
        match msg_arg {
            Value::Nil => {}
            Value::Object(r) if r.as_object().as_string().is_some() => {}
            other => {
                // Return the non-string message as-is
                self.place_results(result_base, num_results, &[other]);
                return;
            }
        }

        let msg = match msg_arg {
            Value::Object(r) => {
                let s = r.as_object().as_string().unwrap();
                std::str::from_utf8(s.as_bytes()).ok().map(|s| s.to_string())
            }
            _ => None,
        };

        let level = match args.get(1) {
            Some(Value::Integer(n)) => *n as usize,
            _ => 1,
        };

        // level in traceback is 1-based from the caller of debug.traceback.
        // We need to account for the fact that debug.traceback is not on the call stack
        // (it's handled inline). So the caller is at frames.len()-1.
        let tb = self.traceback(msg.as_deref(), level);
        let s = self.gc.new_string(tb.as_bytes());
        self.place_results(result_base, num_results, &[Value::Object(s)]);
    }

    /// Handle debug.getinfo([thread,] f [, what])
    fn handle_debug_getinfo(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        // Parse arguments: f (number=level or function), what (optional string)
        let (func_or_level, what_str) = match args.first() {
            Some(Value::Integer(level)) => {
                let what = match args.get(1) {
                    Some(Value::Object(r)) => {
                        r.as_object().as_string()
                            .map(|s| std::str::from_utf8(s.as_bytes()).unwrap_or("").to_string())
                    }
                    _ => None,
                };
                (Ok(*level as usize), what)
            }
            Some(Value::Object(r)) if r.as_object().as_closure().is_some() => {
                let what = match args.get(1) {
                    Some(Value::Object(r)) => {
                        r.as_object().as_string()
                            .map(|s| std::str::from_utf8(s.as_bytes()).unwrap_or("").to_string())
                    }
                    _ => None,
                };
                (Err(*r), what)
            }
            _ => {
                self.place_results(result_base, num_results, &[Value::Nil]);
                return Ok(());
            }
        };

        // Default what = "flnStu"
        let what = what_str.unwrap_or_else(|| "flnStu".to_string());

        match func_or_level {
            Ok(level) => {
                // Level 0 = getinfo itself (which is not on the stack), so
                // level 1 = the function that called getinfo = top frame
                let num_frames = self.frames.len();
                if level == 0 || level > num_frames {
                    self.place_results(result_base, num_results, &[Value::Nil]);
                    return Ok(());
                }
                let fi = num_frames - level;
                let frame = &self.frames[fi];
                let proto = frame.proto.clone();
                let closure_ref = frame.closure;
                let pc = if frame.pc > 0 { frame.pc - 1 } else { 0 };

                let info = self.build_getinfo_table(&proto, Some(closure_ref), Some(pc), fi == 0, &what);
                let info_ref = self.gc.new_table(info);
                self.place_results(result_base, num_results, &[Value::Object(info_ref)]);
            }
            Err(closure_ref) => {
                let closure = closure_ref.as_object().as_closure().unwrap();
                match closure {
                    Closure::Lua(lc) => {
                        let info = self.build_getinfo_table(&lc.proto, Some(closure_ref), None, false, &what);
                        let info_ref = self.gc.new_table(info);
                        self.place_results(result_base, num_results, &[Value::Object(info_ref)]);
                    }
                    Closure::Native(nc) => {
                        let info = self.build_c_getinfo_table(nc.name, Some(closure_ref), &what);
                        let info_ref = self.gc.new_table(info);
                        self.place_results(result_base, num_results, &[Value::Object(info_ref)]);
                    }
                    Closure::NativeDyn(nc) => {
                        let name = nc.name.clone();
                        let info = self.build_c_getinfo_table(&name, Some(closure_ref), &what);
                        let info_ref = self.gc.new_table(info);
                        self.place_results(result_base, num_results, &[Value::Object(info_ref)]);
                    }
                    _ => {
                        self.place_results(result_base, num_results, &[Value::Nil]);
                    }
                }
            }
        }
        Ok(())
    }

    fn build_getinfo_table(
        &mut self,
        proto: &Proto,
        closure_ref: Option<GcRef>,
        pc: Option<usize>,
        is_main: bool,
        what: &str,
    ) -> Table {
        let mut t = Table::new();

        if what.contains('S') {
            let source = proto.source.as_deref().unwrap_or("=?");
            let source_ref = self.gc.new_string(source.as_bytes());
            let key = self.gc.new_string(b"source");
            t.raw_set(Value::Object(key), Value::Object(source_ref));

            // short_src: strip leading @ for file names
            let short_src = source.strip_prefix('@').unwrap_or(source);
            let short_src_ref = self.gc.new_string(short_src.as_bytes());
            let key = self.gc.new_string(b"short_src");
            t.raw_set(Value::Object(key), Value::Object(short_src_ref));

            // linedefined
            let first_line = proto.line_info.first().copied().unwrap_or(0);
            let key = self.gc.new_string(b"linedefined");
            t.raw_set(Value::Object(key), Value::Integer(first_line as i64));

            // lastlinedefined
            let last_line = proto.line_info.last().copied().unwrap_or(0);
            let key = self.gc.new_string(b"lastlinedefined");
            t.raw_set(Value::Object(key), Value::Integer(last_line as i64));

            // what
            let what_val = if is_main { "main" } else { "Lua" };
            let what_ref = self.gc.new_string(what_val.as_bytes());
            let key = self.gc.new_string(b"what");
            t.raw_set(Value::Object(key), Value::Object(what_ref));
        }

        if what.contains('l') {
            if let Some(pc) = pc {
                let line = proto.line_info.get(pc).copied().unwrap_or(0);
                let key = self.gc.new_string(b"currentline");
                t.raw_set(Value::Object(key), Value::Integer(line as i64));
            } else {
                let key = self.gc.new_string(b"currentline");
                t.raw_set(Value::Object(key), Value::Integer(-1));
            }
        }

        if what.contains('u') {
            let key = self.gc.new_string(b"nups");
            t.raw_set(Value::Object(key), Value::Integer(proto.upvalues.len() as i64));

            let key = self.gc.new_string(b"nparams");
            t.raw_set(Value::Object(key), Value::Integer(proto.num_params as i64));

            let key = self.gc.new_string(b"isvararg");
            t.raw_set(Value::Object(key), Value::Boolean(proto.is_vararg));
        }

        if what.contains('n') {
            // We don't have rich name info, but we can try
            let key = self.gc.new_string(b"name");
            t.raw_set(Value::Object(key), Value::Nil);

            let key = self.gc.new_string(b"namewhat");
            let val = self.gc.new_string(b"");
            t.raw_set(Value::Object(key), Value::Object(val));
        }

        if what.contains('t') {
            // istailcall
            let key = self.gc.new_string(b"istailcall");
            t.raw_set(Value::Object(key), Value::Boolean(false));
        }

        if what.contains('f') {
            if let Some(cr) = closure_ref {
                let key = self.gc.new_string(b"func");
                t.raw_set(Value::Object(key), Value::Object(cr));
            }
        }

        if what.contains('L') {
            // activelines
            let mut lines_table = Table::new();
            for &line in &proto.line_info {
                if line > 0 {
                    lines_table.raw_set(Value::Integer(line as i64), Value::Boolean(true));
                }
            }
            let lines_ref = self.gc.new_table(lines_table);
            let key = self.gc.new_string(b"activelines");
            t.raw_set(Value::Object(key), Value::Object(lines_ref));
        }

        t
    }

    fn build_c_getinfo_table(
        &mut self,
        name: &str,
        closure_ref: Option<GcRef>,
        what: &str,
    ) -> Table {
        let mut t = Table::new();

        if what.contains('S') {
            let source_ref = self.gc.new_string(b"=[C]");
            let key = self.gc.new_string(b"source");
            t.raw_set(Value::Object(key), Value::Object(source_ref));

            let short_src_ref = self.gc.new_string(b"[C]");
            let key = self.gc.new_string(b"short_src");
            t.raw_set(Value::Object(key), Value::Object(short_src_ref));

            let key = self.gc.new_string(b"linedefined");
            t.raw_set(Value::Object(key), Value::Integer(-1));

            let key = self.gc.new_string(b"lastlinedefined");
            t.raw_set(Value::Object(key), Value::Integer(-1));

            let what_ref = self.gc.new_string(b"C");
            let key = self.gc.new_string(b"what");
            t.raw_set(Value::Object(key), Value::Object(what_ref));
        }

        if what.contains('l') {
            let key = self.gc.new_string(b"currentline");
            t.raw_set(Value::Object(key), Value::Integer(-1));
        }

        if what.contains('u') {
            let key = self.gc.new_string(b"nups");
            t.raw_set(Value::Object(key), Value::Integer(0));

            let key = self.gc.new_string(b"nparams");
            t.raw_set(Value::Object(key), Value::Integer(0));

            let key = self.gc.new_string(b"isvararg");
            t.raw_set(Value::Object(key), Value::Boolean(true));
        }

        if what.contains('n') {
            let name_ref = self.gc.new_string(name.as_bytes());
            let key = self.gc.new_string(b"name");
            t.raw_set(Value::Object(key), Value::Object(name_ref));

            let val = self.gc.new_string(b"");
            let key = self.gc.new_string(b"namewhat");
            t.raw_set(Value::Object(key), Value::Object(val));
        }

        if what.contains('t') {
            let key = self.gc.new_string(b"istailcall");
            t.raw_set(Value::Object(key), Value::Boolean(false));
        }

        if what.contains('f') {
            if let Some(cr) = closure_ref {
                let key = self.gc.new_string(b"func");
                t.raw_set(Value::Object(key), Value::Object(cr));
            }
        }

        t
    }

    /// Handle debug.getlocal([thread,] f, local)
    fn handle_debug_getlocal(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        // Parse: level (integer), local (integer)
        let (level, local_idx) = match (args.first(), args.get(1)) {
            (Some(Value::Integer(lvl)), Some(Value::Integer(idx))) => (*lvl as usize, *idx),
            (Some(Value::Object(r)), Some(Value::Integer(idx))) => {
                // f is a function: return only the name of parameter
                if let Some(closure) = r.as_object().as_closure() {
                    if let Closure::Lua(lc) = closure {
                        let i = *idx as usize;
                        if i >= 1 && i <= lc.proto.num_params as usize {
                            if let Some(local) = lc.proto.locals.get(i - 1) {
                                let name = self.gc.new_string(local.name.as_bytes());
                                self.place_results(result_base, num_results, &[Value::Object(name)]);
                                return Ok(());
                            }
                        }
                    }
                }
                self.place_results(result_base, num_results, &[Value::Nil]);
                return Ok(());
            }
            _ => {
                self.place_results(result_base, num_results, &[Value::Nil]);
                return Ok(());
            }
        };

        let num_frames = self.frames.len();
        if level == 0 || level > num_frames {
            return Err(LuaError::new("bad argument #1 to 'getlocal' (level out of range)"));
        }
        let fi = num_frames - level;
        let frame = &self.frames[fi];
        let pc = if frame.pc > 0 { frame.pc - 1 } else { 0 };

        if local_idx < 0 {
            // Negative indices: vararg arguments
            let vararg_idx = (-(local_idx)) as usize - 1;
            if vararg_idx < frame.varargs.len() {
                let name = self.gc.new_string(b"(*vararg)");
                let val = frame.varargs[vararg_idx];
                self.place_results(result_base, num_results, &[Value::Object(name), val]);
            } else {
                self.place_results(result_base, num_results, &[Value::Nil]);
            }
            return Ok(());
        }

        let local_idx = local_idx as usize;
        if local_idx == 0 {
            self.place_results(result_base, num_results, &[Value::Nil]);
            return Ok(());
        }

        // Find the local active at the current pc
        let mut active_count = 0usize;
        for local in &frame.proto.locals {
            if pc as u32 >= local.start_pc && pc as u32 <= local.end_pc {
                active_count += 1;
                if active_count == local_idx {
                    let name = self.gc.new_string(local.name.as_bytes());
                    let val = self.stack[frame.base + active_count - 1];
                    self.place_results(result_base, num_results, &[Value::Object(name), val]);
                    return Ok(());
                }
            }
        }

        self.place_results(result_base, num_results, &[Value::Nil]);
        Ok(())
    }

    /// Handle debug.setlocal([thread,] level, local, value)
    fn handle_debug_setlocal(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let (level, local_idx, value) = match (args.first(), args.get(1), args.get(2)) {
            (Some(Value::Integer(lvl)), Some(Value::Integer(idx)), Some(val)) => {
                (*lvl as usize, *idx as usize, *val)
            }
            _ => {
                self.place_results(result_base, num_results, &[Value::Nil]);
                return Ok(());
            }
        };

        let num_frames = self.frames.len();
        if level == 0 || level > num_frames {
            return Err(LuaError::new("bad argument #1 to 'setlocal' (level out of range)"));
        }
        if local_idx == 0 {
            self.place_results(result_base, num_results, &[Value::Nil]);
            return Ok(());
        }
        let fi = num_frames - level;
        let frame = &self.frames[fi];
        let pc = if frame.pc > 0 { frame.pc - 1 } else { 0 };
        let frame_base = frame.base;

        // Find the local active at the current pc
        let mut active_count = 0usize;
        let locals = frame.proto.locals.clone();
        for local in &locals {
            if pc as u32 >= local.start_pc && pc as u32 <= local.end_pc {
                active_count += 1;
                if active_count == local_idx {
                    self.stack[frame_base + active_count - 1] = value;
                    let name = self.gc.new_string(local.name.as_bytes());
                    self.place_results(result_base, num_results, &[Value::Object(name)]);
                    return Ok(());
                }
            }
        }

        self.place_results(result_base, num_results, &[Value::Nil]);
        Ok(())
    }

    /// Handle debug.getupvalue(f, up)
    fn handle_debug_getupvalue(&mut self, args: &[Value], result_base: usize, num_results: i32) {
        let (closure_ref, up_idx) = match (args.first(), args.get(1)) {
            (Some(Value::Object(r)), Some(Value::Integer(idx))) if r.as_object().as_closure().is_some() => {
                (*r, *idx as usize)
            }
            _ => {
                self.place_results(result_base, num_results, &[Value::Nil]);
                return;
            }
        };

        if up_idx == 0 {
            self.place_results(result_base, num_results, &[Value::Nil]);
            return;
        }

        let closure = closure_ref.as_object().as_closure().unwrap();
        match closure {
            Closure::Lua(lc) => {
                if up_idx > lc.upvalues.len() {
                    self.place_results(result_base, num_results, &[Value::Nil]);
                    return;
                }
                let uv_ref = &lc.upvalues[up_idx - 1];
                let val = match &*uv_ref.borrow() {
                    Upvalue::Open(idx) => self.stack[*idx],
                    Upvalue::Closed(v) => *v,
                };
                let name = lc.proto.upvalues.get(up_idx - 1)
                    .and_then(|ud| ud.name.as_deref())
                    .unwrap_or("?");
                let name_ref = self.gc.new_string(name.as_bytes());
                self.place_results(result_base, num_results, &[Value::Object(name_ref), val]);
            }
            _ => {
                // C functions don't have upvalues in our implementation
                self.place_results(result_base, num_results, &[Value::Nil]);
            }
        }
    }

    /// Handle debug.setupvalue(f, up, value)
    fn handle_debug_setupvalue(&mut self, args: &[Value], result_base: usize, num_results: i32) {
        let (closure_ref, up_idx, value) = match (args.first(), args.get(1), args.get(2)) {
            (Some(Value::Object(r)), Some(Value::Integer(idx)), Some(val))
                if r.as_object().as_closure().is_some() =>
            {
                (*r, *idx as usize, *val)
            }
            _ => {
                self.place_results(result_base, num_results, &[Value::Nil]);
                return;
            }
        };

        if up_idx == 0 {
            self.place_results(result_base, num_results, &[Value::Nil]);
            return;
        }

        let closure = closure_ref.as_object().as_closure().unwrap();
        match closure {
            Closure::Lua(lc) => {
                if up_idx > lc.upvalues.len() {
                    self.place_results(result_base, num_results, &[Value::Nil]);
                    return;
                }
                let name = lc.proto.upvalues.get(up_idx - 1)
                    .and_then(|ud| ud.name.as_deref())
                    .unwrap_or("?")
                    .to_string();
                let uv_ref = lc.upvalues[up_idx - 1].clone();
                match &mut *uv_ref.borrow_mut() {
                    Upvalue::Open(idx) => {
                        self.stack[*idx] = value;
                    }
                    Upvalue::Closed(v) => {
                        *v = value;
                    }
                }
                let name_ref = self.gc.new_string(name.as_bytes());
                self.place_results(result_base, num_results, &[Value::Object(name_ref)]);
            }
            _ => {
                self.place_results(result_base, num_results, &[Value::Nil]);
            }
        }
    }

    /// Handle debug.upvalueid(f, n)
    fn handle_debug_upvalueid(&mut self, args: &[Value], result_base: usize, num_results: i32) {
        let (closure_ref, up_idx) = match (args.first(), args.get(1)) {
            (Some(Value::Object(r)), Some(Value::Integer(idx))) if r.as_object().as_closure().is_some() => {
                (*r, *idx as usize)
            }
            _ => {
                self.place_results(result_base, num_results, &[Value::Nil]);
                return;
            }
        };

        if up_idx == 0 {
            self.place_results(result_base, num_results, &[Value::Nil]);
            return;
        }

        let closure = closure_ref.as_object().as_closure().unwrap();
        match closure {
            Closure::Lua(lc) => {
                if up_idx > lc.upvalues.len() {
                    self.place_results(result_base, num_results, &[Value::Nil]);
                    return;
                }
                // Use the Rc pointer address as a unique identifier
                let ptr = std::rc::Rc::as_ptr(&lc.upvalues[up_idx - 1]) as usize;
                self.place_results(result_base, num_results, &[Value::Integer(ptr as i64)]);
            }
            _ => {
                self.place_results(result_base, num_results, &[Value::Nil]);
            }
        }
    }

    /// Handle debug.upvaluejoin(f1, n1, f2, n2)
    fn handle_debug_upvaluejoin(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let (f1_ref, n1, f2_ref, n2) = match (args.first(), args.get(1), args.get(2), args.get(3)) {
            (
                Some(Value::Object(r1)),
                Some(Value::Integer(i1)),
                Some(Value::Object(r2)),
                Some(Value::Integer(i2)),
            ) if r1.as_object().as_closure().is_some() && r2.as_object().as_closure().is_some() => {
                (*r1, *i1 as usize, *r2, *i2 as usize)
            }
            _ => {
                return Err(LuaError::new("bad argument to 'upvaluejoin'"));
            }
        };

        if n1 == 0 || n2 == 0 {
            return Err(LuaError::new("bad argument to 'upvaluejoin'"));
        }

        // Get the upvalue ref from f2
        let uv_from_f2 = {
            let c2 = f2_ref.as_object().as_closure().unwrap();
            match c2 {
                Closure::Lua(lc2) => {
                    if n2 > lc2.upvalues.len() {
                        return Err(LuaError::new("invalid upvalue index"));
                    }
                    lc2.upvalues[n2 - 1].clone()
                }
                _ => return Err(LuaError::new("bad argument #3 to 'upvaluejoin' (Lua function expected)")),
            }
        };

        // Set it on f1
        let c1 = f1_ref.as_object_mut().as_closure_mut().unwrap();
        match c1 {
            Closure::Lua(lc1) => {
                if n1 > lc1.upvalues.len() {
                    return Err(LuaError::new("invalid upvalue index"));
                }
                lc1.upvalues[n1 - 1] = uv_from_f2;
            }
            _ => return Err(LuaError::new("bad argument #1 to 'upvaluejoin' (Lua function expected)")),
        }

        self.place_results(result_base, num_results, &[]);
        Ok(())
    }

    // ── Package / require helpers ──────────────────────────────────

    /// Compile Lua source into a closure with `_ENV` bound to the given table.
    fn compile_chunk(
        &mut self,
        source: &[u8],
        chunk_name: &str,
        env: GcRef,
    ) -> Result<GcRef, LuaError> {
        let mut lexer = crate::lexer::Lexer::new(source, chunk_name);
        let tokens = lexer
            .tokenize()
            .map_err(|e| LuaError::new(format!("{e}")))?;
        let mut parser = crate::parser::Parser::new(tokens);
        let block = parser
            .parse_chunk()
            .map_err(|e| LuaError::new(format!("{e}")))?;
        let proto = crate::compiler::compile(&block, Some(chunk_name.to_string()))?;

        let proto_rc = Rc::new(proto);
        let env_upvalue = Rc::new(RefCell::new(Upvalue::Closed(Value::Object(env))));
        let closure = Closure::new_lua(proto_rc, vec![env_upvalue]);
        Ok(self.gc.new_closure(closure))
    }

    /// Read a table field by string key using raw_get.
    fn table_raw_get_str(&mut self, table: GcRef, key: &[u8]) -> Value {
        let key_ref = self.gc.new_string(key);
        table
            .as_object()
            .as_table()
            .map(|t| t.raw_get(&Value::Object(key_ref)))
            .unwrap_or(Value::Nil)
    }

    /// Call a searcher value with a single string argument (modname).
    /// Dispatches to built-in preload/file searcher handlers when the
    /// searcher matches; otherwise uses the normal call machinery.
    fn call_searcher(&mut self, searcher: Value, modname: Value) -> Result<Vec<Value>, LuaError> {
        if let Value::Object(r) = searcher {
            if self.preload_searcher_ref == Some(r) {
                return self.run_preload_searcher(modname);
            }
            if self.file_searcher_ref == Some(r) {
                return self.run_file_searcher(modname);
            }
        }
        self.call_value(searcher, &[modname])
    }

    /// The preload searcher: look up `package.preload[modname]`.
    fn run_preload_searcher(&mut self, modname: Value) -> Result<Vec<Value>, LuaError> {
        let pkg = match self.package_ref {
            Some(r) => r,
            None => return Ok(vec![Value::Nil]),
        };
        let preload = self.table_raw_get_str(pkg, b"preload");
        let loader = match preload {
            Value::Object(r) if r.as_object().as_table().is_some() => {
                r.as_object().as_table().unwrap().raw_get(&modname)
            }
            _ => Value::Nil,
        };
        if loader.is_nil() {
            let name_str = modname
                .as_str_bytes()
                .map(|b| String::from_utf8_lossy(b).to_string())
                .unwrap_or_default();
            let msg = format!("\n\tno field package.preload['{name_str}']");
            Ok(vec![Value::Object(self.gc.new_string(msg.as_bytes()))])
        } else {
            let data = Value::Object(self.gc.new_string(b":preload:"));
            Ok(vec![loader, data])
        }
    }

    /// The default Lua file searcher: look up the module using `package.path`,
    /// read and compile it, return the loader closure plus filename.
    fn run_file_searcher(&mut self, modname: Value) -> Result<Vec<Value>, LuaError> {
        let pkg = match self.package_ref {
            Some(r) => r,
            None => return Ok(vec![Value::Nil]),
        };
        let name_bytes = match modname.as_str_bytes() {
            Some(b) => b.to_vec(),
            None => return Err(LuaError::new("bad argument to searcher (string expected)")),
        };
        let name = match std::str::from_utf8(&name_bytes) {
            Ok(s) => s.to_string(),
            Err(_) => {
                return Err(LuaError::new("bad argument to searcher (invalid utf-8)"));
            }
        };
        let path_val = self.table_raw_get_str(pkg, b"path");
        let path = match path_val.as_str_bytes() {
            Some(b) => String::from_utf8_lossy(b).to_string(),
            None => return Err(LuaError::new("'package.path' must be a string")),
        };

        let filename = match crate::stdlib::package::searchpath(
            &name,
            &path,
            ".",
            crate::stdlib::package::DIRECTORY_SEP,
        ) {
            Ok(f) => f,
            Err(msg) => {
                return Ok(vec![Value::Object(self.gc.new_string(msg.as_bytes()))]);
            }
        };

        let source = match std::fs::read(&filename) {
            Ok(s) => s,
            Err(e) => {
                let msg = format!("\n\tcannot open '{filename}': {e}");
                return Ok(vec![Value::Object(self.gc.new_string(msg.as_bytes()))]);
            }
        };

        let env = self.globals_ref.expect("globals ref not set");
        let chunk_name = format!("@{filename}");
        let closure_ref = self.compile_chunk(&source, &chunk_name, env)?;
        let fname_val = Value::Object(self.gc.new_string(filename.as_bytes()));
        Ok(vec![Value::Object(closure_ref), fname_val])
    }

    /// Handle `require(modname)`.
    fn handle_require(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let modname = args.first().copied().unwrap_or(Value::Nil);
        let modname_bytes = match modname.as_str_bytes() {
            Some(b) => b.to_vec(),
            None => {
                return Err(LuaError::new(
                    "bad argument #1 to 'require' (string expected)",
                ));
            }
        };

        let pkg = self
            .package_ref
            .ok_or_else(|| LuaError::new("package library not initialized"))?;

        // 1. Check package.loaded[modname]
        let loaded = match self.table_raw_get_str(pkg, b"loaded") {
            Value::Object(r) if r.as_object().as_table().is_some() => r,
            _ => {
                return Err(LuaError::new("'package.loaded' must be a table"));
            }
        };
        let cached = loaded.as_object().as_table().unwrap().raw_get(&modname);
        if !cached.is_nil() {
            self.place_results(result_base, num_results, &[cached]);
            return Ok(());
        }

        // 2. Iterate package.searchers
        let searchers = match self.table_raw_get_str(pkg, b"searchers") {
            Value::Object(r) if r.as_object().as_table().is_some() => r,
            _ => {
                return Err(LuaError::new("'package.searchers' must be a table"));
            }
        };

        let mut messages = String::new();
        let mut loader: Option<(Value, Value)> = None;
        for i in 1..i64::MAX {
            let searcher = searchers
                .as_object()
                .as_table()
                .unwrap()
                .raw_get(&Value::Integer(i));
            if searcher.is_nil() {
                break;
            }
            let results = self.call_searcher(searcher, modname)?;
            match results.first().copied() {
                Some(v) if v.is_function() => {
                    loader = Some((v, results.get(1).copied().unwrap_or(Value::Nil)));
                    break;
                }
                Some(Value::Object(r)) if r.as_object().as_string().is_some() => {
                    messages.push_str(&String::from_utf8_lossy(
                        r.as_object().as_string().unwrap().as_bytes(),
                    ));
                }
                _ => {}
            }
        }

        let (loader_fn, loader_data) = match loader {
            Some(pair) => pair,
            None => {
                let name = String::from_utf8_lossy(&modname_bytes);
                return Err(LuaError::new(format!(
                    "module '{name}' not found:{messages}"
                )));
            }
        };

        // 3. Call loader(modname, loader_data)
        let results = self.call_value(loader_fn, &[modname, loader_data])?;
        let loader_result = results.into_iter().next().unwrap_or(Value::Nil);

        // 4. Decide final value: prefer what loader set in package.loaded, then
        //    what it returned, else `true`.
        let after = loaded.as_object().as_table().unwrap().raw_get(&modname);
        let final_val = if !after.is_nil() {
            after
        } else if !loader_result.is_nil() {
            loader_result
        } else {
            Value::Boolean(true)
        };

        // 5. Store and place.
        loaded
            .as_object_mut()
            .as_table_mut()
            .unwrap()
            .raw_set(modname, final_val);

        self.place_results(result_base, num_results, &[final_val, loader_data]);
        Ok(())
    }

    /// Handle `load(chunk [, chunkname [, mode [, env]]])`.
    /// Only the string-chunk form is supported (function-reader form is TODO).
    fn handle_load(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let chunk = args.first().copied().unwrap_or(Value::Nil);
        let chunk_bytes = match chunk.as_str_bytes() {
            Some(b) => b.to_vec(),
            None => {
                // Function-reader form not yet supported
                let err = self.gc.new_string(
                    b"bad argument #1 to 'load' (string expected; function reader not supported)",
                );
                self.place_results(
                    result_base,
                    num_results,
                    &[Value::Nil, Value::Object(err)],
                );
                return Ok(());
            }
        };

        let chunkname = args
            .get(1)
            .and_then(|v| v.as_str_bytes())
            .map(|b| String::from_utf8_lossy(b).to_string())
            .unwrap_or_else(|| "=(load)".to_string());

        // args[2] (mode) is accepted but not enforced for now.

        let env = match args.get(3).copied() {
            Some(Value::Object(r)) if r.as_object().as_table().is_some() => r,
            Some(Value::Nil) | None => self.globals_ref.expect("globals ref not set"),
            _ => {
                let err = self
                    .gc
                    .new_string(b"bad argument #4 to 'load' (table expected)");
                self.place_results(
                    result_base,
                    num_results,
                    &[Value::Nil, Value::Object(err)],
                );
                return Ok(());
            }
        };

        match self.compile_chunk(&chunk_bytes, &chunkname, env) {
            Ok(closure_ref) => {
                self.place_results(result_base, num_results, &[Value::Object(closure_ref)]);
            }
            Err(e) => {
                let msg = self.gc.new_string(e.message.as_bytes());
                self.place_results(
                    result_base,
                    num_results,
                    &[Value::Nil, Value::Object(msg)],
                );
            }
        }
        Ok(())
    }

    /// Handle `loadfile([filename [, mode [, env]]])`.
    fn handle_loadfile(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let filename = match args.first() {
            Some(v) if !v.is_nil() => match v.as_str_bytes() {
                Some(b) => String::from_utf8_lossy(b).to_string(),
                None => {
                    return Err(LuaError::new(
                        "bad argument #1 to 'loadfile' (string expected)",
                    ));
                }
            },
            _ => {
                // stdin not supported
                let err = self
                    .gc
                    .new_string(b"loadfile: reading from stdin is not supported");
                self.place_results(
                    result_base,
                    num_results,
                    &[Value::Nil, Value::Object(err)],
                );
                return Ok(());
            }
        };

        let source = match std::fs::read(&filename) {
            Ok(s) => s,
            Err(e) => {
                let msg = format!("cannot open {filename}: {e}");
                let err = self.gc.new_string(msg.as_bytes());
                self.place_results(
                    result_base,
                    num_results,
                    &[Value::Nil, Value::Object(err)],
                );
                return Ok(());
            }
        };

        let env = match args.get(2).copied() {
            Some(Value::Object(r)) if r.as_object().as_table().is_some() => r,
            Some(Value::Nil) | None => self.globals_ref.expect("globals ref not set"),
            _ => {
                return Err(LuaError::new(
                    "bad argument #3 to 'loadfile' (table expected)",
                ));
            }
        };

        let chunk_name = format!("@{filename}");
        match self.compile_chunk(&source, &chunk_name, env) {
            Ok(closure_ref) => {
                self.place_results(result_base, num_results, &[Value::Object(closure_ref)]);
            }
            Err(e) => {
                let msg = self.gc.new_string(e.message.as_bytes());
                self.place_results(
                    result_base,
                    num_results,
                    &[Value::Nil, Value::Object(msg)],
                );
            }
        }
        Ok(())
    }

    /// Handle `dofile([filename])`.
    fn handle_dofile(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let filename = match args.first() {
            Some(v) if !v.is_nil() => match v.as_str_bytes() {
                Some(b) => String::from_utf8_lossy(b).to_string(),
                None => {
                    return Err(LuaError::new(
                        "bad argument #1 to 'dofile' (string expected)",
                    ));
                }
            },
            _ => {
                return Err(LuaError::new(
                    "dofile: reading from stdin is not supported",
                ));
            }
        };

        let source = std::fs::read(&filename)
            .map_err(|e| LuaError::new(format!("cannot open {filename}: {e}")))?;

        let env = self.globals_ref.expect("globals ref not set");
        let chunk_name = format!("@{filename}");
        let closure_ref = self.compile_chunk(&source, &chunk_name, env)?;

        let results = self.call_value(Value::Object(closure_ref), &[])?;
        self.place_results(result_base, num_results, &results);
        Ok(())
    }

    /// `collectgarbage([opt [, arg]])`. Implements the subset of options the
    /// VM actually needs:
    ///   - `"collect"` (default): run a full mark-and-sweep cycle and any
    ///                            queued `__gc` finalizers; returns 0.
    ///   - `"count"`            : returns memory in KB (float) and remainder
    ///                            in bytes (integer), matching Lua 5.5.
    ///   - `"stop"`/`"restart"` : no-ops (we don't implement incremental GC);
    ///                            returns the (fake) previous memory count.
    ///   - `"isrunning"`        : returns true (always running).
    ///   - `"step"`             : runs a full cycle and returns true.
    ///   - other                : returns 0 (best-effort no-op).
    fn handle_collectgarbage(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let opt = args
            .first()
            .and_then(|v| v.as_str_bytes())
            .map(|b| b.to_vec())
            .unwrap_or_else(|| b"collect".to_vec());

        let results: Vec<Value> = match opt.as_slice() {
            b"collect" | b"step" => {
                self.collect_garbage();
                if opt.as_slice() == b"step" {
                    vec![Value::Boolean(true)]
                } else {
                    vec![Value::Integer(0)]
                }
            }
            b"count" => {
                let bytes = self.gc.bytes_allocated_approx();
                let kb = (bytes as f64) / 1024.0;
                let rem = (bytes % 1024) as i64;
                vec![Value::Float(kb), Value::Integer(rem)]
            }
            b"stop" | b"restart" | b"isrunning" => {
                if opt.as_slice() == b"isrunning" {
                    vec![Value::Boolean(true)]
                } else {
                    vec![Value::Integer(0)]
                }
            }
            _ => vec![Value::Integer(0)],
        };

        self.place_results(result_base, num_results, &results);
        Ok(())
    }

    // ── For-loop helpers ───────────────────────────────────────────

    fn for_prep_validate(
        &self,
        init: Value,
        limit: Value,
        step: Value,
    ) -> Result<(), LuaError> {
        if !init.is_number() {
            return Err(LuaError::new("'for' initial value must be a number"));
        }
        if !limit.is_number() {
            return Err(LuaError::new("'for' limit must be a number"));
        }
        if !step.is_number() {
            return Err(LuaError::new("'for' step must be a number"));
        }
        match step {
            Value::Integer(0) => Err(LuaError::new("'for' step is zero")),
            Value::Float(f) if f == 0.0 => Err(LuaError::new("'for' step is zero")),
            _ => Ok(()),
        }
    }

    fn for_loop_check(&self, index: Value, limit: Value, step: Value) -> bool {
        let step_positive = match step {
            Value::Integer(s) => s > 0,
            Value::Float(f) => f > 0.0,
            _ => true,
        };
        if step_positive {
            Self::try_compare_le(index, limit).unwrap_or(false)
        } else {
            Self::try_compare_le(limit, index).unwrap_or(false)
        }
    }
}

// ── Lua integer arithmetic helpers ─────────────────────────────────

/// Lua floor division for integers.
fn lua_idiv(a: i64, b: i64) -> i64 {
    let d = a / b;
    if (a ^ b) < 0 && d * b != a {
        d - 1
    } else {
        d
    }
}

/// Lua modulo for integers.
fn lua_imod(a: i64, b: i64) -> i64 {
    let r = a % b;
    if r != 0 && (r ^ b) < 0 {
        r + b
    } else {
        r
    }
}

/// Lua modulo for floats.
fn lua_fmod(a: f64, b: f64) -> f64 {
    let r = a % b;
    if r != 0.0 && r.is_sign_negative() != b.is_sign_negative() {
        r + b
    } else {
        r
    }
}

/// Lua left shift.
fn lua_shl(x: i64, y: i64) -> i64 {
    if y >= 64 || y <= -64 {
        0
    } else if y >= 0 {
        (x as u64).wrapping_shl(y as u32) as i64
    } else {
        (x as u64).wrapping_shr((-y) as u32) as i64
    }
}

/// Lua right shift.
fn lua_shr(x: i64, y: i64) -> i64 {
    lua_shl(x, -y)
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn run_lua(source: &str) -> Result<(), LuaError> {
        let mut lexer = Lexer::new(source.as_bytes(), "test");
        let tokens = lexer.tokenize().expect("lexer failed");
        let mut parser = Parser::new(tokens);
        let block = parser.parse_chunk().expect("parser failed");
        let proto = compiler::compile(&block, Some("test".into())).expect("compiler failed");
        let mut vm = Vm::new();
        vm.execute_main(proto)
    }

    #[test]
    fn test_empty_program() {
        run_lua("").unwrap();
    }

    #[test]
    fn test_print_hello() {
        run_lua(r#"print("hello, world!")"#).unwrap();
    }

    #[test]
    fn test_print_arithmetic() {
        run_lua("print(1 + 2)").unwrap();
    }

    #[test]
    fn test_local_variables() {
        run_lua("local x = 10\nlocal y = 20\nprint(x + y)").unwrap();
    }

    #[test]
    fn test_if_else() {
        run_lua(
            r#"
            local x = 10
            if x > 5 then
                print("big")
            else
                print("small")
            end
            "#,
        )
        .unwrap();
    }

    #[test]
    fn test_while_loop() {
        run_lua(
            r#"
            local i = 1
            while i <= 5 do
                print(i)
                i = i + 1
            end
            "#,
        )
        .unwrap();
    }

    #[test]
    fn test_numeric_for() {
        run_lua(
            r#"
            for i = 1, 5 do
                print(i)
            end
            "#,
        )
        .unwrap();
    }

    #[test]
    fn test_function_def_and_call() {
        run_lua(
            r#"
            function add(a, b)
                return a + b
            end
            print(add(3, 4))
            "#,
        )
        .unwrap();
    }

    #[test]
    fn test_local_function() {
        run_lua(
            r#"
            local function fib(n)
                if n <= 1 then
                    return n
                end
                return fib(n - 1) + fib(n - 2)
            end
            print(fib(10))
            "#,
        )
        .unwrap();
    }

    #[test]
    fn test_table_constructor() {
        run_lua(
            r#"
            local t = {1, 2, 3}
            print(t[1], t[2], t[3])
            "#,
        )
        .unwrap();
    }

    #[test]
    fn test_string_concatenation() {
        run_lua(
            r#"
            local name = "world"
            print("hello, " .. name .. "!")
            "#,
        )
        .unwrap();
    }

    #[test]
    fn test_type_function() {
        run_lua(
            r#"
            print(type(42))
            print(type("hello"))
            print(type(nil))
            print(type(true))
            print(type({}))
            print(type(print))
            "#,
        )
        .unwrap();
    }

    #[test]
    fn test_closures() {
        run_lua(
            r#"
            function counter()
                local n = 0
                return function()
                    n = n + 1
                    return n
                end
            end
            local c = counter()
            print(c())
            print(c())
            print(c())
            "#,
        )
        .unwrap();
    }

    #[test]
    fn test_multiple_return() {
        run_lua(
            r#"
            function multi()
                return 1, 2, 3
            end
            print(multi())
            "#,
        )
        .unwrap();
    }

    // ── Coroutine tests ────────────────────────────────────────────

    #[test]
    fn test_coroutine_basic() {
        run_lua(r#"
            local co = coroutine.create(function(a, b) return a + b end)
            local ok, result = coroutine.resume(co, 10, 20)
            assert(ok == true)
            assert(result == 30)
        "#).unwrap();
    }

    #[test]
    fn test_coroutine_yield() {
        run_lua(r#"
            local co = coroutine.create(function(x)
                local y = coroutine.yield(x + 1)
                return y + 2
            end)
            local ok1, v1 = coroutine.resume(co, 10)
            assert(ok1 and v1 == 11)
            local ok2, v2 = coroutine.resume(co, 100)
            assert(ok2 and v2 == 102)
        "#).unwrap();
    }

    #[test]
    fn test_coroutine_status() {
        run_lua(r#"
            local co = coroutine.create(function() coroutine.yield() end)
            assert(coroutine.status(co) == "suspended")
            coroutine.resume(co)
            assert(coroutine.status(co) == "suspended")
            coroutine.resume(co)
            assert(coroutine.status(co) == "dead")
        "#).unwrap();
    }

    #[test]
    fn test_coroutine_wrap() {
        run_lua(r#"
            local gen = coroutine.wrap(function()
                coroutine.yield(10)
                coroutine.yield(20)
                return 30
            end)
            assert(gen() == 10)
            assert(gen() == 20)
            assert(gen() == 30)
        "#).unwrap();
    }

    #[test]
    fn test_coroutine_wrap_for() {
        run_lua(r#"
            local results = {}
            local gen = coroutine.wrap(function()
                for i = 1, 5 do coroutine.yield(i) end
            end)
            for v in gen do results[#results + 1] = v end
            assert(#results == 5 and results[1] == 1 and results[5] == 5)
        "#).unwrap();
    }

    #[test]
    fn test_coroutine_yield_in_pcall() {
        run_lua(r#"
            local co = coroutine.create(function()
                local ok = pcall(function()
                    coroutine.yield(42)
                end)
                return ok, "done"
            end)
            local ok1, v1 = coroutine.resume(co)
            assert(ok1 and v1 == 42)
            local ok2, v2, v3 = coroutine.resume(co)
            assert(ok2 and v2 == true and v3 == "done")
        "#).unwrap();
    }

    #[test]
    fn test_coroutine_error_in_pcall_after_yield() {
        run_lua(r#"
            local co = coroutine.create(function()
                local ok, err = pcall(function()
                    coroutine.yield()
                    error("boom")
                end)
                return ok, err
            end)
            local ok1 = coroutine.resume(co)
            assert(ok1)
            local ok2, pcall_ok, pcall_err = coroutine.resume(co)
            assert(ok2)
            assert(pcall_ok == false)
        "#).unwrap();
    }

    #[test]
    fn test_coroutine_running() {
        run_lua(r#"
            local t, m = coroutine.running()
            assert(type(t) == "thread" and m == true)
            local co = coroutine.create(function()
                local t2, m2 = coroutine.running()
                assert(type(t2) == "thread" and m2 == false)
            end)
            coroutine.resume(co)
        "#).unwrap();
    }

    #[test]
    fn test_coroutine_isyieldable() {
        run_lua(r#"
            assert(coroutine.isyieldable() == false)
            local co = coroutine.create(function()
                assert(coroutine.isyieldable() == true)
            end)
            coroutine.resume(co)
        "#).unwrap();
    }

    #[test]
    fn test_coroutine_close() {
        run_lua(r#"
            local co = coroutine.create(function() coroutine.yield() end)
            coroutine.resume(co)
            assert(coroutine.close(co))
            assert(coroutine.status(co) == "dead")
        "#).unwrap();
    }

    #[test]
    fn test_io_os_library() {
        let source = std::fs::read_to_string("tests/io_os_test.lua")
            .expect("failed to read tests/io_os_test.lua");
        run_lua(&source).unwrap();
    }

    #[test]
    fn test_utf8_library() {
        run_lua(r#"
            -- utf8.char / utf8.codepoint
            assert(utf8.char(72, 101, 108, 108, 111) == "Hello")
            assert(utf8.char(0x4e16, 0x754c) == "世界")
            local a, b = utf8.codepoint("Hello", 1, 2)
            assert(a == 72 and b == 101)

            -- utf8.len
            assert(utf8.len("Hello") == 5)
            assert(utf8.len("世界") == 2)
            assert(utf8.len("") == 0)

            -- utf8.offset
            assert(utf8.offset("Hello", 1) == 1)
            assert(utf8.offset("Hello", 2) == 2)
            local s = "世界"  -- 6 bytes: 3 + 3
            assert(utf8.offset(s, 1) == 1)
            assert(utf8.offset(s, 2) == 4)

            -- utf8.codes
            local t = {}
            for p, c in utf8.codes("Aé") do
                t[#t + 1] = c
            end
            assert(t[1] == 65)    -- 'A'
            assert(t[2] == 0xe9)  -- 'é'
            assert(#t == 2)

            -- utf8.charpattern
            assert(type(utf8.charpattern) == "string")

            -- roundtrip
            local orig = "Hello, 世界! 🌍"
            local cps = {utf8.codepoint(orig, 1, #orig)}
            local rebuilt = utf8.char(table.unpack(cps))
            assert(rebuilt == orig, "roundtrip failed")
        "#).unwrap();
    }

    #[test]
    fn test_debug_library() {
        let source = std::fs::read_to_string("tests/debug_test.lua")
            .expect("failed to read tests/debug_test.lua");
        run_lua(&source).unwrap();
    }
}

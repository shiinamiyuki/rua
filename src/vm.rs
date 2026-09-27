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
const MM_NAME: &[u8] = b"__name";
const MM_PAIRS: &[u8] = b"__pairs";
const MM_METATABLE: &[u8] = b"__metatable";
const MM_CLOSE: &[u8] = b"__close";
const MM_GC: &[u8] = b"__gc";
const MM_MODE: &[u8] = b"__mode";

// Debug hook mask bits.
const HOOK_CALL: u8 = 1;
const HOOK_RET: u8 = 2;
const HOOK_LINE: u8 = 4;
const HOOK_COUNT: u8 = 8;

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
    /// True when this frame is a debug hook invocation.
    pub(crate) is_hook: bool,
    /// True when this frame was entered by a tail call.
    pub(crate) is_tailcall: bool,
    /// Number of extra arguments added by __call chains.
    pub(crate) extraargs: u8,
    /// Last line seen for line hooks (0 = none yet).
    pub(crate) hook_last_line: u32,
    /// Last executed pc, for the backward-jump line-hook rule.
    pub(crate) hook_last_pc: usize,
    /// True once a line event has fired for this frame.
    pub(crate) hook_seen_event: bool,
    /// Cached: does this proto have at most one distinct nonzero line?
    pub(crate) hook_single_line: Option<bool>,
    /// Metamethod name when this frame was invoked as a metamethod.
    pub(crate) metamethod: Option<String>,
    /// True when this frame was entered from a C-level call (protected
    /// call, metamethod, coroutine resume, ...) rather than a Lua CALL.
    pub(crate) called_from_c: bool,
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
    debug_getregistry_ref: Option<GcRef>,
    debug_sethook_ref: Option<GcRef>,
    debug_gethook_ref: Option<GcRef>,
    /// `tostring` / `print` are VM-special so they can call `__tostring`.
    tostring_ref: Option<GcRef>,
    print_ref: Option<GcRef>,
    /// `string.gsub` is VM-special so function replacements can be called.
    gsub_ref: Option<GcRef>,

    /// `string.format` is VM-special so `%s` can call `__tostring`.
    format_ref: Option<GcRef>,
    /// The registry table returned by `debug.getregistry`.
    registry: Option<GcRef>,
    /// Active debug hook state (per-thread; swapped on coroutine switch).
    hook_func: Option<GcRef>,
    hook_mask: u8,
    hook_count: i64,
    hook_counter: i64,
    /// Re-entrancy guard: true while a hook function runs.
    in_hook: bool,
    /// True while the first frame pushed belongs to a hook invocation.
    calling_hook: bool,
    /// True when the next Lua frame pushed is a tail call.
    next_call_is_tail: bool,
    /// Extra args already counted for the next pushed frame (tail calls).
    next_call_extraargs: u8,
    /// Nesting depth of protected calls (pcall/xpcall) for C-stack limits.
    protected_depth: usize,

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

    /// `table.sort` (VM-special: needs to call the comparator).
    sort_ref: Option<GcRef>,

    /// `table.move` (VM-special: honors `__index`/`__newindex`).
    move_ref: Option<GcRef>,

    /// `table.unpack` (VM-special: honors `__index`/`__len`).
    unpack_ref: Option<GcRef>,

    /// `table.insert` (VM-special: honors `__index`/`__newindex`/`__len`).
    insert_ref: Option<GcRef>,

    /// `table.concat` (VM-special: honors `__index`/`__len`).
    concat_ref: Option<GcRef>,

    /// `table.remove` (VM-special: honors `__index`/`__newindex`/`__len`).
    remove_ref: Option<GcRef>,

    /// `next` native (returned by `pairs`).
    next_ref: Option<GcRef>,
    /// `pairs` (VM-special: honors `__pairs`).
    pairs_ref: Option<GcRef>,
    /// `ipairs` (VM-special: returned iterator honors `__index`).
    ipairs_ref: Option<GcRef>,
    /// The `ipairs` iteration function.
    ipairs_iter_ref: Option<GcRef>,

    // ── Warning system (`warn`) ────────────────────────────────────
    warn_ref: Option<GcRef>,
    /// Warnings are printed when true (`@on`).
    warn_on: bool,
    /// While true (`@store`), messages are accumulated in the `_WARN`
    /// global instead of being printed.
    warn_store: bool,

    /// Re-entrancy guard for `__gc` finalizer dispatch.
    in_finalizer: bool,

    /// Values that must stay rooted across operations that may trigger GC
    /// (e.g. error objects held in Rust locals while closing TBC vars).
    extra_roots: Vec<Value>,

    /// Metamethod associated with the next pushed frame (consumed by
    /// `do_call`).
    pending_metamethod: Option<String>,

    /// Whether the next pushed frame is entered from a C-level call.
    pending_c_call: bool,

    /// True while running an error handler; the stack limit is relaxed so
    /// the handler has room even at maximum recursion depth.
    error_handling: bool,

    /// Name of a C function whose return hook is being fired (so
    /// debug.getinfo(2) inside the hook can name it).
    return_hook_c_name: Option<String>,

    /// Coroutine that is closing itself (via `coroutine.close()` inside it);
    /// the coroutine is unwound to its resume point.
    self_closing: Option<GcRef>,

    /// Coroutine currently being closed by an external `coroutine.close`.
    closing_thread: Option<GcRef>,

    /// Number of active C-level calls that cannot be suspended
    /// (gsub replacements, sort comparators, message handlers, ...).
    unyieldable_depth: usize,

    /// Name of the protected call whose frame is being unwound, if any.
    /// Closing methods report it through debug.getinfo (reference Lua keeps
    /// the C `pcall` frame as their caller).
    closing_pcall_name: Option<&'static str>,

    /// Source and line of a closing method that raised the current error,
    /// reported at the top of tracebacks.
    close_frame_info: Option<(String, u32)>,
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
            debug_getregistry_ref: None,
            debug_sethook_ref: None,
            debug_gethook_ref: None,
            tostring_ref: None,
            print_ref: None,
            gsub_ref: None,
            format_ref: None,
            registry: None,
            hook_func: None,
            hook_mask: 0,
            hook_count: 0,
            hook_counter: 0,
            in_hook: false,
            calling_hook: false,
            next_call_is_tail: false,
            next_call_extraargs: 0,
            protected_depth: 0,
            require_ref: None,
            load_ref: None,
            loadfile_ref: None,
            dofile_ref: None,
            preload_searcher_ref: None,
            file_searcher_ref: None,
            package_ref: None,
            globals_ref: None,
            collectgarbage_ref: None,
            sort_ref: None,
            move_ref: None,
            unpack_ref: None,
            insert_ref: None,
            concat_ref: None,
            remove_ref: None,
            next_ref: None,
            pairs_ref: None,
            ipairs_ref: None,
            ipairs_iter_ref: None,
            warn_ref: None,
            warn_on: false,
            warn_store: false,
            in_finalizer: false,
            extra_roots: Vec::new(),
            pending_metamethod: None,
            pending_c_call: false,
            error_handling: false,
            return_hook_c_name: None,
            self_closing: None,
            closing_thread: None,
            unyieldable_depth: 0,
            closing_pcall_name: None,
            close_frame_info: None,
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
            hook_func: None,
            hook_mask: 0,
            hook_count: 0,
            hook_counter: 0,
            pending_error: None,
        };
        let main_thread_ref = self.gc.new_thread(main_coro);
        self.main_thread = Some(main_thread_ref);
        if let Some(reg) = self.registry {
            if let Some(t) = reg.as_object_mut().as_table_mut() {
                t.raw_set(Value::Integer(1), Value::Object(main_thread_ref));
            }
        }

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
            is_hook: false,
            is_tailcall: false,
            extraargs: 0,
            hook_last_line: 0,
            hook_last_pc: 0,
            hook_seen_event: false,
            hook_single_line: None,
            metamethod: None,
            called_from_c: true,
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
            MM_INDEX, MM_NEWINDEX, MM_CALL, MM_TOSTRING, MM_METATABLE, MM_NAME,
            MM_PAIRS,
            MM_CLOSE, MM_GC, MM_MODE,
        ] {
            self.gc.new_string(name);
        }

        // Register built-in functions
        {
            let c = Closure::new_native("print", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.print_ref = Some(r);
            let k = self.gc.new_string(b"print");
            env.raw_set(Value::Object(k), Value::Object(r));
        }
        self.register_native(&mut env, "type", crate::stdlib::lua_type);
        {
            let c = Closure::new_native("tostring", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.tostring_ref = Some(r);
            let k = self.gc.new_string(b"tostring");
            env.raw_set(Value::Object(k), Value::Object(r));
        }
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

        // warn is special: it manages warning state and the `_WARN` store.
        {
            let warn_closure = Closure::new_native("warn", |_, _| Ok(vec![]));
            let warn_gc = self.gc.new_closure(warn_closure);
            self.warn_ref = Some(warn_gc);
            let key = self.gc.new_string(b"warn");
            env.raw_set(Value::Object(key), Value::Object(warn_gc));
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

        // `next` is a regular native; keep its ref so `pairs` can return it.
        {
            use crate::closure::{Closure, NativeFn};
            let next_closure =
                Closure::new_native("next", crate::stdlib::lua_next as NativeFn);
            let next_gc = self.gc.new_closure(next_closure);
            self.next_ref = Some(next_gc);
            let key = self.gc.new_string(b"next");
            env.raw_set(Value::Object(key), Value::Object(next_gc));
        }
        // `pairs` / `ipairs` are VM-special (they may call metamethods or
        // return a metamethod-aware iterator).
        {
            use crate::closure::Closure;
            let pairs_closure = Closure::new_native("pairs", |_, _| Ok(vec![]));
            let pairs_gc = self.gc.new_closure(pairs_closure);
            self.pairs_ref = Some(pairs_gc);
            let key = self.gc.new_string(b"pairs");
            env.raw_set(Value::Object(key), Value::Object(pairs_gc));

            let ipairs_closure = Closure::new_native("ipairs", |_, _| Ok(vec![]));
            let ipairs_gc = self.gc.new_closure(ipairs_closure);
            self.ipairs_ref = Some(ipairs_gc);
            let key = self.gc.new_string(b"ipairs");
            env.raw_set(Value::Object(key), Value::Object(ipairs_gc));

            let iter_closure = Closure::new_native("ipairs_iterator", |_, _| Ok(vec![]));
            let iter_gc = self.gc.new_closure(iter_closure);
            self.ipairs_iter_ref = Some(iter_gc);
        }
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
        // string.gsub can call Lua functions for replacements.
        {
            let gsub_closure = Closure::new_native("gsub", |_, _| Ok(vec![]));
            let gsub_gc = self.gc.new_closure(gsub_closure);
            self.gsub_ref = Some(gsub_gc);
            let key = self.gc.new_string(b"gsub");
            string_table.raw_set(Value::Object(key), Value::Object(gsub_gc));
        }
        // string.format is VM-special: `%s` honors __tostring.
        {
            let fmt_closure = Closure::new_native("format", |_, _| Ok(vec![]));
            let fmt_gc = self.gc.new_closure(fmt_closure);
            self.format_ref = Some(fmt_gc);
            let key = self.gc.new_string(b"format");
            string_table.raw_set(Value::Object(key), Value::Object(fmt_gc));
        }
        let string_ref = self.gc.new_table(string_table);
        let string_key = self.gc.new_string(b"string");
        env.raw_set(Value::Object(string_key), Value::Object(string_ref));

        // Set up string metatable: { __index = string }
        let mut string_mt = Table::new();
        let index_key = self.gc.new_string(MM_INDEX);
        string_mt.raw_set(Value::Object(index_key), Value::Object(string_ref));
        let string_mt_ref = self.gc.new_table(string_mt);
        self.gc.mt_string = Some(string_mt_ref);

        // table library
        let mut table_table = Table::new();
        for (name, func) in crate::stdlib::table::table_functions() {
            self.register_native(&mut table_table, name, func);
        }
        // table.sort is VM-special: it needs to call Lua comparators and
        // respect __lt/__len during the sort.
        {
            let sort_closure = Closure::new_native("sort", |_, _| Ok(vec![]));
            let sort_gc = self.gc.new_closure(sort_closure);
            self.sort_ref = Some(sort_gc);
            let key = self.gc.new_string(b"sort");
            table_table.raw_set(Value::Object(key), Value::Object(sort_gc));
        }
        // table.move is VM-special: it honors __index/__newindex.
        {
            let move_closure = Closure::new_native("move", |_, _| Ok(vec![]));
            let move_gc = self.gc.new_closure(move_closure);
            self.move_ref = Some(move_gc);
            let key = self.gc.new_string(b"move");
            table_table.raw_set(Value::Object(key), Value::Object(move_gc));
        }
        // table.unpack is VM-special: it honors __index/__len.
        {
            let unpack_closure = Closure::new_native("unpack", |_, _| Ok(vec![]));
            let unpack_gc = self.gc.new_closure(unpack_closure);
            self.unpack_ref = Some(unpack_gc);
            let key = self.gc.new_string(b"unpack");
            table_table.raw_set(Value::Object(key), Value::Object(unpack_gc));
        }
        // table.insert is VM-special: it honors __index/__newindex/__len.
        {
            let insert_closure = Closure::new_native("insert", |_, _| Ok(vec![]));
            let insert_gc = self.gc.new_closure(insert_closure);
            self.insert_ref = Some(insert_gc);
            let key = self.gc.new_string(b"insert");
            table_table.raw_set(Value::Object(key), Value::Object(insert_gc));
        }
        // table.concat is VM-special: it honors __index/__len.
        {
            let concat_closure = Closure::new_native("concat", |_, _| Ok(vec![]));
            let concat_gc = self.gc.new_closure(concat_closure);
            self.concat_ref = Some(concat_gc);
            let key = self.gc.new_string(b"concat");
            table_table.raw_set(Value::Object(key), Value::Object(concat_gc));
        }
        // table.remove is VM-special: it honors __index/__newindex/__len.
        {
            let remove_closure = Closure::new_native("remove", |_, _| Ok(vec![]));
            let remove_gc = self.gc.new_closure(remove_closure);
            self.remove_ref = Some(remove_gc);
            let key = self.gc.new_string(b"remove");
            table_table.raw_set(Value::Object(key), Value::Object(remove_gc));
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

        // Build file metatable: { __index = method_table, __close = close_fn,
        //                         __name = "FILE*" }
        let mut file_mt = Table::new();
        let index_key2 = self.gc.new_string(MM_INDEX);
        file_mt.raw_set(Value::Object(index_key2), Value::Object(file_method_ref));
        let close_key = self.gc.new_string(MM_CLOSE);
        let close_closure = Closure::new_native("file.__close", crate::stdlib::io::file_gc_close);
        let close_ref = self.gc.new_closure(close_closure);
        file_mt.raw_set(Value::Object(close_key), Value::Object(close_ref));
        let gc_key = self.gc.new_string(MM_GC);
        let gc_closure = Closure::new_native("file.__gc", crate::stdlib::io::file_gc_close);
        let gc_ref = self.gc.new_closure(gc_closure);
        file_mt.raw_set(Value::Object(gc_key), Value::Object(gc_ref));
        let tostring_key = self.gc.new_string(MM_TOSTRING);
        let tostring_closure =
            Closure::new_native("file.__tostring", crate::stdlib::io::file_tostring);
        let tostring_ref2 = self.gc.new_closure(tostring_closure);
        file_mt.raw_set(Value::Object(tostring_key), Value::Object(tostring_ref2));
        let name_key = self.gc.new_string(b"__name");
        let file_name = self.gc.new_string(b"FILE*");
        file_mt.raw_set(Value::Object(name_key), Value::Object(file_name));
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
        {
            let c = Closure::new_native("getregistry", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.debug_getregistry_ref = Some(r);
            let k = self.gc.new_string(b"getregistry");
            debug_table.raw_set(Value::Object(k), Value::Object(r));
        }
        {
            let c = Closure::new_native("sethook", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.debug_sethook_ref = Some(r);
            let k = self.gc.new_string(b"sethook");
            debug_table.raw_set(Value::Object(k), Value::Object(r));
        }
        {
            let c = Closure::new_native("gethook", |_, _| Ok(vec![]));
            let r = self.gc.new_closure(c);
            self.debug_gethook_ref = Some(r);
            let k = self.gc.new_string(b"gethook");
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

        // Standard libraries are pre-registered as loaded (`require "debug"`
        // etc. must return the library table without searching files).
        for name in [
            b"_G".as_slice(),
            b"coroutine",
            b"debug",
            b"io",
            b"math",
            b"os",
            b"package",
            b"string",
            b"table",
            b"utf8",
        ] {
            let k = self.gc.new_string(name);
            let v = env.raw_get(&Value::Object(k));
            if !v.is_nil() {
                loaded_ref
                    .as_object_mut()
                    .as_table_mut()
                    .unwrap()
                    .raw_set(Value::Object(k), v);
            }
        }

        let env_ref = self.gc.new_table(env);
        self.globals_ref = Some(env_ref);

        // Registry table (`debug.getregistry`): [1] = main thread (set in
        // `execute_main`), [2] = global environment. It also holds the
        // weak-keyed `_HOOKKEY` table (per-thread hooks).
        {
            let mut reg = Table::new();
            reg.raw_set(Value::Integer(2), Value::Object(env_ref));

            let mut hookkey = Table::new();
            let mut hook_mt = Table::new();
            let mode_key = self.gc.new_string(b"__mode");
            let mode_val = self.gc.new_string(b"k");
            hook_mt.raw_set(Value::Object(mode_key), Value::Object(mode_val));
            let hook_mt_ref = self.gc.new_table(hook_mt);
            hookkey.metatable = Some(hook_mt_ref);
            hookkey.set_weak_mode(Some(b"k"));
            let hookkey_ref = self.gc.new_table(hookkey);
            let hk_key = self.gc.new_string(b"_HOOKKEY");
            reg.raw_set(Value::Object(hk_key), Value::Object(hookkey_ref));

            let reg_ref = self.gc.new_table(reg);
            self.registry = Some(reg_ref);
        }

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
            Upvalue::Open(loc) => self.read_open_upvalue(loc),
            Upvalue::Closed(val) => val,
        }
    }

    /// Read the value of an open upvalue, from whichever thread owns it.
    fn read_open_upvalue(&self, loc: UpvalueLoc) -> Value {
        if loc.thread == self.current_thread {
            self.stack.get(loc.idx).copied().unwrap_or(Value::Nil)
        } else {
            match loc.thread.or(self.main_thread) {
                Some(t) => t
                    .as_object()
                    .as_coroutine()
                    .and_then(|co| co.stack.get(loc.idx).copied())
                    .unwrap_or(Value::Nil),
                None => Value::Nil,
            }
        }
    }

    /// Write the value of an open upvalue, into whichever thread owns it.
    fn write_open_upvalue(&mut self, loc: UpvalueLoc, val: Value) {
        if loc.thread == self.current_thread {
            if loc.idx < self.stack.len() {
                self.stack[loc.idx] = val;
            }
        } else if let Some(t) = loc.thread.or(self.main_thread) {
            let mut t = t;
            if let Some(co) = t.as_object_mut().as_coroutine_mut() {
                if loc.idx < co.stack.len() {
                    co.stack[loc.idx] = val;
                }
            }
        }
    }

    /// Find an existing open upvalue for the given stack index, or create one.
    fn find_or_create_upvalue(&mut self, stack_idx: usize) -> UpvalueRef {
        let loc = UpvalueLoc {
            thread: self.current_thread,
            idx: stack_idx,
        };
        for uv in &self.open_upvalues {
            if let Upvalue::Open(existing) = *uv.borrow() {
                if existing.thread == loc.thread && existing.idx == stack_idx {
                    return Rc::clone(uv);
                }
            }
        }
        let uv = Rc::new(RefCell::new(Upvalue::Open(loc)));
        self.open_upvalues.push(Rc::clone(&uv));
        uv
    }

    /// Close all open upvalues of the active thread with index >= `from`.
    fn close_upvalues(&mut self, from: usize) {
        let current = self.current_thread;
        for uv in &self.open_upvalues {
            let should_close = match *uv.borrow() {
                Upvalue::Open(loc) => loc.thread == current && loc.idx >= from,
                Upvalue::Closed(_) => false,
            };
            if should_close {
                let val = match *uv.borrow() {
                    Upvalue::Open(loc) => self.stack[loc.idx],
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
        // Mirror `luaD_closeprotected`: close from the innermost variable
        // outwards; when a closing method raises an error, the remaining
        // variables are closed with that new error object.
        // Keep the values (and the in-flight error) rooted across calls.
        let rooted_len = self.extra_roots.len();
        let slots: Vec<usize> = self
            .tbc_slots
            .iter()
            .copied()
            .filter(|&s| s >= from)
            .collect();
        for &s in &slots {
            self.extra_roots.push(self.stack[s]);
        }
        if let Some(e) = err_obj {
            self.extra_roots.push(e);
        }
        let mut status_err = err_obj;
        loop {
            let slot = match self.tbc_slots.last() {
                Some(&s) if s >= from => {
                    self.tbc_slots.pop();
                    s
                }
                _ => break,
            };
            let val = self.stack[slot];
            // nil and false are silently ignored
            if val == Value::Nil || val == Value::Boolean(false) {
                continue;
            }
            if let Some(mm) = self.get_metamethod(val, MM_CLOSE) {
                // The error object is passed only when unwinding an error.
                let args: Vec<Value> = match status_err {
                    Some(e) => vec![val, e],
                    None => vec![val],
                };
                let saved_depth = self.frames.len();
                let saved_uv = self.open_upvalues.len();
                self.pending_metamethod = Some("close".to_string());
                let result = self.call_value(mm, &args);
                if let Err(e) = result {
                    let e = self.position_error(e);
                    let ev = e.to_value(&mut self.gc);
                    // Remember where the closing method was, for tracebacks.
                    if let Some(fr) = self.frames.last() {
                        let pc = fr.pc.saturating_sub(1);
                        let line =
                            fr.proto.line_info.get(pc).copied().unwrap_or(0);
                        let src = fr.proto.source.clone().unwrap_or_default();
                        self.close_frame_info = Some((src, line));
                    }
                    // Pop any frames left by the failed closing method
                    // (its own TBC variables stay on the list and are
                    // closed by this loop with the new error object).
                    while self.frames.len() > saved_depth {
                        let fb = self.frames.last().unwrap().base;
                        self.close_upvalues(fb);
                        self.frames.pop();
                    }
                    self.open_upvalues.truncate(saved_uv);
                    self.extra_roots.push(ev);
                    // Continue closing the remaining variables with the
                    // new error object (mirrors luaD_closeprotected).
                    status_err = Some(ev);
                }
            } else {
                // The metamethod may have been removed after the variable
                // was marked; calling a missing value is an error.
                let msg = self
                    .gc
                    .new_string(b"attempt to call a nil value (metamethod 'close')");
                let ev = Value::Object(msg);
                self.extra_roots.push(ev);
                status_err = Some(ev);
            }
        }

        self.extra_roots.truncate(rooted_len);
        match status_err {
            Some(ev) => {
                let mut e = LuaError::with_value(ev);
                e.positioned = true;
                Err(e)
            }
            None => Ok(()),
        }
    }

    // ── Metamethod infrastructure ─────────────────────────────────

    /// Get the metatable of a value (if any).
    fn get_metatable(&self, val: Value) -> Option<GcRef> {
        match val {
            Value::Nil => self.gc.mt_nil,
            Value::Boolean(_) => self.gc.mt_bool,
            Value::Integer(_) | Value::Float(_) => self.gc.mt_number,
            Value::Object(r) => match &r.as_object().kind {
                GcObjectKind::Table(t) => t.metatable,
                GcObjectKind::Closure(_) => self.gc.mt_function,
                GcObjectKind::String(_) => self.gc.mt_string,
                GcObjectKind::Thread(_) => self.gc.mt_thread,
                GcObjectKind::Userdata(ud) => ud.metatable,
            },
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
    /// Going through `do_call` guarantees VM-special functions (pcall,
    /// table.sort, require, ...) behave the same when passed as values.
    fn call_value(&mut self, func: Value, args: &[Value]) -> Result<Vec<Value>, LuaError> {
        // Find a place on the stack for this call
        let call_base = self.find_call_base();
        self.ensure_stack(call_base + args.len() + 2);
        self.stack[call_base] = func;
        for (i, &arg) in args.iter().enumerate() {
            self.stack[call_base + 1 + i] = arg;
        }

        let saved_depth = self.frames.len();
        let result_base = call_base;

        self.pending_c_call = true;
        self.do_call(func, call_base, args, result_base, -1)?;

        if self.frames.len() > saved_depth {
            // Lua function: run the pushed frame to completion.
            self.execute_to_depth(saved_depth)?;
        }

        // Collect results from result_base..self.top. After a yield, `top`
        // may not reflect this call's results yet; clamp defensively.
        let top = self.top.max(result_base);
        let results: Vec<Value> = self.stack[result_base..top.min(self.stack.len())].to_vec();
        Ok(results)
    }

    /// Find a safe call_base for internal metamethod calls (above all active frames).
    fn find_call_base(&self) -> usize {
        let mut stack_top = if self.frames.is_empty() {
            self.top
        } else {
            let last = &self.frames[self.frames.len() - 1];
            let frame_top = last.base + last.proto.max_stack_size as usize;
            self.top.max(frame_top)
        };
        // Pending to-be-closed variables may live above the current frames
        // (e.g. while unwinding); keep calls clear of their slots.
        if let Some(&max_slot) = self.tbc_slots.iter().max() {
            stack_top = stack_top.max(max_slot + 8);
        }
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
                // Full Lua numeral grammar (signs, hex integers/floats).
                crate::stdlib::io::parse_lua_number(s.as_bytes())
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
                if !(f >= -(2f64.powi(63)) && f < 2f64.powi(63)) || f.fract() != 0.0 {
                    return None;
                }
                Some(f as i64)
            }
            Value::Object(r) => {
                let s = r.as_object().as_string()?;
                match crate::stdlib::io::parse_lua_number(s.as_bytes())? {
                    Value::Integer(i) => Some(i),
                    Value::Float(f)
                        if f >= -(2f64.powi(63))
                            && f < 2f64.powi(63)
                            && f.fract() == 0.0 =>
                    {
                        Some(f as i64)
                    }
                    _ => None,
                }
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
                    return Err(LuaError::new("attempt to divide by zero"));
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
        regs: Option<(usize, usize)>,
    ) -> Result<Value, LuaError> {
        if let (Some(x), Some(y)) = (Self::coerce_to_integer(a), Self::coerce_to_integer(b)) {
            return Ok(Value::Integer(raw_fn(x, y)));
        }
        if let Some(mm) = self.get_binop_metamethod(a, b, mm_name) {
            return self.call_metamethod(mm, &[a, b]);
        }
        let (bad, reg) = if Self::coerce_to_integer(a).is_none() {
            (a, regs.map(|r| r.0))
        } else {
            (b, regs.map(|r| r.1))
        };
        Err(self.bitwise_type_error(bad, reg))
    }

    /// Build the reference-Lua bitwise error for a bad operand, including
    /// a variable hint when the operand's register can be identified.
    fn bitwise_type_error(&self, bad: Value, reg: Option<usize>) -> LuaError {
        let hint = reg.map(|r| self.reg_varinfo(r)).unwrap_or_default();
        if bad.is_number() {
            LuaError::new(format!(
                "number{hint} has no integer representation"
            ))
        } else {
            LuaError::new(format!(
                "attempt to perform bitwise operation on a {} value{hint}",
                bad.type_name()
            ))
        }
    }

    /// Variable-description hint for register `reg` of the current frame,
    /// e.g. `" (field 'huge')"`.
    fn reg_varinfo(&self, reg: usize) -> String {
        let fi = match self.frames.len().checked_sub(1) {
            Some(fi) => fi,
            None => return String::new(),
        };
        let frame = &self.frames[fi];
        if reg < frame.base {
            return String::new();
        }
        let rel = (reg - frame.base) as u8;
        let pc = frame.pc.saturating_sub(1);
        let (name, what) = Self::reg_source_name(&frame.proto, pc, rel);
        match name {
            Some(n) => format!(" ({what} '{n}')"),
            None => String::new(),
        }
    }

    /// Inspect the instruction(s) before `pc` to name the value in `reg`.
    fn reg_source_name(proto: &Proto, pc: usize, reg: u8) -> (Option<String>, &'static str) {
        let mut source_inst: Option<(OpCode, u32)> = None;
        let limit = pc.saturating_sub(32);
        let mut idx = pc;
        while idx > limit {
            idx -= 1;
            let inst = proto.code[idx];
            let op = match OpCode::from_u8(decode_op(inst)) {
                Some(op) => op,
                None => break,
            };
            if decode_a(inst) == reg && inst_writes_reg(op) {
                match op {
                    OpCode::GetTabUp
                    | OpCode::GetTable
                    | OpCode::GetUpval
                    | OpCode::LoadK
                    | OpCode::Move => {
                        source_inst = Some((op, inst));
                    }
                    _ => {}
                }
                break;
            }
        }
        let (op, inst) = match source_inst {
            Some(pair) => pair,
            None => return (None, ""),
        };
        match op {
            OpCode::GetTabUp => {
                let up = decode_b(inst);
                let k = decode_c(inst);
                if let Some(key) = constant_string(&proto.constants, k as usize) {
                    let what = if proto
                        .upvalues
                        .get(up as usize)
                        .and_then(|u| u.name.as_deref())
                        == Some("_ENV")
                    {
                        "global"
                    } else {
                        "field"
                    };
                    return (Some(key), what);
                }
            }
            OpCode::GetTable => {
                let key_reg = decode_c(inst);
                let start = pc.saturating_sub(12);
                for idx in (start..pc.saturating_sub(1)).rev() {
                    let inst = proto.code[idx];
                    if decode_op(inst) == OpCode::LoadK as u8 && decode_a(inst) == key_reg {
                        if let Some(key) =
                            constant_string(&proto.constants, decode_bx(inst) as usize)
                        {
                            return (Some(key), "field");
                        }
                        break;
                    }
                }
            }
            OpCode::Move => {
                let src = decode_b(inst);
                if let Some(local) = local_at_reg(proto, src, pc as u32) {
                    return (Some(local), "local");
                }
            }
            OpCode::GetUpval => {
                let uv = decode_b(inst);
                if let Some(u) = proto.upvalues.get(uv as usize) {
                    if let Some(name) = &u.name {
                        return (Some(name.clone()), "upvalue");
                    }
                }
            }
            OpCode::Move => {
                let rb = decode_b(inst);
                if let Some(local) = local_at_reg(proto, rb, pc as u32) {
                    return (Some(local), "local");
                }
            }
            OpCode::LoadK => {
                // A constant has no variable name.
            }
            _ => {}
        }
        (None, "")
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
            (Value::Integer(x), Value::Float(y)) => Some(lt_int_float(x, y)),
            (Value::Float(x), Value::Integer(y)) => Some(lt_float_int(x, y)),
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
            (Value::Integer(x), Value::Float(y)) => Some(le_int_float(x, y)),
            (Value::Float(x), Value::Integer(y)) => Some(le_float_int(x, y)),
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
                        result.extend_from_slice(
                            crate::value::lua_float_to_string(n).as_bytes(),
                        );
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
        if matches!(key, Value::Float(f) if f.is_nan()) {
            return Err(LuaError::new("table index is NaN"));
        }
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

        // Root: temporary values held in Rust locals.
        for val in &self.extra_roots {
            if let Value::Object(r) = val {
                roots.push(*r);
            }
        }

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
        for r in [
            self.gc.mt_nil,
            self.gc.mt_bool,
            self.gc.mt_number,
            self.gc.mt_string,
            self.gc.mt_function,
            self.gc.mt_thread,
        ] {
            if let Some(r) = r {
                roots.push(r);
            }
        }
        if let Some(r) = self.gc.file_metatable {
            roots.push(r);
        }
        if let Some(r) = self.gc.io_input {
            roots.push(r);
        }
        if let Some(r) = self.gc.io_output {
            roots.push(r);
        }
        if let Some(r) = self.gc.ipairs_iter {
            roots.push(r);
        }
        if let Some(r) = self.gc.pairs_next {
            roots.push(r);
        }
        if let Some(r) = self.hook_func {
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
            self.sort_ref,
            self.move_ref,
            self.unpack_ref,
            self.insert_ref,
            self.concat_ref,
            self.remove_ref,
            self.next_ref,
            self.pairs_ref,
            self.ipairs_ref,
            self.ipairs_iter_ref,
            self.warn_ref,
            self.registry,
            self.tostring_ref,
            self.print_ref,
            self.gsub_ref,
            self.format_ref,
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
                    let e = self.position_error(e);
                    let msg = format!("error in __gc finalizer: {e}");
                    self.warning(&msg);
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
        match self.execute_to_depth(0) {
            Ok(()) => Ok(()),
            Err(e) => Err(self.position_error(e)),
        }
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

            // ── Debug hook triggers (count / line) ─────────────────
            if self.hook_mask != 0 && !self.in_hook {
                if self.hook_mask & HOOK_COUNT != 0 {
                    self.hook_counter -= 1;
                    if self.hook_counter <= 0 {
                        self.hook_counter = self.hook_count;
                        self.call_hook("count", None)?;
                    }
                }
                if self.hook_mask & HOOK_LINE != 0 {
                    let line = self.frames[fi]
                        .proto
                        .line_info
                        .get(pc)
                        .copied()
                        .unwrap_or(0);
                    let oldpc = self.frames[fi].hook_last_pc;
                    let oldline = self.frames[fi].hook_last_line;
                    let first = oldpc == 0 && pc == 0;
                    self.frames[fi].hook_last_pc = pc;
                    self.frames[fi].hook_last_line = line;
                    // Line 0 means "no line information": no event.
                    if line != 0 && (first || pc <= oldpc || line != oldline) {
                        // Emulate reference Lua's changedline behaviour at
                        // function entry: a proto with a single distinct line
                        // produces no entry event.
                        let single = match self.frames[fi].hook_single_line {
                            Some(v) => v,
                            None => {
                                let mut seen: Option<u32> = None;
                                let mut single = true;
                                for &l in &self.frames[fi].proto.line_info {
                                    if l == 0 {
                                        continue;
                                    }
                                    match seen {
                                        None => seen = Some(l),
                                        Some(s0) if s0 != l => {
                                            single = false;
                                            break;
                                        }
                                        _ => {}
                                    }
                                }
                                // A one-instruction proto still fires on entry.
                                if self.frames[fi].proto.code.len() <= 1 {
                                    single = false;
                                }
                                self.frames[fi].hook_single_line = Some(single);
                                single
                            }
                        };
                        let entry = !self.frames[fi].hook_seen_event;
                        let is_back = pc <= oldpc;
                        if !(entry && single && !is_back) {
                            self.frames[fi].hook_seen_event = true;
                            self.call_hook("line", Some(line))?;
                        }
                    }
                }
            }
            // Root the temporaries the compiler says are live at this
            // instruction (locals plus in-flight expression operands).
            let need = self.frames[fi]
                .proto
                .stack_top_at
                .get(pc)
                .copied()
                .unwrap_or(self.frames[fi].proto.max_stack_size);
            self.frames[fi].runtime_top = base + need as usize;

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
                        Upvalue::Open(loc) => {
                            let loc = *loc;
                            self.write_open_upvalue(loc, val);
                        }
                        Upvalue::Closed(v) => *v = val,
                    }
                }

                OpCode::GetTabUp => {
                    let k = if c == 255 {
                        let next = self.frames[fi].proto.code[self.frames[fi].pc];
                        self.frames[fi].pc += 1;
                        decode_ax(next) as usize
                    } else {
                        c as usize
                    };
                    let upvalues = &self.frames[fi].upvalues;
                    let table_val = self.get_upvalue_val(upvalues, b);
                    let key = self.frames[fi].proto.constants[k].to_value(&mut self.gc);
                    let result = self.table_get(table_val, key)?;
                    self.set_reg(base, a, result);
                }

                OpCode::SetTabUp => {
                    let k = if b == 255 {
                        let next = self.frames[fi].proto.code[self.frames[fi].pc];
                        self.frames[fi].pc += 1;
                        decode_ax(next) as usize
                    } else {
                        b as usize
                    };
                    let upvalues = &self.frames[fi].upvalues;
                    let table_val = self.get_upvalue_val(upvalues, a);
                    let key = self.frames[fi].proto.constants[k].to_value(&mut self.gc);
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
                    let result =
                        self.bitwise_binop(rb, rc, |x, y| x & y, MM_BAND, Some((base + b, base + c)))?;
                    self.set_reg(base, a, result);
                }

                OpCode::BOr => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result =
                        self.bitwise_binop(rb, rc, |x, y| x | y, MM_BOR, Some((base + b, base + c)))?;
                    self.set_reg(base, a, result);
                }

                OpCode::BXor => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result =
                        self.bitwise_binop(rb, rc, |x, y| x ^ y, MM_BXOR, Some((base + b, base + c)))?;
                    self.set_reg(base, a, result);
                }

                OpCode::Shl => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.bitwise_binop(rb, rc, lua_shl, MM_SHL, Some((base + b, base + c)))?;
                    self.set_reg(base, a, result);
                }

                OpCode::Shr => {
                    let rb = self.reg(base, b);
                    let rc = self.reg(base, c);
                    let result = self.bitwise_binop(rb, rc, lua_shr, MM_SHR, Some((base + b, base + c)))?;
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
                    let result = self.value_length(rb)?;
                    self.set_reg(base, a, result);
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
                    // Strings are coerced to numbers, as in reference Lua.
                    let init = Self::coerce_to_number(init)
                        .ok_or_else(|| LuaError::new("'for' initial value must be a number"))?;
                    let limit = Self::coerce_to_number(limit)
                        .ok_or_else(|| LuaError::new("'for' limit must be a number"))?;
                    let step = Self::coerce_to_number(step)
                        .ok_or_else(|| LuaError::new("'for' step must be a number"))?;
                    match step {
                        Value::Integer(0) => {
                            return Err(LuaError::new("'for' step is zero"));
                        }
                        Value::Float(f) if f == 0.0 => {
                            return Err(LuaError::new("'for' step is zero"));
                        }
                        _ => {}
                    }
                    match (init, step) {
                        (Value::Integer(i0), Value::Integer(st)) => {
                            // Integer loop: compute the iteration count once.
                            let limit_int = self.for_limit(i0, limit, st)?;
                            match limit_int {
                                None => {
                                    // Skip the loop entirely.
                                    self.frames[fi].pc =
                                        (self.frames[fi].pc as i64 + sbx as i64) as usize;
                                }
                                Some(lim) => {
                                    let count = if st > 0 {
                                        (lim as u64).wrapping_sub(i0 as u64) / (st as u64)
                                    } else {
                                        let denom = ((st.wrapping_add(1))
                                            .wrapping_neg() as u64)
                                            .wrapping_add(1);
                                        (i0 as u64).wrapping_sub(lim as u64) / denom
                                    };
                                    self.set_reg(base, a, Value::Integer(count as i64));
                                    self.set_reg(base, a + 1, Value::Integer(st));
                                    self.set_reg(base, a + 2, Value::Integer(i0));
                                }
                            }
                        }
                        _ => {
                            // Float loop: make everything a float.
                            let i0 = Self::as_float(init);
                            let lim = Self::as_float(limit);
                            let st = Self::as_float(step);
                            let skip = if st > 0.0 { lim < i0 } else { i0 < lim };
                            let target = (self.frames[fi].pc as i64 + sbx as i64) as usize;
                            if skip {
                                self.frames[fi].pc = target;
                            } else {
                                self.set_reg(base, a, Value::Float(lim));
                                self.set_reg(base, a + 1, Value::Float(st));
                                self.set_reg(base, a + 2, Value::Float(i0));
                            }
                        }
                    }
                }

                OpCode::ForLoop => {
                    match self.reg(base, a + 1) {
                        Value::Integer(step) => {
                            let count = match self.reg(base, a) {
                                Value::Integer(c) => c as u64,
                                _ => 0,
                            };
                            if count > 0 {
                                let idx = match self.reg(base, a + 2) {
                                    Value::Integer(i) => i,
                                    _ => 0,
                                };
                                self.set_reg(
                                    base,
                                    a,
                                    Value::Integer((count - 1) as i64),
                                );
                                self.set_reg(
                                    base,
                                    a + 2,
                                    Value::Integer(idx.wrapping_add(step)),
                                );
                                self.frames[fi].pc =
                                    (self.frames[fi].pc as i64 + sbx as i64) as usize;
                            }
                        }
                        Value::Float(step) => {
                            let limit = Self::as_float(self.reg(base, a));
                            let idx = Self::as_float(self.reg(base, a + 2)) + step;
                            let go = if step > 0.0 {
                                idx <= limit
                            } else {
                                limit <= idx
                            };
                            if go {
                                self.set_reg(base, a + 2, Value::Float(idx));
                                self.frames[fi].pc =
                                    (self.frames[fi].pc as i64 + sbx as i64) as usize;
                            }
                        }
                        _ => {
                            return Err(LuaError::new("'for' step must be a number"));
                        }
                    }
                }

                OpCode::TForPrep => {
                    // Swap the control value (A+3) with the closing value
                    // (A+2), then mark the closing value as to-be-closed.
                    let close_idx = base + a + 2;
                    let ctrl_idx = base + a + 3;
                    self.ensure_stack(ctrl_idx + 1);
                    self.stack.swap(close_idx, ctrl_idx);
                    let val = self.stack[close_idx];
                    if val != Value::Nil && val != Value::Boolean(false) {
                        if self.get_metamethod(val, MM_CLOSE).is_none() {
                            return Err(LuaError::new(
                                "variable '?' got a non-closable value",
                            ));
                        }
                    }
                    self.tbc_slots.push(close_idx);
                    self.frames[fi].pc =
                        (self.frames[fi].pc as i64 + sbx as i64) as usize;
                }

                OpCode::TForCall => {
                    // A = base, C = #loop variables.  Set up a CALL-style
                    // frame at A+3: iterator, state, control; results are
                    // placed back at A+3 when the callee returns.
                    let iter = self.reg(base, a);
                    let state = self.reg(base, a + 1);
                    let control = self.reg(base, a + 3);
                    self.ensure_stack(base + a + 6);
                    self.set_reg(base, a + 3, iter);
                    self.set_reg(base, a + 4, state);
                    self.set_reg(base, a + 5, control);
                    self.call_function(base + a + 3, 3, c as i32)?;
                }

                OpCode::TForLoop => {
                    // A = base, B = backward jump offset, C = #loop variables.
                    let first_result = self.reg(base, a + 3);
                    if !first_result.is_nil() {
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
                        else if self.warn_ref == Some(r) { 23 }
                        else if self.sort_ref == Some(r) { 24 }
                        else if self.move_ref == Some(r) { 31 }
                        else if self.unpack_ref == Some(r) { 32 }
                        else if self.insert_ref == Some(r) { 33 }
                        else if self.concat_ref == Some(r) { 35 }
                        else if self.remove_ref == Some(r) { 36 }
                        else if self.pairs_ref == Some(r) { 37 }
                        else if self.ipairs_ref == Some(r) { 38 }
                        else if self.ipairs_iter_ref == Some(r) { 39 }
                        else if self.debug_getregistry_ref == Some(r) { 25 }
                        else if self.tostring_ref == Some(r) { 28 }
                        else if self.print_ref == Some(r) { 29 }
                        else if self.gsub_ref == Some(r) { 30 }
                        else if self.format_ref == Some(r) { 34 }
                        else if self.debug_sethook_ref == Some(r) { 26 }
                        else if self.debug_gethook_ref == Some(r) { 27 }
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
                        23 => { // warn
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_warn(&args)?;
                            self.place_results(base + a, num_results, &[]);
                        }
                        24 => { // table.sort
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_sort(&args, base + a, num_results)?;
                        }
                        31 => { // table.move
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_table_move(&args, base + a, num_results)?;
                        }
                        32 => { // table.unpack
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_table_unpack(&args, base + a, num_results)?;
                        }
                        33 => { // table.insert
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_table_insert(&args)?;
                            self.place_results(base + a, num_results, &[]);
                        }
                        35 => { // table.concat
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_table_concat(&args, base + a, num_results)?;
                        }
                        36 => { // table.remove
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_table_remove(&args, base + a, num_results)?;
                        }
                        37 | 38 | 39 => { // pairs / ipairs / ipairs iterator
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            match special {
                                37 => self.handle_pairs(&args, base + a, num_results)?,
                                38 => self.handle_ipairs(&args, base + a, num_results)?,
                                _ => self.handle_ipairs_iter(
                                    &args, base + a, num_results,
                                )?,
                            }
                        }
                        25 => { // debug.getregistry
                            self.handle_debug_getregistry(base + a, num_results);
                        }
                        28 => { // tostring
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_tostring(&args, base + a, num_results)?;
                        }
                        29 => { // print
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_print(&args)?;
                            self.place_results(base + a, num_results, &[]);
                        }
                        30 => { // string.gsub
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_gsub(&args, base + a, num_results)?;
                        }
                        34 => { // string.format
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_string_format(&args, base + a, num_results)?;
                        }
                        26 => { // debug.sethook
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_debug_sethook(&args)?;
                            self.place_results(base + a, num_results, &[]);
                            self.fire_return_hook(Some("sethook"))?;
                        }
                        27 => { // debug.gethook
                            let args: Vec<Value> = (0..num_args)
                                .map(|i| self.stack[base + a + 1 + i])
                                .collect();
                            self.handle_debug_gethook(&args, base + a, num_results)?;
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
                            // A tail call to yield returns the resumed values
                            // directly to this function's caller, so the frame
                            // is finished once the values are delivered.
                            let rb = self.frames[fi].result_base;
                            let nr = self.frames[fi].num_results;
                            self.handle_yield(&args, rb, nr)?;
                            self.frames.pop();
                            continue;
                        }
                    }

                    let num_args = if b > 0 { b - 1 } else { self.top - (base + a) - 1 };
                    let args: Vec<Value> = (0..num_args)
                        .map(|i| self.stack[base + a + 1 + i])
                        .collect();

                    self.frames[fi].runtime_top = base + a + num_args + 1;

                    // Resolve a __call chain so a callable object can still
                    // be tail-call-optimized when it resolves to a Lua function.
                    let mut actual_func = func_val;
                    let mut actual_args = args.clone();
                    let mut call_limit = 16;
                    let is_lua_closure = loop {
                        match actual_func {
                            Value::Object(r) if r.as_object().as_closure().is_some() => {
                                break matches!(
                                    r.as_object().as_closure().unwrap(),
                                    Closure::Lua(_)
                                );
                            }
                            _ => {
                                call_limit -= 1;
                                if call_limit == 0 {
                                    return Err(LuaError::new("'__call' chain too long"));
                                }
                                match self.get_metamethod(actual_func, MM_CALL) {
                                    Some(mm) => {
                                        let mut new_args =
                                            Vec::with_capacity(actual_args.len() + 1);
                                        new_args.push(actual_func);
                                        new_args.extend(actual_args);
                                        actual_func = mm;
                                        actual_args = new_args;
                                    }
                                    None => {
                                        return Err(LuaError::new(format!(
                                            "attempt to call a {} value",
                                            func_val.type_name()
                                        )));
                                    }
                                }
                            }
                        }
                    };

                    self.close_tbc_vars(base, None)?;
                    self.close_upvalues(base);

                    let result_base = self.frames[fi].result_base;
                    let num_results = self.frames[fi].num_results;

                    if is_lua_closure {
                        // Tail call optimization: pop current frame, reuse slot
                        self.frames.pop();

                        // Place arguments at the base for the new frame
                        self.ensure_stack(base + actual_args.len() + 2);
                        for (i, &arg) in actual_args.iter().enumerate() {
                            self.stack[base + 1 + i] = arg;
                        }
                        self.stack[base] = actual_func;

                        self.next_call_is_tail = true;
                        self.next_call_extraargs = actual_args.len().saturating_sub(num_args) as u8;
                        self.do_call(actual_func, base, &actual_args, result_base, num_results)?;
                    } else {
                        // C/native function: keep current frame on stack during the call
                        // so debug functions can see the correct call stack, then pop after
                        self.do_call(actual_func, base + a, &actual_args, result_base, num_results)?;
                        if self.hook_mask & HOOK_RET != 0 && !self.in_hook {
                            self.call_hook("return", None)?;
                        }
                        self.frames.pop();
                        if self.frames.is_empty() {
                            // Native tail call ended the top-level function:
                            // expose its results to resume/wrap.
                            self.last_return_values =
                                self.stack[result_base..self.top.max(result_base)].to_vec();
                        }
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

                    if self.hook_mask & HOOK_RET != 0 && !self.in_hook {
                        self.call_hook("return", None)?;
                    }

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
                    if let Some(va_reg) =
                        self.frames[fi].proto.vararg_name_reg
                    {
                        // Named varargs: `...` reads through the table.
                        let table_val = self.stack[base + va_reg as usize];
                        let table_ref = match table_val {
                            Value::Object(r) => Some(r),
                            _ => None,
                        };
                        let table = table_ref
                            .as_ref()
                            .and_then(|r| r.as_object().as_table());
                        let n: u64 = match table {
                            Some(t) => {
                                let key = match self.gc.find_string(b"n") {
                                    Some(k) => k,
                                    None => self.gc.new_string(b"n"),
                                };
                                match t.raw_get(&Value::Object(key)) {
                                    Value::Integer(i)
                                        if i >= 0
                                            && (i as u64) <= (i32::MAX as u64) / 2 =>
                                    {
                                        i as u64
                                    }
                                    _ => {
                                        return Err(LuaError::new(
                                            "vararg table has no proper 'n'",
                                        ));
                                    }
                                }
                            }
                            None => 0,
                        };
                        let get = |t: &Option<&Table>, i: u64| -> Value {
                            match t {
                                Some(tab) => {
                                    tab.raw_get(&Value::Integer(i as i64))
                                }
                                None => Value::Nil,
                            }
                        };
                        if c == 0 {
                            for i in 0..n {
                                self.ensure_stack(base + a + i as usize);
                                self.stack[base + a + i as usize] =
                                    get(&table, i + 1);
                            }
                            self.top = base + a + n as usize;
                        } else {
                            let want = (c - 1) as u64;
                            for i in 0..want {
                                self.ensure_stack(base + a + i as usize);
                                let v = if i < n {
                                    get(&table, i + 1)
                                } else {
                                    Value::Nil
                                };
                                self.stack[base + a + i as usize] = v;
                            }
                        }
                    } else {
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
                            let name = local_at_reg(&self.frames[fi].proto, a as u8, pc as u32)
                                .unwrap_or_else(|| "?".to_string());
                            return Err(LuaError::new(format!(
                                "variable '{name}' got a non-closable value"
                            )));
                        }
                    }
                    self.tbc_slots.push(base + a);
                }

                OpCode::ErrNNil => {
                    if !self.reg(base, a).is_nil() {
                        let name = if bx > 0 {
                            constant_string(&self.frames[fi].proto.constants, bx - 1)
                                .unwrap_or_else(|| "?".to_string())
                        } else {
                            "?".to_string()
                        };
                        return Err(LuaError::new(format!(
                            "global '{name}' already defined"
                        )));
                    }
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
                // Intercept protected calls (also reached from tail calls)
                if self.pcall_ref == Some(gc_ref) {
                    return self.handle_pcall(&actual_args, result_base, num_results);
                }
                if self.xpcall_ref == Some(gc_ref) {
                    return self.handle_xpcall(&actual_args, result_base, num_results);
                }
                // Intercept coroutine special functions
                if self.coro_resume_ref == Some(gc_ref) {
                    return self.handle_resume(&actual_args, result_base, num_results);
                }
                if self.coro_yield_ref == Some(gc_ref) {
                    return self.handle_yield(&actual_args, result_base, num_results);
                }
                if self.coro_running_ref == Some(gc_ref) {
                    self.handle_running(result_base, num_results);
                    return Ok(());
                }
                if self.coro_isyieldable_ref == Some(gc_ref) {
                    self.handle_isyieldable(&actual_args, result_base, num_results);
                    return Ok(());
                }
                if self.coro_close_ref == Some(gc_ref) {
                    return self.handle_close(&actual_args, result_base, num_results);
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
                if self.warn_ref == Some(gc_ref) {
                    self.handle_warn(&actual_args)?;
                    self.place_results(result_base, num_results, &[]);
                    return Ok(());
                }
                if self.sort_ref == Some(gc_ref) {
                    return self.handle_sort(&actual_args, result_base, num_results);
                }
                if self.move_ref == Some(gc_ref) {
                    return self.handle_table_move(&actual_args, result_base, num_results);
                }
                if self.unpack_ref == Some(gc_ref) {
                    return self.handle_table_unpack(&actual_args, result_base, num_results);
                }
                if self.insert_ref == Some(gc_ref) {
                    self.handle_table_insert(&actual_args)?;
                    self.place_results(result_base, num_results, &[]);
                    return Ok(());
                }
                if self.concat_ref == Some(gc_ref) {
                    return self.handle_table_concat(&actual_args, result_base, num_results);
                }
                if self.remove_ref == Some(gc_ref) {
                    return self.handle_table_remove(&actual_args, result_base, num_results);
                }
                if self.pairs_ref == Some(gc_ref) {
                    return self.handle_pairs(&actual_args, result_base, num_results);
                }
                if self.ipairs_ref == Some(gc_ref) {
                    return self.handle_ipairs(&actual_args, result_base, num_results);
                }
                if self.ipairs_iter_ref == Some(gc_ref) {
                    return self.handle_ipairs_iter(&actual_args, result_base, num_results);
                }
                if self.debug_getregistry_ref == Some(gc_ref) {
                    self.handle_debug_getregistry(result_base, num_results);
                    return Ok(());
                }
                if self.tostring_ref == Some(gc_ref) {
                    return self.handle_tostring(&actual_args, result_base, num_results);
                }
                if self.print_ref == Some(gc_ref) {
                    self.handle_print(&actual_args)?;
                    self.place_results(result_base, num_results, &[]);
                    return Ok(());
                }
                if self.gsub_ref == Some(gc_ref) {
                    return self.handle_gsub(&actual_args, result_base, num_results);
                }
                if self.format_ref == Some(gc_ref) {
                    return self.handle_string_format(&actual_args, result_base, num_results);
                }
                if self.debug_sethook_ref == Some(gc_ref) {
                    self.handle_debug_sethook(&actual_args)?;
                    self.place_results(result_base, num_results, &[]);
                    return Ok(());
                }
                if self.debug_gethook_ref == Some(gc_ref) {
                    return self.handle_debug_gethook(&actual_args, result_base, num_results);
                }
                let depth_before = self.frames.len();
                let results = match gc_ref.as_object().as_closure().unwrap() {
                    Closure::Native(nc) => (nc.func)(&actual_args, &mut self.gc)?,
                    Closure::NativeDyn(nc) => (nc.func)(&actual_args, &mut self.gc)?,
                    Closure::WrapIterator(co) => {
                        let co = *co;
                        return self.handle_wrap_call(co, &actual_args, result_base, num_results);
                    }
                    _ => unreachable!(),
                };
                if self.frames.len() > depth_before {
                    // The native deferred its results to a newly pushed Lua
                    // frame (e.g. `dofile`): that frame will place them.
                    return Ok(());
                }
                self.place_results(result_base, num_results, &results);
                self.fire_return_hook(None)?;
            } else {
                let (proto, upvalues) = match gc_ref.as_object().as_closure().unwrap() {
                    Closure::Lua(lc) => (Rc::clone(&lc.proto), lc.upvalues.clone()),
                    _ => unreachable!(),
                };

                let new_base = call_base + 1;
                let num_params = proto.num_params as usize;
                let is_vararg = proto.is_vararg;
                let max_stack = proto.max_stack_size as usize;

                // Reference Lua's stack limit (LUAI_MAXSTACK slots plus a
                // reserve for error handling).
                let stack_limit = if self.error_handling {
                    1_600_000
                } else {
                    1_100_000
                };
                if new_base + max_stack > stack_limit {
                    return Err(LuaError::new("stack overflow"));
                }
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

                let frame_is_hook = self.calling_hook;
                let frame_is_tailcall = self.next_call_is_tail;
                let frame_extraargs = self
                    .next_call_extraargs
                    .max(actual_args.len().saturating_sub(args.len()) as u8);
                self.calling_hook = false;
                self.next_call_is_tail = false;
                self.next_call_extraargs = 0;

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
                    is_hook: frame_is_hook,
                    is_tailcall: frame_is_tailcall,
                    extraargs: frame_extraargs,
                    hook_last_line: 0,
                    hook_last_pc: 0,
                    hook_seen_event: false,
                    hook_single_line: None,
                    metamethod: self.pending_metamethod.take(),
                    called_from_c: self.pending_c_call,
                });
                self.pending_c_call = false;

                // Call hook (after the frame is in place so the hook can
                // inspect it with debug.getinfo).
                if self.hook_mask & HOOK_CALL != 0 && !self.in_hook {
                    let event = if frame_is_tailcall { "tail call" } else { "call" };
                    self.call_hook(event, None)?;
                }

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
        if self.protected_depth >= 200 {
            let msg = self.gc.new_string(b"C stack overflow");
            self.place_results(
                result_base,
                num_results,
                &[Value::Boolean(false), Value::Object(msg)],
            );
            return Ok(());
        }
        self.protected_depth += 1;
        let result = self.handle_pcall_inner(pcall_args, result_base, num_results);
        self.protected_depth -= 1;
        result
    }

    fn handle_pcall_inner(
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

        self.pending_c_call = true;
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
                            if self.self_closing.is_some() {
                                return Err(e);
                            }
                            let e = self.position_error(e);
                            let err_val = e.to_value(&mut self.gc);
                            let prev = self.closing_pcall_name;
                            self.closing_pcall_name = Some("pcall");
                            let err_val = self
                                .recover_from_error(
                                    saved_depth,
                                    saved_open_uv_len,
                                    Some(err_val),
                                )
                                .unwrap_or(err_val);
                            self.closing_pcall_name = prev;
                            self.close_frame_info = None;
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
                if self.self_closing.is_some() {
                    return Err(e);
                }
                // The call itself failed (e.g. calling a non-function)
                let e = self.position_error(e);
                let err_val = e.to_value(&mut self.gc);
                let prev = self.closing_pcall_name;
                self.closing_pcall_name = Some("pcall");
                let err_val = self
                    .recover_from_error(saved_depth, saved_open_uv_len, Some(err_val))
                    .unwrap_or(err_val);
                self.closing_pcall_name = prev;
                self.close_frame_info = None;
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
    fn recover_from_error(
        &mut self,
        saved_depth: usize,
        saved_open_uv_len: usize,
        err_obj: Option<Value>,
    ) -> Option<Value> {
        // Pop the unwound frames first (closing their upvalues), so that
        // closing methods see the protected function as their caller
        // (reference Lua closes variables after restoring the caller).
        let had_frames = self.frames.len() > saved_depth;
        let from = self
            .frames
            .get(saved_depth)
            .map(|f| f.base)
            .unwrap_or(0);
        while self.frames.len() > saved_depth {
            let frame_base = self.frames.last().unwrap().base;
            self.close_upvalues(frame_base);
            self.frames.pop();
        }
        // Close TBC vars for all frames that were unwound. If a closing
        // method raises an error, that becomes the propagated error.
        let close_err = if had_frames {
            match self.close_tbc_vars(from, err_obj) {
                Ok(()) => None,
                Err(e) => Some(e.to_value(&mut self.gc)),
            }
        } else {
            None
        };
        // Trim any open upvalues that were created inside the failed call
        self.open_upvalues.truncate(saved_open_uv_len);
        // Drop stale stack-top information left by the popped frames.
        if let Some(last) = self.frames.last() {
            let ft = last.base + last.proto.max_stack_size as usize;
            if self.top > ft {
                self.top = ft;
            }
        }
        close_err
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
        co.hook_func = self.hook_func.take();
        co.hook_mask = self.hook_mask;
        co.hook_count = self.hook_count;
        co.hook_counter = self.hook_counter;
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
        self.hook_func = co.hook_func.take();
        self.hook_mask = co.hook_mask;
        self.hook_count = co.hook_count;
        self.hook_counter = co.hook_counter;
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
                return Err(LuaError::new(
                    "bad argument #1 to 'resume' (coroutine expected)",
                ));
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
            let depth_before = self.frames.len();
            if let Err(e) = self.call_function(call_base, resume_args.len() + 1, -1) {
                let err_val = e.to_value(&mut self.gc);
                co_ref.as_object_mut().as_coroutine_mut().unwrap().status =
                    CoroutineStatus::Dead;
                co_ref
                    .as_object_mut()
                    .as_coroutine_mut()
                    .unwrap()
                    .pending_error = Some(err_val);
                self.save_vm_to_thread(co_ref);
                self.load_thread_to_vm(resumer_ref);
                resumer_ref.as_object_mut().as_coroutine_mut().unwrap().status =
                    CoroutineStatus::Running;
                self.current_thread = if resumer_ref == self.main_thread.unwrap() {
                    None
                } else {
                    Some(resumer_ref)
                };
                let rb = resumer_ref
                    .as_object()
                    .as_coroutine()
                    .unwrap()
                    .yield_result_base;
                let nr = resumer_ref
                    .as_object()
                    .as_coroutine()
                    .unwrap()
                    .yield_num_results;
                self.place_results(
                    rb,
                    nr,
                    &[Value::Boolean(false), err_val],
                );
                return Ok(());
            }
            if self.frames.len() == depth_before {
                // Native body finished synchronously.
                self.last_return_values =
                    self.stack[call_base..self.top.max(call_base)].to_vec();
            }
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
                    let e = self.position_error(e);
                    if self.self_closing.is_some() {
                        break Err(e);
                    }
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
                            let prev = self.closing_pcall_name;
                            self.closing_pcall_name = Some(if guard.is_xpcall {
                                "xpcall"
                            } else {
                                "pcall"
                            });
                            let handled = self
                                .recover_from_error(
                                    guard.frame_depth,
                                    guard.open_uv_len,
                                    Some(err_val),
                                )
                                .unwrap_or(handled);
                            self.closing_pcall_name = prev;
                            self.place_results(
                                guard.result_base,
                                guard.num_results,
                                &[Value::Boolean(false), handled],
                            );
                            // The protected call is the coroutine body (or a
                            // nested one): take its results as pending
                            // return values for when the coroutine ends.
                            let rb = guard.result_base;
                            let top =
                                self.top.max(rb).min(self.stack.len());
                            self.last_return_values =
                                self.stack[rb..top].to_vec();
                            continue; // Re-enter execution
                        }
                    }
                    break Err(e);
                }
            }
        };

        // Determine outcome
        if let Err(e) = exec_result {
            if self.self_closing == Some(co_ref) {
                // Closed itself: close remaining variables, discard frames,
                // and finish without results.
                self.self_closing = None;
                let uv_len = self.open_upvalues.len();
                let close_err = self.recover_from_error(0, uv_len, None);
                co_ref.as_object_mut().as_coroutine_mut().unwrap().status =
                    CoroutineStatus::Dead;
                self.save_vm_to_thread(co_ref);
                self.load_thread_to_vm(resumer_ref);
                resumer_ref.as_object_mut().as_coroutine_mut().unwrap().status =
                    CoroutineStatus::Running;
                self.current_thread = if resumer_ref == self.main_thread.unwrap() {
                    None
                } else {
                    Some(resumer_ref)
                };
                let rb = resumer_ref
                    .as_object()
                    .as_coroutine()
                    .unwrap()
                    .yield_result_base;
                let nr = resumer_ref
                    .as_object()
                    .as_coroutine()
                    .unwrap()
                    .yield_num_results;
                match close_err {
                    Some(err) => self.place_results(
                        rb,
                        nr,
                        &[Value::Boolean(false), err],
                    ),
                    None => {
                        self.place_results(rb, nr, &[Value::Boolean(true)])
                    }
                }
                return Ok(());
            }
            // Error: coroutine is now dead
            let err_val = e.to_value(&mut self.gc);
            co_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Dead;
            co_ref
                .as_object_mut()
                .as_coroutine_mut()
                .unwrap()
                .pending_error = Some(err_val);
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
        if self.unyieldable_depth > 0 {
            return Err(LuaError::new(
                "attempt to yield across a C-call boundary",
            ));
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
            let depth_before = self.frames.len();
            if let Err(e) = self.call_function(call_base, args.len() + 1, -1) {
                co_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Dead;
                self.save_vm_to_thread(co_ref);
                self.load_thread_to_vm(resumer_ref);
                resumer_ref.as_object_mut().as_coroutine_mut().unwrap().status = CoroutineStatus::Running;
                self.current_thread = if resumer_ref == self.main_thread.unwrap() { None } else { Some(resumer_ref) };
                return Err(e);
            }
            if self.frames.len() == depth_before {
                // Native body finished synchronously: its results are ready.
                self.last_return_values = self.stack[call_base..self.top.max(call_base)]
                    .to_vec();
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
                    let e = self.position_error(e);
                    if let Some(guard) = self.pcall_guards.last() {
                        if guard.frame_depth <= self.frames.len() {
                            let guard = self.pcall_guards.pop().unwrap();
                            let err_val = e.to_value(&mut self.gc);
                            let handled = if guard.is_xpcall {
                                self.call_message_handler(guard.handler, err_val)
                            } else {
                                err_val
                            };
                            let prev = self.closing_pcall_name;
                            self.closing_pcall_name = Some(if guard.is_xpcall {
                                "xpcall"
                            } else {
                                "pcall"
                            });
                            let handled = self
                                .recover_from_error(
                                    guard.frame_depth,
                                    guard.open_uv_len,
                                    Some(err_val),
                                )
                                .unwrap_or(handled);
                            self.closing_pcall_name = prev;
                            self.place_results(
                                guard.result_base,
                                guard.num_results,
                                &[Value::Boolean(false), handled],
                            );
                            let rb = guard.result_base;
                            let top =
                                self.top.max(rb).min(self.stack.len());
                            self.last_return_values =
                                self.stack[rb..top].to_vec();
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
        // A C-level frame in the running thread makes it unyieldable.
        let in_c_call = self.running_thread() == co_ref && self.unyieldable_depth > 0;
        self.place_results(
            result_base,
            num_results,
            &[Value::Boolean(!is_main && !in_c_call)],
        );
    }

    /// Call a function from a C-level context where yielding is not
    /// allowed (gsub replacements, sort comparators, ...).
    fn call_value_unyieldable(
        &mut self,
        func: Value,
        args: &[Value],
    ) -> Result<Vec<Value>, LuaError> {
        self.unyieldable_depth += 1;
        let r = self.call_value(func, args);
        self.unyieldable_depth -= 1;
        r
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
            Some(v) if !v.is_nil() => {
                return Err(LuaError::new(
                    "bad argument #1 to 'close' (coroutine expected)",
                ));
            }
            _ => self.running_thread(),
        };

        let is_main = Some(co_ref) == self.main_thread;
        if is_main && self.running_thread() == co_ref {
            return Err(LuaError::new("cannot close main thread"));
        }
        let status = co_ref.as_object().as_coroutine().unwrap().status;
        match status {
            CoroutineStatus::Dead => {
                let pending = co_ref
                    .as_object_mut()
                    .as_coroutine_mut()
                    .unwrap()
                    .pending_error
                    .take();
                match pending {
                    Some(err) => self.place_results(
                        result_base,
                        num_results,
                        &[Value::Boolean(false), err],
                    ),
                    None => self.place_results(
                        result_base,
                        num_results,
                        &[Value::Boolean(true)],
                    ),
                }
            }
            CoroutineStatus::Suspended => {
                // Close TBC variables and upvalues of the suspended thread.
                let resumer_ref = self.running_thread();
                self.save_vm_to_thread(resumer_ref);

                self.load_thread_to_vm(co_ref);
                self.closing_thread = Some(co_ref);
                let prev_current = self.current_thread;
                self.current_thread = if co_ref == self.main_thread.unwrap() {
                    None
                } else {
                    Some(co_ref)
                };
                co_ref.as_object_mut().as_coroutine_mut().unwrap().status =
                    CoroutineStatus::Running;
                let close_result = if self.frames.is_empty() {
                    Ok(())
                } else {
                    self.close_tbc_vars(0, None)
                };
                self.close_upvalues(0);
                self.frames.clear();
                let mut err_val = None;
                if let Err(e) = close_result {
                    err_val = Some(e.to_value(&mut self.gc));
                }
                self.save_vm_to_thread(co_ref);
                self.closing_thread = None;
                self.current_thread = prev_current;
                co_ref.as_object_mut().as_coroutine_mut().unwrap().status =
                    CoroutineStatus::Dead;

                self.load_thread_to_vm(resumer_ref);
                match err_val {
                    Some(err) => self.place_results(
                        result_base,
                        num_results,
                        &[Value::Boolean(false), err],
                    ),
                    None => self.place_results(
                        result_base,
                        num_results,
                        &[Value::Boolean(true)],
                    ),
                }
            }
            _ => {
                if status == CoroutineStatus::Running
                    && self.running_thread() == co_ref
                    && !is_main
                {
                    if self.closing_thread == Some(co_ref) {
                        // A closing method re-closing its own coroutine
                        // while it is being closed: tolerated.
                        self.place_results(result_base, num_results, &[]);
                        return Ok(());
                    }
                    // Closing the running coroutine: unwind it to its
                    // resume point (pcall recovery entries are bypassed).
                    self.self_closing = Some(co_ref);
                    return Err(LuaError::new("__coroutine_self_close__"));
                }
                let msg = match status {
                    CoroutineStatus::Running => {
                        "cannot close a running coroutine".to_string()
                    }
                    CoroutineStatus::Normal => {
                        "cannot close a normal coroutine".to_string()
                    }
                    CoroutineStatus::Dead => unreachable!(),
                    CoroutineStatus::Suspended => unreachable!(),
                };
                return Err(LuaError::new(msg));
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
        match frame.proto.source.as_deref() {
            Some(source) => Some(format!("{}:{}", chunkid(source), line)),
            None => Some("?:?".to_string()),
        }
    }

    /// Annotate a runtime error with the current source position, unless it
    /// already carries one. Mirrors `luaG_runerror`/`luaG_addinfo`: only
    /// string error objects get the `source:line: ` prefix.
    fn position_error(&mut self, e: LuaError) -> LuaError {
        if e.positioned {
            return e;
        }
        // Determine the string message to annotate (non-strings pass through).
        let msg: String = match e.value {
            Some(Value::Object(r)) if r.as_object().as_string().is_some() => {
                let s = r.as_object().as_string().unwrap();
                match std::str::from_utf8(s.as_bytes()) {
                    Ok(s) => s.to_string(),
                    Err(_) => return e.mark_positioned(),
                }
            }
            Some(_) => {
                // Nil/table/other error objects are not annotated (nil is
                // turned into "<no error object>" by `to_value`).
                return e.mark_positioned();
            }
            None => e.message.clone(),
        };
        match self.get_source_line(0) {
            Some(loc) => {
                let annotated = format!("{loc}: {msg}");
                let s = self.gc.new_string(annotated.as_bytes());
                LuaError {
                    message: annotated,
                    value: Some(Value::Object(s)),
                    positioned: true,
                }
            }
            None => e.mark_positioned(),
        }
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
        Err(LuaError::with_value(annotated).mark_positioned())
    }

    // ── xpcall ─────────────────────────────────────────────────────

    /// Handle xpcall(f, msgh [, arg1, ...]).
    fn handle_xpcall(
        &mut self,
        xpcall_args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        // The message handler must be a function.
        let msgh = xpcall_args.get(1).copied().unwrap_or(Value::Nil);
        if !msgh.is_function() {
            return Err(LuaError::new(format!(
                "bad argument #2 to 'xpcall' (function expected, got {})",
                msgh.type_name()
            )));
        }
        if self.protected_depth >= 200 {
            let msg = self.gc.new_string(b"C stack overflow");
            self.place_results(
                result_base,
                num_results,
                &[Value::Boolean(false), Value::Object(msg)],
            );
            return Ok(());
        }
        self.protected_depth += 1;
        let result = self.handle_xpcall_inner(xpcall_args, result_base, num_results);
        self.protected_depth -= 1;
        result
    }

    fn handle_xpcall_inner(
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

        self.pending_c_call = true;
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
                            if self.self_closing.is_some() {
                                return Err(e);
                            }
                            let e = self.position_error(e);
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
                if self.self_closing.is_some() {
                    return Err(e);
                }
                let e = self.position_error(e);
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
        // If the handler itself errors, reference Lua reports
        // "error in error handling".
        let prev = self.error_handling;
        self.error_handling = true;
        let result = self.call_value_unyieldable(msgh, &[err_val]);
        self.error_handling = prev;
        match result {
            Ok(results) => results.into_iter().next().unwrap_or(Value::Nil),
            Err(e) => {
                let msg = self.gc.new_string(b"error in error handling");
                Value::Object(msg)
            }
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
        if let Some((src, line)) = &self.close_frame_info {
            result.push_str("\n\t");
            result.push_str(&chunkid(src));
            result.push(':');
            result.push_str(&line.to_string());
            result.push_str(": in metamethod 'close'");
        }

        let num_frames = self.frames.len();
        // Level 1 = the function that called `debug.traceback` (which is
        // handled inline and has no frame of its own).
        let start = if level <= num_frames {
            num_frames - level + 1
        } else {
            0
        };

        for i in (0..start).rev() {
            let frame = &self.frames[i];
            let pc = if frame.pc > 0 { frame.pc - 1 } else { 0 };
            let line = frame.proto.line_info.get(pc).copied().unwrap_or(0);
            let source = match frame.proto.source.as_deref() {
                Some(s) => chunkid(s),
                None => "?".to_string(),
            };

            result.push_str("\n\t");
            result.push_str(&source);
            result.push(':');
            result.push_str(&line.to_string());
            result.push_str(": in ");

            // Try to find function name
            let closure = frame.closure.as_object().as_closure().unwrap();
            if let Some(mm) = &frame.metamethod {
                result.push_str("metamethod '");
                result.push_str(mm);
                result.push('\'');
                continue;
            }
            match closure {
                Closure::Lua(_) => {
                    if frame.is_hook {
                        result.push_str("hook '?'");
                    } else if i == 0 {
                        result.push_str("main chunk");
                    } else {
                        let (name, namewhat) = self.function_name_at(i);
                        if !namewhat.is_empty() {
                            result.push_str(namewhat);
                            result.push_str(" '");
                            result.push_str(&name.unwrap_or_default());
                            result.push('\'');
                        } else {
                            result.push_str("function <");
                            result.push_str(&source);
                            result.push(':');
                            result.push_str(&frame.proto.line_defined.to_string());
                            result.push('>');
                        }
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

    /// Convert a value to a string, honoring a `__tostring` metamethod.
    fn value_to_string(&mut self, v: Value) -> Result<Vec<u8>, LuaError> {
        if let Some(mm) = self.get_metamethod(v, MM_TOSTRING) {
            let results = self.call_value(mm, &[v])?;
            let first = results.first().copied().unwrap_or(Value::Nil);
            match first {
                Value::Object(r) if r.as_object().as_string().is_some() => {
                    return Ok(r.as_object().as_string().unwrap().as_bytes().to_vec());
                }
                Value::Integer(n) => return Ok(format!("{n}").into_bytes()),
                Value::Float(f) => {
                    return Ok(crate::value::lua_float_to_string(f).into_bytes())
                }
                _ => return Err(LuaError::new("'__tostring' must return a string")),
            }
        }
        // Strings are returned verbatim (raw bytes, may not be UTF-8).
        if let Value::Object(r) = v {
            if let Some(s) = r.as_object().as_string() {
                return Ok(s.as_bytes().to_vec());
            }
        }
        // `__name` fallback (only for collectable values that have no
        // dedicated string representation).
        if let Value::Object(r) = v {
            if r.as_object().as_string().is_none() {
                if let Some(Value::Object(nr)) = self.get_metamethod(v, MM_NAME) {
                    if let Some(ns) = nr.as_object().as_string() {
                        let mut out = ns.as_bytes().to_vec();
                        out.extend_from_slice(
                            format!(": 0x{:x}", r.ptr_value()).as_bytes(),
                        );
                        return Ok(out);
                    }
                }
            }
        }
        Ok(format!("{v}").into_bytes())
    }

    /// Handle `string.format(fmt, ...)` (VM-special so `%s` can call
    /// `__tostring`).
    fn handle_string_format(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let fmt = crate::stdlib::string::check_string(args, 0, "format")?;
        let out = {
            let vals: &[Value] = if args.len() > 1 { &args[1..] } else { &[] };
            crate::stdlib::string::format_values(&fmt, vals, &mut |v| {
                self.value_to_string(v)
            })?
        };
        let s = self.gc.new_string(&out);
        self.place_results(result_base, num_results, &[Value::Object(s)]);
        Ok(())
    }

    /// Handle `tostring(v)`.
    fn handle_tostring(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        if args.is_empty() {
            return Err(LuaError::new(
                "bad argument #1 to 'tostring' (value expected)",
            ));
        }
        let bytes = self.value_to_string(args[0])?;
        let s = self.gc.new_string(&bytes);
        self.place_results(result_base, num_results, &[Value::Object(s)]);
        Ok(())
    }

    /// Handle `table.move(a1, f, e, t [, a2])`, mirroring `tmove` from
    /// `ltablib.c`: honors `__index`/`__newindex` and picks the copy
    /// direction that is safe for overlapping ranges.
    fn handle_table_move(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        fn check_int(args: &[Value], idx: usize) -> Result<i64, LuaError> {
            let v = args.get(idx).copied().unwrap_or(Value::Nil);
            match v {
                Value::Integer(i) => Ok(i),
                Value::Float(f) if f.floor() == f => Ok(f as i64),
                _ => Err(LuaError::new(format!(
                    "bad argument #{} to 'move' (number expected, got {})",
                    idx + 1,
                    v.type_name()
                ))),
            }
        }
        let f = check_int(args, 1)?;
        let e = check_int(args, 2)?;
        let t = check_int(args, 3)?;
        let a1 = args.first().copied().unwrap_or(Value::Nil);
        let has_dst = args.len() > 4 && !args[4].is_nil();
        let dst = if has_dst { args[4] } else { a1 };
        let dst_argn = if has_dst { 5 } else { 1 };

        self.check_move_table(a1, 1, false)?;
        self.check_move_table(dst, dst_argn, true)?;

        if e >= f {
            if !(f > 0 || e < i64::MAX.wrapping_add(f)) {
                return Err(LuaError::new(
                    "bad argument #3 to 'move' (too many elements to move)",
                ));
            }
            let n = e.wrapping_sub(f).wrapping_add(1);
            if t > i64::MAX.wrapping_sub(n).wrapping_add(1) {
                return Err(LuaError::new(
                    "bad argument #4 to 'move' (destination wrap around)",
                ));
            }
            let same = !has_dst || a1 == dst;
            if t > e || t <= f || !same {
                let mut i = 0i64;
                while i < n {
                    let v = self.table_get(a1, Value::Integer(f.wrapping_add(i)))?;
                    self.table_set(dst, Value::Integer(t.wrapping_add(i)), v)?;
                    i += 1;
                }
            } else {
                let mut i = n - 1;
                loop {
                    let v = self.table_get(a1, Value::Integer(f.wrapping_add(i)))?;
                    self.table_set(dst, Value::Integer(t.wrapping_add(i)), v)?;
                    if i == 0 {
                        break;
                    }
                    i -= 1;
                }
            }
        }
        self.place_results(result_base, num_results, &[dst]);
        Ok(())
    }

    /// Look up the length of a value like `luaL_len`: honors `__len` and
    /// requires an integer result.
    fn lua_len_integer(&mut self, v: Value) -> Result<i64, LuaError> {
        let l = self.value_length(v)?;
        match l {
            Value::Integer(n) => Ok(n),
            Value::Float(f) if f.floor() == f => Ok(f as i64),
            _ => Err(LuaError::new("object length is not an integer")),
        }
    }

    /// Handle `table.insert(list, [pos,] value)`, mirroring `tinsert` from
    /// `ltablib.c`.
    fn handle_table_insert(&mut self, args: &[Value]) -> Result<(), LuaError> {
        let t = args.first().copied().unwrap_or(Value::Nil);
        self.check_move_table_rw(t, 1)?;
        let e = self.lua_len_integer(t)?.wrapping_add(1);
        let pos;
        match args.len() {
            2 => pos = e,
            3 => {
                let p = match args[1] {
                    Value::Integer(i) => i,
                    Value::Float(f) if f.floor() == f => f as i64,
                    v => {
                        return Err(LuaError::new(format!(
                            "bad argument #2 to 'insert' (number expected, got {})",
                            v.type_name()
                        )));
                    }
                };
                if (p as u64).wrapping_sub(1) >= e as u64 {
                    return Err(LuaError::new(
                        "bad argument #2 to 'insert' (position out of bounds)",
                    ));
                }
                pos = p;
                let mut i = e;
                while i > pos {
                    let v = self.table_get(t, Value::Integer(i - 1))?;
                    self.table_set(t, Value::Integer(i), v)?;
                    i -= 1;
                }
            }
            _ => {
                return Err(LuaError::new("wrong number of arguments to 'insert'"));
            }
        }
        let val = args[args.len() - 1];
        self.table_set(t, Value::Integer(pos), val)?;
        Ok(())
    }

    /// Check a `table.move`-style argument that needs both `__index` and
    /// `__newindex` (TAB_RW).
    fn check_move_table_rw(&self, v: Value, argn: usize) -> Result<(), LuaError> {
        if matches!(v, Value::Object(r) if r.as_object().as_table().is_some()) {
            return Ok(());
        }
        if self.get_metatable(v).is_some()
            && self.get_metamethod(v, MM_INDEX).is_some()
            && self.get_metamethod(v, MM_NEWINDEX).is_some()
        {
            return Ok(());
        }
        Err(LuaError::new(format!(
            "bad argument #{} to 'insert' (table expected, got {})",
            argn,
            v.type_name()
        )))
    }

    /// Handle `table.unpack(list [, i [, j]])`, mirroring `tunpack` from
    /// `ltablib.c`: reads through `__index` and defaults `j` to `#list`
    /// (honoring `__len`).
    fn handle_table_unpack(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        fn check_int(args: &[Value], idx: usize) -> Result<i64, LuaError> {
            let v = args.get(idx).copied().unwrap_or(Value::Nil);
            match v {
                Value::Integer(i) => Ok(i),
                Value::Float(f) if f.floor() == f => Ok(f as i64),
                _ => Err(LuaError::new(format!(
                    "bad argument #{} to 'unpack' (number expected, got {})",
                    idx + 1,
                    v.type_name()
                ))),
            }
        }
        let list = args.first().copied().unwrap_or(Value::Nil);
        let i = if args.len() > 1 && !args[1].is_nil() {
            check_int(args, 1)?
        } else {
            1
        };
        let e = if args.len() > 2 && !args[2].is_nil() {
            check_int(args, 2)?
        } else {
            let l = self.value_length(list)?;
            match l {
                Value::Integer(n) => n,
                Value::Float(f) if f.floor() == f => f as i64,
                _ => {
                    return Err(LuaError::new("object length is not an integer"));
                }
            }
        };
        if i > e {
            self.place_results(result_base, num_results, &[]);
            return Ok(());
        }
        let n = (e as u64).wrapping_sub(i as u64);
        if n >= i32::MAX as u64 || n + 1 > 1_000_000 {
            return Err(LuaError::new("too many results to unpack"));
        }
        let mut results = Vec::with_capacity(n as usize + 1);
        let mut k = i;
        loop {
            results.push(self.table_get(list, Value::Integer(k))?);
            if k == e {
                break;
            }
            k += 1;
        }
        self.place_results(result_base, num_results, &results);
        Ok(())
    }

    /// Check that a `table.move` argument is a table or can behave like
    /// one (has a metatable with the required metamethod).
    fn check_move_table(&self, v: Value, argn: usize, write: bool) -> Result<(), LuaError> {
        self.check_tab_arg(v, argn, write, "move")
    }

    /// Check that an argument is a table (or has the needed metamethods).
    fn check_tab_arg(
        &self,
        v: Value,
        argn: usize,
        write: bool,
        fname: &str,
    ) -> Result<(), LuaError> {
        if matches!(v, Value::Object(r) if r.as_object().as_table().is_some()) {
            return Ok(());
        }
        let mm = if write { MM_NEWINDEX } else { MM_INDEX };
        if self.get_metatable(v).is_some() && self.get_metamethod(v, mm).is_some() {
            return Ok(());
        }
        Err(LuaError::new(format!(
            "bad argument #{} to '{}' (table expected, got {})",
            argn,
            fname,
            v.type_name()
        )))
    }

    /// Handle `pairs(t)` (VM-special: may call `__pairs`).
    fn handle_pairs(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        if args.is_empty() {
            return Err(LuaError::new(
                "bad argument #1 to 'pairs' (value expected)",
            ));
        }
        let v = args[0];
        if let Some(mm) = self.get_metamethod(v, MM_PAIRS) {
            // Call the metamethod like a regular call so that it may yield;
            // its four results are placed at `result_base` on return.
            let cb = self.find_call_base();
            self.ensure_stack(cb + 2);
            self.stack[cb] = mm;
            self.stack[cb + 1] = v;
            self.do_call(mm, cb, &[v], result_base, 4)?;
            return Ok(());
        }
        let next_fn = Value::Object(self.next_ref.unwrap());
        self.place_results(
            result_base,
            num_results,
            &[next_fn, v, Value::Nil, Value::Nil],
        );
        Ok(())
    }

    /// Handle `ipairs(t)` (VM-special: the iterator honors `__index`).
    fn handle_ipairs(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        if args.is_empty() {
            return Err(LuaError::new(
                "bad argument #1 to 'ipairs' (value expected)",
            ));
        }
        let iter = Value::Object(self.ipairs_iter_ref.unwrap());
        self.place_results(
            result_base,
            num_results,
            &[iter, args[0], Value::Integer(0)],
        );
        Ok(())
    }

    /// The `ipairs` iteration function.
    fn handle_ipairs_iter(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let t = args.first().copied().unwrap_or(Value::Nil);
        let i = match args.get(1).copied().unwrap_or(Value::Integer(0)) {
            Value::Integer(n) => n,
            Value::Float(f) => f as i64,
            v => {
                return Err(LuaError::new(format!(
                    "bad argument #2 to 'for iterator' (number expected, got {})",
                    v.type_name()
                )));
            }
        };
        let i = i.wrapping_add(1);
        let v = self.table_get(t, Value::Integer(i))?;
        if v.is_nil() {
            self.place_results(result_base, num_results, &[Value::Nil]);
        } else {
            self.place_results(
                result_base,
                num_results,
                &[Value::Integer(i), v],
            );
        }
        Ok(())
    }

    /// Handle `table.remove(list [, pos])`, mirroring `tremove` from
    /// `ltablib.c`.
    fn handle_table_remove(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let t = args.first().copied().unwrap_or(Value::Nil);
        self.check_move_table_rw(t, 1)?;
        let size = self.lua_len_integer(t)?;
        let pos = if args.len() > 1 && !args[1].is_nil() {
            match args[1] {
                Value::Integer(i) => i,
                Value::Float(f) if f.floor() == f => f as i64,
                v => {
                    return Err(LuaError::new(format!(
                        "bad argument #2 to 'remove' (number expected, got {})",
                        v.type_name()
                    )));
                }
            }
        } else {
            size
        };
        if pos != size && (pos as u64).wrapping_sub(1) > size as u64 {
            return Err(LuaError::new(
                "bad argument #2 to 'remove' (position out of bounds)",
            ));
        }
        let result = self.table_get(t, Value::Integer(pos))?;
        let mut p = pos;
        while p < size {
            let v = self.table_get(t, Value::Integer(p + 1))?;
            self.table_set(t, Value::Integer(p), v)?;
            p += 1;
        }
        self.table_set(t, Value::Integer(p), Value::Nil)?;
        self.place_results(result_base, num_results, &[result]);
        Ok(())
    }

    /// Handle `table.concat(list [, sep [, i [, j]]])`, mirroring
    /// `tconcat` from `ltablib.c`.
    fn handle_table_concat(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        fn check_int(args: &[Value], idx: usize) -> Result<i64, LuaError> {
            let v = args.get(idx).copied().unwrap_or(Value::Nil);
            match v {
                Value::Integer(i) => Ok(i),
                Value::Float(f) if f.floor() == f => Ok(f as i64),
                _ => Err(LuaError::new(format!(
                    "bad argument #{} to 'concat' (number expected, got {})",
                    idx + 1,
                    v.type_name()
                ))),
            }
        }
        let t = args.first().copied().unwrap_or(Value::Nil);
        self.check_tab_arg(t, 1, false, "concat")?;
        let mut last = self.lua_len_integer(t)?;
        let sep: Vec<u8> = match args.get(1) {
            None | Some(Value::Nil) => Vec::new(),
            Some(Value::Object(r)) if r.as_object().as_string().is_some() => {
                r.as_object().as_string().unwrap().as_bytes().to_vec()
            }
            Some(Value::Integer(n)) => n.to_string().into_bytes(),
            Some(Value::Float(f)) => crate::value::lua_float_to_string(*f).into_bytes(),
            Some(v) => {
                return Err(LuaError::new(format!(
                    "bad argument #2 to 'concat' (string expected, got {})",
                    v.type_name()
                )));
            }
        };
        let i = if args.len() > 2 && !args[2].is_nil() {
            check_int(args, 2)?
        } else {
            1
        };
        if args.len() > 3 && !args[3].is_nil() {
            last = check_int(args, 3)?;
        }
        let mut out: Vec<u8> = Vec::new();
        let mut k = i;
        while k < last {
            let v = self.table_get(t, Value::Integer(k))?;
            Self::append_concat_field(&mut out, v, k)?;
            out.extend_from_slice(&sep);
            k += 1;
        }
        if k == last {
            let v = self.table_get(t, Value::Integer(k))?;
            Self::append_concat_field(&mut out, v, k)?;
        }
        let s = self.gc.new_string(&out);
        self.place_results(result_base, num_results, &[Value::Object(s)]);
        Ok(())
    }

    fn append_concat_field(
        out: &mut Vec<u8>,
        v: Value,
        idx: i64,
    ) -> Result<(), LuaError> {
        match v {
            Value::Object(r) if r.as_object().as_string().is_some() => {
                out.extend_from_slice(r.as_object().as_string().unwrap().as_bytes());
            }
            Value::Integer(n) => out.extend_from_slice(n.to_string().as_bytes()),
            Value::Float(f) => {
                out.extend_from_slice(crate::value::lua_float_to_string(f).as_bytes())
            }
            _ => {
                return Err(LuaError::new(format!(
                    "invalid value ({}) at index {} in table for 'concat'",
                    v.type_name(),
                    idx
                )));
            }
        }
        Ok(())
    }

    /// Handle `string.gsub(s, pat, repl [, n])`, mirroring `str_gsub`
    /// from `lstrlib.c` (including the 5.3.3 empty-match rules).
    fn handle_gsub(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        use crate::stdlib::string::{apply_string_replacement, MatchState};
        let s = crate::stdlib::string::check_string(args, 0, "gsub")?;
        let pat = crate::stdlib::string::check_string(args, 1, "gsub")?;
        let repl = args.get(2).copied().unwrap_or(Value::Nil);
        if !matches!(
            repl,
            Value::Integer(_)
                | Value::Float(_)
                | Value::Object(_)
        ) {
            return Err(LuaError::new(
                "bad argument #3 to 'gsub' (string/function/table expected)",
            ));
        }
        let max_s: i64 = match args.get(3).and_then(|v| v.as_integer()) {
            Some(n) => n,
            None => s.len() as i64 + 1,
        };

        let anchored = !pat.is_empty() && pat[0] == b'^';
        let pat_slice = if anchored { &pat[1..] } else { &pat[..] };

        let mut result: Vec<u8> = Vec::new();
        let mut src = 0usize;
        let mut lastmatch: Option<usize> = None;
        let mut n: i64 = 0;
        let mut changed = false;

        while n < max_s {
            let mut ms = MatchState::new(&s, pat_slice);
            let matched = ms.match_pattern(src, 0)?;
            let e_opt = match matched {
                Some(e) if Some(e) != lastmatch => Some(e),
                _ => None,
            };
            if let Some(e) = e_opt {
                n += 1;
                // Build the replacement piece.
                match repl {
                    Value::Object(r) if r.as_object().as_string().is_some() => {
                        let repl_str = r.as_object().as_string().unwrap().as_bytes().to_vec();
                        apply_string_replacement(&mut result, &repl_str, &ms, &s, src, e)?;
                        changed = true;
                    }
                    Value::Object(r) if r.as_object().as_table().is_some() => {
                        let key = if ms.captures.is_empty() {
                            let cap = self.gc.new_string(&s[src..e]);
                            Value::Object(cap)
                        } else {
                            match ms.captures[0].len {
                                crate::stdlib::string::CaptureLen::Position => {
                                    Value::Integer(ms.captures[0].start as i64 + 1)
                                }
                                crate::stdlib::string::CaptureLen::Len(len) => {
                                    let cap = self
                                        .gc
                                        .new_string(&s[ms.captures[0].start..ms.captures[0].start + len]);
                                    Value::Object(cap)
                                }
                                crate::stdlib::string::CaptureLen::Unfinished => Value::Nil,
                            }
                        };
                        let val = self.table_get(Value::Object(r), key)?;
                        if val.is_truthy() {
                            let piece = Self::replacement_bytes(val)?;
                            result.extend_from_slice(&piece);
                            changed = true;
                        } else {
                            result.extend_from_slice(&s[src..e]);
                        }
                    }
                    Value::Object(r) if r.as_object().as_closure().is_some() => {
                        let captures =
                            crate::stdlib::string::get_captures(&ms, &s, src, e, &mut self.gc)?;
                        let results =
                            self.call_value_unyieldable(repl, &captures)?;
                        let first = results.first().copied().unwrap_or(Value::Nil);
                        if first.is_nil() || first == Value::Boolean(false) {
                            result.extend_from_slice(&s[src..e]);
                        } else {
                            let piece = Self::replacement_bytes(first)?;
                            result.extend_from_slice(&piece);
                            changed = true;
                        }
                    }
                    _ => {
                        // Number: formatted like tostring.
                        let piece = Self::replacement_bytes(repl)?;
                        result.extend_from_slice(&piece);
                        changed = true;
                    }
                }
                src = e;
                lastmatch = Some(e);
            } else if src < s.len() {
                result.push(s[src]);
                src += 1;
            } else {
                break;
            }
            if anchored {
                break;
            }
        }

        let out_val = if !changed {
            args[0]
        } else {
            result.extend_from_slice(&s[src..]);
            let r = self.gc.new_string(&result);
            Value::Object(r)
        };
        self.place_results(
            result_base,
            num_results,
            &[out_val, Value::Integer(n)],
        );
        Ok(())
    }

    /// Convert a gsub replacement value to bytes (strings and numbers only).
    fn replacement_bytes(v: Value) -> Result<Vec<u8>, LuaError> {
        match v {
            Value::Object(r) if r.as_object().as_string().is_some() => {
                Ok(r.as_object().as_string().unwrap().as_bytes().to_vec())
            }
            Value::Integer(n) => Ok(format!("{n}").into_bytes()),
            Value::Float(n) => {
                Ok(crate::value::lua_float_to_string(n).into_bytes())
            }
            other => Err(LuaError::new(format!(
                "invalid replacement value (a {})",
                other.type_name()
            ))),
        }
    }

    /// Handle `print(...)` using `__tostring` semantics.
    fn handle_print(&mut self, args: &[Value]) -> Result<(), LuaError> {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                print!("\t");
            }
            let bytes = self.value_to_string(*arg)?;
            print!("{}", String::from_utf8_lossy(&bytes));
        }
        println!();
        Ok(())
    }

    /// Handle debug.getregistry().
    fn handle_debug_getregistry(&mut self, result_base: usize, num_results: i32) {
        let val = match self.registry {
            Some(r) => Value::Object(r),
            None => Value::Nil,
        };
        self.place_results(result_base, num_results, &[val]);
    }

    /// Invoke the active debug hook with an event name and optional line.
    fn call_hook(&mut self, event: &str, line: Option<u32>) -> Result<(), LuaError> {
        let hook = match self.hook_func {
            Some(h) => Value::Object(h),
            None => return Ok(()),
        };
        let saved_in_hook = self.in_hook;
        self.in_hook = true;
        self.calling_hook = true;

        let ev = self.gc.new_string(event.as_bytes());
        let line_val = line
            .map(|l| Value::Integer(l as i64))
            .unwrap_or(Value::Nil);
        let res = self.call_value(hook, &[Value::Object(ev), line_val]);

        self.calling_hook = false;
        self.in_hook = saved_in_hook;
        res.map(|_| ())
    }

    /// Decode a hook mask string + count into mask bits.
    fn hook_mask_from(mask_bytes: &[u8], count: i64) -> u8 {
        let mut mask = 0u8;
        if mask_bytes.contains(&b'c') {
            mask |= HOOK_CALL;
        }
        if mask_bytes.contains(&b'r') {
            mask |= HOOK_RET;
        }
        if mask_bytes.contains(&b'l') {
            mask |= HOOK_LINE;
        }
        if count > 0 {
            mask |= HOOK_COUNT;
        }
        mask
    }

    fn hook_mask_string(mask: u8) -> String {
        let mut s = String::new();
        if mask & HOOK_CALL != 0 {
            s.push('c');
        }
        if mask & HOOK_RET != 0 {
            s.push('r');
        }
        if mask & HOOK_LINE != 0 {
            s.push('l');
        }
        s
    }

    /// Handle debug.sethook([thread,] hook, mask [, count]).
    fn handle_debug_sethook(&mut self, args: &[Value]) -> Result<(), LuaError> {
        let (target, base) = match args.first().copied() {
            Some(Value::Object(r)) if r.as_object().as_coroutine().is_some() => (Some(r), 1),
            _ => (None, 0),
        };

        let hook = args.get(base).copied().unwrap_or(Value::Nil);
        let running = self.running_thread();
        let is_current = target.map_or(true, |t| t == running);

        // Allow clearing with no arguments or nil.
        if hook.is_nil() {
            if target.is_some() && !is_current {
                let t = target.unwrap();
                let co = t.as_object_mut().as_coroutine_mut().unwrap();
                co.hook_func = None;
                co.hook_mask = 0;
                co.hook_count = 0;
                co.hook_counter = 0;
            } else {
                self.hook_func = None;
                self.hook_mask = 0;
                self.hook_count = 0;
                self.hook_counter = 0;
            }
            return Ok(());
        }

        let hook_ref = match hook {
            Value::Object(r) if r.as_object().as_closure().is_some() => r,
            _ => {
                return Err(LuaError::new(
                    "bad argument #1 to 'sethook' (function expected)",
                ))
            }
        };

        let mask_bytes = match args.get(base + 1).and_then(|v| v.as_str_bytes()) {
            Some(b) => b.to_vec(),
            None => {
                return Err(LuaError::new(
                    "bad argument #2 to 'sethook' (string expected)",
                ))
            }
        };
        let count = args
            .get(base + 2)
            .and_then(|v| v.as_integer())
            .unwrap_or(0);
        let mask = Self::hook_mask_from(&mask_bytes, count);

        if is_current {
            self.hook_func = Some(hook_ref);
            self.hook_mask = mask;
            self.hook_count = count;
            self.hook_counter = count;
            // Seed the current frame's line state so installing a hook
            // mid-line does not immediately fire a spurious line event.
            if let Some(f) = self.frames.last_mut() {
                let pc = f.pc.saturating_sub(1);
                f.hook_last_pc = pc;
                f.hook_last_line = f.proto.line_info.get(pc).copied().unwrap_or(0);
            }
        } else {
            let t = target.unwrap();
            let co = t.as_object_mut().as_coroutine_mut().unwrap();
            co.hook_func = Some(hook_ref);
            co.hook_mask = mask;
            co.hook_count = count;
            co.hook_counter = count;
        }
        Ok(())
    }

    /// Handle debug.gethook([thread]).
    fn handle_debug_gethook(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let target = match args.first().copied() {
            Some(Value::Object(r)) if r.as_object().as_coroutine().is_some() => Some(r),
            _ => None,
        };
        let is_current = match target {
            None => true,
            Some(t) => t == self.running_thread(),
        };

        let (func, mask, count, counter) = if is_current {
            (self.hook_func, self.hook_mask, self.hook_count, self.hook_counter)
        } else {
            let t = target.unwrap();
            let co = t.as_object().as_coroutine().unwrap();
            (co.hook_func, co.hook_mask, co.hook_count, co.hook_counter)
        };

        match func {
            Some(f) => {
                let mask_str = Self::hook_mask_string(mask);
                let mask_val = Value::Object(self.gc.new_string(mask_str.as_bytes()));
                let count_val = if mask & HOOK_COUNT != 0 {
                    Value::Integer(count)
                } else {
                    Value::Integer(0)
                };
                let _ = counter;
                self.place_results(
                    result_base,
                    num_results,
                    &[Value::Object(f), mask_val, count_val],
                );
            }
            None => {
                self.place_results(result_base, num_results, &[Value::Nil]);
            }
        }
        Ok(())
    }

    /// Handle debug.traceback([message [, level]])
    /// Fire a return hook for a C-level call (which has no stack frame).
    fn fire_return_hook(&mut self, c_name: Option<&str>) -> Result<(), LuaError> {
        if self.hook_mask & HOOK_RET != 0 && !self.in_hook {
            let prev = self.return_hook_c_name.take();
            if let Some(n) = c_name {
                self.return_hook_c_name = Some(n.to_string());
            }
            let r = self.call_hook("return", None);
            self.return_hook_c_name = prev;
            r?;
        }
        Ok(())
    }

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

        // Validate the option string.
        for ch in what.chars() {
            if !matches!(ch, 'S' | 'l' | 'u' | 't' | 'n' | 'r' | 'f' | 'L') {
                return Err(LuaError::new(
                    "bad argument #2 to 'getinfo' (invalid option)",
                ));
            }
        }

        match func_or_level {
            Ok(level) => {
                // A return hook for a C function: level 2 names that function.
                if level == 2 {
                    if let Some(name) = self.return_hook_c_name.clone() {
                        if self.frames.last().map(|f| f.is_hook).unwrap_or(false) {
                            let info =
                                self.build_c_getinfo_table(&name, None, &what);
                            let info_ref = self.gc.new_table(info);
                            self.place_results(
                                result_base,
                                num_results,
                                &[Value::Object(info_ref)],
                            );
                            return Ok(());
                        }
                    }
                }
                // While unwinding a protected call, its frame is not on the
                // stack; present it as level 2 (reference Lua keeps the C
                // `pcall` frame on the stack during closing methods).
                let mut level = level;
                if self.closing_pcall_name.is_some() && level >= 2 {
                    if level == 2 {
                        let name = self.closing_pcall_name.unwrap().to_string();
                        let info = self.build_c_getinfo_table(&name, None, &what);
                        let info_ref = self.gc.new_table(info);
                        self.place_results(
                            result_base,
                            num_results,
                            &[Value::Object(info_ref)],
                        );
                        return Ok(());
                    }
                    level -= 1;
                }
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
                let (name, namewhat) = if frame.is_hook {
                    (Some("?".to_string()), "hook")
                } else {
                    self.function_name_at(fi)
                };
                let istailcall = frame.is_tailcall;
                let extraargs = frame.extraargs;

                let info = self.build_getinfo_table(
                    &proto,
                    Some(closure_ref),
                    Some(pc),
                    fi == 0,
                    &what,
                    name,
                    namewhat,
                    istailcall,
                    extraargs,
                );
                let info_ref = self.gc.new_table(info);
                self.place_results(result_base, num_results, &[Value::Object(info_ref)]);
            }
            Err(closure_ref) => {
                let closure = closure_ref.as_object().as_closure().unwrap();
                match closure {
                    Closure::Lua(lc) => {
                        let info = self.build_getinfo_table(
                            &lc.proto,
                            Some(closure_ref),
                            None,
                            false,
                            &what,
                            None,
                            "",
                            false,
                            0,
                        );
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

    /// Try to determine the name under which the function of frame `fi`
    /// was called, by inspecting the call instruction in the caller.
    fn function_name_at(&self, fi: usize) -> (Option<String>, &'static str) {
        if let Some(mm) = &self.frames[fi].metamethod {
            return (Some(mm.clone()), "metamethod");
        }
        if fi == 0 {
            // While unwinding a protected call, its frame is gone; report
            // the protected call itself as the caller (like reference Lua).
            if let Some(name) = self.closing_pcall_name {
                return (Some(name.to_string()), "global");
            }
            return (None, "");
        }
        if self.frames[fi].called_from_c {
            return (None, "");
        }
        let caller = &self.frames[fi - 1];
        let proto = &caller.proto;
        if caller.pc == 0 {
            return (None, "");
        }
        let call_pc = caller.pc - 1;
        let call_inst = proto.code[call_pc];
        let op = decode_op(call_inst);
        if op != OpCode::Call as u8 && op != OpCode::TailCall as u8 {
            return (None, "");
        }
        let a = decode_a(call_inst);
        if call_pc == 0 {
            return (None, "");
        }

        // If the function register is a local, that is its name.
        if let Some(local) = local_at_reg(proto, a, call_pc as u32) {
            return (Some(local), "local");
        }

        // Find the instruction that produced the function value in register
        // `a`, scanning backwards past argument setup.
        let mut source_inst: Option<(OpCode, u32)> = None;
        let limit = call_pc.saturating_sub(32);
        let mut idx = call_pc;
        while idx > limit {
            idx -= 1;
            let inst = proto.code[idx];
            let op = match OpCode::from_u8(decode_op(inst)) {
                Some(op) => op,
                None => break,
            };
            if decode_a(inst) == a && inst_writes_reg(op) {
                match op {
                    OpCode::GetTabUp
                    | OpCode::GetTable
                    | OpCode::GetUpval
                    | OpCode::Move => {
                        source_inst = Some((op, inst));
                    }
                    _ => {}
                }
                break;
            }
        }

        let (op, inst) = match source_inst {
            Some(pair) => pair,
            None => return (None, ""),
        };
        match op {
            OpCode::GetTabUp => {
                let up = decode_b(inst);
                let k = decode_c(inst);
                if let Some(key) = constant_string(&proto.constants, k as usize) {
                    let what = if proto
                        .upvalues
                        .get(up as usize)
                        .and_then(|u| u.name.as_deref())
                        == Some("_ENV")
                    {
                        "global"
                    } else {
                        "field"
                    };
                    return (Some(key), what);
                }
            }
            OpCode::GetTable => {
                let key_reg = decode_c(inst);
                // Look back for the constant loaded into the key register.
                let start = call_pc.saturating_sub(12);
                for idx in (start..call_pc.saturating_sub(1)).rev() {
                    let inst = proto.code[idx];
                    if decode_op(inst) == OpCode::LoadK as u8 && decode_a(inst) == key_reg {
                        if let Some(key) =
                            constant_string(&proto.constants, decode_bx(inst) as usize)
                        {
                            return (Some(key), "field");
                        }
                        break;
                    }
                }
            }
            OpCode::Move => {
                let src = decode_b(inst);
                if let Some(local) = local_at_reg(proto, src, call_pc as u32) {
                    return (Some(local), "local");
                }
            }
            OpCode::GetUpval => {
                let uv = decode_b(inst);
                if let Some(u) = proto.upvalues.get(uv as usize) {
                    if let Some(name) = &u.name {
                        return (Some(name.clone()), "upvalue");
                    }
                }
            }
            OpCode::Move => {
                let rb = decode_b(inst);
                if let Some(local) = local_at_reg(proto, rb, call_pc as u32) {
                    return (Some(local), "local");
                }
            }
            _ => {}
        }
        (None, "")
    }

    fn build_getinfo_table(
        &mut self,
        proto: &Proto,
        closure_ref: Option<GcRef>,
        pc: Option<usize>,
        is_main: bool,
        what: &str,
        name: Option<String>,
        namewhat: &str,
        istailcall: bool,
        extraargs: u8,
    ) -> Table {
        let mut t = Table::new();

        if what.contains('S') {
            let source = proto.source.as_deref().unwrap_or("=?");
            let source_ref = self.gc.new_string(source.as_bytes());
            let key = self.gc.new_string(b"source");
            t.raw_set(Value::Object(key), Value::Object(source_ref));

            let short_src = chunkid(source);
            let short_src_ref = self.gc.new_string(short_src.as_bytes());
            let key = self.gc.new_string(b"short_src");
            t.raw_set(Value::Object(key), Value::Object(short_src_ref));

            // linedefined / lastlinedefined
            let key = self.gc.new_string(b"linedefined");
            t.raw_set(Value::Object(key), Value::Integer(proto.line_defined as i64));

            let key = self.gc.new_string(b"lastlinedefined");
            t.raw_set(
                Value::Object(key),
                Value::Integer(proto.last_line_defined as i64),
            );

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
            let name_val = match &name {
                Some(n) => {
                    let r = self.gc.new_string(n.as_bytes());
                    Value::Object(r)
                }
                None => Value::Nil,
            };
            let key = self.gc.new_string(b"name");
            t.raw_set(Value::Object(key), name_val);

            let key = self.gc.new_string(b"namewhat");
            let val = self.gc.new_string(namewhat.as_bytes());
            t.raw_set(Value::Object(key), Value::Object(val));
        }

        if what.contains('t') {
            let key = self.gc.new_string(b"istailcall");
            t.raw_set(Value::Object(key), Value::Boolean(istailcall));
            let key = self.gc.new_string(b"extraargs");
            t.raw_set(Value::Object(key), Value::Integer(extraargs as i64));
        }

        if what.contains('f') {
            if let Some(cr) = closure_ref {
                let key = self.gc.new_string(b"func");
                t.raw_set(Value::Object(key), Value::Object(cr));
            }
        }

        if what.contains('L') {
            // activelines: same as reference Lua, skipping the VARARGPREP
            // instruction of vararg functions.
            let mut lines_table = Table::new();
            let start = if proto.is_vararg { 1 } else { 0 };
            for &line in proto.line_info.iter().skip(start) {
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
        // Optional leading thread argument.
        let (target, base) = match args.first().copied() {
            Some(Value::Object(r)) if r.as_object().as_coroutine().is_some() => (Some(r), 1),
            _ => (None, 0),
        };

        // Function form: return parameter (or vararg) names.
        if let Some(Value::Object(r)) = args.get(base).copied() {
            if let Some(Closure::Lua(lc)) = r.as_object().as_closure() {
                let idx = args
                    .get(base + 1)
                    .and_then(|v| v.as_integer())
                    .unwrap_or(0);
                if idx > 0 && idx <= lc.proto.num_params as i64 {
                    if let Some(local) = lc.proto.locals.get(idx as usize - 1) {
                        let name = self.gc.new_string(local.name.as_bytes());
                        self.place_results(result_base, num_results, &[Value::Object(name)]);
                        return Ok(());
                    }
                } else if idx < 0 && lc.proto.is_vararg {
                    let name = self.gc.new_string(b"(vararg)");
                    self.place_results(result_base, num_results, &[Value::Object(name)]);
                    return Ok(());
                }
            }
            self.place_results(result_base, num_results, &[Value::Nil]);
            return Ok(());
        }

        let level = match args.get(base).copied() {
            Some(Value::Integer(lvl)) => lvl,
            _ => {
                self.place_results(result_base, num_results, &[Value::Nil]);
                return Ok(());
            }
        };
        let local_idx = args
            .get(base + 1)
            .and_then(|v| v.as_integer())
            .unwrap_or(0);

        if level <= 0 {
            return Err(LuaError::new(
                "bad argument #1 to 'getlocal' (level out of range)",
            ));
        }

        // Choose the current thread or the (suspended) target coroutine.
        let (frames, stack): (&[CallFrame], &[Value]) = match target {
            Some(t) if t != self.running_thread() => {
                // SAFETY: the coroutine object is kept alive by the VM stack
                // (it is the function argument being inspected).
                let obj: &crate::gc::GcObject =
                    unsafe { &*(t.ptr_value() as *const crate::gc::GcObject) };
                let co = obj.as_coroutine().unwrap();
                (&co.frames, &co.stack)
            }
            _ => (&self.frames, &self.stack),
        };

        if level as usize > frames.len() {
            return Err(LuaError::new(
                "bad argument #1 to 'getlocal' (level out of range)",
            ));
        }
        let frame = &frames[frames.len() - level as usize];
        let pc = if frame.pc > 0 { frame.pc - 1 } else { 0 };

        if local_idx < 0 {
            // Negative indices: vararg arguments.
            let vararg_idx = (-local_idx) as usize - 1;
            if vararg_idx < frame.varargs.len() {
                let name = self.gc.new_string(b"(vararg)");
                let val = frame.varargs[vararg_idx];
                self.place_results(result_base, num_results, &[Value::Object(name), val]);
            } else {
                self.place_results(result_base, num_results, &[Value::Nil]);
            }
            return Ok(());
        }
        if local_idx == 0 {
            self.place_results(result_base, num_results, &[Value::Nil]);
            return Ok(());
        }

        // Find the local active at the current pc.
        let mut active_count = 0usize;
        for local in &frame.proto.locals {
            if pc as u32 >= local.start_pc && (pc as u32) < local.end_pc {
                active_count += 1;
                if active_count == local_idx as usize {
                    let name = self.gc.new_string(local.name.as_bytes());
                    let val = stack
                        .get(frame.base + active_count - 1)
                        .copied()
                        .unwrap_or(Value::Nil);
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
        // Optional leading thread argument.
        let (target, base) = match args.first().copied() {
            Some(Value::Object(r)) if r.as_object().as_coroutine().is_some() => (Some(r), 1),
            _ => (None, 0),
        };

        let level = match args.get(base).copied() {
            Some(Value::Integer(lvl)) => lvl,
            _ => {
                self.place_results(result_base, num_results, &[Value::Nil]);
                return Ok(());
            }
        };
        let local_idx = args
            .get(base + 1)
            .and_then(|v| v.as_integer())
            .unwrap_or(0);
        let value = args.get(base + 2).copied().unwrap_or(Value::Nil);

        if level <= 0 {
            return Err(LuaError::new(
                "bad argument #1 to 'setlocal' (level out of range)",
            ));
        }

        let use_target = match target {
            Some(t) if t != self.running_thread() => true,
            _ => false,
        };

        if local_idx == 0 {
            self.place_results(result_base, num_results, &[Value::Nil]);
            return Ok(());
        }

        // Access the chosen thread's state.
        let (num_frames, fi, frame_base, pc, locals, varargs_len) = if use_target {
            let t = target.unwrap();
            let obj: &crate::gc::GcObject =
                unsafe { &*(t.ptr_value() as *const crate::gc::GcObject) };
            let co = obj.as_coroutine().unwrap();
            if level as usize > co.frames.len() {
                return Err(LuaError::new(
                    "bad argument #1 to 'setlocal' (level out of range)",
                ));
            }
            let fi = co.frames.len() - level as usize;
            let frame = &co.frames[fi];
            let pc = if frame.pc > 0 { frame.pc - 1 } else { 0 };
            (
                co.frames.len(),
                fi,
                frame.base,
                pc,
                frame.proto.locals.clone(),
                frame.varargs.len(),
            )
        } else {
            if level as usize > self.frames.len() {
                return Err(LuaError::new(
                    "bad argument #1 to 'setlocal' (level out of range)",
                ));
            }
            let fi = self.frames.len() - level as usize;
            let frame = &self.frames[fi];
            let pc = if frame.pc > 0 { frame.pc - 1 } else { 0 };
            (
                self.frames.len(),
                fi,
                frame.base,
                pc,
                frame.proto.locals.clone(),
                frame.varargs.len(),
            )
        };
        let _ = (num_frames, fi);

        // Negative indices refer to vararg arguments.
        if local_idx < 0 {
            let vararg_idx = (-local_idx) as usize - 1;
            if vararg_idx < varargs_len {
                if use_target {
                    let t = target.unwrap();
                    let obj = t.ptr_value() as *mut crate::gc::GcObject;
                    let co = unsafe { (*obj).as_coroutine_mut().unwrap() };
                    let idx = co.frames.len() - level as usize;
                    co.frames[idx].varargs[vararg_idx] = value;
                } else {
                    let idx = self.frames.len() - level as usize;
                    self.frames[idx].varargs[vararg_idx] = value;
                }
                let name = self.gc.new_string(b"(vararg)");
                self.place_results(result_base, num_results, &[Value::Object(name)]);
                return Ok(());
            }
            self.place_results(result_base, num_results, &[Value::Nil]);
            return Ok(());
        }

        // Find the local active at the current pc.
        let mut active_count = 0usize;
        for local in &locals {
            if pc as u32 >= local.start_pc && (pc as u32) < local.end_pc {
                active_count += 1;
                if active_count == local_idx as usize {
                    let slot = frame_base + active_count - 1;
                    if use_target {
                        let t = target.unwrap();
                        let obj = t.ptr_value() as *mut crate::gc::GcObject;
                        let co = unsafe { (*obj).as_coroutine_mut().unwrap() };
                        if slot < co.stack.len() {
                            co.stack[slot] = value;
                        }
                    } else {
                        self.stack[slot] = value;
                    }
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
                    Upvalue::Open(loc) => self.read_open_upvalue(*loc),
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
                    Upvalue::Open(loc) => {
                        let loc = *loc;
                        self.write_open_upvalue(loc, value);
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
                // Native closures have implementation-defined upvalues; report
                // a stable identifier for the first one.
                if up_idx == 1 {
                    let ptr = closure_ref.ptr_value() as i64;
                    self.place_results(result_base, num_results, &[Value::Integer(ptr)]);
                } else {
                    self.place_results(result_base, num_results, &[Value::Nil]);
                }
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
        env: Value,
    ) -> Result<GcRef, LuaError> {
        let mut lexer = crate::lexer::Lexer::new(source, chunk_name);
        let tokens = lexer
            .tokenize()
            .map_err(|e| LuaError::new(format!("{e}")).mark_positioned())?;
        let mut parser = crate::parser::Parser::new(tokens);
        let block = parser
            .parse_chunk()
            .map_err(|e| LuaError::new(format!("{e}")).mark_positioned())?;
        let proto = crate::compiler::compile(&block, Some(chunk_name.to_string()))
            .map_err(|e| e.mark_positioned())?;

        let proto_rc = Rc::new(proto);
        let env_upvalue = Rc::new(RefCell::new(Upvalue::Closed(env)));
        let closure = Closure::new_lua(proto_rc, vec![env_upvalue]);
        Ok(self.gc.new_closure(closure))
    }

    /// Skip a UTF-8 BOM and an initial `#` comment line (as `luaL_loadfile`
    /// does). For text chunks the comment line is replaced by a newline so
    /// line numbers still count it; for binary chunks it is dropped.
    fn skip_bom_and_comment(bytes: &[u8]) -> Vec<u8> {
        let mut s = bytes;
        if s.starts_with(b"\xEF\xBB\xBF") {
            s = &s[3..];
        }
        if s.first() == Some(&b'#') {
            let rest = match s.iter().position(|&b| b == b'\n') {
                Some(i) => &s[i + 1..],
                None => &s[s.len()..],
            };
            if rest.first() == Some(&0x1b) {
                return rest.to_vec();
            }
            let mut v = vec![b'\n'];
            v.extend_from_slice(rest);
            return v;
        }
        s.to_vec()
    }

    /// Load chunk bytes as either source text or a precompiled binary
    /// chunk, according to `mode` ("b", "t" or "bt"). Binary chunks
    /// receive fresh upvalues: the first is bound to `env`, the rest
    /// start as nil.
    fn load_chunk(
        &mut self,
        bytes: &[u8],
        chunk_name: &str,
        mode: &str,
        env: Value,
    ) -> Result<GcRef, LuaError> {
        let is_binary = bytes.first() == Some(&0x1b);
        if is_binary && !mode.contains('b') {
            return Err(LuaError::new(format!(
                "attempt to load a binary chunk (mode is '{mode}')"
            )));
        }
        if !is_binary && !mode.contains('t') {
            return Err(LuaError::new(format!(
                "attempt to load a text chunk (mode is '{mode}')"
            )));
        }

        if !is_binary {
            return self.compile_chunk(bytes, chunk_name, env);
        }

        // Binary chunk (PUC-Rio chunks are detected as binary too but
        // rejected by `undump` below).
        let proto = crate::chunk::undump(bytes, chunk_name)
            .map_err(|why| LuaError::new(format!("bad binary format ({why})")))?;

        let num_upvalues = proto.upvalues.len();
        let mut upvalues: Vec<UpvalueRef> = (0..num_upvalues)
            .map(|_| Rc::new(RefCell::new(Upvalue::Closed(Value::Nil))))
            .collect();
        if num_upvalues > 0 {
            upvalues[0] = Rc::new(RefCell::new(Upvalue::Closed(env)));
        }

        let closure = Closure::new_lua(Rc::new(proto), upvalues);
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

        let env = Value::Object(self.globals_ref.expect("globals ref not set"));
        let chunk_name = format!("@{filename}");
        let text = Self::skip_bom_and_comment(&source);
        let closure_ref = self.load_chunk(&text, &chunk_name, "bt", env)?;
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
    /// `chunk` may be a string or a reader function returning pieces.
    fn handle_load(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let chunk = args.first().copied().unwrap_or(Value::Nil);

        let is_reader = matches!(chunk, Value::Object(r) if r.as_object().as_closure().is_some());

        let chunk_bytes = if is_reader {
            // Call the reader repeatedly until it returns nil or an empty
            // string. Reader errors become load failures (nil + message).
            let mut buf: Vec<u8> = Vec::new();
            loop {
                let results = match self.call_value_unyieldable(chunk, &[]) {
                    Ok(r) => r,
                    Err(e) => {
                        let msg = e.to_value(&mut self.gc);
                        let msg = if msg.is_nil() {
                            Value::Object(self.gc.new_string(b"<no error object>"))
                        } else {
                            msg
                        };
                        self.place_results(
                            result_base,
                            num_results,
                            &[Value::Nil, msg],
                        );
                        return Ok(());
                    }
                };
                match results.first().copied().unwrap_or(Value::Nil) {
                    Value::Nil => break,
                    Value::Object(r) if r.as_object().as_string().is_some() => {
                        let piece = r.as_object().as_string().unwrap().as_bytes();
                        if piece.is_empty() {
                            break;
                        }
                        buf.extend_from_slice(piece);
                    }
                    _ => {
                        let err = self
                            .gc
                            .new_string(b"reader function must return a string");
                        self.place_results(
                            result_base,
                            num_results,
                            &[Value::Nil, Value::Object(err)],
                        );
                        return Ok(());
                    }
                }
            }
            buf
        } else {
            match chunk.as_str_bytes() {
                Some(b) => b.to_vec(),
                None => {
                    let err = self
                        .gc
                        .new_string(b"bad argument #1 to 'load' (string expected)");
                    self.place_results(
                        result_base,
                        num_results,
                        &[Value::Nil, Value::Object(err)],
                    );
                    return Ok(());
                }
            }
        };

        let chunkname = args
            .get(1)
            .and_then(|v| v.as_str_bytes())
            .map(|b| String::from_utf8_lossy(b).to_string())
            .unwrap_or_else(|| {
                if is_reader {
                    "=(load)".to_string()
                } else {
                    // Reference Lua uses the chunk itself as the name.
                    String::from_utf8_lossy(&chunk_bytes).to_string()
                }
            });

        let mode = args
            .get(2)
            .and_then(|v| v.as_str_bytes())
            .map(|b| String::from_utf8_lossy(b).to_string())
            .unwrap_or_else(|| "bt".to_string());
        if !mode.bytes().all(|c| c == b'b' || c == b't') {
            return Err(LuaError::new(
                "bad argument #3 to 'load' (invalid mode)",
            ));
        }

        // Any value is valid as the environment (`_ENV` can be anything).
        // An explicitly given nil is used as-is; only an absent argument
        // falls back to the global environment.
        let env = match args.get(3).copied() {
            None => Value::Object(self.globals_ref.expect("globals ref not set")),
            Some(v) => v,
        };

        match self.load_chunk(&chunk_bytes, &chunkname, &mode, env) {
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
        let text = Self::skip_bom_and_comment(&source);

        let mode = args
            .get(1)
            .and_then(|v| v.as_str_bytes())
            .map(|b| String::from_utf8_lossy(b).to_string())
            .unwrap_or_else(|| "bt".to_string());

        let env = match args.get(2).copied() {
            None => Value::Object(self.globals_ref.expect("globals ref not set")),
            Some(v) => v,
        };

        let chunk_name = format!("@{filename}");
        match self.load_chunk(&text, &chunk_name, &mode, env) {
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

        let env = Value::Object(self.globals_ref.expect("globals ref not set"));
        let chunk_name = format!("@{filename}");
        let text = Self::skip_bom_and_comment(&source);
        let closure_ref = self.load_chunk(&text, &chunk_name, "bt", env)?;

        // Run the chunk as a regular call (from this call site) so that it
        // can yield; results are placed at `result_base` when it returns.
        self.ensure_stack(result_base + 1);
        self.stack[result_base] = Value::Object(closure_ref);
        self.call_function(result_base, 1, num_results)?;
        Ok(())
    }

    // ── table.sort (VM-special) ────────────────────────────────────

    /// Compute `#v`, honoring a `__len` metamethod.
    fn value_length(&mut self, v: Value) -> Result<Value, LuaError> {
        let has_mm = match v {
            Value::Object(r) if r.as_object().as_string().is_some() => false,
            _ => self.get_metamethod(v, MM_LEN).is_some(),
        };
        if has_mm {
            let mm = self.get_metamethod(v, MM_LEN).unwrap();
            return self.call_metamethod(mm, &[v, v]);
        }
        match v {
            Value::Object(r) => match &r.as_object().kind {
                GcObjectKind::String(s) => Ok(Value::Integer(s.len() as i64)),
                GcObjectKind::Table(t) => Ok(Value::Integer(t.length() as i64)),
                _ => Err(LuaError::new(format!(
                    "attempt to get length of a {} value",
                    v.type_name()
                ))),
            },
            _ => Err(LuaError::new(format!(
                "attempt to get length of a {} value",
                v.type_name()
            ))),
        }
    }

    /// `table.sort(t [, comp])` — a port of reference Lua's quicksort so
    /// invalid order functions and comparator side effects behave the same.
    fn handle_sort(
        &mut self,
        args: &[Value],
        result_base: usize,
        num_results: i32,
    ) -> Result<(), LuaError> {
        let tv = args.first().copied().unwrap_or(Value::Nil);
        let table_ref = match tv {
            Value::Object(r) if r.as_object().as_table().is_some() => r,
            _ => {
                return Err(LuaError::new(
                    "bad argument #1 to 'table.sort' (table expected)",
                ))
            }
        };

        let len_val = self.value_length(tv)?;
        let n = match len_val {
            Value::Integer(i) => i,
            Value::Float(f) if f == f.floor() => f as i64,
            _ => return Err(LuaError::new("object length is not an integer")),
        };

        if n > 1 {
            if n >= i32::MAX as i64 {
                return Err(LuaError::new(
                    "bad argument #1 to 'table.sort' (array too big)",
                ));
            }
            let comp = args.get(1).copied().unwrap_or(Value::Nil);
            if !comp.is_nil() && !comp.is_function() {
                return Err(LuaError::new(format!(
                    "bad argument #2 to 'table.sort' (function expected, got {})",
                    comp.type_name()
                )));
            }
            self.auxsort(tv, 1, n as u32, 0, comp)?;
        }

        self.place_results(result_base, num_results, &[]);
        Ok(())
    }

    fn sort_get(&mut self, table: Value, i: u32) -> Result<Value, LuaError> {
        self.table_get(table, Value::Integer(i as i64))
    }

    fn sort_set(&mut self, table: Value, i: u32, v: Value) -> Result<(), LuaError> {
        self.table_set(table, Value::Integer(i as i64), v)
    }

    fn sort_comp(&mut self, a: Value, b: Value, comp: Value) -> Result<bool, LuaError> {
        if comp.is_nil() {
            self.compare_lt(a, b)
        } else {
            let res = self.call_value_unyieldable(comp, &[a, b])?;
            Ok(res.first().copied().unwrap_or(Value::Nil).is_truthy())
        }
    }

    fn auxsort(
        &mut self,
        table: Value,
        mut lo: u32,
        mut up: u32,
        mut rnd: u32,
        comp: Value,
    ) -> Result<(), LuaError> {
        while lo < up {
            let a_lo = self.sort_get(table, lo)?;
            let a_up = self.sort_get(table, up)?;
            if self.sort_comp(a_up, a_lo, comp)? {
                self.sort_set(table, lo, a_up)?;
                self.sort_set(table, up, a_lo)?;
            }
            if up - lo == 1 {
                return Ok(());
            }

            let mut p = if up - lo < 100 || rnd == 0 {
                (lo + up) / 2
            } else {
                choose_pivot(lo, up, rnd)
            };

            let a_p = self.sort_get(table, p)?;
            let a_lo = self.sort_get(table, lo)?;
            if self.sort_comp(a_p, a_lo, comp)? {
                self.sort_set(table, p, a_lo)?;
                self.sort_set(table, lo, a_p)?;
            } else {
                let a_up = self.sort_get(table, up)?;
                if self.sort_comp(a_up, a_p, comp)? {
                    self.sort_set(table, p, a_up)?;
                    self.sort_set(table, up, a_p)?;
                }
            }
            if up - lo == 2 {
                return Ok(());
            }

            let pivot = self.sort_get(table, p)?;
            let a_up1 = self.sort_get(table, up - 1)?;
            self.sort_set(table, p, a_up1)?;
            self.sort_set(table, up - 1, pivot)?;

            p = self.partition(table, lo, up, comp, pivot)?;

            let n;
            if p - lo < up - p {
                self.auxsort(table, lo, p - 1, rnd, comp)?;
                n = p - lo;
                lo = p + 1;
            } else {
                self.auxsort(table, p + 1, up, rnd, comp)?;
                n = up - p;
                up = p - 1;
            }
            if (up.wrapping_sub(lo)) / 128 > n {
                rnd = random_u32();
            }
        }
        Ok(())
    }

    fn partition(
        &mut self,
        table: Value,
        lo: u32,
        up: u32,
        comp: Value,
        pivot: Value,
    ) -> Result<u32, LuaError> {
        let mut i = lo;
        let mut j = up - 1;
        loop {
            let ai = loop {
                i += 1;
                let v = self.sort_get(table, i)?;
                if !self.sort_comp(v, pivot, comp)? {
                    break v;
                }
                if i == up - 1 {
                    return Err(LuaError::new("invalid order function for sorting"));
                }
            };
            let aj = loop {
                j -= 1;
                let v = self.sort_get(table, j)?;
                if !self.sort_comp(pivot, v, comp)? {
                    break v;
                }
                if j < i {
                    return Err(LuaError::new("invalid order function for sorting"));
                }
            };
            if j < i {
                self.sort_set(table, up - 1, ai)?;
                self.sort_set(table, i, pivot)?;
                return Ok(i);
            }
            self.sort_set(table, i, aj)?;
            self.sort_set(table, j, ai)?;
        }
    }

    // ── Warning system ─────────────────────────────────────────────
    /// Emit a warning message (already composed). Handles the `@store`
    /// mode that accumulates messages in the `_WARN` global; otherwise
    /// prints `Lua warning: <msg>` to stderr when warnings are on.
    fn warning(&mut self, msg: &str) {
        if self.warn_store {
            let globals = match self.globals_ref {
                Some(g) => g,
                None => return,
            };
            let key = self.gc.new_string(b"_WARN");
            let cur = globals
                .as_object()
                .as_table()
                .map(|t| t.raw_get(&Value::Object(key)))
                .unwrap_or(Value::Nil);
            let mut buf = match cur {
                Value::Object(r) if r.as_object().as_string().is_some() => {
                    r.as_object().as_string().unwrap().as_bytes().to_vec()
                }
                _ => Vec::new(),
            };
            buf.extend_from_slice(msg.as_bytes());
            let s = self.gc.new_string(&buf);
            globals
                .as_object_mut()
                .as_table_mut()
                .unwrap()
                .raw_set(Value::Object(key), Value::Object(s));
            return;
        }
        if self.warn_on {
            eprintln!("Lua warning: {msg}");
        }
    }

    /// Handle `warn(msg1, ...)`. All arguments must be strings (numbers
    /// are accepted and converted). A single argument starting with `@`
    /// is a control message (`@on`, `@off`, `@store`, `@normal`).
    fn handle_warn(&mut self, args: &[Value]) -> Result<(), LuaError> {
        if args.is_empty() {
            return Err(LuaError::new(
                "bad argument #1 to 'warn' (string expected, got no value)",
            ));
        }

        let mut pieces: Vec<String> = Vec::with_capacity(args.len());
        for (i, arg) in args.iter().enumerate() {
            match arg {
                Value::Object(r) if r.as_object().as_string().is_some() => {
                    let s = r.as_object().as_string().unwrap();
                    pieces.push(String::from_utf8_lossy(s.as_bytes()).to_string());
                }
                Value::Integer(n) => pieces.push(format!("{n}")),
                Value::Float(n) => pieces.push(format!("{n}")),
                v => {
                    return Err(LuaError::new(format!(
                        "bad argument #{} to 'warn' (string expected, got {})",
                        i + 1,
                        v.type_name()
                    )));
                }
            }
        }

        if pieces.len() == 1 {
            if let Some(control) = pieces[0].strip_prefix('@') {
                match control {
                    "on" => {
                        self.warn_on = true;
                        self.warn_store = false;
                    }
                    "off" => self.warn_on = false,
                    "store" => self.warn_store = true,
                    "normal" => self.warn_store = false,
                    _ => {} // unknown control messages are ignored
                }
                return Ok(());
            }
        }

        let msg = pieces.concat();
        self.warning(&msg);
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
                // Reference Lua's `count` returns live memory; run a cycle
                // first so garbage created since the last one is not counted.
                self.collect_garbage();
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

    /// Convert a numeric value to `f64` (assumes it is already a number).
    fn as_float(v: Value) -> f64 {
        match v {
            Value::Integer(i) => i as f64,
            Value::Float(f) => f,
            _ => 0.0,
        }
    }

    /// Convert a limit value to an integer for the integer loop, mirroring
    /// reference Lua's `forlimit`. Returns None when the loop must be skipped.
    fn for_limit(
        &self,
        init: i64,
        lim: Value,
        step: i64,
    ) -> Result<Option<i64>, LuaError> {
        let ceil = step < 0;
        let converted = match lim {
            Value::Integer(i) => Some(i),
            Value::Float(f) => {
                if f.is_nan() || f.is_infinite() {
                    None
                } else {
                    let r = if ceil { f.ceil() } else { f.floor() };
                    if r >= -(2f64.powi(63)) && r < 2f64.powi(63) {
                        Some(r as i64)
                    } else {
                        None
                    }
                }
            }
            Value::Object(r) if r.as_object().as_string().is_some() => {
                let s = r.as_object().as_string().unwrap();
                match crate::stdlib::io::parse_lua_number(s.as_bytes()) {
                    Some(Value::Integer(i)) => Some(i),
                    Some(Value::Float(f)) => {
                        let r = if ceil { f.ceil() } else { f.floor() };
                        if r >= -(2f64.powi(63)) && r < 2f64.powi(63) {
                            Some(r as i64)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            _ => None,
        };
        let p = match converted {
            Some(p) => p,
            None => {
                // Not coercible to an integer: try as a float out of bounds.
                let f = Self::coerce_to_number(lim)
                    .ok_or_else(|| LuaError::new("'for' limit must be a number"))?;
                let f = Self::as_float(f);
                if f > 0.0 {
                    if step < 0 {
                        return Ok(None);
                    }
                    i64::MAX
                } else {
                    if step > 0 {
                        return Ok(None);
                    }
                    i64::MIN
                }
            }
        };
        let skip = if step > 0 { init > p } else { init < p };
        Ok(if skip { None } else { Some(p) })
    }

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

/// True when the integer `i` fits in a float exactly (reference Lua's
/// `l_intfitsf` for a 53-bit mantissa).
#[inline]
fn int_fits_float(i: i64) -> bool {
    (i as i128).abs() <= (1i128 << 53)
}

/// Exact `i < f` (reference Lua 5.5 semantics).
fn lt_int_float(i: i64, f: f64) -> bool {
    if int_fits_float(i) {
        (i as f64) < f
    } else if f.is_finite() {
        let c = f.ceil();
        if c >= -(2f64.powi(63)) && c < 2f64.powi(63) {
            i < c as i64
        } else {
            f > 0.0
        }
    } else {
        false
    }
}

/// Exact `i <= f`.
fn le_int_float(i: i64, f: f64) -> bool {
    if int_fits_float(i) {
        (i as f64) <= f
    } else if f.is_finite() {
        let fl = f.floor();
        if fl >= -(2f64.powi(63)) && fl < 2f64.powi(63) {
            i <= fl as i64
        } else {
            f > 0.0
        }
    } else {
        false
    }
}

/// Exact `f < i`.
fn lt_float_int(f: f64, i: i64) -> bool {
    if int_fits_float(i) {
        f < (i as f64)
    } else if f.is_finite() {
        let fl = f.floor();
        if fl >= -(2f64.powi(63)) && fl < 2f64.powi(63) {
            (fl as i64) < i
        } else {
            f < 0.0
        }
    } else {
        false
    }
}

/// Exact `f <= i`.
fn le_float_int(f: f64, i: i64) -> bool {
    if int_fits_float(i) {
        f <= (i as f64)
    } else if f.is_finite() {
        let c = f.ceil();
        if c >= -(2f64.powi(63)) && c < 2f64.powi(63) {
            (c as i64) <= i
        } else {
            f < 0.0
        }
    } else {
        false
    }
}

/// Lua floor division for integers (wrapping, like reference Lua).
fn lua_idiv(a: i64, b: i64) -> i64 {
    // The only overflowing case is MININTEGER // -1, which wraps to
    // MININTEGER in Lua.
    let d = a.wrapping_div(b);
    if (a ^ b) < 0 && d.wrapping_mul(b) != a {
        d.wrapping_sub(1)
    } else {
        d
    }
}

/// Lua modulo for integers (wrapping, like reference Lua).
fn lua_imod(a: i64, b: i64) -> i64 {
    let r = a.wrapping_rem(b);
    if r != 0 && (r ^ b) < 0 {
        r.wrapping_add(b)
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
/// Format a chunk source name the way `luaO_chunkid` does:
///   `@file`  → the file name (truncated with a leading `...`)
///   `=name`  → the literal name (truncated)
///   other    → `[string "..."]` (stopping at the first newline)
pub fn chunkid(source: &str) -> String {
    const IDSIZE: usize = 60;
    let bytes = source.as_bytes();
    // Reference Lua passes the length including the terminating NUL.
    let srclen = bytes.len() + 1;
    match bytes.first() {
        Some(b'=') => {
            if srclen <= IDSIZE {
                source[1..].to_string()
            } else {
                String::from_utf8_lossy(&bytes[1..IDSIZE]).to_string()
            }
        }
        Some(b'@') => {
            if srclen <= IDSIZE {
                source[1..].to_string()
            } else {
                let keep = IDSIZE - 3; // space left after "..."
                let body = &bytes[1..];
                let tail = &body[body.len() - (keep - 1)..];
                format!("...{}", String::from_utf8_lossy(tail))
            }
        }
        _ => {
            const PRE: &str = "[string \"";
            const POS: &str = "\"]";
            const RETS: &str = "...";
            let bufflen = IDSIZE - (PRE.len() + RETS.len() + POS.len()) - 1;
            let nl = source.find('\n');
            if srclen < bufflen && nl.is_none() {
                format!("{PRE}{source}{POS}")
            } else {
                let mut body = match nl {
                    Some(nl) => &source[..nl],
                    None => source,
                };
                if body.len() > bufflen {
                    body = &body[..bufflen];
                }
                format!("{PRE}{body}{RETS}{POS}")
            }
        }
    }
}

/// True if executing `op` may write the A register.
fn inst_writes_reg(op: OpCode) -> bool {
    matches!(
        op,
        OpCode::Move
            | OpCode::LoadI
            | OpCode::LoadK
            | OpCode::LoadKX
            | OpCode::LoadBool
            | OpCode::LoadNil
            | OpCode::GetUpval
            | OpCode::GetTabUp
            | OpCode::GetTable
            | OpCode::NewTable
            | OpCode::Add
            | OpCode::Sub
            | OpCode::Mul
            | OpCode::Div
            | OpCode::IDiv
            | OpCode::Mod
            | OpCode::Pow
            | OpCode::Unm
            | OpCode::BAnd
            | OpCode::BOr
            | OpCode::BXor
            | OpCode::Shl
            | OpCode::Shr
            | OpCode::BNot
            | OpCode::Not
            | OpCode::Concat
            | OpCode::Len
            | OpCode::TestSet
            | OpCode::Closure
            | OpCode::Call
            | OpCode::VarArg
    )
}

/// String constant at `idx`, if it is a string.
fn constant_string(constants: &[Constant], idx: usize) -> Option<String> {
    match constants.get(idx) {
        Some(Constant::String(s)) => Some(String::from_utf8_lossy(s).to_string()),
        _ => None,
    }
}

/// Name of the local that occupies register `reg` at `pc`, if any. Locals
/// map 1:1 to registers in declaration order.
fn local_at_reg(proto: &Proto, reg: u8, pc: u32) -> Option<String> {
    proto
        .locals
        .iter()
        .rev()
        .find(|l| l.reg == reg && l.start_pc <= pc && pc < l.end_pc)
        .map(|l| l.name.clone())
}

fn lua_shr(x: i64, y: i64) -> i64 {
    lua_shl(x, -y)
}

/// Choose a pivot in the middle half of `[lo, up]`, "randomized" by `rnd`
/// (matches reference Lua's `choosePivot`).
fn choose_pivot(lo: u32, up: u32, rnd: u32) -> u32 {
    let r4 = (up - lo) / 4;
    (rnd ^ lo ^ up) % (r4 * 2) + (lo + r4)
}

/// Produce a pseudo-random value used to break up imbalanced partitions.
fn random_u32() -> u32 {
    use std::time::{SystemTime, UNIX_EPOCH};
    let t = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0);
    let mut x = (t ^ (t >> 32)) as u32;
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    x
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

use std::cell::{Cell, RefCell};
use std::collections::hash_map::RandomState;
use std::collections::HashMap;
use std::hash::{BuildHasher, Hasher};
use std::sync::Arc;

use parking_lot::RwLock;

use crate::closure::{Closure, UpValue, UpValueInner};
use crate::gc::{gc_new, Gc, Heap, TraceContext, Traceable};
use crate::table::Table;
use crate::value::{Callable, NativeFunction, StringKey, Value};
use crate::vm::{ByteCodeModule, MAX_REG};
pub(crate) struct Context {
    pub(crate) globals: RwLock<HashMap<StringKey, Value>>,
    pub(crate) cached_string: RwLock<HashMap<String, Gc<String>>>,
}

impl Context {
    pub(crate) fn new_cached_string(&self, s: String) -> Gc<String> {
        let c = self.cached_string.read();
        if let Some(s) = c.get(&s) {
            *s
        } else {
            std::mem::drop(c);
            let mut c = self.cached_string.write();
            let t = gc_new(s.clone());
            c.insert(s, t);
            t
        }
    }
    pub(crate) fn new() -> Self {
        let globals = RwLock::new(HashMap::new());
        let ctx = Self {
            globals,
            cached_string: RwLock::new(HashMap::new()),
        };
        ctx.set_global(
            ctx.new_cached_string("print".into()),
            ctx.new_callable(NativeFunction::new(|_, args| {
                for a in args{
                    print!("{} ", a.to_string());
                }
                println!();
                Ok(Value::nil())
            })),
        );
        ctx
    }
    pub(crate) fn new_module(&self, mut module: ByteCodeModule) -> Gc<Closure> {
        module.string_pool_cache.clear();
        for s in &module.string_pool {
            let t = self.new_cached_string(s.clone());
            module.string_pool_cache.push(Value::String(t));
        }
        let module = Arc::new(module);
        let entry = module.functions.len() - 1;
        let n_upvalues = module.functions.last().unwrap().upvalues.len();
        gc_new(Closure {
            module,
            entry,
            is_method: false,
            stack_frame_size: MAX_REG,
            upvalues: (0..n_upvalues)
                .map(|_| UpValue {
                    inner: RefCell::new(Arc::new(Cell::new(UpValueInner::Empty))),
                })
                .collect(),
        })
    }
    pub(crate) fn new_closure(&self, c: Closure) -> Value {
        Value::Closure(gc_new(c))
    }
    pub(crate) fn new_table(&self, t: Table) -> Value {
        Value::Table(gc_new(RwLock::new(t)))
    }
    pub(crate) fn new_string(&self, s: String) -> Value {
        Value::String(gc_new(s))
    }
    pub(crate) fn new_callable<T: Callable + 'static>(&self, callable: T) -> Value {
        let p: Box<dyn Callable> = Box::new(callable);
        Value::Callable(gc_new(p))
    }
    pub(crate) fn set_global(&self, name: Gc<String>, v: Value) {
        let mut globals = self.globals.write();
        globals.insert(StringKey(name), v);
    }
    pub(crate) fn get_global(&self, name: Gc<String>) -> Option<Value> {
        let globals = self.globals.read();
        globals.get(&StringKey(name)).map(|x| *x)
    }
}
unsafe impl Sync for Context {}
unsafe impl Send for Context {}
unsafe impl Traceable for Context {
    fn trace(&self, ctx: &mut TraceContext) {
        let globals = self.globals.read();
        for (k, v) in globals.iter() {
            ctx.trace_ptr(k.0);
            ctx.trace(v);
        }
    }
}

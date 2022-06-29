use crate::{
    closure::Closure,
    gc::{Gc, TraceContext, Traceable},
    util::FixedVec,
    value::Value,
};

pub(crate) struct StackFrame {
    pub(crate) start: usize,
    pub(crate) end: usize,
    pub(crate) ip: usize,
    pub(crate) closure: Gc<Closure>,
    pub(crate) ret: Value,
    pub(crate) ret_reg: Option<u8>,
    pub(crate) self_: Value,
}

pub(crate) struct Thread {
    pub(crate) locals: FixedVec<Value>,
    pub(crate) stack_frames: Vec<StackFrame>,
    pub(crate) finished: bool,
}
impl Thread {
    pub(crate) fn pop_stack_frame(&mut self) -> StackFrame {
        let f = self.stack_frames.pop().unwrap();
        if self.stack_frames.is_empty() {
            self.finished = true;
        }
        f
    }
    pub(crate) fn create_stack_frame(&mut self, closure: Gc<Closure>) {
        let frame = StackFrame {
            ret: Value::nil(),
            start: self.locals.len(),
            end: self.locals.len() + closure.stack_frame_size,
            ip: 0,
            closure,
            ret_reg: None,
            self_: Value::Nil,
        };
        self.locals
            .resize(self.locals.len() + closure.stack_frame_size, Value::nil()).unwrap();
        self.stack_frames.push(frame);
    }
    pub(crate) fn new(closure: Gc<Closure>, args: &[Value]) -> Self {
        let mut thread = Thread {
            locals: FixedVec::new(256 * 128),
            stack_frames: vec![],
            finished: false,
        };
        thread.create_stack_frame(closure);
        thread.locals_mut(0, args.len()).copy_from_slice(args);
        thread
    }
    pub(crate) fn top_frame_mut(&mut self) -> &mut StackFrame {
        self.stack_frames.last_mut().unwrap()
    }
    pub(crate) fn top_frame(&self) -> &StackFrame {
        self.stack_frames.last().unwrap()
    }
    pub(crate) fn local_mut(&mut self, i: usize) -> &mut Value {
        let base = self.top_frame_mut().start;
        &mut self.locals[base + i]
    }
    pub(crate) fn local(&mut self, i: usize) -> Value {
        let base = self.top_frame().start;
        self.locals[base + i]
    }
    pub(crate) fn locals(&self, start: usize, end: usize) -> &[Value] {
        let base = self.top_frame().start;
        &self.locals[base + start..base + end]
    }
    pub(crate) fn locals_mut(&mut self, start: usize, end: usize) -> &mut [Value] {
        let base = self.top_frame().start;
        &mut self.locals[base + start..base + end]
    }
}
unsafe impl Traceable for Thread {
    fn trace(&self, ctx: &mut TraceContext) {
        for i in 0..self.locals.len() {
            ctx.trace(&self.locals[i]);
        }
        for f in &self.stack_frames {
            ctx.trace_ptr(f.closure);
        }
    }
}

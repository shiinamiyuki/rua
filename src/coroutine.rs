//! Coroutine (thread) implementation.

use crate::closure::UpvalueRef;
use crate::gc::GcRef;
use crate::value::Value;
use crate::vm::{CallFrame, PcallGuard};

/// Coroutine status (maps to Lua's coroutine.status strings).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoroutineStatus {
    /// Has not started or yielded (may be resumed).
    Suspended,
    /// Currently executing.
    Running,
    /// Has resumed another coroutine (waiting for it to yield/return).
    Normal,
    /// Finished (returned or errored), cannot be resumed.
    Dead,
}

impl CoroutineStatus {
    pub fn as_str(&self) -> &'static str {
        match self {
            CoroutineStatus::Suspended => "suspended",
            CoroutineStatus::Running => "running",
            CoroutineStatus::Normal => "normal",
            CoroutineStatus::Dead => "dead",
        }
    }
}

/// A Lua coroutine (thread): independent execution state.
pub struct Coroutine {
    pub status: CoroutineStatus,
    /// Register stack (swapped with Vm on resume/yield).
    pub stack: Vec<Value>,
    /// Call frames (swapped with Vm on resume/yield).
    pub frames: Vec<CallFrame>,
    /// Open upvalues (swapped with Vm on resume/yield).
    pub open_upvalues: Vec<UpvalueRef>,
    /// To-be-closed variable slots (swapped with Vm on resume/yield).
    pub tbc_slots: Vec<usize>,
    /// Stack top marker (swapped with Vm on resume/yield).
    pub top: usize,
    /// The body function (consumed on first resume).
    pub body: Option<GcRef>,
    /// Where resume should place yield results (saved on yield).
    pub yield_result_base: usize,
    /// How many results the resume caller expects (saved on yield).
    pub yield_num_results: i32,
    /// Whether this is the main thread.
    pub is_main: bool,
    /// Pcall/xpcall guards (swapped with Vm on resume/yield).
    pub pcall_guards: Vec<PcallGuard>,
}

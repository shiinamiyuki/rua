use crate::{
    gc::Traceable,
    runtime::{GcValue, RuntimeError, ValueRef},
    value::UserData,
};

pub trait BaseApi {
    fn create_number<'a>(&'a self, x: f64) -> ValueRef<'a>;
    fn create_bool<'a>(&self, x: bool) -> ValueRef<'a>;
    fn create_userdata<'a, T: UserData + Traceable>(&self, userdata: T) -> ValueRef<'a>;
    fn create_string<'a>(&self, s: String) -> ValueRef<'a>;
    fn upgrade<'a>(&'a self, v: ValueRef<'_>) -> GcValue;
}

pub trait StateApi: BaseApi {
    fn table_set<'a>(
        &'a self,
        table: ValueRef<'a>,
        key: ValueRef<'a>,
        value: ValueRef<'a>,
    ) -> Result<(), RuntimeError>;
    fn table_get<'a>(
        &'a self,
        table: ValueRef<'a>,
        key: ValueRef<'a>,
    ) -> Result<ValueRef<'a>, RuntimeError>;
    // fn add<'a>(&'a self, a: ValueRef<'a>, b: ValueRef<'a>) -> Result<ValueRef<'a>, RuntimeError>;
    
}

pub trait CallApi: StateApi {
    fn arg_count(&self) -> usize;
    fn arg_or_nil<'a>(&'a self, i: usize) -> ValueRef<'a>;
    fn arg<'a>(&'a self, i: usize) -> Result<ValueRef<'a>, RuntimeError>;
    fn ret<'a>(&'a self, i: usize, value: ValueRef<'a>);
    fn call<'a>(
        &self,
        closure: ValueRef<'a>,
        args: &[ValueRef<'a>],
    ) -> Result<ValueRef<'a>, RuntimeError>;
}

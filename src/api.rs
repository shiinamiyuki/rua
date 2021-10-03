use crate::{
    closure::Callable,
    gc::Traceable,
    runtime::{GcValue, RuntimeError, Value},
    value::UserData,
};

pub trait BaseApi {
    fn create_number<'a>(&'a self, x: f64) -> Value<'a>;
    fn create_bool<'a>(&self, x: bool) -> Value<'a>;
    fn create_userdata<'a, T: UserData + Traceable>(&self, userdata: T) -> Value<'a>;
    fn create_string<'a>(&self, s: String) -> Value<'a>;
    fn create_closure<'a>(&self, closure: Box<dyn Callable>) -> Value<'a>;
    fn create_table<'a>(&self) -> Value<'a>;
    fn set_metatable<'a>(&self, v: Value<'a>, mt: Value<'a>);
    fn get_metatable<'a>(&self, v: Value<'a>) -> Value<'a>;
    fn get_global_env<'a>(&self) -> Value<'a>;
    fn table_rawset<'a>(
        &'a self,
        table: Value<'a>,
        key: Value<'a>,
        value: Value<'a>,
    ) -> Result<(), RuntimeError>;
    fn table_rawget<'a>(
        &'a self,
        table: Value<'a>,
        key: Value<'a>,
    ) -> Result<Value<'a>, RuntimeError>;
    fn upgrade<'a>(&'a self, v: Value<'_>) -> GcValue;
}

pub trait StateApi: BaseApi {
    fn table_set<'a>(
        &'a self,
        table: Value<'a>,
        key: Value<'a>,
        value: Value<'a>,
    ) -> Result<(), RuntimeError>;
    fn table_get<'a>(
        &'a self,
        table: Value<'a>,
        key: Value<'a>,
    ) -> Result<Value<'a>, RuntimeError>;
    // fn add<'a>(&'a self, a: ValueRef<'a>, b: ValueRef<'a>) -> Result<ValueRef<'a>, RuntimeError>;
}

pub trait CallApi: StateApi {
    fn arg_count(&self) -> usize;
    fn arg_or_nil<'a>(&'a self, i: usize) -> Value<'a>;
    fn arg<'a>(&'a self, i: usize) -> Result<Value<'a>, RuntimeError>;
    fn ret<'a>(&'a self, i: usize, value: Value<'a>);
    fn call<'a>(
        &self,
        closure: Value<'a>,
        args: &[Value<'a>],
    ) -> Result<Value<'a>, RuntimeError>;
}

use std::cell::RefCell;

use nalgebra as na;

use crate::{
    api::{BaseApi, CallApi, StateApi},
    runtime::{Runtime, ValueRef},
    state::CallContext,
    value::{Managed, ManagedCell, UserData},
};

pub(crate) fn add_na_lib(runtime: &Runtime) {
    let mut m = runtime.create_module();
    let instance = runtime.create_instance();
    // let dmatrix = instance.create_table();
    // instance.table_rawset(dmatrix, key, value)
    {
        let mut dmatrix = runtime.create_module();
        let mt = dmatrix.module;
        let new = move |ctx: &CallContext<'_>, m| {
            let object = ManagedCell::new(RefCell::new(m));
            object.set_metatable(ValueRef::new(mt));
            ctx.create_userdata(object)
        };
        instance
            .table_rawset(
                ValueRef::new(mt),
                instance.create_string("__index".into()),
                ValueRef::new(mt),
            )
            .unwrap();
        dmatrix.function("__tostring".into(), move |ctx| {
            let m = ctx.arg(0)?;
            let m = m.cast::<RefCell<na::DMatrix<f64>>>()?;
            ctx.ret(0, ctx.create_string(format!("{}", m.borrow())));
            Ok(())
        });
        dmatrix.function("__add".into(), move |ctx| {
            let m1 = ctx.arg(0)?.cast::<RefCell<na::DMatrix<f64>>>()?;
            let m1 = m1.borrow();
            let m2 = ctx.arg(1)?.cast::<RefCell<na::DMatrix<f64>>>()?;
            let m2 = m2.borrow();
            ctx.ret(0, new(ctx, &*m1 + &*m2));
            Ok(())
        });
        dmatrix.function("new".into(), move |ctx| {
            let m: na::DMatrix<f64> = {
                if ctx.arg_count() == 0 {
                    na::DMatrix::from_element(0, 0, 0.0)
                } else {
                    let nrows = *ctx.arg(0)?.cast::<f64>()? as usize;
                    let ncols = *ctx.arg(1)?.cast::<f64>()? as usize;
                    if ctx.arg_count() == 2 {
                        na::DMatrix::from_element(nrows, ncols, 0.0)
                    } else {
                        let data = ctx.arg(2)?;
                        if let Some(x) = data.as_f64() {
                            na::DMatrix::from_element(nrows, ncols, *x)
                        } else {
                            na::DMatrix::from_element(nrows, ncols, 0.0)
                        }
                    }
                }
            };
            let object = ManagedCell::new(RefCell::new(m));
            object.set_metatable(ValueRef::new(mt));
            ctx.ret(0, ctx.create_userdata(object));
            Ok(())
        });
        m.submodule("DMatrix".into(), dmatrix);
    }
    runtime.add_module("nalgebra".into(), m);
}

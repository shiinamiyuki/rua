use std::cell::RefCell;
use std::fs::{File, OpenOptions};
use std::rc::Rc;

use crate::runtime::{self, Runtime};
use crate::runtime::{ErrorKind, RuntimeError};
use crate::value::{Managed, Value};

pub(crate) fn add_math_lib(runtime: &Runtime) {
    let mut math = runtime.create_module();
    macro_rules! unary_func {
        ($func:ident) => {{
            math.function(stringify!($func).into(), |ctx| {
                let x:f64 = ctx.arg(0)?.cast::<f64>()?;
                // println!("math.{}({})",stringify!($func), x);
                ctx.ret(0, ctx.create_number(x.$func()));
                Ok(())
            });
        }};
        ($name:literal, $func:ident) => {{
            math.function($name.into(), |ctx| {
                let x:f64 = ctx.arg(0)?.cast::<f64>()?;
                ctx.ret(0, ctx.create_number(x.$func()));
                Ok(())
            });
        }};
    }
    unary_func!(sqrt);
    unary_func!(sin);
    unary_func!(cos);
    unary_func!(tan);
    unary_func!(floor);
    unary_func!(ceil);
    unary_func!(atan);
    unary_func!(asin);
    unary_func!(acos);
    unary_func!(exp);
    unary_func!("log", ln);
    unary_func!(log2);
    unary_func!(log10);
    runtime.add_module("math".into(), math);
}

// struct IOContext {
//     file: Option<std::fs::File>,
// }
// struct FileHandle{
//     file:std::fs::File,
// }
// pub(crate) fn add_io_lib(runtime: &Runtime) {
//     let mut io = runtime.create_module();
//     let ctx = Rc::new(RefCell::new(IOContext { file: None }));
//     type FileHandle = Managed<File>;
//     io.function("open".into(), |ctx| {
//         let filename = arg!(ctx, 0, String);
//         let mut options = OpenOptions::new();

//         let file = if ctx.get_arg_count() > 1 {
//             let mode = arg!(ctx, 1, String);
//             if mode == "r" {
//                 options.read(true);
//             }
//             if mode == "w" {
//                 options.write(true);
//             }
//             if mode == "a" {
//                 options.append(true);
//             }
//             if mode == "r+" {
//                 options.read(true).write(true);
//             }
//             if mode == "w+" {
//                 options.write(true).truncate(true);
//             }
//             match options.open(filename) {
//                 Err(e) => {
//                     return Err(RuntimeError {
//                         kind: ErrorKind::ExternalError,
//                         msg: format!("{}", e),
//                     });
//                 }
//                 Ok(f) => f,
//             }
//         } else {
//             match std::fs::File::open(filename) {
//                 Err(e) => {
//                     return Err(RuntimeError {
//                         kind: ErrorKind::ExternalError,
//                         msg: format!("{}", e),
//                     });
//                 }
//                 Ok(f) => f,
//             }
//         };
//         ctx.ret(0, ctx.state.create_userdata(Managed::new(Some(file))));
//         Ok(())
//     });
//     runtime.add_module("io".into(), io);
// }

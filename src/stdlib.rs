use std::cell::RefCell;
use std::fs::{File, OpenOptions};
use std::io::{BufReader, BufWriter};
use std::rc::Rc;

use crate::runtime::{ErrorKind, Runtime, RuntimeError};
use crate::api::*;



// macro_rules! bind_function {
//     ($func:ident, fn($arg0:ty)->$ret:ty) => {
//        |ctx|{
//             let a0= ctx.arg(0)?;
//             let a0 =cast!(a0,$arg0);

//        }
//     };
// }
// macro_rules! cast {
//     ($x:expr,&$t:ty) => {
//         x.cast::<$t>()?
//     };
//     ($x:expr,$t:ty) => {
//         x.cast::<$t>()?.clone()
//     };
// }

pub(crate) fn add_math_lib(runtime: &Runtime) {
    let mut math = runtime.create_module();
    macro_rules! unary_func {
        ($func:ident) => {{
            math.function(stringify!($func).into(), |ctx| {
                let x: f64 = *ctx.arg(0)?.cast::<f64>()?;
                // println!("math.{}({})",stringify!($func), x);
                ctx.ret(0, ctx.create_number(x.$func()));
                Ok(())
            });
        }};
        ($name:literal, $func:ident) => {{
            math.function($name.into(), |ctx| {
                let x: f64 = *ctx.arg(0)?.cast::<f64>()?;
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
    unary_func!("deg", to_degrees);
    unary_func!("rad", to_radians);
    math.function("max".into(), |ctx| {
        let x: f64 = *ctx.arg(0)?.cast::<f64>()?;
        let y: f64 = *ctx.arg(1)?.cast::<f64>()?;
        ctx.ret(0, ctx.create_number(x.max(y)));
        Ok(())
    });
    math.function("min".into(), |ctx| {
        let x: f64 = *ctx.arg(0)?.cast::<f64>()?;
        let y: f64 = *ctx.arg(1)?.cast::<f64>()?;
        ctx.ret(0, ctx.create_number(x.min(y)));
        Ok(())
    });
    runtime.add_module("math".into(), math);
}
// type FileHandle = Rc<RefCell<FileRecord>>;
// struct IOContext {
//     input: Option<FileHandle>,
//     output: Option<FileHandle>,
// }
// struct FileRecord {
//     file: Option<std::fs::File>,
//     reader: Option<std::io::BufReader<std::fs::File>>,
//     writer: Option<std::io::BufWriter<std::fs::File>>,
// }
// impl FileRecord {
//     fn is_read(&mut self) -> bool {
//         self.reader.is_some()
//     }
//     fn is_write(&mut self) -> bool {
//         self.writer.is_some()
//     }
//     fn to_read(&mut self) {
//         let file = std::mem::replace(&mut self.file, None).unwrap();
//         self.reader = Some(BufReader::new(file));
//     }
//     fn to_write(&mut self) {
//         let file = std::mem::replace(&mut self.file, None).unwrap();
//         self.writer = Some(BufWriter::new(file));
//     }
// }
// pub(crate) fn add_io_lib(runtime: &Runtime) {
//     let mut io = runtime.create_module();
//     let io_ctx = Rc::new(RefCell::new(IOContext {
//         input: None,
//         output: None,
//     }));

//     {
//         io.function("open".into(), move |ctx| {
//             let filename = ctx.arg(0)?;
//             let filename = filename.cast_ref::<String>()?;
//             let mut options = OpenOptions::new();

//             let file = if ctx.get_arg_count() > 1 {
//                 let mode = ctx.arg(1)?;
//                 let mode = mode.cast_ref::<String>()?;
//                 if mode == "r" {
//                     options.read(true);
//                 } else if mode == "w" {
//                     options.write(true).truncate(true);
//                 } else if mode == "a" {
//                     options.append(true);
//                 } else if mode == "r+" {
//                     // options.read(true).write(true);
//                     return Err(RuntimeError {
//                         kind: ErrorKind::ExternalError,
//                         msg: "r+ is not suppored".into(),
//                     });
//                 } else if mode == "w+" {
//                     // options.write(true).truncate(true);
//                     return Err(RuntimeError {
//                         kind: ErrorKind::ExternalError,
//                         msg: "w+ is not suppored".into(),
//                     });
//                 } else {
//                     return Err(RuntimeError {
//                         kind: ErrorKind::ExternalError,
//                         msg: format!("unrecognized mode {}", mode),
//                     });
//                 }
//                 match options.open(filename) {
//                     Err(e) => {
//                         return Err(RuntimeError {
//                             kind: ErrorKind::ExternalError,
//                             msg: format!("{}", e),
//                         });
//                     }
//                     Ok(f) => f,
//                 }
//             } else {
//                 match std::fs::File::open(filename) {
//                     Err(e) => {
//                         return Err(RuntimeError {
//                             kind: ErrorKind::ExternalError,
//                             msg: format!("{}", e),
//                         });
//                     }
//                     Ok(f) => f,
//                 }
//             };
//             ctx.ret(
//                 0,
//                 ctx.create_userdata(Managed::<FileHandle>::new(Rc::new(RefCell::new(
//                     FileRecord {
//                         file: Some(file),
//                         reader: None,
//                         writer: None,
//                     },
//                 )))),
//             );
//             Ok(())
//         });
//     }
//     {
//         let io_ctx = io_ctx.clone();
//         io.function("input".into(), move |ctx| {
//             let arg0 = ctx.arg(0)?;
//             let handle = arg0.cast_ref::<FileHandle>()?;
//             {
//                 let mut handle = handle.borrow_mut();
//                 if handle.is_write() {
//                     return ctx.error("file is set to write".into());
//                 }
//                 if !handle.is_read() {
//                     handle.to_read();
//                 }
//             }
//             let mut io_ctx = io_ctx.borrow_mut();
//             io_ctx.input = Some(handle.clone());
//             Ok(())
//         });
//     }
//     {
//         let io_ctx = io_ctx.clone();
//         io.function("output".into(), move |ctx| {
//             let arg0 = ctx.arg(0)?;
//             if arg0.is_nil(){
//                 let 
//             }
//             let handle = arg0.cast_ref::<FileHandle>()?;
//             {
//                 let mut handle = handle.borrow_mut();
//                 if handle.is_read() {
//                     return ctx.error("file is set to read".into());
//                 }
//                 if !handle.is_write() {
//                     handle.to_write();
//                 }
//             }
//             let mut io_ctx = io_ctx.borrow_mut();
//             io_ctx.output = Some(handle.clone());
//             Ok(())
//         });
//     }
//     {
//         let io_ctx = io_ctx.clone();
//         io.function("read".into(), move |ctx| {
//             let mut io_ctx = io_ctx.borrow_mut();
//             if io_ctx.input.is_none() {
//                 let s = 
//             }
//             let mut input = io_ctx.input.
//             Ok(())
//         });
//     }
//     runtime.add_module("io".into(), io);
// }

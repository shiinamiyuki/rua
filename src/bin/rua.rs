use std::process::exit;

use rua::{
    compile::{compile, compile_repl},
    parse::{self, parse_impl, tokenize},
    runtime::Runtime,
    vm::Instance,
};

fn exec_file(path: &str) {
    let show_ast = std::env::var("PRINT_AST").map_or(false, |x| x == "1");
    let show_module = std::env::var("PRINT_BYTECODE").map_or(false, |x| x == "1");
    let src = std::fs::read_to_string("test.lua").unwrap();
    let tokens = tokenize("test.lua", &src);
    let tokens = match tokens {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!(
                "error: {}\n  --> {}:{}:{}",
                err.msg, *err.loc.file, err.loc.line, err.loc.col
            );
            exit(1);
        }
    };

    let expr = parse_impl(tokens);
    let expr = match expr {
        Ok(expr) => expr,
        Err(err) => {
            eprintln!(
                "error: {}\n  --> {}:{}:{}",
                err.msg, *err.loc.file, err.loc.line, err.loc.col
            );
            exit(1);
        }
    };
    if show_ast {
        println!("{:#?}", expr);
    }

    let module = compile(expr).unwrap();
    if show_module {
        println!("{:#?}", module);
    }
    let mut runtime = Runtime::new();

    let instance = runtime.create_instance();
    match instance.exec(module) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("error: {}", e.msg)
        }
    }
}
fn repl(runtime: &Runtime, instance: &Instance) -> bool {
    use std::io::{self, Write};
    let mut src = String::new();
    let mut last_empty = false;
    loop {
        let mut line = String::new();
        if src.is_empty() {
            print!("> ");
        } else {
            print!(">> ");
        }
        io::stdout().flush().unwrap();
        match std::io::stdin().read_line(&mut line) {
            Ok(_) => {}
            Err(e) => {
                eprintln!("stdin: {}", e);
            }
        }
        if line.is_empty() {
            if last_empty {
                return false;
            } else {
                last_empty = true;
            }
        } else {
            last_empty = false;
        }
        src.push_str(&line);
        let tokens = match tokenize("stdin", &src) {
            Ok(x) => x,
            Err(e) => {
                eprintln!("error: {}", e.msg);
                return false;
            }
        };
        let expr = match parse_impl(tokens) {
            Ok(x) => x,
            Err(e) => {
                if e.kind == parse::ErrorKind::UnexpectedEOF {
                    continue;
                }
                eprintln!("error: {}", e.msg);
                return false;
            }
        };
        let module = compile_repl(expr).unwrap();

        match instance.eval_repl(module) {
            Ok(_) => {
                return true;
            }
            Err(e) => {
                eprintln!("error: {}", e.msg);
                return false;
            }
        }
    }
}
fn main() {
    let args: Vec<_> = std::env::args().collect();
    if args.len() == 1 {
        let mut runtime = Runtime::new();
        let instance = runtime.create_instance();
        loop {
            repl(&mut runtime, &instance);
        }
    } else {
        let filename = &args[1];
        exec_file(&filename);
    }
}

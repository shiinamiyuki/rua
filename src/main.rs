//! Rua CLI: Lua 5.5 interpreter.

use std::env;
use std::fs;
use std::process;

use rua::compiler;
use rua::lexer::Lexer;
use rua::parser::Parser;
use rua::vm::Vm;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: rua <script.lua>");
        eprintln!("       rua -e \"<code>\"");
        process::exit(1);
    }

    let (source, name) = if args[1] == "-e" {
        if args.len() < 3 {
            eprintln!("Usage: rua -e \"<code>\"");
            process::exit(1);
        }
        (args[2].clone(), "<string>".to_string())
    } else {
        let filename = &args[1];
        match fs::read_to_string(filename) {
            Ok(s) => (s, filename.clone()),
            Err(e) => {
                eprintln!("rua: cannot open {filename}: {e}");
                process::exit(1);
            }
        }
    };

    // Lex
    let mut lexer = Lexer::new(source.as_bytes(), &name);
    let tokens = match lexer.tokenize() {
        Ok(t) => t,
        Err(e) => {
            eprintln!("rua: {e}");
            process::exit(1);
        }
    };

    // Parse
    let mut parser = Parser::new(tokens);
    let block = match parser.parse_chunk() {
        Ok(b) => b,
        Err(e) => {
            eprintln!("rua: {e}");
            process::exit(1);
        }
    };

    // Compile
    let proto = match compiler::compile(&block, Some(name)) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("rua: {e}");
            process::exit(1);
        }
    };

    // Execute
    let mut vm = Vm::new();
    if let Err(e) = vm.execute_main(proto) {
        eprintln!("rua: {e}");
        process::exit(1);
    }
}

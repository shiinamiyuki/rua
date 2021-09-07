use std::process::exit;

use rua::{compile::compile, parse::{parse_impl, tokenize}};

fn main(){
    let src = std::fs::read_to_string("test.lua").unwrap();
    let tokens = tokenize("test.lua", &src);
    let tokens = match tokens {
        Ok(tokens)=>{
            tokens
        }
        Err(err)=>{
            eprintln!("error: {}\n  --> {}:{}:{}", err.msg, *err.loc.file, err.loc.line, err.loc.col);
            exit(1);
        }
    };
    for tok in &tokens{
        println!("{:?}", tok);
    }
    let expr = parse_impl(tokens);
    let expr = match expr {
        Ok(expr)=>{
            expr
        }
        Err(err)=>{
            eprintln!("error: {}\n  --> {}:{}:{}", err.msg, *err.loc.file, err.loc.line, err.loc.col);
            exit(1);
        }
    };
    println!("{:#?}", expr);
    let module = compile(expr).unwrap();
    println!("{:#?}", module);
}
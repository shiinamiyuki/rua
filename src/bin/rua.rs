use rua::parse::tokenize;

fn main(){
    let src = std::fs::read_to_string("test.lua").unwrap();
    let tokens = tokenize("test.lua", &src);
    match tokens {
        Ok(tokens)=>{
            for tok in tokens{
                println!("{:?}", tok);
            }
        }
        Err(err)=>{
            eprintln!("error: {}\n  --> {}:{}:{}", err.msg, *err.loc.file, err.loc.line, err.loc.col);
        }
    }
}
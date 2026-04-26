//! Integration test: parse all upstream Lua test files without errors.
//!
//! This verifies that our parser can parse every file in the official
//! Lua test suite without panicking or returning an error.

use rua::lexer::Lexer;
use rua::parser::Parser;
use std::fs;
use std::path::Path;

fn parse_file(path: &Path) -> Result<(), String> {
    let source =
        fs::read(path).map_err(|e| format!("failed to read {}: {}", path.display(), e))?;

    // Skip the shebang line if present
    let source = if source.starts_with(b"#") {
        match source.iter().position(|&b| b == b'\n') {
            Some(pos) => &source[pos..],
            None => &source[..],
        }
    } else {
        &source[..]
    };

    let mut lexer = Lexer::new(source, path.to_string_lossy().to_string());
    let tokens = lexer
        .tokenize()
        .map_err(|e| format!("{}: lex error: {}", path.display(), e))?;

    let mut parser = Parser::new(tokens);
    parser
        .parse_chunk()
        .map_err(|e| format!("{}: parse error: {}", path.display(), e))?;

    Ok(())
}

#[test]
fn parse_all_upstream_lua_tests() {
    let test_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/lua-upstream-tests");
    assert!(
        test_dir.exists(),
        "upstream test directory not found: {}",
        test_dir.display()
    );

    let mut files_tested = 0;
    let mut errors = Vec::new();

    for entry in fs::read_dir(&test_dir).expect("failed to read test directory") {
        let entry = entry.expect("failed to read directory entry");
        let path = entry.path();

        if path.extension().and_then(|e| e.to_str()) != Some("lua") {
            continue;
        }

        files_tested += 1;
        if let Err(e) = parse_file(&path) {
            errors.push(e);
        }
    }

    assert!(files_tested > 0, "no .lua files found in test directory");

    if !errors.is_empty() {
        let msg = format!(
            "{} of {} files failed to parse:\n{}",
            errors.len(),
            files_tested,
            errors.join("\n")
        );
        panic!("{}", msg);
    }

    eprintln!(
        "Successfully parsed {} upstream Lua test files",
        files_tested
    );
}

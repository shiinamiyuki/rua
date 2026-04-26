//! Integration test: lex all upstream Lua test files without errors.
//!
//! This verifies that our lexer can tokenize every file in the official
//! Lua test suite without panicking or returning an error.

use rua::lexer::Lexer;
use std::fs;
use std::path::Path;

fn lex_file(path: &Path) -> Result<usize, String> {
    let source = fs::read(path).map_err(|e| format!("failed to read {}: {}", path.display(), e))?;

    // Skip the shebang line if present (some Lua files may have it)
    let source = if source.starts_with(b"#") {
        // Skip to end of first line
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
        .map_err(|e| format!("{}: {}", path.display(), e))?;
    Ok(tokens.len())
}

#[test]
fn lex_all_upstream_lua_tests() {
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

        match lex_file(&path) {
            Ok(token_count) => {
                eprintln!(
                    "  OK: {} ({} tokens)",
                    path.file_name().unwrap().to_string_lossy(),
                    token_count
                );
                files_tested += 1;
            }
            Err(msg) => {
                errors.push(msg);
            }
        }
    }

    // Also test files in subdirectories (e.g., libs/)
    let libs_dir = test_dir.join("libs");
    if libs_dir.exists() {
        for entry in fs::read_dir(&libs_dir).expect("failed to read libs directory") {
            let entry = entry.expect("failed to read directory entry");
            let path = entry.path();

            if path.extension().and_then(|e| e.to_str()) != Some("lua") {
                continue;
            }

            match lex_file(&path) {
                Ok(token_count) => {
                    eprintln!(
                        "  OK: libs/{} ({} tokens)",
                        path.file_name().unwrap().to_string_lossy(),
                        token_count
                    );
                    files_tested += 1;
                }
                Err(msg) => {
                    errors.push(msg);
                }
            }
        }
    }

    eprintln!("\n{} files lexed successfully", files_tested);

    if !errors.is_empty() {
        eprintln!("\n{} files had lexer errors:", errors.len());
        for err in &errors {
            eprintln!("  ERROR: {}", err);
        }
        panic!(
            "{} out of {} files failed to lex",
            errors.len(),
            files_tested + errors.len()
        );
    }

    assert!(files_tested > 0, "no .lua test files found");
}

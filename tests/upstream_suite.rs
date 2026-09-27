//! M3 milestone check: run the curated upstream Lua 5.5 test suite.
//!
//! Each file runs in its own `rua` process with a per-file timeout, so a
//! crash or hang in one test does not hide the results of the others.
//!
//! Excluded by design (documented in `design_notes/ROADMAP.md`):
//!   - `api.lua`, `memerr.lua`, `cstack.lua`: need the C API / internal
//!     `T` test library.
//!   - `db.lua`: its line-hook sequence tests encode PUC's internal
//!     lineinfo delta encoding, and its `debug.getlocal` numbering tests
//!     depend on PUC 5.5 pseudo-locals ("(vararg table)", "(for state)").
//!     Both are implementation internals, covered instead by
//!     `tests/debug_test.lua` and `tests/test_m3_stdlib.lua`.
//!   - `main.lua`: needs the full CLI (M4.7).
//!   - `gc.lua`, `gengc.lua`, `tracegc.lua`: need incremental/generational
//!     GC modes and parameters (M4.1/M4.2).
//!   - `all.lua`: the aggregate runner (it also loads the files above).
//!   - `big.lua`, `heavy.lua`: resource-heavy stress tests.
//!
//! The suite is skipped in debug builds (too slow); run it with
//! `cargo test --release --test upstream_suite`.

use std::fs;
use std::io::Read;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

/// Language and standard library tests that must pass.
const CURATED: &[&str] = &[
    "bwcoercion.lua",
    "calls.lua",
    "closure.lua",
    "code.lua",
    "constructs.lua",
    "coroutine.lua",
    "errors.lua",
    "events.lua",
    "files.lua",
    "goto.lua",
    "literals.lua",
    "locals.lua",
    "math.lua",
    "nextvar.lua",
    "pm.lua",
    "sort.lua",
    "strings.lua",
    "tpack.lua",
    "utf8.lua",
    "vararg.lua",
    "verybig.lua",
];

const TIMEOUT: Duration = Duration::from_secs(240);

fn suite_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/lua-upstream-tests")
}

/// Run one test file; returns `Err(diagnostics)` on failure.
fn run_test(bin: &str, dir: &PathBuf, file: &str) -> Result<(), String> {
    let out_path = std::env::temp_dir().join(format!(
        "rua_upstream_{}_{}.out",
        std::process::id(),
        file.replace('.', "_")
    ));
    let err_path = std::env::temp_dir().join(format!(
        "rua_upstream_{}_{}.err",
        std::process::id(),
        file.replace('.', "_")
    ));
    let out_file = fs::File::create(&out_path).map_err(|e| e.to_string())?;
    let err_file = fs::File::create(&err_path).map_err(|e| e.to_string())?;

    // `dofile` keeps the test file as a normal chunk (its own line info,
    // names, etc.). Errors propagate and make the process exit non-zero.
    let wrapper = format!(
        "_port = true; _soft = true; _nomsg = true; \
         arg = {{ [0] = 'rua' }}; dofile('{file}')"
    );

    let mut child = Command::new(bin)
        .current_dir(dir)
        .arg("-e")
        .arg(&wrapper)
        .stdin(Stdio::null())
        .stdout(Stdio::from(out_file))
        .stderr(Stdio::from(err_file))
        .spawn()
        .map_err(|e| format!("failed to spawn: {e}"))?;

    let start = Instant::now();
    let mut status = None;
    while start.elapsed() < TIMEOUT {
        match child.try_wait() {
            Ok(Some(s)) => {
                status = Some(s);
                break;
            }
            Ok(None) => std::thread::sleep(Duration::from_millis(20)),
            Err(e) => return Err(format!("wait failed: {e}")),
        }
    }

    let read = |p: &PathBuf| -> String {
        let mut s = String::new();
        if let Ok(mut f) = fs::File::open(p) {
            let _ = f.read_to_string(&mut s);
        }
        s
    };

    let result = match status {
        Some(s) if s.success() => Ok(()),
        Some(s) => {
            let out = read(&out_path);
            let err = read(&err_path);
            Err(format!(
                "exit status {s}\n--- stdout (tail) ---\n{}\n--- stderr ---\n{}",
                tail(&out, 30),
                tail(&err, 30)
            ))
        }
        None => {
            let _ = child.kill();
            let _ = child.wait();
            Err(format!("TIMEOUT after {TIMEOUT:?}\n{}", tail(&read(&out_path), 20)))
        }
    };

    let _ = fs::remove_file(&out_path);
    let _ = fs::remove_file(&err_path);
    result
}

fn tail(s: &str, lines: usize) -> String {
    let v: Vec<&str> = s.lines().collect();
    let start = v.len().saturating_sub(lines);
    v[start..].join("\n")
}

#[test]
#[ignore = "M3 milestone check; run explicitly: cargo test --release --test upstream_suite -- --ignored --nocapture"]
fn curated_upstream_suite() {
    if cfg!(debug_assertions) {
        eprintln!("skipping curated upstream suite in debug build; run with --release");
        return;
    }

    let bin = env!("CARGO_BIN_EXE_rua");
    let dir = suite_dir();
    assert!(dir.is_dir(), "missing suite directory: {}", dir.display());

    let mut failures: Vec<(String, String)> = Vec::new();
    for file in CURATED {
        let path = dir.join(file);
        if !path.exists() {
            failures.push((file.to_string(), "test file not found".to_string()));
            continue;
        }
        match run_test(bin, &dir, file) {
            Ok(()) => eprintln!("ok: {file}"),
            Err(diag) => {
                eprintln!("FAIL: {file}\n{diag}");
                failures.push((file.to_string(), diag));
            }
        }
    }

    assert!(
        failures.is_empty(),
        "{} of {} curated upstream tests failed: {}",
        failures.len(),
        CURATED.len(),
        failures
            .iter()
            .map(|(f, _)| f.as_str())
            .collect::<Vec<_>>()
            .join(", ")
    );
}

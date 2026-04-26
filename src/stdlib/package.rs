//! package library: `require`, `package.*`.
//!
//! The VM dispatches `require`, `load`, `loadfile`, `dofile`, and the default
//! searchers (preload + Lua file) as VM-special functions because they need
//! access to the compiler and to build closures with an `_ENV` upvalue. This
//! module hosts the pieces that can live as pure native functions:
//! `package.searchpath`, the default `package.path`, and `package.config`.

use crate::error::LuaError;
use crate::gc::Gc;
use crate::value::Value;

/// `package.config` per the Lua 5.5 reference:
/// line 1: directory separator
/// line 2: path separator (`;`)
/// line 3: mark to replace in templates (`?`)
/// line 4: mark that the executable directory is replaced (`!`)
/// line 5: mark that the first `-` before `.` is replaced by the module name
#[cfg(windows)]
pub const PACKAGE_CONFIG: &str = "\\\n;\n?\n!\n-\n";

#[cfg(not(windows))]
pub const PACKAGE_CONFIG: &str = "/\n;\n?\n!\n-\n";

/// The directory separator character used when `package.searchpath` replaces
/// the separator-mark (default `.`) in a module name.
#[cfg(windows)]
pub const DIRECTORY_SEP: &str = "\\";

#[cfg(not(windows))]
pub const DIRECTORY_SEP: &str = "/";

/// Default value for `package.path`. Honors the `LUA_PATH_5_5`, `LUA_PATH_5`,
/// and `LUA_PATH` environment variables in that order. A `;;` in the env value
/// is replaced by the built-in default.
pub fn default_path() -> String {
    let builtin = "./?.lua;./?/init.lua";
    for var in ["LUA_PATH_5_5", "LUA_PATH_5", "LUA_PATH"] {
        if let Ok(val) = std::env::var(var) {
            if val.contains(";;") {
                return val.replace(";;", builtin);
            }
            return val;
        }
    }
    builtin.to_string()
}

/// Search for a file in a Lua-style path string.
///
/// `name` — module name.
/// `path` — semicolon-separated templates, each containing `?` as the mark.
/// `sep` — separator character within `name` (default `.`).
/// `rep` — replacement for `sep` (default the directory separator).
///
/// Returns `Ok(filename)` on success, or `Err(error_message)` listing all the
/// filenames that were tried.
pub fn searchpath(
    name: &str,
    path: &str,
    sep: &str,
    rep: &str,
) -> Result<String, String> {
    let effective_name = if sep.is_empty() {
        name.to_string()
    } else {
        name.replace(sep, rep)
    };

    let mut tried: Vec<String> = Vec::new();
    for template in path.split(';') {
        if template.is_empty() {
            continue;
        }
        let filename = template.replace('?', &effective_name);
        match std::fs::metadata(&filename) {
            Ok(_) => return Ok(filename),
            Err(_) => tried.push(format!("\n\tno file '{filename}'")),
        }
    }
    Err(tried.concat())
}

/// `package.searchpath(name, path [, sep [, rep]])`
pub fn lua_searchpath(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let name_val = args.first().copied().unwrap_or(Value::Nil);
    let path_val = args.get(1).copied().unwrap_or(Value::Nil);

    let name = match name_val.as_str_bytes() {
        Some(b) => std::str::from_utf8(b)
            .map_err(|_| LuaError::new("bad argument #1 to 'searchpath' (invalid utf-8)"))?
            .to_string(),
        None => return Err(LuaError::new(
            "bad argument #1 to 'searchpath' (string expected)",
        )),
    };
    let path = match path_val.as_str_bytes() {
        Some(b) => std::str::from_utf8(b)
            .map_err(|_| LuaError::new("bad argument #2 to 'searchpath' (invalid utf-8)"))?
            .to_string(),
        None => return Err(LuaError::new(
            "bad argument #2 to 'searchpath' (string expected)",
        )),
    };
    let sep = match args.get(2).copied().unwrap_or(Value::Nil).as_str_bytes() {
        Some(b) => std::str::from_utf8(b)
            .map_err(|_| LuaError::new("bad argument #3 to 'searchpath' (invalid utf-8)"))?
            .to_string(),
        None => ".".to_string(),
    };
    let rep = match args.get(3).copied().unwrap_or(Value::Nil).as_str_bytes() {
        Some(b) => std::str::from_utf8(b)
            .map_err(|_| LuaError::new("bad argument #4 to 'searchpath' (invalid utf-8)"))?
            .to_string(),
        None => DIRECTORY_SEP.to_string(),
    };

    match searchpath(&name, &path, &sep, &rep) {
        Ok(file) => Ok(vec![Value::Object(gc.new_string(file.as_bytes()))]),
        Err(msg) => Ok(vec![
            Value::Nil,
            Value::Object(gc.new_string(msg.as_bytes())),
        ]),
    }
}

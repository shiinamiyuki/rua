//! os library: operating system facilities.

use std::time::{Instant, SystemTime, UNIX_EPOCH};

use crate::closure::NativeFn;
use crate::error::LuaError;
use crate::gc::Gc;
use crate::value::Value;

// ── os functions ───────────────────────────────────────────────────

pub fn os_clock(_args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    // Return CPU time in seconds (approximate using process time)
    // Use thread_local to track start time
    thread_local! {
        static START: Instant = Instant::now();
    }
    let elapsed = START.with(|start| start.elapsed());
    Ok(vec![Value::Float(elapsed.as_secs_f64())])
}

pub fn os_time(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if args.is_empty() || args[0] == Value::Nil {
        let secs = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();
        return Ok(vec![Value::Integer(secs as i64)]);
    }
    // os.time(table) — construct time from table fields
    // For now, return current time regardless of table
    let secs = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();
    Ok(vec![Value::Integer(secs as i64)])
}

pub fn os_difftime(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let t2 = match args.first() {
        Some(Value::Integer(n)) => *n as f64,
        Some(Value::Float(n)) => *n,
        _ => return Err(LuaError::new("bad argument #1 to 'difftime' (number expected)")),
    };
    let t1 = match args.get(1) {
        Some(Value::Integer(n)) => *n as f64,
        Some(Value::Float(n)) => *n,
        _ => return Err(LuaError::new("bad argument #2 to 'difftime' (number expected)")),
    };
    Ok(vec![Value::Float(t2 - t1)])
}

pub fn os_date(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let format = args.first()
        .and_then(|v| v.as_str_bytes())
        .map(|b| std::str::from_utf8(b).unwrap_or("%c"))
        .unwrap_or("%c");

    // Handle "*t" format — return table
    if format == "*t" || format == "!*t" {
        return os_date_table(args, gc);
    }

    // For simple date formatting, use a basic implementation
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs();

    let result = format_date(format, now as i64);
    Ok(vec![Value::Object(gc.new_string(result.as_bytes()))])
}

fn os_date_table(_args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs() as i64;

    // Simple epoch → broken-down time (UTC)
    let (year, month, day, hour, min, sec, wday, yday) = epoch_to_fields(now);

    let table_ref = gc.new_table(crate::table::Table::new());
    {
        let table = table_ref.as_object_mut().as_table_mut().unwrap();
        table.raw_set(Value::Object(gc.new_string(b"year")), Value::Integer(year));
        table.raw_set(Value::Object(gc.new_string(b"month")), Value::Integer(month));
        table.raw_set(Value::Object(gc.new_string(b"day")), Value::Integer(day));
        table.raw_set(Value::Object(gc.new_string(b"hour")), Value::Integer(hour));
        table.raw_set(Value::Object(gc.new_string(b"min")), Value::Integer(min));
        table.raw_set(Value::Object(gc.new_string(b"sec")), Value::Integer(sec));
        table.raw_set(Value::Object(gc.new_string(b"wday")), Value::Integer(wday));
        table.raw_set(Value::Object(gc.new_string(b"yday")), Value::Integer(yday));
        table.raw_set(Value::Object(gc.new_string(b"isdst")), Value::Boolean(false));
    }

    Ok(vec![Value::Object(table_ref)])
}

/// Convert UTC epoch seconds to (year, month, day, hour, min, sec, wday, yday).
fn epoch_to_fields(epoch: i64) -> (i64, i64, i64, i64, i64, i64, i64, i64) {
    let secs_per_day: i64 = 86400;
    let mut days = epoch / secs_per_day;
    let mut rem = epoch % secs_per_day;
    if rem < 0 { days -= 1; rem += secs_per_day; }

    let hour = rem / 3600;
    rem %= 3600;
    let min = rem / 60;
    let sec = rem % 60;

    // Day of week (1970-01-01 was Thursday = day 4, Lua wday: 1=Sunday)
    let wday = ((days + 4) % 7 + 7) % 7 + 1; // 1=Sunday

    // Year calculation
    let mut year = 1970i64;
    loop {
        let days_in_year = if is_leap(year) { 366 } else { 365 };
        if days < days_in_year {
            break;
        }
        days -= days_in_year;
        year += 1;
    }

    let yday = days + 1;

    // Month calculation
    let month_days = if is_leap(year) {
        [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    } else {
        [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    };
    let mut month = 0i64;
    let mut day = days;
    for (i, &md) in month_days.iter().enumerate() {
        if day < md {
            month = (i + 1) as i64;
            break;
        }
        day -= md;
    }
    let day = day + 1;

    (year, month, day, hour, min, sec, wday, yday)
}

fn is_leap(year: i64) -> bool {
    (year % 4 == 0 && year % 100 != 0) || year % 400 == 0
}

/// Basic strftime-like formatting.
fn format_date(fmt: &str, epoch: i64) -> String {
    let (year, month, day, hour, min, sec, wday, yday) = epoch_to_fields(epoch);
    let mut result = String::new();
    let mut chars = fmt.chars().peekable();

    // Skip leading '!' for UTC indicator
    if chars.peek() == Some(&'!') {
        chars.next();
    }

    while let Some(c) = chars.next() {
        if c == '%' {
            match chars.next() {
                Some('Y') => result.push_str(&format!("{:04}", year)),
                Some('y') => result.push_str(&format!("{:02}", year % 100)),
                Some('m') => result.push_str(&format!("{:02}", month)),
                Some('d') => result.push_str(&format!("{:02}", day)),
                Some('H') => result.push_str(&format!("{:02}", hour)),
                Some('M') => result.push_str(&format!("{:02}", min)),
                Some('S') => result.push_str(&format!("{:02}", sec)),
                Some('j') => result.push_str(&format!("{:03}", yday)),
                Some('w') => result.push_str(&format!("{}", (wday - 1) % 7)), // 0=Sunday
                Some('A') => {
                    let names = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];
                    result.push_str(names[((wday - 1) % 7) as usize]);
                }
                Some('a') => {
                    let names = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
                    result.push_str(names[((wday - 1) % 7) as usize]);
                }
                Some('B') => {
                    let names = ["January", "February", "March", "April", "May", "June",
                                 "July", "August", "September", "October", "November", "December"];
                    if month >= 1 && month <= 12 { result.push_str(names[(month - 1) as usize]); }
                }
                Some('b') | Some('h') => {
                    let names = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
                    if month >= 1 && month <= 12 { result.push_str(names[(month - 1) as usize]); }
                }
                Some('c') => {
                    result.push_str(&format_date("%a %b %d %H:%M:%S %Y", epoch));
                }
                Some('p') => result.push_str(if hour < 12 { "AM" } else { "PM" }),
                Some('X') | Some('T') => {
                    result.push_str(&format!("{:02}:{:02}:{:02}", hour, min, sec));
                }
                Some('x') | Some('D') => {
                    result.push_str(&format!("{:02}/{:02}/{:02}", month, day, year % 100));
                }
                Some('%') => result.push('%'),
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some(other) => { result.push('%'); result.push(other); }
                None => result.push('%'),
            }
        } else {
            result.push(c);
        }
    }
    result
}

pub fn os_execute(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if args.is_empty() || args[0] == Value::Nil {
        // os.execute() with no args → check if shell is available
        return Ok(vec![Value::Boolean(true)]);
    }
    let cmd = match args.first() {
        Some(Value::Object(r)) if r.as_object().as_string().is_some() => {
            let s = r.as_object().as_string().unwrap();
            std::str::from_utf8(s.as_bytes())
                .map_err(|_| LuaError::new("invalid command"))?
                .to_string()
        }
        _ => return Err(LuaError::new("bad argument #1 to 'execute' (string expected)")),
    };

    #[cfg(target_os = "windows")]
    let result = std::process::Command::new("cmd").args(["/C", &cmd]).status();
    #[cfg(not(target_os = "windows"))]
    let result = std::process::Command::new("sh").args(["-c", &cmd]).status();

    match result {
        Ok(status) => {
            let code = status.code().unwrap_or(-1);
            if status.success() {
                let s = gc.new_string(b"exit");
                Ok(vec![Value::Boolean(true), Value::Object(s), Value::Integer(code as i64)])
            } else {
                let s = gc.new_string(b"exit");
                Ok(vec![Value::Nil, Value::Object(s), Value::Integer(code as i64)])
            }
        }
        Err(e) => {
            let msg = gc.new_string(e.to_string().as_bytes());
            Ok(vec![Value::Nil, Value::Object(msg)])
        }
    }
}

pub fn os_exit(args: &[Value], _gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let code = match args.first() {
        Some(Value::Integer(n)) => *n as i32,
        Some(Value::Boolean(true)) | None => 0,
        Some(Value::Boolean(false)) => 1,
        Some(Value::Float(n)) => *n as i32,
        _ => 0,
    };
    std::process::exit(code);
}

pub fn os_getenv(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let name = match args.first() {
        Some(Value::Object(r)) if r.as_object().as_string().is_some() => {
            let s = r.as_object().as_string().unwrap();
            std::str::from_utf8(s.as_bytes())
                .map_err(|_| LuaError::new("invalid env name"))?
                .to_string()
        }
        _ => return Err(LuaError::new("bad argument #1 to 'getenv' (string expected)")),
    };

    match std::env::var(&name) {
        Ok(val) => Ok(vec![Value::Object(gc.new_string(val.as_bytes()))]),
        Err(_) => Ok(vec![Value::Nil]),
    }
}

pub fn os_remove(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let filename = match args.first() {
        Some(Value::Object(r)) if r.as_object().as_string().is_some() => {
            let s = r.as_object().as_string().unwrap();
            std::str::from_utf8(s.as_bytes())
                .map_err(|_| LuaError::new("invalid filename"))?
                .to_string()
        }
        _ => return Err(LuaError::new("bad argument #1 to 'remove' (string expected)")),
    };

    match std::fs::remove_file(&filename) {
        Ok(()) => Ok(vec![Value::Boolean(true)]),
        Err(e) => {
            let msg = gc.new_string(format!("{}: {}", filename, e).as_bytes());
            Ok(vec![Value::Nil, Value::Object(msg)])
        }
    }
}

pub fn os_rename(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let oldname = match args.first() {
        Some(Value::Object(r)) if r.as_object().as_string().is_some() => {
            let s = r.as_object().as_string().unwrap();
            std::str::from_utf8(s.as_bytes())
                .map_err(|_| LuaError::new("invalid filename"))?
                .to_string()
        }
        _ => return Err(LuaError::new("bad argument #1 to 'rename' (string expected)")),
    };
    let newname = match args.get(1) {
        Some(Value::Object(r)) if r.as_object().as_string().is_some() => {
            let s = r.as_object().as_string().unwrap();
            std::str::from_utf8(s.as_bytes())
                .map_err(|_| LuaError::new("invalid filename"))?
                .to_string()
        }
        _ => return Err(LuaError::new("bad argument #2 to 'rename' (string expected)")),
    };

    match std::fs::rename(&oldname, &newname) {
        Ok(()) => Ok(vec![Value::Boolean(true)]),
        Err(e) => {
            let msg = gc.new_string(format!("{}: {}", oldname, e).as_bytes());
            Ok(vec![Value::Nil, Value::Object(msg)])
        }
    }
}

pub fn os_tmpname(_args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let dir = std::env::temp_dir();
    let path = dir.join(format!("lua_tmp_{}", std::process::id()));
    let s = path.to_string_lossy().to_string();
    Ok(vec![Value::Object(gc.new_string(s.as_bytes()))])
}

pub fn os_setlocale(_args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    // Stub: always return "C" locale
    Ok(vec![Value::Object(gc.new_string(b"C"))])
}

// ── Registration helpers ───────────────────────────────────────────

pub fn os_functions() -> Vec<(&'static str, NativeFn)> {
    vec![
        ("clock", os_clock as NativeFn),
        ("time", os_time),
        ("date", os_date),
        ("difftime", os_difftime),
        ("execute", os_execute),
        ("exit", os_exit),
        ("getenv", os_getenv),
        ("remove", os_remove),
        ("rename", os_rename),
        ("tmpname", os_tmpname),
        ("setlocale", os_setlocale),
    ]
}

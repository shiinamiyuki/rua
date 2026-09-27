//! os library: operating system facilities.

use std::time::{Instant, SystemTime, UNIX_EPOCH};

use crate::closure::NativeFn;
use crate::error::LuaError;
use crate::gc::{Gc, GcRef};
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

const INT_MAX_I: i64 = i32::MAX as i64;
const INT_MIN_I: i64 = i32::MIN as i64;

/// Read an integer field from a date table, with reference-Lua's bounds
/// checks (`field 'x' is not an integer` / `is out-of-bound` / `missing`).
fn get_date_field(
    table: GcRef,
    key: &[u8],
    default: Option<i64>,
    delta: i64,
    gc: &mut Gc,
) -> Result<i64, LuaError> {
    let k = gc.new_string(key);
    let v = table
        .as_object()
        .as_table()
        .map(|t| t.raw_get(&Value::Object(k)))
        .unwrap_or(Value::Nil);
    let name = String::from_utf8_lossy(key);
    let res = match v {
        Value::Nil => match default {
            Some(d) => d,
            None => {
                return Err(LuaError::new(format!(
                    "field '{name}' missing in date table"
                )))
            }
        },
        Value::Integer(i) => i,
        Value::Float(f) if f.fract() == 0.0 && f >= -(2f64.powi(63)) && f < 2f64.powi(63) => {
            f as i64
        }
        _ => {
            return Err(LuaError::new(format!(
                "field '{name}' is not an integer"
            )))
        }
    };
    let in_bounds = if res >= 0 {
        res - delta <= INT_MAX_I
    } else {
        INT_MIN_I + delta <= res
    };
    if !in_bounds {
        return Err(LuaError::new(format!(
            "field '{name}' is out-of-bound"
        )));
    }
    Ok(res - delta)
}

/// Days from 1970-01-01 to y-m-d (proleptic Gregorian).
fn days_from_civil(y: i64, m: i64, d: i64) -> i64 {
    let y = if m <= 2 { y - 1 } else { y };
    let era = if y >= 0 { y } else { y - 399 } / 400;
    let yoe = y - era * 400;
    let mp = (m + 9) % 12;
    let doy = (153 * mp + 2) / 5 + d - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146097 + doe - 719468
}

/// Inverse of `days_from_civil`.
fn civil_from_days(z: i64) -> (i64, i64, i64) {
    let z = z + 719468;
    let era = if z >= 0 { z } else { z - 146096 } / 146097;
    let doe = z - era * 146097;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    (if m <= 2 { y + 1 } else { y }, m, d)
}

/// Normalize raw date fields (allowing out-of-range values) into an epoch.
fn fields_to_epoch(
    year: i64,
    month: i64,
    day: i64,
    hour: i64,
    min: i64,
    sec: i64,
) -> Option<i64> {
    // Normalize month into 1..12.
    let mut y = year as i128;
    let mut m = month as i128 - 1;
    y += m.div_euclid(12);
    m = m.rem_euclid(12);
    let m = m + 1;
    let days = days_from_civil(y as i64, m as i64, day) as i128;
    let total = days * 86400 + hour as i128 * 3600 + min as i128 * 60 + sec as i128;
    if total < i64::MIN as i128 || total > i64::MAX as i128 {
        None
    } else {
        Some(total as i64)
    }
}

fn set_date_fields(t: GcRef, epoch: i64, gc: &mut Gc) {
    let secs_per_day = 86400i64;
    let days = epoch.div_euclid(secs_per_day);
    let rem = epoch.rem_euclid(secs_per_day);
    let (year, month, day) = civil_from_days(days);
    let hour = rem / 3600;
    let min = (rem % 3600) / 60;
    let sec = rem % 60;
    let yday = days - days_from_civil(year, 1, 1) + 1;
    let wday = (days + 4).rem_euclid(7) + 1; // 1 = Sunday
    let mut set = |name: &[u8], v: Value, gc: &mut Gc| {
        let k = gc.new_string(name);
        t.as_object_mut()
            .as_table_mut()
            .unwrap()
            .raw_set(Value::Object(k), v);
    };
    set(b"year", Value::Integer(year), gc);
    set(b"month", Value::Integer(month), gc);
    set(b"day", Value::Integer(day), gc);
    set(b"hour", Value::Integer(hour), gc);
    set(b"min", Value::Integer(min), gc);
    set(b"sec", Value::Integer(sec), gc);
    set(b"yday", Value::Integer(yday), gc);
    set(b"wday", Value::Integer(wday), gc);
    set(b"isdst", Value::Boolean(false), gc);
}

pub fn os_time(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    if args.is_empty() || args[0].is_nil() {
        let secs = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();
        return Ok(vec![Value::Integer(secs as i64)]);
    }
    let table = match args[0] {
        Value::Object(r) if r.as_object().as_table().is_some() => r,
        _ => {
            return Err(LuaError::new(
                "bad argument #1 to 'time' (table expected)",
            ))
        }
    };
    let year = get_date_field(table, b"year", None, 1900, gc)? + 1900;
    let month = get_date_field(table, b"month", None, 1, gc)? + 1;
    let day = get_date_field(table, b"day", None, 0, gc)?;
    let hour = get_date_field(table, b"hour", Some(12), 0, gc)?;
    let min = get_date_field(table, b"min", Some(0), 0, gc)?;
    let sec = get_date_field(table, b"sec", Some(0), 0, gc)?;

    match fields_to_epoch(year, month, day, hour, min, sec) {
        Some(epoch) => {
            set_date_fields(table, epoch, gc);
            Ok(vec![Value::Integer(epoch)])
        }
        None => Err(LuaError::new(
            "time result cannot be represented in this installation",
        )),
    }
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

/// Valid one-character strftime options (C99 set) plus the two-character
/// 'E'/'O' variants accepted by reference Lua.
fn valid_specifier(bytes: &[u8], pos: usize) -> Option<usize> {
    const ONE: &[u8] = b"aAbBcCdDeFgGhHIjmMnprRStTuUVwWxXyYzZ%";
    const TWO: [&[u8]; 26] = [
        b"Ec", b"EC", b"Ex", b"EX", b"Ey", b"EY", b"Od", b"Oe", b"OH", b"OI",
        b"Om", b"OM", b"OS", b"Ou", b"OU", b"OV", b"Ow", b"OW", b"Oy", b"#c",
        b"#x", b"#d", b"#H", b"#I", b"#j", b"#m",
    ];
    let first = *bytes.get(pos)?;
    if ONE.contains(&first) {
        return Some(1);
    }
    if pos + 1 <= bytes.len() {
        let pair = &bytes[pos..bytes.len().min(pos + 2)];
        if pair.len() == 2 && TWO.iter().any(|t| *t == pair) {
            return Some(2);
        }
    }
    None
}

pub fn os_date(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    let format = args
        .first()
        .map(|v| {
            v.as_str_bytes()
                .map(|b| b.to_vec())
                .unwrap_or_else(|| b"%c".to_vec())
        })
        .unwrap_or_else(|| b"%c".to_vec());

    let epoch = match args.get(1).copied() {
        None | Some(Value::Nil) => SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs() as i64,
        Some(Value::Integer(i)) => i,
        Some(Value::Float(f)) if f.fract() == 0.0 => f as i64,
        Some(_) => {
            return Err(LuaError::new(
                "bad argument #2 to 'date' (number expected)",
            ))
        }
    };

    let mut fmt: &[u8] = &format;
    if fmt.first() == Some(&b'!') {
        fmt = &fmt[1..];
    }

    if fmt == b"*t" {
        let t = gc.new_table(crate::table::Table::new());
        set_date_fields(t, epoch, gc);
        return Ok(vec![Value::Object(t)]);
    }

    // Validate every conversion specifier.
    let mut i = 0;
    while i < fmt.len() {
        if fmt[i] != b'%' {
            i += 1;
            continue;
        }
        i += 1;
        match valid_specifier(fmt, i) {
            Some(n) => i += n,
            None => {
                let spec = String::from_utf8_lossy(&fmt[i.min(fmt.len())..]).to_string();
                return Err(LuaError::new(format!(
                    "bad argument #1 to 'date' (invalid conversion specifier '%{spec}')"
                )));
            }
        }
    }

    let result = format_date_bytes(fmt, epoch);
    Ok(vec![Value::Object(gc.new_string(&result))])
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

/// Format a date using strftime-like specifiers (already validated).
fn format_date_bytes(fmt: &[u8], epoch: i64) -> Vec<u8> {
    let (year, month, day, hour, min, sec, wday, yday) = epoch_to_fields(epoch);
    let mut result: Vec<u8> = Vec::new();
    let mut i = 0usize;
    while i < fmt.len() {
        let c = fmt[i];
        if c != b'%' {
            result.push(c);
            i += 1;
            continue;
        }
        i += 1;
        let n = valid_specifier(fmt, i).unwrap_or(1);
        let spec: Vec<u8> = fmt[i..fmt.len().min(i + n)].to_vec();
        i += n;
        let mut push = |s: String| result.extend_from_slice(s.as_bytes());
        // Map 'E'/'O' variants to their base specifier.
        let base = match spec.as_slice() {
            b"Ec" | b"#c" => b'c',
            b"EC" => b'C',
            b"Ex" | b"#x" => b'x',
            b"EX" => b'X',
            b"Ey" | b"#y" => b'y',
            b"EY" => b'Y',
            b"Od" | b"#d" => b'd',
            b"Oe" => b'e',
            b"OH" | b"#H" => b'H',
            b"OI" | b"#I" => b'I',
            b"Om" | b"#m" => b'm',
            b"OM" => b'M',
            b"OS" => b'S',
            b"Ou" => b'u',
            b"OU" => b'U',
            b"OV" => b'V',
            b"Ow" => b'w',
            b"OW" => b'W',
            b"Oy" => b'y',
            b"#j" => b'j',
            other => other.first().copied().unwrap_or(b'%'),
        };
        match base {
            b'%' => result.push(b'%'),
            b'Y' => push(format!("{:04}", year)),
            _ => {
                let text = format_base_specifier(base, year, month, day, hour, min, sec, wday, yday);
                result.extend_from_slice(text.as_bytes());
            }
        }
    }
    result
}

fn format_base_specifier(
    c: u8,
    year: i64,
    month: i64,
    day: i64,
    hour: i64,
    min: i64,
    sec: i64,
    wday: i64,
    yday: i64,
) -> String {
    match c {
        b'y' => format!("{:02}", year.rem_euclid(100)),
        b'm' => format!("{:02}", month),
        b'd' => format!("{:02}", day),
        b'e' => format!("{:2}", day),
        b'H' => format!("{:02}", hour),
        b'M' => format!("{:02}", min),
        b'S' => format!("{:02}", sec),
        b'j' => format!("{:03}", yday),
        b'w' => format!("{}", (wday - 1).rem_euclid(7)),
        b'u' => format!("{}", if wday == 1 { 7 } else { wday - 1 }),
        b'A' => {
            let names = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];
            names[((wday - 1).rem_euclid(7)) as usize].to_string()
        }
        b'a' => {
            let names = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
            names[((wday - 1).rem_euclid(7)) as usize].to_string()
        }
        b'B' => {
            let names = ["January", "February", "March", "April", "May", "June",
                         "July", "August", "September", "October", "November", "December"];
            names[(month - 1).rem_euclid(12) as usize].to_string()
        }
        b'b' | b'h' => {
            let names = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
            names[(month - 1).rem_euclid(12) as usize].to_string()
        }
        b'p' => if hour < 12 { "AM".to_string() } else { "PM".to_string() },
        b'C' => format!("{:02}", year.div_euclid(100)),
        b'D' => format!("{:02}/{:02}/{:02}", month, day, year.rem_euclid(100)),
        b'F' => format!("{:04}-{:02}-{:02}", year, month, day),
        b'R' => format!("{:02}:{:02}", hour, min),
        b'T' => format!("{:02}:{:02}:{:02}", hour, min, sec),
        b'r' => format!(
            "{:02}:{:02}:{:02} {}",
            if hour % 12 == 0 { 12 } else { hour % 12 },
            min,
            sec,
            if hour < 12 { "AM" } else { "PM" }
        ),
        b'c' => format!(
            "{} {} {:2} {:02}:{:02}:{:02} {}",
            ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"][((wday - 1).rem_euclid(7)) as usize],
            ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
                [(month - 1).rem_euclid(12) as usize],
            day, hour, min, sec, year
        ),
        b'x' => format!("{:02}/{:02}/{:02}", month, day, year.rem_euclid(100)),
        b'X' => format!("{:02}:{:02}:{:02}", hour, min, sec),
        b'n' => "\n".to_string(),
        b't' => "\t".to_string(),
        b'z' => "+0000".to_string(),
        b'Z' => "UTC".to_string(),
        b'G' => format!("{:04}", year),
        b'g' => format!("{:02}", year.rem_euclid(100)),
        b'V' => {
            let week = ((yday - 1) / 7 + 1).min(53);
            format!("{:02}", week)
        }
        b'U' => format!("{:02}", (yday + 6 - wday % 7) / 7),
        b'W' => format!("{:02}", (yday + 6 - (wday + 6) % 7) / 7),
        _ => String::new(),
    }
}

/// Basic strftime-like formatting (unvalidated).
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
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    let n = COUNTER.fetch_add(1, Ordering::Relaxed);
    let t = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0);
    let dir = std::env::temp_dir();
    let path = dir.join(format!("lua_{}_{}_{}", std::process::id(), n, t));
    let s = path.to_string_lossy().to_string();
    Ok(vec![Value::Object(gc.new_string(s.as_bytes()))])
}

/// `os.setlocale([locale [, category]])`.
///
/// Only the "C"/"POSIX" locale is available; requests for other locales
/// fail (return nil) without changing anything, so locale-dependent tests
/// are skipped just like on a system without those locales installed.
pub fn os_setlocale(args: &[Value], gc: &mut Gc) -> Result<Vec<Value>, LuaError> {
    const CATEGORIES: [&[u8]; 6] = [
        b"all",
        b"collate",
        b"ctype",
        b"monetary",
        b"numeric",
        b"time",
    ];

    // Validate the category option (defaults to "all").
    let category = args.get(1).copied().unwrap_or(Value::Nil);
    let category_ok = match category {
        Value::Nil => true,
        Value::Object(r) if r.as_object().as_string().is_some() => {
            let b = r.as_object().as_string().unwrap().as_bytes();
            CATEGORIES.iter().any(|c| *c == b)
        }
        _ => false,
    };
    if !category_ok {
        return Err(LuaError::new(format!(
            "bad argument #2 to 'setlocale' (invalid option '{}')",
            category
        )));
    }

    let locale = args.first().copied().unwrap_or(Value::Nil);
    let available = match locale {
        Value::Nil => true, // query the current locale
        Value::Object(r) if r.as_object().as_string().is_some() => {
            let b = r.as_object().as_string().unwrap().as_bytes();
            b.is_empty() || b == b"C" || b == b"POSIX"
        }
        _ => false,
    };

    if available {
        Ok(vec![Value::Object(gc.new_string(b"C"))])
    } else {
        Ok(vec![Value::Nil])
    }
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

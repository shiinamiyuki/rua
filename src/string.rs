//! Lua string type: byte-buffer backed, with precomputed hash.

/// Maximum length for "short" strings that are automatically interned.
pub const SHORT_STRING_MAX: usize = 40;

/// A Lua string: an immutable byte buffer with a precomputed hash.
///
/// Lua strings are arbitrary byte sequences (not necessarily valid UTF-8).
/// Short strings (≤40 bytes) are interned for fast equality checks.
#[derive(Debug)]
pub struct LuaString {
    /// Precomputed hash value.
    hash: u64,
    /// The raw bytes of the string.
    data: Box<[u8]>,
}

impl LuaString {
    /// Create a new LuaString from a byte slice.
    pub fn new(data: &[u8]) -> Self {
        let hash = hash_bytes(data);
        LuaString {
            hash,
            data: data.into(),
        }
    }

    /// Get the precomputed hash value.
    #[inline]
    pub fn hash(&self) -> u64 {
        self.hash
    }

    /// Get the string as a byte slice.
    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        &self.data
    }

    /// Get the length in bytes.
    #[inline]
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Check if the string is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Check if this is a "short" string (eligible for interning).
    #[inline]
    pub fn is_short(&self) -> bool {
        self.data.len() <= SHORT_STRING_MAX
    }
}

impl PartialEq for LuaString {
    fn eq(&self, other: &Self) -> bool {
        // Fast path: if hashes differ, strings differ
        if self.hash != other.hash {
            return false;
        }
        self.data == other.data
    }
}

impl Eq for LuaString {}

impl std::fmt::Display for LuaString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match std::str::from_utf8(&self.data) {
            Ok(s) => write!(f, "{s}"),
            Err(_) => write!(f, "<string ({} bytes)>", self.data.len()),
        }
    }
}

/// Hash function for byte strings, based on Lua's string hash (luaS_hash).
///
/// Uses a sampling approach for long strings: only every `(len >> 5) + 1`
/// bytes are hashed, making the cost O(1) for long strings while still
/// providing good distribution for typical inputs.
fn hash_bytes(data: &[u8]) -> u64 {
    // Fixed seed for now; randomize in Phase 4 for hash-DoS protection
    const SEED: u64 = 0x_DEAD_BEEF_CAFE_BABE;

    let len = data.len();
    let mut h: u64 = SEED ^ (len as u64);
    let step = (len >> 5) + 1;
    let mut i = len;
    while i >= step {
        h ^= (h << 5)
            .wrapping_add(h >> 2)
            .wrapping_add(data[i - 1] as u64);
        i -= step;
    }
    h
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic() {
        let s = LuaString::new(b"hello");
        assert_eq!(s.as_bytes(), b"hello");
        assert_eq!(s.len(), 5);
        assert!(!s.is_empty());
        assert!(s.is_short());
    }

    #[test]
    fn test_empty() {
        let s = LuaString::new(b"");
        assert_eq!(s.len(), 0);
        assert!(s.is_empty());
        assert!(s.is_short());
    }

    #[test]
    fn test_equality() {
        let s1 = LuaString::new(b"hello");
        let s2 = LuaString::new(b"hello");
        let s3 = LuaString::new(b"world");
        assert_eq!(s1, s2);
        assert_ne!(s1, s3);
    }

    #[test]
    fn test_hash_consistency() {
        let s1 = LuaString::new(b"test");
        let s2 = LuaString::new(b"test");
        assert_eq!(s1.hash(), s2.hash());
    }

    #[test]
    fn test_hash_different() {
        let s1 = LuaString::new(b"foo");
        let s2 = LuaString::new(b"bar");
        assert_ne!(s1.hash(), s2.hash());
    }

    #[test]
    fn test_long_string() {
        let long = vec![b'x'; SHORT_STRING_MAX + 1];
        let s = LuaString::new(&long);
        assert!(!s.is_short());
        assert_eq!(s.len(), SHORT_STRING_MAX + 1);
    }

    #[test]
    fn test_binary_content() {
        // Lua strings can contain arbitrary bytes including null
        let s = LuaString::new(&[0, 1, 2, 255, 0]);
        assert_eq!(s.len(), 5);
        assert_eq!(s.as_bytes(), &[0, 1, 2, 255, 0]);
    }

    #[test]
    fn test_display_utf8() {
        let s = LuaString::new(b"hello");
        assert_eq!(format!("{s}"), "hello");
    }

    #[test]
    fn test_display_non_utf8() {
        let s = LuaString::new(&[0xFF, 0xFE]);
        assert_eq!(format!("{s}"), "<string (2 bytes)>");
    }

    #[test]
    fn test_boundary_short_string() {
        let exactly = vec![b'a'; SHORT_STRING_MAX];
        let s = LuaString::new(&exactly);
        assert!(s.is_short());

        let one_over = vec![b'a'; SHORT_STRING_MAX + 1];
        let s = LuaString::new(&one_over);
        assert!(!s.is_short());
    }
}

use std::{hash::Hash, ptr::NonNull};

use crate::lox_object::lox_string::LoxString;

pub struct HashMap<V, K = *const LoxString> {
    cap: usize,
    len: usize,
    entries: Vec<Entry<K, V>>,
}

impl<V> HashMap<V> {
    pub fn new() -> Self {
        HashMap {
            cap: 0,
            len: 0,
            entries: Vec::new(),
        }
    }
}

pub struct Entry<K, V> {
    key: K,
    val: V,
}

// If you want to use rust ver.

#[derive(Debug)]
pub struct RLoxHashMapKey(pub NonNull<LoxString>);

impl PartialEq for RLoxHashMapKey {
    fn eq(&self, other: &Self) -> bool {
        unsafe {
            self.0.as_ref().hash == other.0.as_ref().hash
                && self.0.as_ref().chars == other.0.as_ref().chars
        }
    }
}

impl Eq for RLoxHashMapKey {}

impl Hash for RLoxHashMapKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        unsafe {
            self.0.as_ref().chars.hash(state);
        }
    }
}

use std::ops::Add;

use crate::{
    gc,
    object::{LoxObj, LoxObjType},
};

#[inline(always)]
fn fnv_1a(item: &[u8]) -> u32 {
    item.iter().fold(2166136261u32, |mut acc, &x| {
        acc ^= x as u32;
        acc.wrapping_mul(16777619)
    })
}

#[derive(Debug)]
#[repr(C)]
pub struct LoxString {
    pub obj: LoxObj,
    pub hash: u32,
    pub chars: String,
}

impl LoxString {
    pub fn new(chars: String) -> *const LoxString {
        let obj = LoxObj {
            obj_type: LoxObjType::String,
            next: std::ptr::null_mut(),
        };
        let hash = fnv_1a(chars.as_bytes());
        let result = LoxString { obj, hash, chars };
        let ptr = Box::into_raw(Box::new(result));
        gc::register(ptr);
        ptr
    }
}

impl PartialEq for LoxString {
    fn eq(&self, other: &Self) -> bool {
        self.chars.eq(&other.chars)
    }
}

impl Eq for LoxString {}

impl PartialOrd for LoxString {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LoxString {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.chars.cmp(&other.chars)
    }
}

impl Add<&LoxString> for LoxString {
    type Output = LoxString;
    fn add(self, rhs: &LoxString) -> Self::Output {
        let obj = LoxObj {
            obj_type: LoxObjType::String,
            next: std::ptr::null_mut(),
        };
        let chars = self.chars.add(&rhs.chars);
        let hash = fnv_1a(chars.as_bytes());
        LoxString { obj, hash, chars }
    }
}

impl From<String> for LoxString {
    fn from(value: String) -> Self {
        let obj = LoxObj {
            obj_type: LoxObjType::String,
            next: std::ptr::null_mut(),
        };
        LoxString {
            obj,
            hash: fnv_1a(value.as_bytes()),
            chars: value,
        }
    }
}

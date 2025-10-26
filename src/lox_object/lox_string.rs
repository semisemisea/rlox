use std::ops::{Add, AddAssign};

use crate::object::{LoxObj, LoxObjType};

#[repr(C)]
pub struct LoxString {
    pub obj: LoxObj,
    pub chars: String,
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

// impl Add<&LoxString> for LoxString {
//     type Output = LoxString;
//     fn add(self, rhs: &LoxString) -> Self::Output {
//         let obj = LoxObj {
//             obj_type: LoxObjType::String,
//             next: std::ptr::null(),
//         };
//         let chars = self.chars.add(&rhs.chars);
//         LoxString { obj, chars }
//     }
// }

impl From<String> for LoxString {
    fn from(value: String) -> Self {
        let obj = LoxObj {
            obj_type: LoxObjType::String,
            next: std::ptr::null_mut(),
        };
        LoxString { obj, chars: value }
    }
}

use std::collections::hash_map::Entry;
use std::fmt::Debug;
use std::ptr::NonNull;

use crate::comp::hash_table::RLoxHashMapKey;
use crate::comp::vm::INTERNED_STRING;
use crate::gc;
use crate::lox_object::lox_function::LoxFunction;
use crate::lox_object::lox_string::LoxString;
use crate::object::{LoxObj, LoxObjType};

pub const NIL: Value = Value {
    v_type: ValueType::Nil,
    v_fill: Fillings { boolean: false },
};
pub const FALSE: Value = Value {
    v_type: ValueType::Boolean,
    v_fill: Fillings { boolean: false },
};
pub const TRUE: Value = Value {
    v_type: ValueType::Boolean,
    v_fill: Fillings { boolean: true },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ValueType {
    Boolean,
    #[default]
    Nil,
    Number,
    LoxObject,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub union Fillings {
    pub boolean: bool,
    pub number: f64,
    pub obj_ptr: *mut LoxObj,
}

impl Default for Fillings {
    fn default() -> Self {
        Self { boolean: false }
    }
}

impl Debug for Fillings {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Possible outcomes: boolean: {} number: {}",
            unsafe { self.boolean },
            unsafe { self.number }
        )
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Value {
    v_type: ValueType,
    v_fill: Fillings,
}

// WARNING: This version of rlox is single vm, single threaded version.
// So we can impl these unsafe trait for more use.
unsafe impl Send for Value {}
unsafe impl Sync for Value {}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        if self.v_type == other.v_type {
            if self.v_type == ValueType::LoxObject {
                let lhs = unsafe { self.v_fill.obj_ptr.as_ref().unwrap() };
                let rhs = unsafe { other.v_fill.obj_ptr.as_ref().unwrap() };
                return lhs == rhs;
            }
            let lhs = unsafe { std::mem::transmute::<Fillings, u64>(self.v_fill) };
            let rhs = unsafe { std::mem::transmute::<Fillings, u64>(other.v_fill) };
            lhs == rhs
        } else {
            false
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.v_type != other.v_type {
            return None;
        }
        match self.v_type {
            ValueType::Nil => None,
            ValueType::Boolean => unsafe { self.v_fill.boolean.partial_cmp(&other.v_fill.boolean) },
            ValueType::Number => unsafe { self.v_fill.number.partial_cmp(&other.v_fill.number) },
            ValueType::LoxObject => todo!(),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.v_type {
            ValueType::Nil => write!(f, "nil"),
            ValueType::Boolean => write!(f, "{}", unsafe { self.v_fill.boolean }),
            ValueType::Number => write!(f, "{}", unsafe { self.v_fill.number }),
            ValueType::LoxObject => {
                let obj_ptr = unsafe { self.v_fill.obj_ptr };
                let obj_type = unsafe { obj_ptr.as_ref().unwrap().obj_type };
                match obj_type {
                    LoxObjType::String => {
                        let casted_ptr = obj_ptr as *const LoxString;
                        write!(f, "\"{}\"", unsafe { &(*casted_ptr).chars })
                    }
                    LoxObjType::Function => {
                        let casted_ptr = obj_ptr as *const LoxFunction;
                        write!(f, "<fn {}>", unsafe { &(*(*casted_ptr).name).chars })
                    }
                }
            }
        }
    }
}

impl Value {
    pub fn mut_filling(&mut self) -> &mut Fillings {
        &mut self.v_fill
    }

    pub fn new_num(num: f64) -> Value {
        Value {
            v_type: ValueType::Number,
            v_fill: Fillings { number: num },
        }
    }

    pub fn new_bool(boolean: bool) -> Value {
        if boolean { TRUE } else { FALSE }
    }

    fn new_obj(obj_ptr: *const LoxObj) -> Value {
        match unsafe { (*obj_ptr).obj_type } {
            LoxObjType::String => gc::register(obj_ptr as *mut LoxString),
            LoxObjType::Function => gc::register(obj_ptr as *mut LoxFunction),
        }
        Value {
            v_type: ValueType::LoxObject,
            v_fill: Fillings {
                obj_ptr: obj_ptr as *mut LoxObj,
            },
        }
    }

    pub fn new_string<T: Into<LoxString>>(item: T) -> Value {
        // String intern.
        let lox_str = Box::into_raw(Box::new(item.into()));
        let entry_key = RLoxHashMapKey(NonNull::new(lox_str).unwrap());
        INTERNED_STRING.with_borrow_mut(|map| match map.entry(entry_key) {
            Entry::Vacant(entry) => {
                let val = Value::new_obj(lox_str as *mut LoxObj as *const LoxObj);
                *entry.insert(val)
            }
            Entry::Occupied(entry) => {
                unsafe { drop(Box::from_raw(lox_str)) };
                *entry.get()
            }
        })

        // Original logic.
        // Value::new_obj(Box::into_raw(Box::new(lox_str)) as *mut LoxObj)
    }

    // Definition.
    // pub struct LoxFunction<'a> {
    //     obj: LoxObj,
    //     arity: usize,
    //     chunk: Chunk,
    //     name: &'a LoxString,
    // }
    pub fn new_function(obj_ptr: *mut LoxFunction) -> Value {
        Value {
            v_type: ValueType::LoxObject,
            v_fill: Fillings {
                obj_ptr: obj_ptr as _,
            },
        }
    }

    #[inline(always)]
    pub fn type_of(&self) -> ValueType {
        self.v_type
    }

    #[inline(always)]
    pub fn is_nil(&self) -> bool {
        self.type_of() == ValueType::Nil
    }

    #[inline(always)]
    pub fn is_bool(&self) -> bool {
        self.type_of() == ValueType::Boolean
    }

    #[inline(always)]
    pub fn is_number(&self) -> bool {
        self.type_of() == ValueType::Number
    }

    #[inline(always)]
    pub fn is_object(&self) -> bool {
        self.type_of() == ValueType::LoxObject
    }

    pub fn is_string(&self) -> bool {
        self.is_object()
            && unsafe { self.as_object().as_ref() }.unwrap().obj_type == LoxObjType::String
    }

    pub fn is_function(&self) -> bool {
        self.is_object()
            && unsafe { self.as_object().as_ref() }.unwrap().obj_type == LoxObjType::Function
    }

    pub fn as_bool(&self) -> bool {
        debug_assert!(self.is_bool());
        unsafe { self.v_fill.boolean }
    }

    pub fn as_number(&self) -> f64 {
        debug_assert!(self.is_number());
        unsafe { self.v_fill.number }
    }

    pub fn as_object(&self) -> *const LoxObj {
        debug_assert!(self.is_object());
        unsafe { self.v_fill.obj_ptr }
    }

    pub fn as_mut_object(&self) -> *mut LoxObj {
        debug_assert!(self.is_object());
        unsafe { self.v_fill.obj_ptr }
    }

    pub fn as_obj_string(&self) -> &LoxString {
        debug_assert!(self.is_string());
        unsafe { (self.as_object() as *const LoxString).as_ref().unwrap() }
    }

    pub fn is_falsy(&self) -> bool {
        self.is_nil() || (self.is_bool() && !self.as_bool())
    }
}

use std::fmt::Debug;

use crate::object::LoxObj;

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

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        if self.v_type == other.v_type {
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
            ValueType::LoxObject => todo!(),
        }
    }
}

impl Value {
    pub fn change_type_to(&mut self, new_type: ValueType) {
        self.v_type = new_type
    }

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

    pub fn new_nil() -> Value {
        NIL
    }

    pub fn new_obj<T: Into<*mut LoxObj>>(obj_ptr: T) -> Value {
        Value {
            v_type: ValueType::LoxObject,
            v_fill: Fillings {
                obj_ptr: obj_ptr.into(),
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

    pub fn is_falsy(&self) -> bool {
        self.is_nil() || (self.is_bool() && !self.as_bool())
    }
}

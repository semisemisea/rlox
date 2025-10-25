use std::fmt::Debug;

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
}

#[derive(Clone, Copy)]
#[repr(C)]
pub union Fillings {
    pub boolean: bool,
    pub number: f64,
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
            "Possible outcomes: \nboolean: {}\nnumber: {}",
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

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.v_type {
            ValueType::Nil => write!(f, "nil"),
            ValueType::Boolean => write!(f, "{}", unsafe { self.v_fill.boolean }),
            ValueType::Number => write!(f, "{}", unsafe { self.v_fill.number }),
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

    pub fn as_bool(&self) -> bool {
        debug_assert!(self.is_bool());
        unsafe { self.v_fill.boolean }
    }

    pub fn as_number(&self) -> f64 {
        debug_assert!(self.is_number());
        unsafe { self.v_fill.number }
    }
}

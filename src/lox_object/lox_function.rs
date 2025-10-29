use crate::{
    comp::op_code::Chunk,
    lox_object::lox_string::LoxString,
    object::{LoxObj, LoxObjType},
    value::Value,
};

#[repr(C)]
#[derive(Debug)]
pub struct LoxFunction {
    pub obj: LoxObj,
    pub arity: usize,
    pub chunk: Chunk,
    pub name: *const LoxString,
}

#[derive(Debug)]
pub enum FuncType {
    Function,
    Script,
}

impl LoxFunction {
    pub fn new_fn(arity: usize, chunk: Chunk, name: String) -> LoxFunction {
        let obj = LoxObj {
            obj_type: LoxObjType::Function,
            next: std::ptr::null_mut(),
        };
        let name = Box::into_raw(Box::new(name.into())) as *const LoxString;

        LoxFunction {
            obj,
            arity,
            chunk,
            name,
        }
    }
}

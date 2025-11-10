use crate::{comp::op_code::Chunk, lox_object::lox_string::LoxString, object::LoxObj};

#[repr(C)]
#[derive(Debug)]
pub struct LoxFunction {
    pub obj: LoxObj,
    pub arity: u8,
    pub upvalue_cnt: u8,
    pub chunk: Chunk,
    pub name: *const LoxString,
}

#[derive(Debug)]
pub enum FuncType {
    Function,
    Script,
}

// impl LoxFunction {
//     pub fn new_fn(arity: u8, chunk: Chunk, name: String) -> LoxFunction {
//         let obj = LoxObj {
//             obj_type: LoxObjType::Function,
//             next: std::ptr::null_mut(),
//         };
//         let name = Box::into_raw(Box::new(name.into())) as *const LoxString;
//
//         LoxFunction {
//             obj,
//             arity,
//             chunk,
//             name,
//         }
//     }
// }

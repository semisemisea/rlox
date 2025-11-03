use crate::{
    lox_object::{lox_function::LoxFunction, lox_string::LoxString},
    object::LoxObj,
};

pub mod lox_function;
pub mod lox_string;

pub trait SpecifiedObject {
    fn next(&mut self) -> &mut *mut LoxObj;
}

impl SpecifiedObject for LoxFunction {
    fn next(&mut self) -> &mut *mut LoxObj {
        &mut self.obj.next
    }
}
impl SpecifiedObject for LoxString {
    fn next(&mut self) -> &mut *mut LoxObj {
        &mut self.obj.next
    }
}

use crate::{
    gc,
    lox_object::lox_function::LoxFunction,
    object::{LoxObj, LoxObjType},
};

#[repr(C)]
#[derive(Debug)]
pub struct LoxClosure {
    pub obj: LoxObj,
    pub func: *mut LoxFunction,
}

impl LoxClosure {
    pub fn raw_new(func: *mut LoxFunction) -> *mut LoxClosure {
        let obj = LoxObj {
            obj_type: LoxObjType::Closure,
            next: std::ptr::null_mut(),
        };
        let closure = LoxClosure { obj, func };
        let closure_ptr = Box::into_raw(Box::new(closure));
        gc::register(closure_ptr);
        closure_ptr
    }
}

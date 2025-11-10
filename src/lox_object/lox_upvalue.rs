use crate::{
    gc,
    object::{LoxObj, LoxObjType},
    value::Value,
};

#[derive(Debug)]
pub struct LoxUpvalue {
    pub obj: LoxObj,
    pub location: *mut Value,
}

impl LoxUpvalue {
    pub fn raw_new(slots: *mut Value) -> *const LoxUpvalue {
        let obj = LoxObj {
            obj_type: LoxObjType::Upvalue,
            next: std::ptr::null_mut(),
        };
        let upvalue_raw = Box::into_raw(Box::new(LoxUpvalue {
            obj,
            location: slots,
        }));
        gc::register(upvalue_raw);
        upvalue_raw
    }
}

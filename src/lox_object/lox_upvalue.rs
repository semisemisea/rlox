use crate::{
    gc,
    object::{LoxObj, LoxObjType},
    value::Value,
};

#[derive(Debug)]
pub struct LoxUpvalue {
    pub obj: LoxObj,
    pub location: *mut Value,
    pub closed: Value,
    pub next: *mut LoxUpvalue,
}

impl LoxUpvalue {
    pub fn raw_new(slots: *mut Value) -> *mut LoxUpvalue {
        let obj = LoxObj {
            obj_type: LoxObjType::Upvalue,
            next: std::ptr::null_mut(),
        };
        let upvalue_raw = Box::into_raw(Box::new(LoxUpvalue {
            obj,
            location: slots,
            closed: Value::new_nil(),
            next: std::ptr::null_mut(),
        }));
        gc::register(upvalue_raw);
        upvalue_raw
    }
}

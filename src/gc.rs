use std::cell::RefCell;

use crate::{
    lox_object::lox_string::LoxString,
    object::{LoxObj, LoxObjType},
};

pub fn free_objects() {
    let mut p_obj = OBJ_HEAD_PTR.with_borrow(|x| *x);
    while !p_obj.is_null() {
        let next = unsafe { p_obj.read().next };
        free_object(p_obj);
        p_obj = next;
    }
}

fn free_object(p_obj: *mut LoxObj) {
    match unsafe { p_obj.read().obj_type } {
        LoxObjType::String => unsafe {
            // NOTE: Then it goes out of the scope and eventually get droped.
            let _ = Box::from_raw(p_obj as *mut LoxString);
        },
    }
}

thread_local! {
    pub static OBJ_HEAD_PTR: RefCell<*mut LoxObj> = const{ RefCell::new(std::ptr::null_mut())}
}

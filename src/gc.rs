use std::cell::RefCell;

use crate::{
    lox_object::{lox_function::LoxFunction, lox_string::LoxString},
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
    // NOTE: Object will be boxed again and goes out of the scope and eventually get droped.
    match unsafe { (*p_obj).obj_type } {
        LoxObjType::String => unsafe {
            let _ = Box::from_raw(p_obj as *mut LoxString);
        },
        LoxObjType::Function => unsafe {
            let func_obj = p_obj as *mut LoxFunction;
            free_object((*func_obj).name as *mut LoxObj);
            let _ = Box::from_raw(func_obj);
        },
    }
}

pub fn register(obj_ptr: *mut LoxObj) {
    OBJ_HEAD_PTR.with_borrow_mut(|vm_obj_ptr_head| {
        unsafe { obj_ptr.as_mut().unwrap().next = *vm_obj_ptr_head }
        *vm_obj_ptr_head = obj_ptr as *mut LoxObj;
    });
}

thread_local! {
    pub static OBJ_HEAD_PTR: RefCell<*mut LoxObj> = const{ RefCell::new(std::ptr::null_mut())}
}

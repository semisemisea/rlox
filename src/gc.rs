use crate::{
    lox_object::{
        SpecifiedObject, lox_closure::LoxClosure, lox_function::LoxFunction, lox_string::LoxString,
        lox_upvalue::LoxUpvalue,
    },
    object::{LoxObj, LoxObjType},
};
use std::cell::{Cell, RefCell};

thread_local! {
    static OBJ_CNT: Cell<usize> = const{Cell::new(0)};
}

pub fn show_all_objects() {
    println!();
    println!("----HEAP OBJECTS----");
    let mut p_obj = OBJ_HEAD_PTR.with_borrow(|x| *x);
    while !p_obj.is_null() {
        println!();
        match unsafe { (*p_obj).obj_type } {
            LoxObjType::String => {
                let str_obj = p_obj as *mut LoxString;
                print!("String   Object at {:<12p}:  ", str_obj);
                print!("next obj: {:<12p}", unsafe { (*str_obj).obj.next });

                println!();
                print!("content: {:?}", unsafe { &(*str_obj).chars });
                println!();
            }
            LoxObjType::Function => {
                let func_obj = p_obj as *mut LoxFunction;
                print!("Function Object at {:<12p}:  ", func_obj);
                print!("next obj: {:<12p}  ", unsafe { (*func_obj).obj.next });
                println!();
                print!("<fn {}>  ", unsafe { &(*(*func_obj).name).chars });
                print!("arity: {}  ", unsafe { &(*func_obj).arity });
                println!()
            }
            LoxObjType::Closure => {
                let closure_obj = p_obj as *mut LoxClosure;
                print!("Closure Object at {:<12p}:  ", closure_obj);
                print!("next obj: {:<12p}", unsafe { (*closure_obj).obj.next });
                println!();
                // TODO:
                println!("TODO");
                // todo!("Function showing and captured variable.");
            }
            LoxObjType::Native => todo!(),
            LoxObjType::Upvalue => {
                let upvalue_obj = p_obj as *mut LoxUpvalue;
                print!("Upvalue Object at {:<12p}:  ", upvalue_obj);
                print!("next obj: {:<12p}", unsafe { (*upvalue_obj).obj.next });
                println!();
                // TODO:
                println!("TODO");
            }
        }
        p_obj = unsafe { p_obj.read().next };
    }
    println!();
    println!("--------------------");
}

pub fn free_objects() {
    let mut p_obj = OBJ_HEAD_PTR.with_borrow(|x| *x);
    while !p_obj.is_null() {
        let next = unsafe { p_obj.read().next };
        free_object(p_obj);
        p_obj = next;
    }
    debug_assert!(OBJ_CNT.with(|x| x.take() == 0));
}
/// NOTE: Object will be boxed again and goes out of the scope and eventually get droped.
fn free_object(p_obj: *mut LoxObj) {
    #[cfg(debug_assertions)]
    println!("Ready to free object at {:p}", p_obj);
    OBJ_CNT.with(|x| x.update(|x| x - 1));
    match unsafe { (*p_obj).obj_type } {
        LoxObjType::String => unsafe {
            let _ = Box::from_raw(p_obj as *mut LoxString);
        },
        LoxObjType::Function => unsafe {
            let _ = Box::from_raw(p_obj as *mut LoxFunction);
        },
        LoxObjType::Closure => unsafe {
            let _ = Box::from_raw(p_obj as *mut LoxClosure);
        },
        LoxObjType::Native => todo!(),
        LoxObjType::Upvalue => unsafe {
            let _ = Box::from_raw(p_obj as *mut LoxUpvalue);
        },
    }
}

pub fn register<T: SpecifiedObject>(obj_ptr: *mut T) {
    #[cfg(debug_assertions)]
    println!("Register a object at {:<12p}", obj_ptr);
    OBJ_CNT.with(|x| x.update(|x| x + 1));
    OBJ_HEAD_PTR.with_borrow_mut(|vm_obj_ptr_head| {
        let next_obj = unsafe { (*obj_ptr).next() };
        *next_obj = *vm_obj_ptr_head;
        *vm_obj_ptr_head = obj_ptr as *mut LoxObj;
    });
}

thread_local! {
    pub static OBJ_HEAD_PTR: RefCell<*mut LoxObj> = const{ RefCell::new(std::ptr::null_mut())}
}

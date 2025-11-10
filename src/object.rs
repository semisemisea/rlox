use crate::lox_object::lox_string::LoxString;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LoxObjType {
    String,
    Function,
    Closure,
    Upvalue,
    // temporary unused
    #[allow(unused)]
    Native,
}

#[derive(Debug)]
#[repr(C)]
pub struct LoxObj {
    pub obj_type: LoxObjType,
    pub next: *mut LoxObj,
}

impl PartialEq for LoxObj {
    fn eq(&self, other: &Self) -> bool {
        if self.obj_type != other.obj_type {
            return false;
        }
        match self.obj_type {
            LoxObjType::String => {
                let lhs = unsafe {
                    (self as *const LoxObj as *const LoxString)
                        .as_ref()
                        .unwrap()
                };
                let rhs = unsafe {
                    (other as *const LoxObj as *const LoxString)
                        .as_ref()
                        .unwrap()
                };
                lhs == rhs
            }
            LoxObjType::Upvalue
            | LoxObjType::Native
            | LoxObjType::Closure
            | LoxObjType::Function => todo!(),
        }
    }
}

pub enum LoxObjType {
    String,
}

#[repr(C)]
pub struct LoxObj {
    obj_type: LoxObjType,
}

#[repr(C)]
pub struct LoxString {
    obj: LoxObj,
    chars: String,
}

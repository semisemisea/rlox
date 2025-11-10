use std::cell::RefCell;
use std::collections::HashMap;

use num_enum::UnsafeFromPrimitive;

use crate::comp::hash_table::RLoxHashMapKey;
use crate::comp::op_code::OpCode;
use crate::lox_object::lox_closure::LoxClosure;
use crate::lox_object::lox_string::LoxString;
use crate::lox_object::lox_upvalue::LoxUpvalue;
use crate::object::LoxObjType;
use crate::value::{self, Value, ValueType};

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * u8::MAX as usize;

#[derive(Debug, thiserror::Error)]
pub enum RuntimeError {
    #[error("Token should be {should_be:?} but is actually {actual}")]
    TypeError {
        // line: usize,
        should_be: ValueType,
        actual: Value,
    },
    #[error("Undefined variable {var_name}")]
    UndefinedVariable { var_name: String },
    #[error("Only functions and classes can be called. Trying to call {0}")]
    InvalidCall(Value),
    #[error("Expect arguments count to be {arity} but actually {argc}")]
    ArityDismatch { arity: u8, argc: u8 },
    #[error("Stack Overflow.")]
    StackOverflow,
}

thread_local! {
    pub static INTERNED_STRING: RefCell<HashMap<RLoxHashMapKey, Value>> = RefCell::new(HashMap::new());
}

#[derive(Debug)]
pub struct VM {
    frames: [CallFrame; 64],
    frame_count: usize,
    stack: [Value; STACK_MAX],
    stack_top: *mut Value,
    globals: HashMap<RLoxHashMapKey, Value>,
    open_upvalues: *mut LoxUpvalue,
}

#[derive(Debug, Default, Clone, Copy)]
struct CallFrame {
    closure: *mut LoxClosure,
    ip: *const u8,
    slots: *mut Value,
}

macro_rules! type_error {
    ($expect:expr, $actual:expr) => {
        return Err(RuntimeError::TypeError {
            should_be: $expect,
            actual: $actual,
        })
    };
}

macro_rules! simple_type_check {
    ($self:expr, $dist:expr, $exp_type:expr) => {
        if $self.peek($dist).type_of() != $exp_type {
            type_error!($exp_type, $self.peek($dist))
        }
    };
}

impl VM {
    pub fn new() -> VM {
        VM {
            frames: [CallFrame::default(); FRAMES_MAX],
            frame_count: 0,
            stack: [Value::default(); STACK_MAX],
            stack_top: std::ptr::null_mut(),
            globals: HashMap::new(),
            open_upvalues: std::ptr::null_mut(),
        }
    }

    pub fn init_vm(&mut self, main_func: *const LoxClosure) {
        self.reset_stack();
        let script_val = Value::new_closure(main_func.cast_mut());
        self.push(script_val);
        self.frames[0] = CallFrame {
            closure: script_val.as_closure(),
            ip: unsafe { (*(*main_func).func).chunk.code_top_ptr() },
            slots: self.stack.as_mut_ptr(),
        };
        self.frame_count = 1;
    }

    fn read_u8(&mut self) -> u8 {
        let ip = self.ip();
        let ret = unsafe { **ip };
        unsafe {
            *ip = ip.add(1);
        }
        ret
    }

    fn read_u16(&mut self) -> u16 {
        let ip = self.ip();
        let mut ret = unsafe { **ip as u16 } << 8;
        unsafe {
            *ip = ip.add(1);
        }
        ret |= unsafe { **ip as u16 };
        unsafe {
            *ip = ip.add(1);
        }
        ret
    }

    fn ip(&mut self) -> &mut *const u8 {
        &mut self.current_frame_mut().ip
    }

    fn ip_move_forward(&mut self, step: usize) {
        let ip = self.ip();
        *ip = unsafe { ip.add(step) };
    }

    fn ip_move_backward(&mut self, step: usize) {
        let ip = self.ip();
        *ip = unsafe { ip.sub(step) };
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        &mut self.frames[self.frame_count - 1]
    }

    fn current_frame(&self) -> &CallFrame {
        &self.frames[self.frame_count - 1]
    }

    fn peek(&self, distance: usize) -> Value {
        unsafe { self.stack_top.sub(1 + distance).read() }
        // unsafe { self.current_frame().slots.add(distance).read() }
    }

    fn read_constant(&mut self) -> Value {
        let idx = self.read_u8() as usize;
        let chunk = unsafe { &(*(*self.current_frame_mut().closure).func).chunk };
        chunk.constants()[idx]
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        loop {
            let chunk = unsafe { &(*(*self.current_frame_mut().closure).func).chunk };
            #[cfg(debug_assertions)]
            chunk.show_one_op_code(&mut self.ip().clone());
            let op = unsafe { OpCode::unchecked_transmute_from(self.read_u8()) };
            match op {
                OpCode::Return => {
                    let result = self.pop();
                    let return_frame_slot = self.current_frame().slots;
                    self.close_upvalue(self.current_frame().slots);
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.pop();
                        break;
                    }
                    self.stack_top = return_frame_slot;
                    self.push(result);
                }
                OpCode::Constant => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                OpCode::ConstantLong => todo!(),
                OpCode::Negate => unsafe {
                    let top_val = self.stack_top.sub(1);
                    if !(*top_val).is_number() {
                        return Err(RuntimeError::TypeError {
                            should_be: ValueType::Number,
                            actual: (*top_val),
                        });
                    }
                    let filling = (*top_val).mut_filling();
                    filling.number = -filling.number
                },
                OpCode::Add => unsafe {
                    let mut rhs = self.pop();
                    if rhs.is_number() {
                        simple_type_check!(self, 0, ValueType::Number);
                        let rhs_filling = rhs.mut_filling();
                        let lhs_filling = (*self.stack_top.sub(1)).mut_filling();
                        lhs_filling.number += rhs_filling.number;
                    }
                    if rhs.is_string() {
                        let lhs = self.pop();
                        if !lhs.is_string() {
                            type_error!(ValueType::LoxObject, lhs);
                        }
                        let rhs = (rhs.as_mut_object() as *mut LoxString).as_ref().unwrap();
                        let lhs = (lhs.as_mut_object() as *mut LoxString).read();
                        let ret = Value::new_string(lhs + rhs);
                        self.push(ret);
                    }
                },
                OpCode::Subtract => unsafe {
                    simple_type_check!(self, 0, ValueType::Number);
                    simple_type_check!(self, 1, ValueType::Number);
                    let mut rhs = self.pop();
                    let rhs_filling = rhs.mut_filling();
                    let lhs_filling = (*self.stack_top.sub(1)).mut_filling();
                    lhs_filling.number -= rhs_filling.number;
                },
                OpCode::Multiply => unsafe {
                    simple_type_check!(self, 0, ValueType::Number);
                    simple_type_check!(self, 1, ValueType::Number);
                    let mut rhs = self.pop();
                    let rhs_filling = rhs.mut_filling();
                    let lhs_filling = (*self.stack_top.sub(1)).mut_filling();
                    lhs_filling.number *= rhs_filling.number;
                },
                OpCode::Divide => unsafe {
                    simple_type_check!(self, 0, ValueType::Number);
                    simple_type_check!(self, 1, ValueType::Number);
                    let mut rhs = self.pop();
                    let rhs_filling = rhs.mut_filling();
                    let lhs_filling = (*self.stack_top.sub(1)).mut_filling();
                    lhs_filling.number /= rhs_filling.number;
                },
                OpCode::Nil => {
                    self.push(value::NIL);
                }
                OpCode::True => {
                    self.push(value::TRUE);
                }
                OpCode::False => {
                    self.push(value::FALSE);
                }
                OpCode::Not => {
                    let item = self.pop();
                    self.push(Value::new_bool(item.is_falsy()));
                }
                OpCode::Equal => {
                    let rhs = self.pop();
                    simple_type_check!(self, 0, rhs.type_of());
                    let lhs = self.pop();
                    self.push(Value::new_bool(lhs == rhs));
                }
                OpCode::Less => {
                    let rhs = self.pop();
                    simple_type_check!(self, 0, rhs.type_of());
                    let lhs = self.pop();
                    self.push(Value::new_bool(lhs < rhs));
                }
                OpCode::Greater => {
                    let rhs = self.pop();
                    simple_type_check!(self, 0, rhs.type_of());
                    let lhs = self.pop();
                    self.push(Value::new_bool(lhs > rhs));
                }
                OpCode::Print => {
                    println!("{}", self.pop());
                }
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::DefGlob => {
                    let idx = self.read_u8();
                    let var_name = chunk.constants()[idx as usize].as_obj_string();
                    let map_key = RLoxHashMapKey(var_name.into());
                    let item = self.peek(0);
                    self.globals.insert(map_key, item);
                }
                OpCode::GetGlob => {
                    let idx = self.read_u8();
                    let var_name = chunk.constants()[idx as usize].as_obj_string();
                    let map_key = RLoxHashMapKey(var_name.into());
                    let Some(val) = self.globals.get(&map_key) else {
                        return Err(RuntimeError::UndefinedVariable {
                            var_name: var_name.chars.clone(),
                        });
                    };
                    self.push(*val);
                }
                OpCode::SetGlob => {
                    let idx = self.read_u8();
                    let var_name = chunk.constants()[idx as usize].as_obj_string();
                    let map_key = RLoxHashMapKey(var_name.into());
                    let item = self.peek(0);
                    let Some(val) = self.globals.get_mut(&map_key) else {
                        return Err(RuntimeError::UndefinedVariable {
                            var_name: var_name.chars.clone(),
                        });
                    };
                    *val = item;
                }
                OpCode::SetLocal => {
                    let slot = self.read_u8();
                    unsafe {
                        self.current_frame()
                            .slots
                            .add(slot as _)
                            .write(self.peek(0))
                    };
                }
                OpCode::GetLocal => {
                    let slot = self.read_u8();
                    unsafe { self.push(self.current_frame().slots.add(slot as usize).read()) };
                    // self.push(self.stack[slot as usize]);
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_u16();
                    if self.peek(0).is_falsy() {
                        self.ip_move_forward(offset as usize);
                    }
                }
                OpCode::Jump => {
                    let offset = self.read_u16();
                    self.ip_move_forward(offset as usize);
                }
                OpCode::Loop => {
                    let offset = self.read_u16();
                    self.ip_move_backward(offset as usize);
                }
                OpCode::Call => {
                    let argc = self.read_u8();
                    self.call_func(self.peek(argc as usize), argc)?;
                }
                OpCode::Closure => {
                    let closure_raw = self.read_constant().as_closure();
                    let closure = Value::new_closure(closure_raw);
                    self.push(closure);
                    for i in 0..unsafe { (*(*closure_raw).func).upvalue_cnt as usize } {
                        let is_local = self.read_u8() == 1;
                        let index = self.read_u8() as usize;
                        unsafe {
                            closure.as_closure().as_mut().unwrap().upvalues[i] = if is_local {
                                self.capture_upvalue(index)
                                // LoxUpvalue::raw_new(self.current_frame().slots.add(index))
                            } else {
                                self.current_frame().closure.as_ref().unwrap().upvalues[index]
                            }
                        }
                    }
                }
                OpCode::GetUpvalue => {
                    let slot = self.read_u8() as usize;
                    unsafe {
                        let item = *(*self.current_frame().closure.as_ref().unwrap().upvalues
                            [slot])
                            .location;
                        self.push(item)
                    }
                }
                OpCode::SetUpvalue => {
                    let slot = self.read_u8() as usize;
                    unsafe {
                        *(*self.current_frame_mut().closure.as_mut().unwrap().upvalues[slot])
                            .location = self.peek(0);
                    }
                }
                OpCode::CloseUpvalue => {
                    self.close_upvalue(unsafe { self.stack_top.sub(1) });
                    self.pop();
                }
            }
        }
        Ok(())
    }

    fn close_upvalue(&mut self, last: *mut Value) {
        unsafe {
            while !self.open_upvalues.is_null() && (*self.open_upvalues).location >= last {
                let upvalue = self.open_upvalues;
                (*upvalue).closed = *(*upvalue).location;
                (*upvalue).location = &mut (*upvalue).closed;
                self.open_upvalues = (*upvalue).next;
            }
        }
    }

    fn capture_upvalue(&mut self, index: usize) -> *mut LoxUpvalue {
        unsafe {
            let local = self.current_frame().slots.add(index);
            let mut prev_upvalue = std::ptr::null_mut();
            let mut upvalue = self.open_upvalues;
            while !upvalue.is_null() && (*upvalue).location > local {
                prev_upvalue = upvalue;
                upvalue = (*upvalue).next;
            }
            if !upvalue.is_null() && (*upvalue).location == local {
                return upvalue;
            }
            let created_upvalue = LoxUpvalue::raw_new(local);
            (*created_upvalue).next = upvalue;
            if prev_upvalue.is_null() {
                self.open_upvalues = created_upvalue;
            } else {
                (*prev_upvalue).next = created_upvalue;
            }
            created_upvalue
        }
    }

    fn call_func(&mut self, callee: Value, argc: u8) -> Result<(), RuntimeError> {
        match callee.obj_type() {
            LoxObjType::Closure => {
                let func_ptr = callee.as_closure();
                return self.call(func_ptr, argc);
            }
            LoxObjType::Native => todo!(),
            _ => {}
        }
        Err(RuntimeError::InvalidCall(callee))
    }

    fn call(&mut self, closure: *mut LoxClosure, argc: u8) -> Result<(), RuntimeError> {
        let func = unsafe { (*closure).func };
        if argc != unsafe { (*func).arity } {
            return Err(RuntimeError::ArityDismatch {
                arity: unsafe { (*func).arity },
                argc,
            });
        }
        if self.frame_count == FRAMES_MAX {
            return Err(RuntimeError::StackOverflow);
        }
        let frame = &mut self.frames[self.frame_count];
        frame.closure = closure;
        frame.ip = unsafe { (*func).chunk.code_top_ptr() };
        frame.slots = unsafe { self.stack_top.sub(argc as usize + 1) };
        self.frame_count += 1;
        Ok(())
    }

    pub fn reset_stack(&mut self) {
        self.stack_top = self.stack.as_mut_ptr();
    }

    fn pop(&mut self) -> Value {
        unsafe {
            self.stack_top = self.stack_top.sub(1);
            debug_assert!(self.stack_top >= self.stack.as_mut_ptr());
            *self.stack_top
        }
    }

    fn push(&mut self, value: Value) {
        unsafe {
            *self.stack_top = value;
            self.stack_top = self.stack_top.add(1);
        }
    }

    pub fn show(&self) {
        println!("Find error. Showing stack.");
        println!();
        println!();
        println!();
        println!("----  Stack Info  ----");
        let mut slot = self.stack.as_ptr();
        while slot < self.stack_top {
            print!("[ ");
            unsafe {
                print!("{}", *slot);
                slot = slot.add(1);
            }
            print!(" ]");
            println!();
        }
        println!("---- End of stack ----");
    }
}

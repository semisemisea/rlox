use std::cell::RefCell;
use std::collections::HashMap;

use num_enum::UnsafeFromPrimitive;

use crate::comp::hash_table::RLoxHashMapKey;
use crate::comp::op_code::OpCode;
use crate::lox_object::lox_function::LoxFunction;
use crate::lox_object::lox_string::LoxString;
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
    #[error("UndefinedVariable {var_name}")]
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
}

#[derive(Debug, Default, Clone, Copy)]
struct CallFrame {
    func: *mut LoxFunction,
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
        }
    }

    pub fn init_vm(&mut self, main_func: *const LoxFunction) {
        self.reset_stack();
        let script_val = Value::new_function(main_func.cast_mut());
        self.push(script_val);
        self.frames[0] = CallFrame {
            func: main_func.cast_mut(),
            ip: unsafe { (*main_func).chunk.code_top_ptr() },
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
        let chunk = unsafe { &(*self.current_frame_mut().func).chunk };
        chunk.constants()[idx]
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        loop {
            let chunk = unsafe { &(*self.current_frame_mut().func).chunk };
            #[cfg(debug_assertions)]
            chunk.show_one_op_code(&mut self.ip().clone());
            let op = unsafe { OpCode::unchecked_transmute_from(self.read_u8()) };
            match op {
                OpCode::Return => {
                    let result = self.pop();

                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.pop();
                        break;
                    }
                    self.stack_top = self.current_frame_mut().slots;
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
                // BUG: Misuse of OpCode
                OpCode::SetLocal => {
                    let slot = self.read_u8();
                    // BUG: slot and index is confused
                    self.stack[slot as usize] = self.peek(0);
                }
                OpCode::GetLocal => {
                    let slot = self.read_u8();
                    unsafe { self.push(self.current_frame().slots.add(1 + slot as usize).read()) };
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
            }
        }
        Ok(())
    }

    fn call_func(&mut self, callee: Value, argc: u8) -> Result<(), RuntimeError> {
        if callee.is_function() {
            let func_ptr = callee.as_object() as *mut LoxFunction;
            return self.call(func_ptr, argc);
        }
        Err(RuntimeError::InvalidCall(callee))
    }

    fn call(&mut self, func: *mut LoxFunction, argc: u8) -> Result<(), RuntimeError> {
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
        frame.func = func;
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

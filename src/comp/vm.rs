use std::cell::RefCell;
use std::collections::HashMap;

use num_enum::UnsafeFromPrimitive;

use crate::comp::hash_table::RLoxHashMapKey;
use crate::comp::op_code::{Chunk, OpCode};
use crate::lox_object::lox_string::LoxString;
use crate::value::{self, Value, ValueType};

const STACK_MAX: usize = 2048;

#[derive(Debug)]
pub enum RuntimeError {
    TypeError {
        // line: usize,
        should_be: ValueType,
        actual: ValueType,
    },
    UndefinedVariable {
        var_name: String,
    },
}

thread_local! {
    pub static INTERNED_STRING: RefCell<HashMap<RLoxHashMapKey, Value>> = RefCell::new(HashMap::new());
}

pub struct VM {
    chunk: Chunk,
    ip: *const u8,
    stack: [Value; STACK_MAX],
    stack_top: *mut Value,
    globals: HashMap<RLoxHashMapKey, Value>,
}

// unsafe impl Send for VM {}
// unsafe impl Sync for VM {}

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
            type_error!($exp_type, $self.peek($dist).type_of())
        }
    };
}

impl VM {
    pub fn new(chunk: Chunk) -> VM {
        VM {
            chunk,
            ip: std::ptr::null(),
            stack: [Value::default(); STACK_MAX],
            stack_top: std::ptr::null_mut(),
            globals: HashMap::new(),
        }
    }

    pub fn init_vm(&mut self) {
        self.reset_stack();
        self.ip = self.chunk.code_top_ptr();
    }

    fn read_byte(&mut self) -> u8 {
        let ret = unsafe { *self.ip };
        unsafe {
            self.ip = self.ip.add(1);
        }
        ret
    }

    fn peek(&self, distance: usize) -> Value {
        unsafe { self.stack_top.sub(1 + distance).read() }
    }

    fn read_constant(&mut self) -> Value {
        let idx = self.read_byte() as usize;
        self.chunk.constants()[idx]
    }

    pub fn dbg_print(&self) {
        self.chunk.dbg_print();
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        loop {
            self.chunk.show_one_op_code(&mut self.ip.clone());
            let op = unsafe { OpCode::unchecked_transmute_from(self.read_byte()) };
            match op {
                OpCode::Return => {
                    break;
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
                            actual: (*top_val).type_of(),
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
                            type_error!(ValueType::LoxObject, lhs.type_of());
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
                    let idx = self.read_byte();
                    let var_name = self.chunk.constants()[idx as usize].as_obj_string();
                    let map_key = RLoxHashMapKey(var_name.into());
                    let item = self.peek(0);
                    self.globals.insert(map_key, item);
                }
                OpCode::GetGlob => {
                    let idx = self.read_byte();
                    let var_name = self.chunk.constants()[idx as usize].as_obj_string();
                    let map_key = RLoxHashMapKey(var_name.into());
                    let Some(val) = self.globals.get(&map_key) else {
                        return Err(RuntimeError::UndefinedVariable {
                            var_name: var_name.chars.clone(),
                        });
                    };
                    self.push(*val);
                }
                OpCode::SetGlob => {
                    let idx = self.read_byte();
                    let var_name = self.chunk.constants()[idx as usize].as_obj_string();
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
                    let slot = self.read_byte();
                    self.push(self.stack[slot as usize]);
                }
                OpCode::GetLocal => {
                    let slot = self.read_byte();
                    self.stack[slot as usize] = self.peek(0);
                }
            }
        }
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

    fn show(&self) {
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

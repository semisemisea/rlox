use crate::comp::op_code::{Chunk, OpCode};
use crate::value::{self, Value, ValueType};

const STACK_MAX: usize = 2048;

#[derive(Debug)]
pub enum RuntimeError {
    TypeError {
        // line: usize,
        should_be: ValueType,
        actual: ValueType,
    },
}

pub struct VM {
    chunk: Chunk,
    ip: *const u8,
    stack: [Value; STACK_MAX],
    stack_top: *mut Value,
}

macro_rules! type_check {
    ($self:expr, $dist:expr, $($exp_type:expr),+ $(,)?) => {
        $(
            if $self.peek($dist).type_of() != $exp_type {
                return Err(RuntimeError::TypeError {
                    should_be: $exp_type,
                    actual: $self.peek($dist).type_of(),
                });
            }
        )+
    };
}

impl VM {
    pub fn new(chunk: Chunk) -> VM {
        let mut vm = VM {
            chunk,
            ip: std::ptr::null(),
            stack: [Value::default(); STACK_MAX],
            stack_top: std::ptr::null_mut(),
        };
        vm.ip = vm.chunk.code_top_ptr();
        vm
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
            let op = self.read_byte().into();
            match op {
                OpCode::Return => {
                    println!("print the stack_top {}", self.pop());
                    break;
                }
                OpCode::Constant => {
                    let constant = self.read_constant();
                    self.push(constant);
                    println!("{constant}");
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
                    type_check!(self, 0, ValueType::Number);
                    type_check!(self, 1, ValueType::Number);
                    let mut rhs = self.pop();
                    let rhs_filling = rhs.mut_filling();
                    let lhs_filling = (*self.stack_top.sub(1)).mut_filling();
                    lhs_filling.number += rhs_filling.number;
                },
                OpCode::Subtract => unsafe {
                    type_check!(self, 0, ValueType::Number);
                    type_check!(self, 1, ValueType::Number);
                    let mut rhs = self.pop();
                    let rhs_filling = rhs.mut_filling();
                    let lhs_filling = (*self.stack_top.sub(1)).mut_filling();
                    lhs_filling.number -= rhs_filling.number;
                },
                OpCode::Multiply => unsafe {
                    type_check!(self, 0, ValueType::Number);
                    type_check!(self, 1, ValueType::Number);
                    let mut rhs = self.pop();
                    let rhs_filling = rhs.mut_filling();
                    let lhs_filling = (*self.stack_top.sub(1)).mut_filling();
                    lhs_filling.number *= rhs_filling.number;
                },
                OpCode::Divide => unsafe {
                    type_check!(self, 0, ValueType::Number);
                    type_check!(self, 1, ValueType::Number);
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
                    type_check!(self, 0, rhs.type_of());
                    let lhs = self.pop();
                    self.push(Value::new_bool(lhs == rhs));
                }
                OpCode::Less => {
                    let rhs = self.pop();
                    type_check!(self, 0, rhs.type_of());
                    let lhs = self.pop();
                    self.push(Value::new_bool(lhs < rhs));
                }
                OpCode::Greater => {
                    let rhs = self.pop();
                    type_check!(self, 0, rhs.type_of());
                    let lhs = self.pop();
                    self.push(Value::new_bool(lhs > rhs));
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

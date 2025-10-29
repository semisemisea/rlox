use crate::value::Value;
use num_enum::{IntoPrimitive, UnsafeFromPrimitive};

///Grammar:
///
///
#[repr(u8)]
#[derive(Debug, Clone, Copy, IntoPrimitive, UnsafeFromPrimitive)]
pub enum OpCode {
    Return = 0,
    Constant,
    ConstantLong,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Nil,
    True,
    False,
    Not,
    Equal,
    Less,
    Greater,
    Print,
    Pop,
    DefGlob,
    GetGlob,
    SetGlob,
    GetLocal,
    SetLocal,
    JumpIfFalse,
    Jump,
    Loop,
}

#[derive(Debug, Default)]
pub struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

// #[macro_export]
// macro_rules! write_in {
//     ($chunk:expr, $byte: expr) => {
//         $chunk.write_in($byte as u8);
//     };
//     ($chunk:expr, $byte: expr, $($rest:expr),+ $(,)?) => {
//         $chunk.write_in($byte);
//         write_in!($chunk, $($rest:expr),+);
//     };
// }

impl Chunk {
    pub fn constants(&self) -> &Vec<Value> {
        &self.constants
    }

    pub fn code_top_ptr(&self) -> *const u8 {
        self.code.as_ptr()
    }

    pub fn new() -> Self {
        Self::default()
    }

    pub fn ip_len(&self) -> usize {
        self.code.len()
    }

    pub fn modify(&mut self, loc: usize, into: u8) {
        self.code[loc] = into;
    }

    pub fn write_in(&mut self, byte: u8, line_no: usize) {
        self.code.push(byte);
        if line_no >= self.lines.len() {
            self.lines.resize(line_no + 1, 0);
        }
        debug_assert!(
            self.lines[line_no] < self.code.len(),
            "l:{} r:{}",
            self.lines[line_no],
            self.code.len() - 1
        );
        self.lines[line_no] = self.code.len() - 1;
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    fn get_line(&self, offset: usize) -> usize {
        self.lines.partition_point(|&x| x <= offset) - 1
    }

    fn show_constants(&self) {
        println!();
        println!("-----Constant area-----");
        for (idx, val) in self.constants.iter().enumerate() {
            println!("{} {}", idx, val);
        }
        println!("-----Constant end------");
        println!();
    }

    pub fn show_one_op_code(&self, ip: &mut *const u8) {
        let op = unsafe { **ip };
        match unsafe { OpCode::unchecked_transmute_from(op) } {
            OpCode::Return => {
                print!("{:<20}", "OP_RETURN:");
            }
            OpCode::Constant => {
                print!("{:<20}", "OP_CONSTANT:");
                unsafe { *ip = ip.add(1) }
                let idx = unsafe { **ip };
                print!("{:>4}    {}", idx, self.constants[idx as usize]);
            }
            OpCode::ConstantLong => {
                print!("{:<20}", "OP_CONSTANT_LONG:");
                unsafe { *ip = ip.add(1) }
                let mut idx = (unsafe { **ip } as u32) << 16;
                unsafe { *ip = ip.add(1) }
                idx |= (unsafe { **ip } as u32) << 8;
                unsafe { *ip = ip.add(1) }
                idx |= unsafe { **ip } as u32;
                print!("{:>4}    {}", idx, self.constants[idx as usize].as_number());
            }
            OpCode::Negate => print!("{:<20}", "OP_NEGATE:"),
            OpCode::Add => print!("{:<20}", "OP_ADD:"),
            OpCode::Subtract => print!("{:<20}", "OP_SUBTRACT:"),
            OpCode::Multiply => print!("{:<20}", "OP_MULTIPLY:"),
            OpCode::Divide => print!("{:<20}", "OP_DIVIDE:"),
            OpCode::Nil => print!("{:<20}", "LITERAL_NIL:"),
            OpCode::True => print!("{:<20}", "LITERAL_TRUE:"),
            OpCode::False => print!("{:<20}", "LITERAL_FALSE:"),
            OpCode::Not => print!("{:<20}", "OP_NOT:"),
            OpCode::Equal => print!("{:<20}", "OP_EQUAL:"),
            OpCode::Less => print!("{:<20}", "OP_LESS:"),
            OpCode::Greater => print!("{:<20}", "OP_GREATER:"),
            OpCode::Print => print!("{:<20}", "OP_PRINT:"),
            OpCode::Pop => print!("{:<20}", "OP_POP:"),
            OpCode::DefGlob => {
                print!("{:<20}", "OP_DEFINE_GLOBAL:");
                unsafe { *ip = ip.add(1) }
                let idx = unsafe { **ip };
                print!("{:>4}    {}", idx, self.constants[idx as usize]);
            }
            OpCode::GetGlob => {
                print!("{:<20}", "OP_GET_GLOBAL:");
                unsafe { *ip = ip.add(1) }
                let idx = unsafe { **ip };
                print!("{:>4}    {}", idx, self.constants[idx as usize]);
            }
            OpCode::SetGlob => {
                print!("{:<20}", "OP_SET_GLOBAL:");
                unsafe { *ip = ip.add(1) }
                let idx = unsafe { **ip };
                print!("{:>4}    {}", idx, self.constants[idx as usize]);
            }
            OpCode::GetLocal => {
                print!("{:<20}", "OP_GET_LOCAL:");
                unsafe { *ip = ip.add(1) }
                let idx = unsafe { **ip };
                print!("{:>4}    {}", idx, self.constants[idx as usize]);
            }
            OpCode::SetLocal => {
                print!("{:<20}", "OP_SET_LOCAL:");
                unsafe { *ip = ip.add(1) }
                let idx = unsafe { **ip };
                print!("{:>4}    {}", idx, self.constants[idx as usize]);
            }
            OpCode::JumpIfFalse => {
                print!("{:<20}", "OP_JUMP_IF_FALSE:");
                unsafe { *ip = ip.add(1) }
                let mut offset = (unsafe { **ip } as u16) << 8;
                unsafe { *ip = ip.add(1) }
                offset |= unsafe { **ip } as u16;
                print!("{:>4}", offset);
            }
            OpCode::Jump => {
                print!("{:<20}", "OP_JUMP:");
                unsafe { *ip = ip.add(1) }
                let mut offset = (unsafe { **ip } as u16) << 8;
                unsafe { *ip = ip.add(1) }
                offset |= unsafe { **ip } as u16;
                print!("{:>4}", offset);
            }
            OpCode::Loop => {
                print!("{:<20}", "OP_LOOP:");
                unsafe { *ip = ip.add(1) }
                let mut offset = (unsafe { **ip } as u16) << 8;
                unsafe { *ip = ip.add(1) }
                offset |= unsafe { **ip } as u16;
                print!("{:>4}", offset);
            }
        }
        println!();
        unsafe { *ip = ip.add(1) }
    }

    pub fn show_all_op_code(&self) {
        let p_start = self.code_top_ptr();
        let mut ip = p_start;
        let p_end = unsafe { ip.add(self.code.len()) };
        // while let Some((offset, &op)) = it.next() {
        while ip < p_end {
            let offset = unsafe { ip.offset_from(p_start) } as usize;
            print!("{:04}  ", offset);

            let line_no = self.get_line(offset);
            print!("{:>4}  ", line_no);
            self.show_one_op_code(&mut ip);
        }
    }

    pub fn dbg_print(&self) {
        println!("======chunk info=======");
        self.show_all_op_code();
        self.show_constants();
        println!("=======info end========");
    }
}

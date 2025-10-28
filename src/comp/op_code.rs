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

    pub fn extend(&mut self, other: &[u8]) {
        self.code.extend_from_slice(other);
    }

    pub fn iter(&self) -> std::slice::Iter<'_, u8> {
        self.code.iter()
    }

    fn get_line(&self, offset: usize) -> usize {
        self.lines.partition_point(|&x| x <= offset) - 1
    }

    fn show_constants(&self) {
        println!();
        println!("-----Constant area-----");
        for (idx, val) in self.constants.iter().enumerate() {
            println!("{} {}", idx, val.as_number());
        }
        println!("-----Constant end------");
        println!();
    }

    pub fn show_one_op_code(&self, ip: &mut *const u8) {
        let op = unsafe { **ip };
        match unsafe { OpCode::unchecked_transmute_from(op) } {
            OpCode::Return => {
                print!("OP_RETURN");
            }
            OpCode::Constant => {
                print!("OP_CONSTANT");
                print!("    ");
                unsafe { *ip = ip.add(1) }
                let idx = unsafe { **ip };
                print!("{idx:>4}");
                print!("    ");
                print!("{}", self.constants[idx as usize]);
            }
            OpCode::ConstantLong => {
                print!("OP_CONSTANT_LONG");
                print!("    ");
                unsafe { *ip = ip.add(1) }
                let mut idx = (unsafe { **ip } as u32) << 16;
                unsafe { *ip = ip.add(1) }
                idx ^= (unsafe { **ip } as u32) << 8;
                unsafe { *ip = ip.add(1) }
                idx ^= unsafe { **ip } as u32;
                print!("{idx:>4}");
                print!("    ");
                print!("{}", self.constants[idx as usize].as_number());
            }
            OpCode::Negate => {
                print!("OP_NEGATE");
            }
            OpCode::Add => {
                print!("OP_ADD");
            }
            OpCode::Subtract => {
                print!("OP_SUBTRACT");
            }
            OpCode::Multiply => {
                print!("OP_MULTIPLY");
            }
            OpCode::Divide => {
                print!("OP_DIVIDE");
            }
            OpCode::Nil => {
                print!("LITERAL_NIL");
            }
            OpCode::True => {
                print!("LITERAL_TRUE");
            }
            OpCode::False => {
                print!("LITERAL_FALSE");
            }
            OpCode::Not => {
                print!("OP_NOT")
            }
            OpCode::Equal => {
                print!("OP_EQUAL")
            }
            OpCode::Less => {
                print!("OP_LESS")
            }
            OpCode::Greater => {
                print!("OP_GREATER")
            }
            OpCode::Print => {
                print!("OP_PRINT")
            }
            OpCode::Pop => {
                print!("OP_POP")
            }
            OpCode::DefGlob => {
                print!("OP_DEFINE_GLOBAL")
            }
            OpCode::GetGlob => {
                print!("OP_GET_GLOBAL")
            }
            OpCode::SetGlob => {
                print!("OP_SET_GLOBAL")
            }
        };
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

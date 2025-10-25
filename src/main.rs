use std::env;
use std::fs;
use std::process::exit;

mod comp;
mod compiler;
mod scanner;
mod value;
use bytes::Bytes;
use comp::op_code::*;

use crate::comp::vm::RuntimeError;
use crate::comp::vm::VM;
use crate::compiler::Parser;
use crate::compiler::compile;
use crate::scanner::Scanner;

fn main() {
    let args = env::args().collect::<Vec<_>>();
    if args.len() == 1 {
        repl();
    } else if args.len() == 2 {
        run_file(&args[1]);
    } else {
        eprintln!("Usage: clox [path]");
        exit(64);
    }

    std::process::exit(0);
}

fn repl() {
    todo!()
}

fn run_file(file_path: &String) {
    let file = fs::File::open(file_path).unwrap();
    let mmap = unsafe { memmap2::Mmap::map(&file).unwrap() };
    let file_content = Bytes::from_owner(mmap);
    let chunk = compile(&file_content).unwrap();
    let mut vm = VM::new(chunk);
    vm.init_vm();
    vm.run().unwrap();
}

fn interpret(source: &String) {}

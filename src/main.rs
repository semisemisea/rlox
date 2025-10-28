use crate::comp::vm::VM;
use crate::compiler::compile;
use bytes::Bytes;
use std::env;
use std::fs;
use std::io::Read;
use std::process::exit;

mod comp;
mod compiler;
mod gc;
mod lox_object;
mod object;
mod scanner;
mod value;

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
    // let file = fs::File::open(file_path).unwrap();
    let file_content = fs::read(file_path).unwrap().into();
    // let mmap = unsafe { memmap2::Mmap::map(&file).unwrap() };
    // let file_content = Bytes::from_owner(mmap);
    let chunk = match compile(&file_content) {
        Err(e) => {
            println!("compile time: {e:?}");
            return;
        }
        Ok(chunk) => chunk,
    };
    let mut vm = VM::new(chunk);
    vm.init_vm();
    if let Err(e) = vm.run() {
        println!("vm crash into: {e:?}");
    }
}

fn interpret(source: &String) {}

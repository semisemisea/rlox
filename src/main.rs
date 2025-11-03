use crate::comp::vm::VM;
use crate::compiler::compile;
use crate::gc::show_all_objects;
use bytes::Bytes;
use color_eyre::eyre::Result;
use std::env;
use std::fs;
use std::process::exit;

mod comp;
mod compiler;
mod gc;
mod lox_object;
mod object;
mod scanner;
mod value;

fn main() -> Result<()> {
    #[cfg(debug_assertions)]
    unsafe {
        std::env::set_var("RUST_BACKTRACE", "1");
        // std::env::set_var("COLORBT_SHOW_HIDDEN", "1");
        color_eyre::install()?;
    };
    let args = env::args().collect::<Vec<_>>();
    if args.len() == 1 {
        repl();
    } else if args.len() == 2 {
        run_file(&args[1])?;
    } else {
        eprintln!("Usage: clox [path]");
        exit(64);
    }

    std::process::exit(0);
}

fn repl() {
    todo!()
}

fn run_file(file_path: &str) -> Result<()> {
    let file = fs::File::open(file_path).unwrap();
    let mmap = unsafe { memmap2::Mmap::map(&file).unwrap() };
    let file_content = Bytes::from_owner(mmap);
    let main_func = compile(&file_content)?;
    #[cfg(debug_assertions)]
    unsafe {
        (*main_func).chunk.dbg_print();
    }
    let mut vm = VM::new();
    vm.init_vm(main_func);
    vm.run().inspect_err(|_| vm.show())?;
    #[cfg(debug_assertions)]
    show_all_objects();
    gc::free_objects();
    Ok(())
}

fn _interpret(_source: &str) {}

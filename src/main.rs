use std::env;
use std::fs;
use std::process;
use std::rc::Rc;

use rustc_hash::FxHashMap;

use pavo::ast::{Context, Executable};
use pavo::functions::Function;
use pavo::parser::{ParsedProgram, parse};
use pavo::types::StructDef;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("usage: pavo <file.pv>");
        process::exit(1);
    }

    let path = &args[1];
    let source = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: could not read '{}': {}", path, e);
            process::exit(1);
        }
    };

    let program = match parse(&source) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        }
    };

    let ParsedProgram { functions, structs } = program;

    // Build lookup tables
    let mut fn_map: FxHashMap<String, Function> = FxHashMap::default();
    for f in functions {
        fn_map.insert(f.name.clone(), f);
    }

    let mut struct_map: FxHashMap<String, StructDef> = FxHashMap::default();
    for s in structs {
        struct_map.insert(s.name.clone(), s);
    }

    let fn_rc = Rc::new(fn_map);
    let struct_rc = Rc::new(struct_map);

    // Look for main()
    let main_fn = match fn_rc.get("main") {
        Some(f) => f.clone(),
        None => {
            eprintln!("error: no 'main' function found");
            process::exit(1);
        }
    };

    let mut ctx = Context::new(fn_rc, struct_rc);

    for stmt in &main_fn.body {
        match stmt.execute(&mut ctx) {
            Ok(_) => {
                if ctx.has_returned() {
                    break;
                }
            }
            Err(e) => {
                eprintln!("runtime error: {}", e);
                process::exit(1);
            }
        }
    }
}

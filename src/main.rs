use std::collections::HashSet;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;
use std::rc::Rc;
use std::thread;

use rustc_hash::FxHashMap;

use pavo::ast::{Context, Executable};
use pavo::functions::Function;
use pavo::parser::parse;
use pavo::types::StructDef;

fn main() {
    let child = thread::Builder::new()
        .stack_size(16 * 1024 * 1024)
        .spawn(run_interpreter)
        .unwrap();

    child.join().unwrap();
}

/// Recursively load a .pv file and all its imports.
/// Imported files have their zero-arg `main` stripped so only the entry file's main runs.
fn load_program(
    file_path: &Path,
    is_entry: bool,
    loaded: &mut HashSet<PathBuf>,
    all_functions: &mut Vec<Function>,
    all_structs: &mut Vec<StructDef>,
) -> Result<(), String> {
    let canonical = fs::canonicalize(file_path)
        .map_err(|e| format!("error: could not resolve '{}': {}", file_path.display(), e))?;

    if !loaded.insert(canonical.clone()) {
        return Ok(()); // already loaded, skip (handles circular imports)
    }

    let source = fs::read_to_string(&canonical)
        .map_err(|e| format!("error: could not read '{}': {}", canonical.display(), e))?;

    let program = parse(&source)?;

    // Process imports first (relative to this file's directory)
    let dir = canonical.parent().unwrap();
    for import_path in &program.imports {
        let resolved = dir.join(import_path);
        load_program(&resolved, false, loaded, all_functions, all_structs)?;
    }

    // Add functions — strip zero-arg main() from imported (non-entry) files
    for f in program.functions {
        if !is_entry && f.name == "main" && f.args.is_empty() {
            continue;
        }
        all_functions.push(f);
    }

    all_structs.extend(program.structs);

    Ok(())
}

fn run_interpreter() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("usage: pavo <file.pv>");
        process::exit(1);
    }

    let path = Path::new(&args[1]);

    // Recursively load entry file + all imports
    let mut loaded = HashSet::new();
    let mut all_functions = Vec::new();
    let mut all_structs = Vec::new();

    if let Err(e) = load_program(
        path,
        true,
        &mut loaded,
        &mut all_functions,
        &mut all_structs,
    ) {
        eprintln!("{}", e);
        process::exit(1);
    }

    // Build lookup tables (allowing multiple overloads per function name)
    let mut fn_map: FxHashMap<String, Vec<Function>> = FxHashMap::default();
    for f in all_functions {
        fn_map
            .entry(f.name.clone())
            .or_insert_with(Vec::new)
            .push(f);
    }

    let mut struct_map: FxHashMap<String, StructDef> = FxHashMap::default();
    for s in all_structs {
        struct_map.insert(s.name.clone(), s);
    }

    let fn_rc = Rc::new(fn_map);
    let struct_rc = Rc::new(struct_map);

    // Look for main() - get the zero-argument version
    let main_fn = match fn_rc.get("main") {
        Some(overloads) => match overloads.iter().find(|f| f.args.is_empty()) {
            Some(f) => f.clone(),
            None => {
                eprintln!("error: no 'main' function with 0 arguments found");
                process::exit(1);
            }
        },
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

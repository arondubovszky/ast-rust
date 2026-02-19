use std::cell::RefCell;
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
use pavo::templates::{expand_templates, expand_templates_only};
use pavo::types::StructDef;

/// Embedded stdlib — baked into the binary at compile time.
const STDLIB_STD: &str = include_str!("../stdlib/std.pv");

/// Returns the embedded source for a known stdlib filename, if any.
fn get_embedded_stdlib(name: &str) -> Option<&'static str> {
    match name {
        "std.pv" => Some(STDLIB_STD),
        _ => None,
    }
}

fn main() {
    let child = thread::Builder::new()
        .stack_size(16 * 1024 * 1024)
        .spawn(run_interpreter)
        .unwrap();

    child.join().unwrap();
}

/// Load an embedded stdlib file by name. Tracks loaded names to prevent duplicates.
fn load_embedded(
    name: &str,
    source: &str,
    loaded_embedded: &mut HashSet<String>,
    loaded: &mut HashSet<PathBuf>,
    all_functions: &mut Vec<Function>,
    all_structs: &mut Vec<StructDef>,
) -> Result<(), String> {
    if !loaded_embedded.insert(name.to_string()) {
        return Ok(()); // already loaded
    }

    let expanded = expand_templates(source)?;
    let program = parse(&expanded)?;

    // Embedded stdlib files can import other embedded files
    for import_path in &program.imports {
        if let Some(embedded_src) = get_embedded_stdlib(import_path) {
            load_embedded(
                import_path,
                embedded_src,
                loaded_embedded,
                loaded,
                all_functions,
                all_structs,
            )?;
        } else {
            return Err(format!(
                "error: embedded file '{}' imports '{}' which is not a known stdlib file",
                name, import_path
            ));
        }
    }

    // Never include main() from stdlib files
    for f in program.functions {
        if f.name == "main" && f.args.is_empty() {
            continue;
        }
        all_functions.push(f);
    }

    all_structs.extend(program.structs);

    Ok(())
}

/// Recursively load a .pv file and all its imports.
/// Imported files have their zero-arg `main` stripped so only the entry file's main runs.
fn load_program(
    file_path: &Path,
    is_entry: bool,
    loaded: &mut HashSet<PathBuf>,
    loaded_embedded: &mut HashSet<String>,
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

    let expanded = expand_templates(&source)?;
    let program = parse(&expanded)?;

    // Process imports first — check embedded stdlib before filesystem
    let dir = canonical.parent().unwrap();
    for import_path in &program.imports {
        if let Some(embedded_src) = get_embedded_stdlib(import_path) {
            load_embedded(
                import_path,
                embedded_src,
                loaded_embedded,
                loaded,
                all_functions,
                all_structs,
            )?;
        } else {
            let resolved = dir.join(import_path);
            load_program(
                &resolved,
                false,
                loaded,
                loaded_embedded,
                all_functions,
                all_structs,
            )?;
        }
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
        eprintln!("usage: pavo [--expand] <file.pv>");
        process::exit(1);
    }

    let expand_only = args.iter().any(|a| a == "--expand");
    let file_arg = args.iter().find(|a| !a.starts_with('-') && *a != &args[0]);
    let file_arg = match file_arg {
        Some(f) => f,
        None => {
            eprintln!("usage: pavo [--expand] <file.pv>");
            process::exit(1);
        }
    };

    if expand_only {
        let source = fs::read_to_string(file_arg).unwrap_or_else(|e| {
            eprintln!("error: could not read '{}': {}", file_arg, e);
            process::exit(1);
        });
        match expand_templates_only(&source) {
            Ok(expanded) => {
                print!("{}", expanded);
                return;
            }
            Err(e) => {
                eprintln!("{}", e);
                process::exit(1);
            }
        }
    }

    let path = Path::new(file_arg);

    // Recursively load entry file + all imports
    let mut loaded = HashSet::new();
    let mut loaded_embedded = HashSet::new();
    let mut all_functions = Vec::new();
    let mut all_structs = Vec::new();

    if let Err(e) = load_program(
        path,
        true,
        &mut loaded,
        &mut loaded_embedded,
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
    let static_vars = Rc::new(RefCell::new(FxHashMap::default()));

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

    let mut ctx = Context::new(fn_rc, struct_rc, static_vars);

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

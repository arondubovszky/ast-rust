use rustc_hash::FxHashMap;

use crate::functions::Function;
use crate::types::{StructDef, Type, TypeKind};

/// Compile-time environment: tracks variable types, function signatures, and struct definitions.
/// Analogous to the runtime `Context`, but holds `TypeKind`s instead of `Type` values.
pub struct StaticContext {
    pub vars: FxHashMap<String, TypeKind>,
    pub functions: FxHashMap<String, Vec<Function>>,
    pub structs: FxHashMap<String, StructDef>,
}

impl StaticContext {
    pub fn new() -> Self {
        Self {
            vars: FxHashMap::default(),
            functions: FxHashMap::default(),
            structs: FxHashMap::default(),
        }
    }

    /// Resolve which overload of `name` matches the given argument kinds.
    /// Mirrors `Context::resolve_function` but operates on `TypeKind`s instead of runtime values.
    pub fn resolve_overload(&self, name: &str, arg_kinds: &[TypeKind]) -> Option<&Function> {
        self.functions.get(name)?.iter().find(|f| {
            f.args.len() == arg_kinds.len()
                && f.args
                    .iter()
                    .zip(arg_kinds.iter())
                    .all(|(param, &kind)| self.kind_matches(param.param_type, param.array_element_type, kind))
        })
    }

    fn kind_matches(&self, param_type: TypeKind, _array_elem: Option<TypeKind>, arg: TypeKind) -> bool {
        match (param_type, arg) {
            (TypeKind::Array, TypeKind::Array) => true, // element type unknown at call site without deeper analysis
            (p, a) => p == a,
        }
    }
}

/// Level 1 — Type checking.
/// Resolves the `TypeKind` produced by this node given a compile-time `StaticContext`.
pub trait TypeCheck {
    fn type_check(&self, env: &mut StaticContext) -> Result<TypeKind, String>;
}

/// Level 2 — Constant folding.
/// Attempts to fully evaluate this expression at compile time.
/// Returns `Some(value)` if the expression reduced to a constant, `None` if it could not.
pub trait ConstFold {
    fn const_fold(&self) -> Result<Option<Type>, String>;
}

#[macro_use]
extern crate lazy_static;

pub mod ast;
pub mod emit;
pub mod functions;
pub mod parser;
pub mod templates;
pub mod types;

pub use ast::{ASTNode, Context, DefineVariable, Executable, ExprNode, SetVariable, Symbol};
pub use emit::Emit;
pub use functions::{Function, FunctionParam};
pub use parser::parse;
pub use types::{Castable, StructDef, Type, TypeKind};

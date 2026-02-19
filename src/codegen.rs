use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use rustc_hash::FxHashMap;

use crate::types::Type;

/// Holds all inkwell state needed during code generation.
/// Analogous to the runtime `Context`.
pub struct CodegenContext<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub vars: FxHashMap<String, PointerValue<'ctx>>,
    pub functions: FxHashMap<String, FunctionValue<'ctx>>,
}

impl<'ctx> CodegenContext<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        Self {
            context,
            module: context.create_module(module_name),
            builder: context.create_builder(),
            vars: FxHashMap::default(),
            functions: FxHashMap::default(),
        }
    }
}

/// Compile this node into LLVM IR, returning the value it produces.
pub trait Compilable<'ctx> {
    fn compile(&self, ctx: &mut CodegenContext<'ctx>) -> Result<BasicValueEnum<'ctx>, String>;
}

impl<'ctx> Compilable<'ctx> for Type {
    fn compile(&self, ctx: &mut CodegenContext<'ctx>) -> Result<BasicValueEnum<'ctx>, String> {
        match self {
            Type::Int32(x)  => Ok(ctx.context.i32_type().const_int(*x as u64, true).into()),
            Type::Int64(x)  => Ok(ctx.context.i64_type().const_int(*x as u64, true).into()),
            Type::Float32(x) => Ok(ctx.context.f32_type().const_float(*x as f64).into()),
            Type::Float64(x) => Ok(ctx.context.f64_type().const_float(*x).into()),
            Type::Bool(x)   => Ok(ctx.context.bool_type().const_int(*x as u64, false).into()),
            _ => Err(format!("cannot compile {:?} as a constant", self.get_kind())),
        }
    }
}

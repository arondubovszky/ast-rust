use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use rustc_hash::FxHashMap;

use crate::ast::{ExprNode, Symbol};
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

impl<'ctx> Compilable<'ctx> for ExprNode {
    fn compile(&self, ctx: &mut CodegenContext<'ctx>) -> Result<BasicValueEnum<'ctx>, String> {
        match self {
            ExprNode::Literal(t) => match t {
                // VarRef needs a vars map — not supported yet
                Type::VarRef(_) => Err("variable references not yet supported in codegen".to_string()),
                _ => t.compile(ctx),
            },
            ExprNode::BinaryOp { op, left, right } => {
                let l = left.compile(ctx)?;
                let r = right
                    .as_ref()
                    .ok_or("binary op missing right operand")?
                    .compile(ctx)?;

                compile_binop(ctx, *op, l, r)
            }
            _ => Err(format!("expression variant not yet supported in codegen")),
        }
    }
}

fn compile_binop<'ctx>(
    ctx: &mut CodegenContext<'ctx>,
    op: Symbol,
    l: BasicValueEnum<'ctx>,
    r: BasicValueEnum<'ctx>,
) -> Result<BasicValueEnum<'ctx>, String> {
    match (l, r) {
        (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => {
            let v = match op {
                Symbol::Plus     => ctx.builder.build_int_add(l, r, "add").map(Into::into),
                Symbol::Minus    => ctx.builder.build_int_sub(l, r, "sub").map(Into::into),
                Symbol::Multiply => ctx.builder.build_int_mul(l, r, "mul").map(Into::into),
                Symbol::Divide   => ctx.builder.build_int_signed_div(l, r, "div").map(Into::into),
                Symbol::Modulo   => ctx.builder.build_int_signed_rem(l, r, "rem").map(Into::into),
                Symbol::Eq    => ctx.builder.build_int_compare(IntPredicate::EQ,  l, r, "eq").map(Into::into),
                Symbol::NotEq => ctx.builder.build_int_compare(IntPredicate::NE,  l, r, "ne").map(Into::into),
                Symbol::Lt    => ctx.builder.build_int_compare(IntPredicate::SLT, l, r, "lt").map(Into::into),
                Symbol::Gt    => ctx.builder.build_int_compare(IntPredicate::SGT, l, r, "gt").map(Into::into),
                Symbol::LtEq  => ctx.builder.build_int_compare(IntPredicate::SLE, l, r, "le").map(Into::into),
                Symbol::GtEq  => ctx.builder.build_int_compare(IntPredicate::SGE, l, r, "ge").map(Into::into),
                _ => return Err(format!("operator {:?} not supported for integers", op)),
            };
            v.map_err(|e| e.to_string())
        }
        (BasicValueEnum::FloatValue(l), BasicValueEnum::FloatValue(r)) => {
            let v = match op {
                Symbol::Plus     => ctx.builder.build_float_add(l, r, "fadd").map(Into::into),
                Symbol::Minus    => ctx.builder.build_float_sub(l, r, "fsub").map(Into::into),
                Symbol::Multiply => ctx.builder.build_float_mul(l, r, "fmul").map(Into::into),
                Symbol::Divide   => ctx.builder.build_float_div(l, r, "fdiv").map(Into::into),
                Symbol::Modulo   => ctx.builder.build_float_rem(l, r, "frem").map(Into::into),
                Symbol::Eq    => ctx.builder.build_float_compare(FloatPredicate::OEQ, l, r, "feq").map(Into::into),
                Symbol::NotEq => ctx.builder.build_float_compare(FloatPredicate::ONE, l, r, "fne").map(Into::into),
                Symbol::Lt    => ctx.builder.build_float_compare(FloatPredicate::OLT, l, r, "flt").map(Into::into),
                Symbol::Gt    => ctx.builder.build_float_compare(FloatPredicate::OGT, l, r, "fgt").map(Into::into),
                Symbol::LtEq  => ctx.builder.build_float_compare(FloatPredicate::OLE, l, r, "fle").map(Into::into),
                Symbol::GtEq  => ctx.builder.build_float_compare(FloatPredicate::OGE, l, r, "fge").map(Into::into),
                _ => return Err(format!("operator {:?} not supported for floats", op)),
            };
            v.map_err(|e| e.to_string())
        }
        (l, r) => Err(format!(
            "mixed-type operations not yet supported in codegen ({:?} and {:?})",
            l.get_type(),
            r.get_type()
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::process::Command;
    use std::rc::Rc;
    use inkwell::context::Context;
    use crate::ast::{ExprNode, Symbol};
    use crate::types::Type;

    /// Compile `expr` into a main() that prints the i32 result via printf,
    /// write the IR to a temp file, run it with clang, and return stdout.
    fn run_i32_expr(name: &str, expr: ExprNode) -> String {
        let context = Context::create();
        let mut ctx = CodegenContext::new(&context, "test");

        // declare printf
        let i32_t = context.i32_type();
        let ptr_t = context.ptr_type(inkwell::AddressSpace::default());
        let printf_t = i32_t.fn_type(&[ptr_t.into()], true);
        let printf = ctx.module.add_function("printf", printf_t, None);

        // define main() -> i32
        let main_t = i32_t.fn_type(&[], false);
        let main_fn = ctx.module.add_function("main", main_t, None);
        let bb = context.append_basic_block(main_fn, "entry");
        ctx.builder.position_at_end(bb);

        // compile the expression
        let val = expr.compile(&mut ctx).unwrap();

        // printf("%d\n", val)
        let fmt = ctx.builder.build_global_string_ptr("%d\n", "fmt").unwrap();
        ctx.builder.build_call(printf, &[fmt.as_pointer_value().into(), val.into()], "").unwrap();
        ctx.builder.build_return(Some(&i32_t.const_int(0, false))).unwrap();

        ctx.module.verify().expect("IR verification failed");

        // write IR to temp file (unique per test to avoid parallel collisions)
        let ll_path = format!("/tmp/pavo_codegen_{}.ll", name);
        let bin_path = format!("/tmp/pavo_codegen_{}", name);
        let ll_path = ll_path.as_str();
        let bin_path = bin_path.as_str();
        ctx.module.print_to_file(std::path::Path::new(ll_path)).unwrap();

        // compile and run
        let status = Command::new("clang")
            .args([ll_path, "-o", bin_path, "-Wno-override-module"])
            .status()
            .expect("clang not found");
        assert!(status.success(), "clang compilation failed");

        let output = Command::new(bin_path)
            .output()
            .expect("failed to run compiled binary");

        String::from_utf8(output.stdout).unwrap()
    }

    #[test]
    fn test_i32_add() {
        let expr = ExprNode::BinaryOp {
            op: Symbol::Plus,
            left: Rc::new(ExprNode::Literal(Type::Int32(3))),
            right: Some(Rc::new(ExprNode::Literal(Type::Int32(4)))),
        };
        assert_eq!(run_i32_expr("add", expr).trim(), "7");
    }

    #[test]
    fn test_i32_sub() {
        let expr = ExprNode::BinaryOp {
            op: Symbol::Minus,
            left: Rc::new(ExprNode::Literal(Type::Int32(10))),
            right: Some(Rc::new(ExprNode::Literal(Type::Int32(3)))),
        };
        assert_eq!(run_i32_expr("sub", expr).trim(), "7");
    }

    #[test]
    fn test_i32_mul() {
        let expr = ExprNode::BinaryOp {
            op: Symbol::Multiply,
            left: Rc::new(ExprNode::Literal(Type::Int32(3))),
            right: Some(Rc::new(ExprNode::Literal(Type::Int32(4)))),
        };
        assert_eq!(run_i32_expr("mul", expr).trim(), "12");
    }

    #[test]
    fn test_nested_expr() {
        // (2 + 3) * 4 = 20
        let inner = ExprNode::BinaryOp {
            op: Symbol::Plus,
            left: Rc::new(ExprNode::Literal(Type::Int32(2))),
            right: Some(Rc::new(ExprNode::Literal(Type::Int32(3)))),
        };
        let expr = ExprNode::BinaryOp {
            op: Symbol::Multiply,
            left: Rc::new(inner),
            right: Some(Rc::new(ExprNode::Literal(Type::Int32(4)))),
        };
        assert_eq!(run_i32_expr("nested", expr).trim(), "20");
    }
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

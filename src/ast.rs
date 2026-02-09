use rand::Rng;
use rustc_hash::FxHashMap;
use std::rc::Rc;

use crate::functions::Function;
use crate::types::{StructDef, Type, TypeKind};

pub trait Executable {
    fn execute(&self, ctx: &mut Context) -> Result<Type, String>;
}

pub struct Context {
    pub vars: FxHashMap<String, Type>,
    pub functions: Rc<FxHashMap<String, Vec<Function>>>,
    pub structs: Rc<FxHashMap<String, StructDef>>,
    pub return_value: Option<Type>,
}

impl Context {
    pub fn new(
        fns: Rc<FxHashMap<String, Vec<Function>>>,
        structs: Rc<FxHashMap<String, StructDef>>,
    ) -> Self {
        Self {
            vars: FxHashMap::default(),
            functions: fns,
            structs,
            return_value: None,
        }
    }

    /// convenience
    pub fn empty() -> Self {
        Self {
            vars: FxHashMap::default(),
            functions: Rc::new(FxHashMap::default()),
            structs: Rc::new(FxHashMap::default()),
            return_value: None,
        }
    }

    /// Helper to resolve a function by name and argument types.
    /// Matches argument values against parameter types, considering array element types.
    fn resolve_function(&self, name: &str, arg_values: &[Type]) -> Option<Function> {
        self.functions.get(name).and_then(|overloads| {
            overloads
                .iter()
                .find(|f| {
                    f.args.len() == arg_values.len()
                        && f.args
                            .iter()
                            .zip(arg_values.iter())
                            .all(|(param, arg_val)| self.type_matches(param.param_type, arg_val))
                })
                .cloned()
        })
    }

    /// Check if a parameter type matches an argument value's type
    fn type_matches(&self, param_type: TypeKind, arg_val: &Type) -> bool {
        match (param_type, arg_val) {
            (TypeKind::Array, Type::Array(elems)) => {
                // All elements must have the same type
                if elems.is_empty() {
                    return true; // Empty arrays match any array type
                }
                let elem_kind = elems[0].get_kind();
                elems.iter().all(|e| e.get_kind() == elem_kind)
            }
            (param_type, arg_val) => param_type == arg_val.get_kind(),
        }
    }

    pub fn set_return(&mut self, val: Type) {
        self.return_value = Some(val);
    }

    pub fn has_returned(&self) -> bool {
        self.return_value.is_some()
    }

    pub fn take_return(&mut self) -> Option<Type> {
        self.return_value.take()
    }

    pub fn add_variable(&mut self, name: &str, val: Type) -> Result<(), String> {
        self.vars.insert(String::from(name), val);

        Ok(())
    }

    pub fn get_variable_reference(&mut self, name: &str) -> Type {
        self.vars.get(name).unwrap().clone()
    }

    // works for inserting vars aswell
    pub fn set_variable(&mut self, name: &str, new_val: Type) -> Result<(), String> {
        match self.vars.get(name) {
            Some(val) => {
                if val.get_kind() != new_val.get_kind() {
                    return Err(format!(
                        "Cannot change the type of variable {:?} from {:?} to {:?} during assignment",
                        name,
                        val.get_kind(),
                        new_val.get_kind()
                    ));
                }
            }
            None => {}
        }

        self.vars
            .entry(String::from(name))
            .and_modify(|v| *v = new_val);

        return Ok(());
    }

    pub fn _debug_print(&self) {
        for (name, value) in &self.vars {
            println!("  {} = {:?}", name, value);
        }
    }
}

#[derive(Clone)]
pub enum ASTNode {
    Expr(ExprNode),
    DefineVar(DefineVariable),
    SetVar(SetVariable),
    // (assign_field user name "Bob")
    SetField {
        object_name: String,
        field_name: String,
        value: Rc<ExprNode>,
    },
    // arr[0] = 5;
    SetIndex {
        array_name: String,
        index: Rc<ExprNode>,
        value: Rc<ExprNode>,
    },
    Print(ExprNode),
    Println(ExprNode),
    Note {
        note: String,
        subtree: Rc<ASTNode>,
    },
    Return(ExprNode),
}

#[derive(Clone)]
pub struct DefineVariable {
    pub var_type: Type,
    pub name: String,
    pub value: Rc<ExprNode>,
}

impl DefineVariable {
    pub fn new(vtype: Type, n: &str, val: Rc<ExprNode>) -> Self {
        DefineVariable {
            var_type: vtype,
            name: String::from(n),
            value: val,
        }
    }

    fn type_matches(&self, value: &Type) -> bool {
        self.var_type.get_kind() == value.get_kind()
    }
}

impl Executable for DefineVariable {
    fn execute(&self, ctx: &mut Context) -> Result<Type, String> {
        let expr_res = self.value.execute_core(ctx)?;

        if !self.type_matches(&expr_res) {
            return Err(format!(
                "[AST] mismatched types: variable {:?} has type {:?} but has type {:?} assigned to it",
                self.name, self.var_type, expr_res
            ));
        }

        let result = expr_res.clone(); // because of let x := (y := 3) + 2;
        ctx.add_variable(&self.name, expr_res)?;

        Ok(result)
    }
}

#[derive(Clone)]
pub struct SetVariable {
    pub var_type: Type,
    pub name: String,
    pub value: Rc<ExprNode>,
}

impl SetVariable {
    pub fn new(vtype: Type, n: &str, val: Rc<ExprNode>) -> Self {
        SetVariable {
            var_type: vtype,
            name: String::from(n),
            value: val,
        }
    }

    fn type_matches(&self, value: &Type) -> bool {
        self.var_type.get_kind() == value.get_kind()
    }
}

impl Executable for SetVariable {
    fn execute(&self, ctx: &mut Context) -> Result<Type, String> {
        let expr_res = self.value.execute_core(ctx)?;

        // skip compile-time type check if var_type is Void (runtime type check in Context::set_variable)
        // the parser only indentifies the pattern and does not know at this point what type given identifier is so it gives it type void
        if self.var_type.get_kind() != crate::types::TypeKind::Void && !self.type_matches(&expr_res)
        {
            return Err(format!(
                "[AST] mismatched types: variable {:?} has type {:?} but has type {:?} assigned to it",
                self.name, self.var_type, expr_res
            ));
        }

        let result = expr_res.clone(); // because of let x := (y := 3) + 2;
        ctx.set_variable(&self.name, expr_res)?;

        Ok(result)
    }
}

#[derive(Clone)]
pub enum ExprNode {
    BinaryOp {
        op: Symbol,
        left: Rc<ExprNode>,
        right: Option<Rc<ExprNode>>,
    },
    Literal(Type),
    ArrayLiteral(Vec<Rc<ExprNode>>),
    Index {
        array: Rc<ExprNode>,
        index: Rc<ExprNode>,
    },
    IfElseNode {
        statement: Rc<ExprNode>,
        then_branch: Rc<Vec<ASTNode>>,
        else_branch: Option<Rc<Vec<ASTNode>>>,
    },
    LoopNode {
        statement: Rc<ExprNode>,
        body: Rc<Vec<ASTNode>>,
    },
    // [length; fill_value]
    ArraySized {
        length: Rc<ExprNode>,
        fill: Rc<ExprNode>,
    },
    // function definitions will be handled during parsing
    FunctionCall {
        name: String,
        args: Vec<Rc<ExprNode>>,
    },
    // (struct User (id 1) (name "Alice"))
    StructLiteral {
        struct_name: String,
        fields: Vec<(String, Rc<ExprNode>)>, // (field_name, value_expr)
    },
    // (field user name)
    Field {
        object: Rc<ExprNode>,
        field_name: String,
    },
    Len(Rc<ExprNode>),
    Maybe,
}

#[warn(unused)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Symbol {
    // Arithmetic
    Plus,     // +
    Minus,    // -
    Multiply, // *
    Divide,   // /
    FloorDiv, // //
    Modulo,   // %
    Power,    // **

    Increment, // ++
    Decrement, // --

    // Bitwise
    BitAnd, // &
    BitOr,  // |
    BitXor, // ^
    BitNot, // ~

    // Logical
    And, // &&
    Or,  // ||
    Not, // !

    // Comparison
    Eq,    // ==
    NotEq, // !=
    Lt,    // <
    Gt,    // >
    LtEq,  // <=
    GtEq,  // >=

    // Assignment
    Assign, // =
    Init,   // :=

    // Access/Navigation
    Dot,   // .
    Arrow, // -> (cast)

    // Delimiters (if needed for operators)
    Comma, // ,

    // Operations (not just syntax)
    Index, // [] operation
    Call,  // () operation
}

impl Executable for ASTNode {
    fn execute(&self, ctx: &mut Context) -> Result<Type, String> {
        match self {
            ASTNode::Expr(e) => e.execute_core(ctx),
            ASTNode::DefineVar(d) => d.execute(ctx),
            ASTNode::SetVar(s) => s.execute(ctx),
            ASTNode::Print(e) => {
                let res = e.execute_core(ctx);

                match res {
                    Ok(x) => {
                        match &x {
                            Type::Bool(b) => print!("{}", b),
                            Type::Float32(f) => print!("{}", f),
                            Type::Float64(f) => print!("{}", f),
                            Type::Int32(i) => print!("{}", i),
                            Type::Int64(i) => print!("{}", i),
                            Type::Str(s) => print!("{}", s),
                            _ => print!("{:?}", x),
                        }
                        return Ok(x);
                    }
                    Err(e) => Err(format!("error: {:?}", e)),
                }
            }
            ASTNode::Println(e) => {
                let res = e.execute_core(ctx);

                match res {
                    Ok(x) => {
                        match &x {
                            Type::Bool(b) => println!("{}", b),
                            Type::Float32(f) => println!("{}", f),
                            Type::Float64(f) => println!("{}", f),
                            Type::Int32(i) => println!("{}", i),
                            Type::Int64(i) => println!("{}", i),
                            Type::Str(s) => println!("{}", s),
                            _ => print!("{:?}", x),
                        }
                        return Ok(x);
                    }
                    Err(e) => Err(format!("error: {:?}", e)),
                }
            }
            ASTNode::Return(e) => {
                let val = e.execute_core(ctx)?;
                ctx.set_return(val.clone());
                Ok(val)
            }
            ASTNode::SetField {
                object_name,
                field_name,
                value,
            } => {
                let new_val = value.execute_core(ctx)?;

                // Get the object from context, modify it, put it back
                let mut obj = ctx.get_variable_reference(object_name);
                obj.set_field(field_name, new_val.clone())?;
                ctx.set_variable(object_name, obj)?;

                Ok(new_val)
            }
            ASTNode::SetIndex {
                array_name,
                index,
                value,
            } => {
                let idx_val = index.execute_core(ctx)?;
                let new_val = value.execute_core(ctx)?;

                let idx = match idx_val {
                    Type::Int32(i) => i as usize,
                    Type::Int64(i) => i as usize,
                    _ => return Err(format!("array index must be an integer, got {:?}", idx_val)),
                };

                let mut arr = ctx.get_variable_reference(array_name);
                arr.set_index(idx, new_val.clone())?;
                ctx.set_variable(array_name, arr)?;

                Ok(new_val)
            }
            ASTNode::Note { subtree, .. } => subtree.execute(ctx),
        }
    }
}

impl ExprNode {
    fn execute_core(&self, ctx: &mut Context) -> Result<Type, String> {
        match self {
            ExprNode::Literal(lit) => match lit {
                Type::VarRef(name) => Ok(ctx.get_variable_reference(name)),
                Type::Ref(inner) => Ok(Type::Ref(inner.clone())),
                _ => Ok(lit.clone()),
            },
            ExprNode::ArrayLiteral(elements) => {
                let mut values = Vec::new();
                for elem in elements {
                    values.push(elem.execute_core(ctx)?);
                }
                Ok(Type::Array(values))
            }
            ExprNode::ArraySized { length, fill } => {
                let len_val = length.execute_core(ctx)?;
                let fill_val = fill.execute_core(ctx)?;
                let len = match len_val {
                    Type::Int32(n) => n as usize,
                    Type::Int64(n) => n as usize,
                    _ => return Err(format!("array size must be an integer, got {:?}", len_val)),
                };
                Ok(Type::Array(vec![fill_val; len]))
            }
            ExprNode::Index { array, index } => {
                let arr_val = array.execute_core(ctx)?;
                let idx_val = index.execute_core(ctx)?;

                match (arr_val, idx_val) {
                    (Type::Array(arr), Type::Int32(i)) => {
                        let idx = i as usize;
                        if idx >= arr.len() {
                            Err(format!("index out of bounds: {} >= {}", idx, arr.len()))
                        } else {
                            Ok(arr[idx].clone())
                        }
                    }
                    (Type::Array(arr), Type::Int64(i)) => {
                        let idx = i as usize;
                        if idx >= arr.len() {
                            Err(format!("index out of bounds: {} >= {}", idx, arr.len()))
                        } else {
                            Ok(arr[idx].clone())
                        }
                    }
                    (Type::Str(s), Type::Int32(i)) => {
                        let idx = i as usize;
                        if idx >= s.len() {
                            Err(format!("index out of bounds: {} >= {}", idx, s.len()))
                        } else {
                            Ok(Type::Str(s.chars().nth(idx).unwrap().to_string()))
                        }
                    }
                    (Type::Str(s), Type::Int64(i)) => {
                        let idx = i as usize;
                        if idx >= s.len() {
                            Err(format!("index out of bounds: {} >= {}", idx, s.len()))
                        } else {
                            Ok(Type::Str(s.chars().nth(idx).unwrap().to_string()))
                        }
                    }
                    (arr, idx) => Err(format!("cannot index {:?} with {:?}", arr, idx)),
                }
            }
            ExprNode::BinaryOp { op, left, right } => {
                // Increment/Decrement need to mutate the variable in-place
                if matches!(op, Symbol::Increment | Symbol::Decrement) {
                    if let ExprNode::Literal(Type::VarRef(name)) = left.as_ref() {
                        let mut val = ctx.get_variable_reference(name);
                        match op {
                            Symbol::Increment => val.increment()?,
                            Symbol::Decrement => val.decrement()?,
                            _ => unreachable!(),
                        }
                        let result = val.clone();
                        ctx.set_variable(name, val)?;
                        return Ok(result);
                    } else {
                        let mut val = left.execute_core(ctx)?;
                        match op {
                            Symbol::Increment => val.increment()?,
                            Symbol::Decrement => val.decrement()?,
                            _ => unreachable!(),
                        }
                        return Ok(val);
                    }
                }

                let l = left.execute_core(ctx)?;
                let r = right
                    .as_ref()
                    .map(|r_expr| r_expr.execute_core(ctx))
                    .transpose()?;

                match op {
                    Symbol::Plus => l + r.unwrap(),
                    Symbol::Minus => match r {
                        Some(_) => l - r.unwrap(),
                        None => Type::Int32(-1) * l,
                    },
                    Symbol::Multiply => l * r.unwrap(),
                    Symbol::Divide => l / r.unwrap(),
                    Symbol::Increment | Symbol::Decrement => unreachable!(),
                    Symbol::Eq => match (l, r.unwrap()) {
                        (Type::Int32(a), Type::Int32(b)) => Ok(Type::Bool(a == b)),
                        (Type::Int64(a), Type::Int64(b)) => Ok(Type::Bool(a == b)),
                        (Type::Int32(a), Type::Int64(b)) => Ok(Type::Bool(a as i64 == b)),
                        (Type::Int64(a), Type::Int32(b)) => Ok(Type::Bool(a == b as i64)),
                        (Type::Float32(a), Type::Float32(b)) => Ok(Type::Bool(a == b)),
                        (Type::Float64(a), Type::Float64(b)) => Ok(Type::Bool(a == b)),
                        (Type::Int32(a), Type::Float64(b)) => Ok(Type::Bool(a as f64 == b)),
                        (Type::Float64(a), Type::Int32(b)) => Ok(Type::Bool(a == b as f64)),
                        (Type::Int64(a), Type::Float64(b)) => Ok(Type::Bool(a as f64 == b)),
                        (Type::Float64(a), Type::Int64(b)) => Ok(Type::Bool(a == b as f64)),
                        (Type::Bool(a), Type::Bool(b)) => Ok(Type::Bool(a == b)),
                        (Type::Str(a), Type::Str(b)) => Ok(Type::Bool(a == b)),
                        (Type::Null, Type::Null) => Ok(Type::Bool(true)),
                        (a, b) => Err(format!("Cannot compare {:?} and {:?}", a, b)),
                    },
                    Symbol::NotEq => match (l, r.unwrap()) {
                        (Type::Int32(a), Type::Int32(b)) => Ok(Type::Bool(a != b)),
                        (Type::Int64(a), Type::Int64(b)) => Ok(Type::Bool(a != b)),
                        (Type::Int32(a), Type::Int64(b)) => Ok(Type::Bool(a as i64 != b)),
                        (Type::Int64(a), Type::Int32(b)) => Ok(Type::Bool(a != b as i64)),
                        (Type::Float32(a), Type::Float32(b)) => Ok(Type::Bool(a != b)),
                        (Type::Float64(a), Type::Float64(b)) => Ok(Type::Bool(a != b)),
                        (Type::Int32(a), Type::Float64(b)) => Ok(Type::Bool(a as f64 != b)),
                        (Type::Float64(a), Type::Int32(b)) => Ok(Type::Bool(a != b as f64)),
                        (Type::Int64(a), Type::Float64(b)) => Ok(Type::Bool(a as f64 != b)),
                        (Type::Float64(a), Type::Int64(b)) => Ok(Type::Bool(a != b as f64)),
                        (Type::Bool(a), Type::Bool(b)) => Ok(Type::Bool(a != b)),
                        (Type::Str(a), Type::Str(b)) => Ok(Type::Bool(a != b)),
                        (Type::Null, Type::Null) => Ok(Type::Bool(false)),
                        (a, b) => Err(format!("Cannot compare {:?} and {:?}", a, b)),
                    },
                    Symbol::Lt => match (l, r.unwrap()) {
                        (Type::Int32(a), Type::Int32(b)) => Ok(Type::Bool(a < b)),
                        (Type::Int64(a), Type::Int64(b)) => Ok(Type::Bool(a < b)),
                        (Type::Int32(a), Type::Int64(b)) => Ok(Type::Bool((a as i64) < b)),
                        (Type::Int64(a), Type::Int32(b)) => Ok(Type::Bool(a < b as i64)),
                        (Type::Float32(a), Type::Float32(b)) => Ok(Type::Bool(a < b)),
                        (Type::Float64(a), Type::Float64(b)) => Ok(Type::Bool(a < b)),
                        (Type::Int32(a), Type::Float64(b)) => Ok(Type::Bool((a as f64) < b)),
                        (Type::Float64(a), Type::Int32(b)) => Ok(Type::Bool(a < b as f64)),
                        (Type::Int64(a), Type::Float64(b)) => Ok(Type::Bool((a as f64) < b)),
                        (Type::Float64(a), Type::Int64(b)) => Ok(Type::Bool(a < b as f64)),
                        (Type::Str(a), Type::Str(b)) => Ok(Type::Bool(a < b)),
                        (a, b) => Err(format!("Cannot compare {:?} < {:?}", a, b)),
                    },
                    Symbol::Gt => match (l, r.unwrap()) {
                        (Type::Int32(a), Type::Int32(b)) => Ok(Type::Bool(a > b)),
                        (Type::Int64(a), Type::Int64(b)) => Ok(Type::Bool(a > b)),
                        (Type::Int32(a), Type::Int64(b)) => Ok(Type::Bool((a as i64) > b)),
                        (Type::Int64(a), Type::Int32(b)) => Ok(Type::Bool(a > b as i64)),
                        (Type::Float32(a), Type::Float32(b)) => Ok(Type::Bool(a > b)),
                        (Type::Float64(a), Type::Float64(b)) => Ok(Type::Bool(a > b)),
                        (Type::Int32(a), Type::Float64(b)) => Ok(Type::Bool((a as f64) > b)),
                        (Type::Float64(a), Type::Int32(b)) => Ok(Type::Bool(a > b as f64)),
                        (Type::Int64(a), Type::Float64(b)) => Ok(Type::Bool((a as f64) > b)),
                        (Type::Float64(a), Type::Int64(b)) => Ok(Type::Bool(a > b as f64)),
                        (Type::Str(a), Type::Str(b)) => Ok(Type::Bool(a > b)),
                        (a, b) => Err(format!("Cannot compare {:?} > {:?}", a, b)),
                    },
                    Symbol::LtEq => match (l, r.unwrap()) {
                        (Type::Int32(a), Type::Int32(b)) => Ok(Type::Bool(a <= b)),
                        (Type::Int64(a), Type::Int64(b)) => Ok(Type::Bool(a <= b)),
                        (Type::Int32(a), Type::Int64(b)) => Ok(Type::Bool((a as i64) <= b)),
                        (Type::Int64(a), Type::Int32(b)) => Ok(Type::Bool(a <= b as i64)),
                        (Type::Float32(a), Type::Float32(b)) => Ok(Type::Bool(a <= b)),
                        (Type::Float64(a), Type::Float64(b)) => Ok(Type::Bool(a <= b)),
                        (Type::Int32(a), Type::Float64(b)) => Ok(Type::Bool((a as f64) <= b)),
                        (Type::Float64(a), Type::Int32(b)) => Ok(Type::Bool(a <= b as f64)),
                        (Type::Int64(a), Type::Float64(b)) => Ok(Type::Bool((a as f64) <= b)),
                        (Type::Float64(a), Type::Int64(b)) => Ok(Type::Bool(a <= b as f64)),
                        (Type::Str(a), Type::Str(b)) => Ok(Type::Bool(a <= b)),
                        (a, b) => Err(format!("Cannot compare {:?} <= {:?}", a, b)),
                    },
                    Symbol::GtEq => match (l, r.unwrap()) {
                        (Type::Int32(a), Type::Int32(b)) => Ok(Type::Bool(a >= b)),
                        (Type::Int64(a), Type::Int64(b)) => Ok(Type::Bool(a >= b)),
                        (Type::Int32(a), Type::Int64(b)) => Ok(Type::Bool((a as i64) >= b)),
                        (Type::Int64(a), Type::Int32(b)) => Ok(Type::Bool(a >= b as i64)),
                        (Type::Float32(a), Type::Float32(b)) => Ok(Type::Bool(a >= b)),
                        (Type::Float64(a), Type::Float64(b)) => Ok(Type::Bool(a >= b)),
                        (Type::Int32(a), Type::Float64(b)) => Ok(Type::Bool((a as f64) >= b)),
                        (Type::Float64(a), Type::Int32(b)) => Ok(Type::Bool(a >= b as f64)),
                        (Type::Int64(a), Type::Float64(b)) => Ok(Type::Bool((a as f64) >= b)),
                        (Type::Float64(a), Type::Int64(b)) => Ok(Type::Bool(a >= b as f64)),
                        (Type::Str(a), Type::Str(b)) => Ok(Type::Bool(a >= b)),
                        (a, b) => Err(format!("Cannot compare {:?} >= {:?}", a, b)),
                    },
                    Symbol::Modulo => match (l, r.unwrap()) {
                        (Type::Int32(a), Type::Int32(b)) => Ok(Type::Int32(a % b)),
                        (Type::Int64(a), Type::Int64(b)) => Ok(Type::Int64(a % b)),
                        (Type::Int32(a), Type::Int64(b)) => Ok(Type::Int64(a as i64 % b)),
                        (Type::Int64(a), Type::Int32(b)) => Ok(Type::Int64(a % b as i64)),
                        (Type::Float32(a), Type::Float32(b)) => Ok(Type::Float32(a % b)),
                        (Type::Float64(a), Type::Float64(b)) => Ok(Type::Float64(a % b)),
                        (Type::Int32(a), Type::Float64(b)) => Ok(Type::Float64(a as f64 % b)),
                        (Type::Float64(a), Type::Int32(b)) => Ok(Type::Float64(a % b as f64)),
                        (Type::Int64(a), Type::Float64(b)) => Ok(Type::Float64(a as f64 % b)),
                        (Type::Float64(a), Type::Int64(b)) => Ok(Type::Float64(a % b as f64)),
                        (a, b) => Err(format!("Cannot modulo {:?} and {:?}", a, b)),
                    },
                    Symbol::Power => match (l, r.unwrap()) {
                        (Type::Int32(a), Type::Int32(b)) => {
                            if b >= 0 {
                                Ok(Type::Int32(a.pow(b as u32)))
                            } else {
                                Ok(Type::Float64((a as f64).powi(b)))
                            }
                        }
                        (Type::Int64(a), Type::Int32(b)) => {
                            if b >= 0 {
                                Ok(Type::Int64(a.pow(b as u32)))
                            } else {
                                Ok(Type::Float64((a as f64).powi(b)))
                            }
                        }
                        (Type::Float32(a), Type::Int32(b)) => Ok(Type::Float32(a.powi(b))),
                        (Type::Float64(a), Type::Int32(b)) => Ok(Type::Float64(a.powi(b))),
                        (Type::Float32(a), Type::Float32(b)) => Ok(Type::Float32(a.powf(b))),
                        (Type::Float64(a), Type::Float64(b)) => Ok(Type::Float64(a.powf(b))),
                        (Type::Int32(a), Type::Float64(b)) => Ok(Type::Float64((a as f64).powf(b))),
                        (Type::Int64(a), Type::Float64(b)) => Ok(Type::Float64((a as f64).powf(b))),
                        (a, b) => Err(format!("Cannot raise {:?} to power {:?}", a, b)),
                    },
                    Symbol::And => {
                        let l_bool = match l {
                            Type::Bool(b) => b,
                            _ => return Err(format!("Cannot use && on {:?}", l)),
                        };
                        if !l_bool {
                            Ok(Type::Bool(false))
                        } else {
                            let r_val = r.unwrap();
                            match r_val {
                                Type::Bool(b) => Ok(Type::Bool(b)),
                                _ => Err(format!("Cannot use && on {:?}", r_val)),
                            }
                        }
                    }
                    // Logical Or (short-circuit)
                    Symbol::Or => {
                        let l_bool = match l {
                            Type::Bool(b) => b,
                            _ => return Err(format!("Cannot use || on {:?}", l)),
                        };
                        if l_bool {
                            Ok(Type::Bool(true))
                        } else {
                            let r_val = r.unwrap();
                            match r_val {
                                Type::Bool(b) => Ok(Type::Bool(b)),
                                _ => Err(format!("Cannot use || on {:?}", r_val)),
                            }
                        }
                    }
                    // Logical Not (unary)
                    Symbol::Not => match l {
                        Type::Bool(b) => Ok(Type::Bool(!b)),
                        _ => Err(format!("Cannot use ! on {:?}", l)),
                    },
                    // Floor division
                    Symbol::FloorDiv => match (l, r.unwrap()) {
                        (Type::Int32(a), Type::Int32(b)) => Ok(Type::Int32(a / b)),
                        (Type::Int64(a), Type::Int64(b)) => Ok(Type::Int64(a / b)),
                        (Type::Int32(a), Type::Int64(b)) => Ok(Type::Int64(a as i64 / b)),
                        (Type::Int64(a), Type::Int32(b)) => Ok(Type::Int64(a / b as i64)),
                        (Type::Float32(a), Type::Float32(b)) => Ok(Type::Float32((a / b).floor())),
                        (Type::Float64(a), Type::Float64(b)) => Ok(Type::Float64((a / b).floor())),
                        (Type::Int32(a), Type::Float64(b)) => {
                            Ok(Type::Float64((a as f64 / b).floor()))
                        }
                        (Type::Float64(a), Type::Int32(b)) => {
                            Ok(Type::Float64((a / b as f64).floor()))
                        }
                        (Type::Int64(a), Type::Float64(b)) => {
                            Ok(Type::Float64((a as f64 / b).floor()))
                        }
                        (Type::Float64(a), Type::Int64(b)) => {
                            Ok(Type::Float64((a / b as f64).floor()))
                        }
                        (a, b) => Err(format!("Cannot floor divide {:?} and {:?}", a, b)),
                    },
                    // Bitwise
                    Symbol::BitAnd => match (l, r.unwrap()) {
                        (Type::Int32(a), Type::Int32(b)) => Ok(Type::Int32(a & b)),
                        (Type::Int64(a), Type::Int64(b)) => Ok(Type::Int64(a & b)),
                        (Type::Int32(a), Type::Int64(b)) => Ok(Type::Int64(a as i64 & b)),
                        (Type::Int64(a), Type::Int32(b)) => Ok(Type::Int64(a & b as i64)),
                        (a, b) => Err(format!("Cannot bitwise and {:?} and {:?}", a, b)),
                    },
                    Symbol::BitOr => match (l, r.unwrap()) {
                        (Type::Int32(a), Type::Int32(b)) => Ok(Type::Int32(a | b)),
                        (Type::Int64(a), Type::Int64(b)) => Ok(Type::Int64(a | b)),
                        (Type::Int32(a), Type::Int64(b)) => Ok(Type::Int64(a as i64 | b)),
                        (Type::Int64(a), Type::Int32(b)) => Ok(Type::Int64(a | b as i64)),
                        (a, b) => Err(format!("Cannot bitwise or {:?} and {:?}", a, b)),
                    },
                    Symbol::BitXor => match (l, r.unwrap()) {
                        (Type::Int32(a), Type::Int32(b)) => Ok(Type::Int32(a ^ b)),
                        (Type::Int64(a), Type::Int64(b)) => Ok(Type::Int64(a ^ b)),
                        (Type::Int32(a), Type::Int64(b)) => Ok(Type::Int64(a as i64 ^ b)),
                        (Type::Int64(a), Type::Int32(b)) => Ok(Type::Int64(a ^ b as i64)),
                        (a, b) => Err(format!("Cannot bitwise xor {:?} and {:?}", a, b)),
                    },
                    Symbol::BitNot => match l {
                        Type::Int32(a) => Ok(Type::Int32(!a)),
                        Type::Int64(a) => Ok(Type::Int64(!a)),
                        a => Err(format!("Cannot bitwise not {:?}", a)),
                    },
                    // Structural symbols - not used as operators in BinaryOp
                    Symbol::Assign
                    | Symbol::Init
                    | Symbol::Dot
                    | Symbol::Arrow
                    | Symbol::Comma
                    | Symbol::Index
                    | Symbol::Call => Err(format!(
                        "Symbol {:?} is not a valid operator in BinaryOp",
                        op
                    )),
                }
            }
            ExprNode::IfElseNode {
                statement,
                then_branch,
                else_branch,
            } => {
                let cond_res = statement.execute_core(ctx)?;

                let cond_res_bool = match cond_res {
                    Type::Bool(b) => b,
                    _ => return Err(format!("the if condition cannot be casted to <bool>")),
                };

                if cond_res_bool {
                    let mut last_result = Type::Void;
                    for i in then_branch.iter() {
                        last_result = i.execute(ctx)?;
                        if ctx.has_returned() {
                            break;
                        }
                    }
                    Ok(last_result)
                } else {
                    match else_branch {
                        Some(else_b) => {
                            let mut last_result = Type::Void;
                            for i in else_b.iter() {
                                last_result = i.execute(ctx)?;
                                if ctx.has_returned() {
                                    break;
                                }
                            }
                            Ok(last_result)
                        }
                        None => Ok(Type::Void),
                    }
                }
            }
            ExprNode::LoopNode { statement, body } => {
                let mut last_result = Type::Void;

                loop {
                    let cond_res = statement.execute_core(ctx)?;

                    let cond_bool = match cond_res {
                        Type::Bool(b) => b,
                        _ => return Err(format!("the loop condition cannot be cast to <bool>")),
                    };

                    if !cond_bool {
                        break;
                    }

                    for stmt in body.iter() {
                        last_result = stmt.execute(ctx)?;
                        if ctx.has_returned() {
                            return Ok(last_result);
                        }
                    }
                }

                Ok(last_result)
            }
            ExprNode::FunctionCall { name, args } => {
                let mut arg_values = Vec::new();
                for arg_expr in args.iter() {
                    arg_values.push(arg_expr.execute_core(ctx)?);
                }

                let func = ctx.resolve_function(name, &arg_values).ok_or_else(|| {
                    let type_names = arg_values
                        .iter()
                        .map(|v| format!("{:?}", v.get_kind()))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!(
                        "runtime error: no matching function '{}' with argument types ({})",
                        name, type_names
                    )
                })?;

                let mut local_ctx = Context::new(ctx.functions.clone(), ctx.structs.clone());
                for (param, val) in func.args.iter().zip(arg_values.into_iter()) {
                    local_ctx.add_variable(&param.name, val)?;
                }

                let mut result = Type::Void;
                for stmt in &func.body {
                    result = stmt.execute(&mut local_ctx)?;
                    if local_ctx.has_returned() {
                        result = local_ctx.take_return().unwrap();
                        break;
                    }
                }
                Ok(result)
            }
            ExprNode::Len(expr) => {
                let val = expr.execute_core(ctx)?;
                match val {
                    Type::Array(arr) => Ok(Type::Int32(arr.len() as i32)),
                    Type::Str(s) => Ok(Type::Int32(s.len() as i32)),
                    _ => Err(format!("cannot get length of {:?}", val.get_kind())),
                }
            }
            ExprNode::Maybe => {
                let random_val: bool = rand::thread_rng().r#gen();

                Ok(Type::Bool(random_val))
            }
            ExprNode::StructLiteral {
                struct_name,
                fields,
            } => {
                let struct_def = ctx
                    .structs
                    .get(struct_name)
                    .ok_or_else(|| format!("unknown struct: '{}'", struct_name))?
                    .clone();

                let mut field_values: Vec<(String, Type)> = Vec::new();
                for (name, expr) in fields {
                    let val = expr.execute_core(ctx)?;
                    field_values.push((name.clone(), val));
                }

                struct_def.instantiate(field_values)
            }
            ExprNode::Field { object, field_name } => {
                let obj = object.execute_core(ctx)?;
                obj.get_field(field_name)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_add_intint() {
        let a = Type::Int32(2);
        let b = Type::Int32(3);

        let res = (a + b).unwrap();

        match res {
            Type::Int32(x) => assert_eq!(x, 5),
            _ => panic!("int + int fucked"),
        }
    }

    #[test]
    fn test_type_add_strstr() {
        let a = Type::Str(String::from("abc"));
        let b = Type::Str(String::from("def"));

        let res = (a + b).unwrap();

        match res {
            Type::Str(s) => assert_eq!(s, "abcdef"),
            _ => panic!("str + str test failed"),
        }
    }

    #[test]
    fn test_type_add_floatint() {
        let a = Type::Float64(67.67);
        let b = Type::Int32(12);

        let res = (a + b).unwrap();

        match res {
            Type::Float64(x) => assert_eq!(x, 79.67),
            _ => panic!("float + int failed"),
        }
    }

    #[test]
    fn add_strstr() {
        let a = Type::Str(String::from("abc"));
        let b = Type::Str(String::from("def"));

        let res = (a + b).unwrap();

        match res {
            Type::Str(s) => assert_eq!(s, "abcdef"),
            _ => panic!("str + str failed"),
        }
    }

    #[test]
    fn add_strint() {
        let a = Type::Str(String::from("abc"));
        let b = Type::Int32(12);

        let res = (a + b).unwrap();

        match res {
            Type::Str(s) => assert_eq!(s, "abc12"),
            _ => panic!("str + int failed"),
        }
    }

    #[test]
    fn add_strfloat() {
        let a = Type::Str(String::from("abc"));
        let b = Type::Float64(3.14);

        let res = (a + b).unwrap();

        match res {
            Type::Str(s) => assert_eq!(s, "abc3.14"),
            _ => panic!("str + float failed"),
        }
    }

    #[test]
    fn test_complex_expr() {
        //(2 + 3)
        let ctx = &mut Context::empty();

        let a = ExprNode::Literal(Type::Int32(2));
        let b = ExprNode::Literal(Type::Int32(3));
        let e = ASTNode::Expr(ExprNode::BinaryOp {
            op: Symbol::Plus,
            left: Rc::new(a),
            right: Some(Rc::new(b)),
        });

        let res = e.execute(ctx).unwrap();

        match res {
            Type::Int32(x) => assert_eq!(x, 5),
            _ => panic!("failed 2 + 3"),
        }
    }

    #[test]
    fn test_complex_expr_str_res() {
        let ctx = &mut Context::empty();

        let a = ExprNode::Literal(Type::Str(String::from("pluh")));
        let b = ExprNode::Literal(Type::Int32(12));
        let c = ExprNode::Literal(Type::Int32(3));

        let e1 = ExprNode::BinaryOp {
            op: Symbol::Plus,
            left: Rc::new(b),
            right: Some(Rc::new(c)),
        };

        let e2 = ExprNode::BinaryOp {
            op: Symbol::Plus,
            left: Rc::new(a),
            right: Some(Rc::new(e1)),
        };

        let a = ASTNode::Expr(e2);

        let res = a.execute(ctx).unwrap();

        match res {
            Type::Str(s) => assert_eq!(s, "pluh15"),
            _ => panic!("str + (int + int) failed"),
        }
    }

    #[test]
    fn test_sub_intint() {
        let a = Type::Int32(5);
        let b = Type::Int32(2);

        let res = (a - b).unwrap();

        match res {
            Type::Int32(x) => assert_eq!(x, 3),
            _ => panic!("int - int failed"),
        }
    }

    #[test]
    fn test_mult() {
        let a = Type::Int32(12);
        let b = Type::Int32(2);

        let res = (a * b).unwrap();

        match res {
            Type::Int32(x) => assert_eq!(x, 24),
            _ => panic!("int * int faield"),
        }
    }

    #[test]
    fn test_div() {
        let a = Type::Int32(6);
        let b = Type::Int32(2);

        let res1 = (a / b).unwrap();

        match res1 {
            Type::Float64(x) => assert_eq!(x, 3 as f64),
            _ => panic!("int / int failed"),
        }
    }

    #[test]
    fn test_var_def_basic() {
        let ctx = &mut Context::empty();

        println!("vars before declaration:");
        ctx._debug_print();

        let a = ExprNode::Literal(Type::Int32(5));

        let def = ASTNode::DefineVar(DefineVariable::new(Type::Int32(1), "x", Rc::new(a)));

        let res = def.execute(ctx).unwrap();
        println!("\nvars after declaration:");
        ctx._debug_print();

        match res {
            Type::Int32(x) => assert_eq!(x, 5),
            _ => panic!("expected Int32(5), got: {:?}", res),
        }

        match ctx.vars.get("x") {
            Some(Type::Int32(val)) => assert_eq!(*val, 5),
            Some(other) => panic!("Variable x has wrong type: {:?}", other),
            None => panic!("Variable x was not added to context"),
        }
    }

    #[test]
    fn test_var_ref() {
        let ctx = &mut Context::empty();

        println!("vars before declaration:");
        ctx._debug_print();

        let a = ExprNode::Literal(Type::Int32(5));

        let def = ASTNode::DefineVar(DefineVariable::new(Type::Int32(1), "x", Rc::new(a)));

        let res = def.execute(ctx).unwrap();
        println!("\nvars after declaration:");
        ctx._debug_print();

        match res {
            Type::Int32(x) => assert_eq!(x, 5),
            _ => panic!("expected Int32(5), got: {:?}", res),
        }

        match ctx.vars.get("x") {
            Some(Type::Int32(val)) => assert_eq!(*val, 5),
            Some(other) => panic!("Variable x has wrong type: {:?}", other),
            None => panic!("Variable x was not added to context"),
        }

        //res = x + 2

        let ref_node = ExprNode::Literal(Type::VarRef(String::from("x")));

        let a = ASTNode::Expr(ExprNode::BinaryOp {
            op: Symbol::Plus,
            left: Rc::new(ref_node),
            right: Some(Rc::new(ExprNode::Literal(Type::Int32(2)))),
        });

        let res = a.execute(ctx).unwrap();
        match res {
            Type::Int32(x) => assert_eq!(x, 7),
            _ => panic!("var ref failed"),
        }
    }

    #[test]
    fn test_if_1() {
        let ctx = &mut Context::empty();

        // if (true) { x := 10 } else { x := 20 }
        // should set x to 10

        let then_branch = vec![ASTNode::DefineVar(DefineVariable::new(
            Type::Int32(1),
            "x",
            Rc::new(ExprNode::Literal(Type::Int32(10))),
        ))];

        let else_branch = vec![ASTNode::DefineVar(DefineVariable::new(
            Type::Int32(1),
            "x",
            Rc::new(ExprNode::Literal(Type::Int32(20))),
        ))];

        let if_node = ASTNode::Expr(ExprNode::IfElseNode {
            statement: Rc::new(ExprNode::Literal(Type::Bool(true))),
            then_branch: Rc::new(then_branch),
            else_branch: Some(Rc::new(else_branch)),
        });

        let _res = if_node.execute(ctx).unwrap();

        match ctx.vars.get("x") {
            Some(Type::Int32(val)) => assert_eq!(*val, 10),
            Some(other) => panic!("Variable x has wrong type: {:?}", other),
            None => panic!("Variable x was not created"),
        }
    }

    #[test]
    fn test_print() {
        let ctx = &mut Context::empty();

        let e = ExprNode::Literal(Type::Float64(12.67));
        let print = ASTNode::Print(e);

        let res = print.execute(ctx).unwrap();

        match res {
            Type::Float64(x) => assert_eq!(x, 12.67),
            _ => panic!("print failed"),
        }
    }

    #[test]
    fn test_while_loop() {
        // let i: int = 0; while (i < 5) { i = i + 1 }; i should be 5
        let ctx = &mut Context::empty();

        // init i
        let define_i = ASTNode::DefineVar(DefineVariable::new(
            Type::Int32(0),
            "i",
            Rc::new(ExprNode::Literal(Type::Int32(0))),
        ));
        define_i.execute(ctx).unwrap();

        // condition: i < 5
        let condition = ExprNode::BinaryOp {
            op: Symbol::Lt,
            left: Rc::new(ExprNode::Literal(Type::VarRef(String::from("i")))),
            right: Some(Rc::new(ExprNode::Literal(Type::Int32(5)))),
        };

        // body: i = i + 1
        let increment = ASTNode::SetVar(SetVariable::new(
            Type::Int32(0),
            "i",
            Rc::new(ExprNode::BinaryOp {
                op: Symbol::Plus,
                left: Rc::new(ExprNode::Literal(Type::VarRef(String::from("i")))),
                right: Some(Rc::new(ExprNode::Literal(Type::Int32(1)))),
            }),
        ));

        let while_loop = ASTNode::Expr(ExprNode::LoopNode {
            statement: Rc::new(condition),
            body: Rc::new(vec![increment]),
        });

        while_loop.execute(ctx).unwrap();

        match ctx.vars.get("i") {
            Some(Type::Int32(val)) => assert_eq!(*val, 5),
            Some(other) => panic!("Variable i has wrong type: {:?}", other),
            None => panic!("Variable i not found"),
        }
    }

    #[test]
    fn test_while_loop_no_iterations() {
        // while (false) { } should not execute body
        let ctx = &mut Context::empty();

        // x := 10
        let define_x = ASTNode::DefineVar(DefineVariable::new(
            Type::Int32(0),
            "x",
            Rc::new(ExprNode::Literal(Type::Int32(10))),
        ));
        define_x.execute(ctx).unwrap();

        // condition: false
        let condition = ExprNode::Literal(Type::Bool(false));

        // body: x = 99 (should never run)
        let set_x = ASTNode::SetVar(SetVariable::new(
            Type::Int32(0),
            "x",
            Rc::new(ExprNode::Literal(Type::Int32(99))),
        ));

        let while_loop = ASTNode::Expr(ExprNode::LoopNode {
            statement: Rc::new(condition),
            body: Rc::new(vec![set_x]),
        });

        while_loop.execute(ctx).unwrap();

        // x should still be 10
        match ctx.vars.get("x") {
            Some(Type::Int32(val)) => assert_eq!(*val, 10),
            _ => panic!("x should still be 10"),
        }
    }

    #[test]
    fn test_struct_instantiation() {
        // (struct User (fields (id i32) (name str)))
        // (let user User (struct User (id 1) (name "Alice")))

        use crate::types::{StructDef, TypeKind};

        let mut structs = FxHashMap::default();
        structs.insert(
            "User".to_string(),
            StructDef::new("User")
                .with_field("id", TypeKind::Int32)
                .with_field("name", TypeKind::Str),
        );

        let functions: FxHashMap<String, Vec<Function>> = FxHashMap::default();
        let ctx = &mut Context::new(Rc::new(functions), Rc::new(structs));

        // (struct User (id 1) (name "Alice"))
        let struct_literal = ExprNode::StructLiteral {
            struct_name: "User".to_string(),
            fields: vec![
                ("id".to_string(), Rc::new(ExprNode::Literal(Type::Int32(1)))),
                (
                    "name".to_string(),
                    Rc::new(ExprNode::Literal(Type::Str("Alice".to_string()))),
                ),
            ],
        };

        let result = ASTNode::Expr(struct_literal).execute(ctx).unwrap();

        match result {
            Type::Object { type_name, fields } => {
                assert_eq!(type_name, "User");
                assert_eq!(fields.get("id"), Some(&Type::Int32(1)));
                assert_eq!(fields.get("name"), Some(&Type::Str("Alice".to_string())));
            }
            _ => panic!("expected Object, got {:?}", result),
        }
    }

    #[test]
    fn test_field_access() {
        // (field user name) -> "Alice"

        use crate::types::{StructDef, TypeKind};

        let mut structs = FxHashMap::default();
        structs.insert(
            "User".to_string(),
            StructDef::new("User")
                .with_field("id", TypeKind::Int32)
                .with_field("name", TypeKind::Str),
        );

        let functions: FxHashMap<String, Vec<Function>> = FxHashMap::default();
        let ctx = &mut Context::new(Rc::new(functions), Rc::new(structs));

        // Create user and store in variable
        let struct_literal = ExprNode::StructLiteral {
            struct_name: "User".to_string(),
            fields: vec![
                (
                    "id".to_string(),
                    Rc::new(ExprNode::Literal(Type::Int32(42))),
                ),
                (
                    "name".to_string(),
                    Rc::new(ExprNode::Literal(Type::Str("Bob".to_string()))),
                ),
            ],
        };

        let define_user = ASTNode::DefineVar(DefineVariable::new(
            Type::Object {
                type_name: "User".to_string(),
                fields: std::collections::BTreeMap::new(),
            },
            "user",
            Rc::new(struct_literal),
        ));
        define_user.execute(ctx).unwrap();

        // (field user name)
        let field_access = ExprNode::Field {
            object: Rc::new(ExprNode::Literal(Type::VarRef("user".to_string()))),
            field_name: "name".to_string(),
        };

        let result = ASTNode::Expr(field_access).execute(ctx).unwrap();

        match result {
            Type::Str(s) => assert_eq!(s, "Bob"),
            _ => panic!("expected Str, got {:?}", result),
        }
    }

    #[test]
    fn test_set_field() {
        // (assign_field user name "cro")

        use crate::types::{StructDef, TypeKind};

        let mut structs = FxHashMap::default();
        structs.insert(
            "User".to_string(),
            StructDef::new("User")
                .with_field("id", TypeKind::Int32)
                .with_field("name", TypeKind::Str),
        );

        let functions: FxHashMap<String, Vec<Function>> = FxHashMap::default();
        let ctx = &mut Context::new(Rc::new(functions), Rc::new(structs));

        // Create user
        let struct_literal = ExprNode::StructLiteral {
            struct_name: "User".to_string(),
            fields: vec![
                ("id".to_string(), Rc::new(ExprNode::Literal(Type::Int32(1)))),
                (
                    "name".to_string(),
                    Rc::new(ExprNode::Literal(Type::Str("-".to_string()))),
                ),
            ],
        };

        let define_user = ASTNode::DefineVar(DefineVariable::new(
            Type::Object {
                type_name: "User".to_string(),
                fields: std::collections::BTreeMap::new(),
            },
            "user",
            Rc::new(struct_literal),
        ));
        define_user.execute(ctx).unwrap();

        // (assign_field user name "Charlie")
        let set_field = ASTNode::SetField {
            object_name: "user".to_string(),
            field_name: "name".to_string(),
            value: Rc::new(ExprNode::Literal(Type::Str("cro".to_string()))),
        };
        set_field.execute(ctx).unwrap();

        // Verify the field was updated
        let field_access = ExprNode::Field {
            object: Rc::new(ExprNode::Literal(Type::VarRef("user".to_string()))),
            field_name: "name".to_string(),
        };

        let result = ASTNode::Expr(field_access).execute(ctx).unwrap();

        match result {
            Type::Str(s) => assert_eq!(s, "cro"),
            _ => panic!("expected Str 'Charlie', got {:?}", result),
        }
    }
}

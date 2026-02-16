use crate::ast::{ASTNode, Context, Executable};
use crate::types::{Type, TypeKind};

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct FunctionParam {
    pub name: String,
    pub param_type: TypeKind,
    /// For array params, the element type (e.g. `[i32]` → Some(Int32)).
    pub array_element_type: Option<TypeKind>,
}

//the function call node provides the context
#[derive(Clone)]
pub struct Function {
    pub name: String,
    pub return_type: Type,
    pub args: Vec<FunctionParam>,
    pub body: Vec<ASTNode>,
}

// usable vars are brought in explicitly --you can kinda just pass tf you want
impl Function {
    pub fn new(n: &str, rt: Type) -> Self {
        Function {
            name: String::from(n),
            return_type: rt,
            args: Vec::new(),
            body: Vec::new(),
        }
    }

    pub fn execute_with_args(
        &self,
        ctx: &mut Context,
        arg_values: Vec<(String, Type)>,
    ) -> Result<Type, String> {
        let mut local_ctx = Context::new(
            ctx.functions.clone(),
            ctx.structs.clone(),
            ctx.static_vars.clone(),
        );

        for (name, val) in arg_values {
            local_ctx.add_variable(&name, val)?;
        }

        let mut last_res = Type::Void;
        for stmt in &self.body {
            last_res = stmt.execute(&mut local_ctx)?;
        }

        Ok(last_res)
    }
}

impl Executable for Function {
    fn execute(&self, ctx: &mut Context) -> Result<Type, String> {
        let mut local_ctx = Context::new(
            ctx.functions.clone(),
            ctx.structs.clone(),
            ctx.static_vars.clone(),
        );

        for i in self.args.iter() {
            let res = local_ctx.add_variable(&i.name, ctx.get_variable_reference(&i.name));

            match res {
                Ok(_) => {}
                Err(e) => return Err(format!("runtime error: {:?}", e)),
            }
        }

        let mut last_res = Type::Void;

        for i in self.body.iter() {
            let res = i.execute(&mut local_ctx);

            match res {
                Ok(r) => last_res = r,
                Err(e) => return Err(format!("runtime error: {:?}", e)),
            }
        }

        if last_res.get_kind() == self.return_type.get_kind() {
            return Ok(last_res);
        }

        return Err(format!("mismatched return type"));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ExprNode;
    use rustc_hash::FxHashMap;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn fn_test_basic() {
        let mut ctx = Context::empty();

        let mut func = Function::new("get_ten", Type::Int32(0));
        func.body
            .push(ASTNode::Expr(ExprNode::Literal(Type::Int32(10))));

        let result = func.execute(&mut ctx).unwrap();

        match result {
            Type::Int32(val) => {
                println!("Function executed, returned: {:?}", val);
                assert_eq!(val, 10);
            }
            _ => panic!("Expected Int32 return type"),
        }
    }

    #[test]
    fn test_add() {
        let mut ctx = Context::empty();

        /*
        pretty much fn add(x: int, y: int) -> int {
           x + y
        }
        called as: add(2, 3) -> 5
        */

        let mut func = Function::new("add", Type::Int32(0));

        func.body.push(ASTNode::Expr(ExprNode::BinaryOp {
            op: crate::ast::Symbol::Plus,
            left: Rc::new(ExprNode::Literal(Type::VarRef(String::from("x")))),
            right: Some(Rc::new(ExprNode::Literal(Type::VarRef(String::from("y"))))),
        }));

        func.args.push(FunctionParam {
            name: String::from("x"),
            param_type: TypeKind::Int32,
            array_element_type: None,
        });
        func.args.push(FunctionParam {
            name: String::from("y"),
            param_type: TypeKind::Int32,
            array_element_type: None,
        });

        ctx.add_variable("x", Type::Int32(2)).unwrap();
        ctx.add_variable("y", Type::Int32(3)).unwrap();

        let res = func.execute(&mut ctx).unwrap();

        match res {
            Type::Int32(val) => {
                assert_eq!(val, 5);
            }
            _ => panic!("expected INT"),
        }
    }

    #[test]
    fn test_add_with_execute_with_args() {
        let mut ctx = Context::empty();

        /*
        fn add(x: int, y: int) -> int {
           x + y
        }
        called as: add(10, 25) -> 35
        */

        let mut func = Function::new("add", Type::Int32(0));

        func.body.push(ASTNode::Expr(ExprNode::BinaryOp {
            op: crate::ast::Symbol::Plus,
            left: Rc::new(ExprNode::Literal(Type::VarRef(String::from("x")))),
            right: Some(Rc::new(ExprNode::Literal(Type::VarRef(String::from("y"))))),
        }));

        func.args.push(FunctionParam {
            name: String::from("x"),
            param_type: TypeKind::Int32,
            array_element_type: None,
        });
        func.args.push(FunctionParam {
            name: String::from("y"),
            param_type: TypeKind::Int32,
            array_element_type: None,
        });

        let arg_values = vec![
            (String::from("x"), Type::Int32(10)),
            (String::from("y"), Type::Int32(25)),
        ];

        let res = func.execute_with_args(&mut ctx, arg_values).unwrap();

        match res {
            Type::Int32(val) => {
                assert_eq!(val, 35);
            }
            _ => panic!("expected INT"),
        }
    }

    #[test]
    fn test_function_call_via_ast() {
        /*
        fn get_value() -> int {
            42
        }
        called via FunctionCall node
        */

        let mut functions: FxHashMap<String, Vec<Function>> = FxHashMap::default();

        let mut get_value = Function::new("get_value", Type::Int32(0));
        get_value
            .body
            .push(ASTNode::Expr(ExprNode::Literal(Type::Int32(42))));

        functions.insert(String::from("get_value"), vec![get_value]);

        let functions_rc = Rc::new(functions);
        let mut ctx = Context::new(
            functions_rc,
            Rc::new(FxHashMap::default()),
            Rc::new(RefCell::new(FxHashMap::default())),
        );

        let call = ExprNode::FunctionCall {
            name: String::from("get_value"),
            args: Vec::new(),
        };

        let res = ASTNode::Expr(call).execute(&mut ctx).unwrap();

        match res {
            Type::Int32(val) => {
                assert_eq!(val, 42);
            }
            _ => panic!("expected INT"),
        }
    }

    #[test]
    fn test_recursive_factorial() {
        /*
        fn factorial(n: int) -> int {
            if (n == 0) {
                1
            } else {
                n * factorial(n - 1)
            }
        }
        called as: factorial(5) -> 120
        */

        let mut functions: FxHashMap<String, Vec<Function>> = FxHashMap::default();

        let mut factorial = Function::new("factorial", Type::Int32(0));

        // n == 0
        let condition = ExprNode::BinaryOp {
            op: crate::ast::Symbol::Eq,
            left: Rc::new(ExprNode::Literal(Type::VarRef(String::from("n")))),
            right: Some(Rc::new(ExprNode::Literal(Type::Int32(0)))),
        };

        // then branch: 1
        let then_branch = vec![ASTNode::Expr(ExprNode::Literal(Type::Int32(1)))];

        // else branch: n * factorial(n - 1)
        // first: n - 1
        let n_minus_1 = ExprNode::BinaryOp {
            op: crate::ast::Symbol::Minus,
            left: Rc::new(ExprNode::Literal(Type::VarRef(String::from("n")))),
            right: Some(Rc::new(ExprNode::Literal(Type::Int32(1)))),
        };

        // factorial(n - 1)
        let recursive_call = ExprNode::FunctionCall {
            name: String::from("factorial"),
            args: vec![Rc::new(n_minus_1)],
        };

        // n * factorial(n - 1)
        let multiplication = ExprNode::BinaryOp {
            op: crate::ast::Symbol::Multiply,
            left: Rc::new(ExprNode::Literal(Type::VarRef(String::from("n")))),
            right: Some(Rc::new(recursive_call)),
        };

        let else_branch = vec![ASTNode::Expr(multiplication)];

        // complete if-else
        let if_else = ExprNode::IfElseNode {
            statement: Rc::new(condition),
            then_branch: Rc::new(then_branch),
            else_branch: Some(Rc::new(else_branch)),
        };

        factorial.body.push(ASTNode::Expr(if_else));

        factorial.args.push(FunctionParam {
            name: String::from("n"),
            param_type: TypeKind::Int32,
            array_element_type: None,
        });

        functions.insert(String::from("factorial"), vec![factorial]);

        let functions_rc = Rc::new(functions);
        let mut ctx = Context::new(
            functions_rc,
            Rc::new(FxHashMap::default()),
            Rc::new(RefCell::new(FxHashMap::default())),
        );

        // Call factorial(5)
        let call = ExprNode::FunctionCall {
            name: String::from("factorial"),
            args: vec![Rc::new(ExprNode::Literal(Type::Int32(5)))],
        };

        let res = ASTNode::Expr(call).execute(&mut ctx).unwrap();

        match res {
            Type::Int32(val) => {
                assert_eq!(val, 120); // 5! = 120
            }
            _ => panic!("expected INT"),
        }
    }

    #[test]
    fn test_function_overloading_by_type() {
        /*
        fn ol(s: str) -> void { println("overload1 (string)") }
        fn ol(s: i32) -> void { println("overload2 (int)") }

        ol("hello") -> calls first overload (Str)
        ol(42) -> calls second overload (Int32)
        */

        let mut functions: FxHashMap<String, Vec<Function>> = FxHashMap::default();

        // First overload: ol(s: str)
        let mut ol_str = Function::new("ol", Type::Void);
        ol_str
            .body
            .push(ASTNode::Println(ExprNode::Literal(Type::Str(
                "overload1 (string)".to_string(),
            ))));
        ol_str.args.push(FunctionParam {
            name: String::from("s"),
            param_type: TypeKind::Str,
            array_element_type: None,
        });

        // Second overload: ol(s: i32)
        let mut ol_int = Function::new("ol", Type::Void);
        ol_int
            .body
            .push(ASTNode::Println(ExprNode::Literal(Type::Str(
                "overload2 (int)".to_string(),
            ))));
        ol_int.args.push(FunctionParam {
            name: String::from("s"),
            param_type: TypeKind::Int32,
            array_element_type: None,
        });

        functions.insert(String::from("ol"), vec![ol_str, ol_int]);

        let functions_rc = Rc::new(functions);
        let mut ctx = Context::new(
            functions_rc,
            Rc::new(FxHashMap::default()),
            Rc::new(RefCell::new(FxHashMap::default())),
        );

        // Test ol("hello") -> should call first overload (Str)
        let call_str = ExprNode::FunctionCall {
            name: String::from("ol"),
            args: vec![Rc::new(ExprNode::Literal(Type::Str("hello".to_string())))],
        };

        let res1 = ASTNode::Expr(call_str).execute(&mut ctx).unwrap();
        match res1 {
            Type::Str(s) => assert_eq!(s, "overload1 (string)"), // Println returns the printed string
            _ => panic!("expected Str"),
        }

        // Test ol(42) -> should call second overload (Int32)
        let call_int = ExprNode::FunctionCall {
            name: String::from("ol"),
            args: vec![Rc::new(ExprNode::Literal(Type::Int32(42)))],
        };

        let res2 = ASTNode::Expr(call_int).execute(&mut ctx).unwrap();
        match res2 {
            Type::Str(s) => assert_eq!(s, "overload2 (int)"), // Println returns the printed string
            _ => panic!("expected Str"),
        }
    }

    #[test]
    fn test_early_return() {
        /*
        fn early_return_test(x: int) -> int {
            if (x == 5) {
                return 99;
            }
            return 42;
        }
        called as: early_return_test(5) -> 99
        */

        let mut functions: FxHashMap<String, Vec<Function>> = FxHashMap::default();

        let mut func = Function::new("early_return_test", Type::Int32(0));

        // x == 5
        let condition = ExprNode::BinaryOp {
            op: crate::ast::Symbol::Eq,
            left: Rc::new(ExprNode::Literal(Type::VarRef(String::from("x")))),
            right: Some(Rc::new(ExprNode::Literal(Type::Int32(5)))),
        };

        // then branch: return 99
        let then_branch = vec![ASTNode::Return(ExprNode::Literal(Type::Int32(99)))];

        // if statement
        let if_stmt = ASTNode::Expr(ExprNode::IfElseNode {
            statement: Rc::new(condition),
            then_branch: Rc::new(then_branch),
            else_branch: None,
        });

        func.body.push(if_stmt);
        // ts should NOT execute if x == 5
        func.body
            .push(ASTNode::Return(ExprNode::Literal(Type::Int32(42))));

        func.args.push(FunctionParam {
            name: String::from("x"),
            param_type: TypeKind::Int32,
            array_element_type: None,
        });

        functions.insert(String::from("early_return_test"), vec![func]);

        let functions_rc = Rc::new(functions);
        let mut ctx = Context::new(
            functions_rc,
            Rc::new(FxHashMap::default()),
            Rc::new(RefCell::new(FxHashMap::default())),
        );

        let call = ExprNode::FunctionCall {
            name: String::from("early_return_test"),
            args: vec![Rc::new(ExprNode::Literal(Type::Int32(5)))],
        };

        let res = ASTNode::Expr(call).execute(&mut ctx).unwrap();

        match res {
            Type::Int32(val) => {
                assert_eq!(val, 99); // Should early return 99, not 42
            }
            _ => panic!("expected INT"),
        }
    }
}

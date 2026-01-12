use crate::ast::{ASTNode, ExprNode, Symbol};
use crate::functions::{Function, FunctionParam};
use crate::types::{StructDef, Type, TypeKind};

pub trait Emit {
    fn emit(&self) -> String;
}

impl Emit for Type {
    fn emit(&self) -> String {
        match self {
            Type::Int32(i) => i.to_string(),
            Type::Int64(i) => i.to_string(),
            Type::Float32(f) => {
                let s = f.to_string();
                if s.contains('.') {
                    s
                } else {
                    format!("{}.0", s)
                }
            }
            Type::Float64(f) => {
                let s = f.to_string();
                if s.contains('.') {
                    s
                } else {
                    format!("{}.0", s)
                }
            }
            Type::Str(s) => format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\"")),
            Type::Bool(b) => b.to_string(),
            Type::Null => "null".to_string(),
            Type::VarRef(name) => name.clone(),
            Type::Void => "void".to_string(),
            Type::Array(elements) => {
                let inner: Vec<String> = elements.iter().map(|e| e.emit()).collect();
                format!("(list {})", inner.join(" "))
            }
            Type::Optional(inner) => format!("{}!", inner.emit()),
            Type::Object { type_name, fields } => {
                let field_strs: Vec<String> = fields
                    .iter()
                    .map(|(name, val)| format!("({} {})", name, val.emit()))
                    .collect();
                format!("(struct {} {})", type_name, field_strs.join(" "))
            }
            Type::Ref(inner) => format!("(ref {})", inner.emit()),
            Type::Break => "break".to_string(),
            Type::Continue => "continue".to_string(),
        }
    }
}

impl Emit for TypeKind {
    fn emit(&self) -> String {
        match self {
            TypeKind::Int32 => "i32".to_string(),
            TypeKind::Int64 => "i64".to_string(),
            TypeKind::Float32 => "f32".to_string(),
            TypeKind::Float64 => "f64".to_string(),
            TypeKind::Str => "str".to_string(),
            TypeKind::Bool => "bool".to_string(),
            TypeKind::Void => "void".to_string(),
            TypeKind::Null => "null".to_string(),
            TypeKind::Array => "(array)".to_string(), // incomplete without inner type
            TypeKind::Optional => "optional".to_string(),
            TypeKind::Object => "object".to_string(),
            TypeKind::VarRef => "varref".to_string(),
            TypeKind::Ref => "ref".to_string(),
            TypeKind::Break => "break".to_string(),
            TypeKind::Continue => "continue".to_string(),
        }
    }
}

impl Emit for Symbol {
    fn emit(&self) -> String {
        match self {
            Symbol::Plus => "+".to_string(),
            Symbol::Minus => "-".to_string(),
            Symbol::Multiply => "*".to_string(),
            Symbol::Divide => "/".to_string(),
            Symbol::FloorDiv => "//".to_string(),
            Symbol::Modulo => "%".to_string(),
            Symbol::Power => "**".to_string(),
            Symbol::BitAnd => "&".to_string(),
            Symbol::BitOr => "|".to_string(),
            Symbol::BitXor => "^".to_string(),
            Symbol::BitNot => "~".to_string(),
            Symbol::And => "&&".to_string(),
            Symbol::Or => "||".to_string(),
            Symbol::Not => "!".to_string(),
            Symbol::Eq => "==".to_string(),
            Symbol::NotEq => "!=".to_string(),
            Symbol::Lt => "<".to_string(),
            Symbol::Gt => ">".to_string(),
            Symbol::LtEq => "<=".to_string(),
            Symbol::GtEq => ">=".to_string(),
            Symbol::Assign => "=".to_string(),
            Symbol::Init => ":=".to_string(),
            Symbol::Dot => ".".to_string(),
            Symbol::Arrow => "->".to_string(),
            Symbol::Comma => ",".to_string(),
            Symbol::Index => "index".to_string(),
            Symbol::Call => "call".to_string(),
        }
    }
}

impl Emit for ExprNode {
    fn emit(&self) -> String {
        match self {
            ExprNode::Literal(t) => t.emit(),
            ExprNode::ArrayLiteral(elements) => {
                let inner: Vec<String> = elements.iter().map(|e| e.emit()).collect();
                format!("(list {})", inner.join(" "))
            }
            ExprNode::BinaryOp { op, left, right } => match right {
                Some(r) => format!("({} {} {})", op.emit(), left.emit(), r.emit()),
                None => format!("({} {})", op.emit(), left.emit()),
            },
            ExprNode::Index { array, index } => {
                format!("(index {} {})", array.emit(), index.emit())
            }
            ExprNode::IfElseNode {
                statement,
                then_branch,
                else_branch,
            } => {
                let then_strs: Vec<String> = then_branch.iter().map(|n| n.emit()).collect();
                let then_body = then_strs.join(" ");

                match else_branch {
                    Some(else_b) => {
                        let else_strs: Vec<String> = else_b.iter().map(|n| n.emit()).collect();
                        let else_body = else_strs.join(" ");
                        format!("(if {} {} {})", statement.emit(), then_body, else_body)
                    }
                    None => format!("(if {} {})", statement.emit(), then_body),
                }
            }
            ExprNode::LoopNode { statement, body } => {
                let body_strs: Vec<String> = body.iter().map(|n| n.emit()).collect();
                format!("(while {} {})", statement.emit(), body_strs.join(" "))
            }
            ExprNode::FunctionCall { name, args } => {
                let arg_strs: Vec<String> = args.iter().map(|a| a.emit()).collect();
                format!("(call {} ({}))", name, arg_strs.join(" "))
            }
            ExprNode::StructLiteral {
                struct_name,
                fields,
            } => {
                let field_strs: Vec<String> = fields
                    .iter()
                    .map(|(name, val)| format!("({} {})", name, val.emit()))
                    .collect();
                format!("(struct {} {})", struct_name, field_strs.join(" "))
            }
            ExprNode::Field { object, field_name } => {
                format!("(field {} {})", object.emit(), field_name)
            }
            ExprNode::Maybe => "maybe".to_string(),
        }
    }
}

impl Emit for ASTNode {
    fn emit(&self) -> String {
        match self {
            ASTNode::Expr(e) => e.emit(),
            ASTNode::DefineVar(def) => def.emit(),
            ASTNode::SetVar(set) => set.emit(),
            ASTNode::SetField {
                object_name,
                field_name,
                value,
            } => {
                format!(
                    "(assign_field {} {} {})",
                    object_name,
                    field_name,
                    value.emit()
                )
            }
            ASTNode::Print(e) => format!("(print {})", e.emit()),
            ASTNode::Println(e) => format!("(println {})", e.emit()),
            ASTNode::Note { note, subtree } => {
                format!("(note \"{}\" {})", note, subtree.emit())
            }
            ASTNode::Return(e) => format!("(return {})", e.emit()),
        }
    }
}

impl Emit for crate::ast::DefineVariable {
    fn emit(&self) -> String {
        format!(
            "(let {} {} {})",
            self.name,
            self.var_type.emit_type(),
            self.value.emit()
        )
    }
}

impl Emit for crate::ast::SetVariable {
    fn emit(&self) -> String {
        format!("(assign {} {})", self.name, self.value.emit())
    }
}

// Helper trait for emitting type annotations
trait EmitType {
    fn emit_type(&self) -> String;
}

impl EmitType for Type {
    fn emit_type(&self) -> String {
        match self {
            Type::Int32(_) => "i32".to_string(),
            Type::Int64(_) => "i64".to_string(),
            Type::Float32(_) => "f32".to_string(),
            Type::Float64(_) => "f64".to_string(),
            Type::Str(_) => "str".to_string(),
            Type::Bool(_) => "bool".to_string(),
            Type::Void => "void".to_string(),
            Type::Null => "null".to_string(),
            Type::Array(elements) => {
                if let Some(first) = elements.first() {
                    format!("(array {})", first.emit_type())
                } else {
                    "(array)".to_string()
                }
            }
            Type::Optional(inner) => format!("{}!", inner.emit_type()),
            Type::Object { type_name, .. } => type_name.clone(),
            Type::VarRef(_) => "varref".to_string(),
            Type::Ref(inner) => format!("(ref {})", inner.emit_type()),
            Type::Break => "break".to_string(),
            Type::Continue => "continue".to_string(),
        }
    }
}

impl Emit for FunctionParam {
    fn emit(&self) -> String {
        format!("({} {})", self.name, self.param_type.emit())
    }
}

impl Emit for Function {
    fn emit(&self) -> String {
        let params: Vec<String> = self.args.iter().map(|p| p.emit()).collect();
        let body: Vec<String> = self.body.iter().map(|s| s.emit()).collect();

        format!(
            "(fn {}\n  (params\n    {}\n  )\n  (returns {})\n  (body\n    {}\n  )\n)",
            self.name,
            params.join("\n    "),
            self.return_type.emit_type(),
            body.join("\n    ")
        )
    }
}

impl Emit for StructDef {
    fn emit(&self) -> String {
        let fields: Vec<String> = self
            .fields
            .iter()
            .map(|(name, kind)| format!("({} {})", name, kind.emit()))
            .collect();

        format!(
            "(struct {}\n  (fields\n    {}\n  )\n)",
            self.name,
            fields.join("\n    ")
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::rc::Rc;

    #[test]
    fn test_emit_literals() {
        assert_eq!(Type::Int32(42).emit(), "42");
        assert_eq!(Type::Float64(3.14).emit(), "3.14");
        assert_eq!(Type::Str("hello".to_string()).emit(), "\"hello\"");
        assert_eq!(Type::Bool(true).emit(), "true");
        assert_eq!(Type::Null.emit(), "null");
    }

    #[test]
    fn test_emit_binary_op() {
        let expr = ExprNode::BinaryOp {
            op: Symbol::Plus,
            left: Rc::new(ExprNode::Literal(Type::Int32(1))),
            right: Some(Rc::new(ExprNode::Literal(Type::Int32(2)))),
        };
        assert_eq!(expr.emit(), "(+ 1 2)");
    }

    #[test]
    fn test_emit_unary_op() {
        let expr = ExprNode::BinaryOp {
            op: Symbol::Not,
            left: Rc::new(ExprNode::Literal(Type::Bool(true))),
            right: None,
        };
        assert_eq!(expr.emit(), "(! true)");
    }

    #[test]
    fn test_emit_function_call() {
        let expr = ExprNode::FunctionCall {
            name: "add".to_string(),
            args: vec![
                Rc::new(ExprNode::Literal(Type::Int32(1))),
                Rc::new(ExprNode::Literal(Type::Int32(2))),
            ],
        };
        assert_eq!(expr.emit(), "(call add (1 2))");
    }

    #[test]
    fn test_emit_field_access() {
        let expr = ExprNode::Field {
            object: Rc::new(ExprNode::Literal(Type::VarRef("user".to_string()))),
            field_name: "name".to_string(),
        };
        assert_eq!(expr.emit(), "(field user name)");
    }

    #[test]
    fn test_emit_struct_literal() {
        let expr = ExprNode::StructLiteral {
            struct_name: "User".to_string(),
            fields: vec![
                ("id".to_string(), Rc::new(ExprNode::Literal(Type::Int32(1)))),
                (
                    "name".to_string(),
                    Rc::new(ExprNode::Literal(Type::Str("Alice".to_string()))),
                ),
            ],
        };
        assert_eq!(expr.emit(), "(struct User (id 1) (name \"Alice\"))");
    }

    #[test]
    fn test_emit_note() {
        let node = ASTNode::Note {
            note: "await".to_string(),
            subtree: Rc::new(ASTNode::Expr(ExprNode::FunctionCall {
                name: "fetch".to_string(),
                args: vec![],
            })),
        };
        assert_eq!(node.emit(), "(note \"await\" (call fetch ()))");
    }

    #[test]
    fn test_emit_struct_def() {
        let def = StructDef::new("User")
            .with_field("id", TypeKind::Int32)
            .with_field("name", TypeKind::Str);

        let expected = "(struct User\n  (fields\n    (id i32)\n    (name str)\n  )\n)";
        assert_eq!(def.emit(), expected);
    }

    #[test]
    fn test_emit_ast_to_file() {
        use std::fs;
        use std::io::Write;

        // Build a small program AST:
        // let x i32 5
        // let y i32 (+ x 10)
        // (if (< x y) (println x) (println y))

        let define_x = ASTNode::DefineVar(crate::ast::DefineVariable::new(
            Type::Int32(0),
            "x",
            Rc::new(ExprNode::Literal(Type::Int32(5))),
        ));

        let define_y = ASTNode::DefineVar(crate::ast::DefineVariable::new(
            Type::Int32(0),
            "y",
            Rc::new(ExprNode::BinaryOp {
                op: Symbol::Plus,
                left: Rc::new(ExprNode::Literal(Type::VarRef("x".to_string()))),
                right: Some(Rc::new(ExprNode::Literal(Type::Int32(10)))),
            }),
        ));

        let condition = ExprNode::BinaryOp {
            op: Symbol::Lt,
            left: Rc::new(ExprNode::Literal(Type::VarRef("x".to_string()))),
            right: Some(Rc::new(ExprNode::Literal(Type::VarRef("y".to_string())))),
        };

        let if_node = ASTNode::Expr(ExprNode::IfElseNode {
            statement: Rc::new(condition),
            then_branch: Rc::new(vec![ASTNode::Println(ExprNode::Literal(Type::VarRef(
                "x".to_string(),
            )))]),
            else_branch: Some(Rc::new(vec![ASTNode::Println(ExprNode::Literal(
                Type::VarRef("y".to_string()),
            ))])),
        });

        let program: Vec<ASTNode> = vec![define_x, define_y, if_node];

        // Emit the AST to a string
        let mut output = String::new();
        output.push_str("; AST dump\n");
        for node in &program {
            output.push_str(&node.emit());
            output.push('\n');
        }

        // Write to file
        let path = std::env::temp_dir().join("ast_dump_test.uir");
        let mut file = fs::File::create(&path).expect("failed to create file");
        file.write_all(output.as_bytes())
            .expect("failed to write to file");

        println!("AST written to: {}", path.display());

        // Verify the file contents
        let contents = fs::read_to_string(&path).expect("failed to read file");

        assert!(contents.contains("(let x i32 5)"));
        assert!(contents.contains("(let y i32 (+ x 10))"));
        assert!(contents.contains("(if (< x y)"));
        assert!(contents.contains("(println x)"));
        assert!(contents.contains("(println y)"));
    }
}

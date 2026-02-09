use pest::Parser;
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::PrattParser;
use pest_derive::Parser;
use std::rc::Rc;

use crate::ast::{ASTNode, DefineVariable, ExprNode, SetVariable, Symbol};
use crate::functions::{Function, FunctionParam};
use crate::types::{StructDef, Type, TypeKind};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct Lang1Parser;

lazy_static! {
    static ref PRATT: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc, Op};

        PrattParser::new()
            .op(Op::infix(Rule::or_op, Assoc::Left))
            .op(Op::infix(Rule::and_op, Assoc::Left))
            .op(Op::infix(Rule::bit_or_op, Assoc::Left))
            .op(Op::infix(Rule::bit_xor_op, Assoc::Left))
            .op(Op::infix(Rule::bit_and_op, Assoc::Left))
            .op(Op::infix(Rule::eq_op, Assoc::Left) | Op::infix(Rule::neq_op, Assoc::Left))
            .op(Op::infix(Rule::lt_op, Assoc::Left)
                | Op::infix(Rule::gt_op, Assoc::Left)
                | Op::infix(Rule::lte_op, Assoc::Left)
                | Op::infix(Rule::gte_op, Assoc::Left))
            .op(Op::infix(Rule::add_op, Assoc::Left) | Op::infix(Rule::sub_op, Assoc::Left))
            .op(Op::infix(Rule::mul_op, Assoc::Left)
                | Op::infix(Rule::div_op, Assoc::Left)
                | Op::infix(Rule::modulo_op, Assoc::Left))
            .op(Op::infix(Rule::power, Assoc::Right))
            .op(Op::prefix(Rule::not_op) | Op::prefix(Rule::neg_op) | Op::prefix(Rule::bit_not_op))
            .op(Op::postfix(Rule::postfix))
    };
}

/// Parsed output: top-level items split into functions, structs, and imports.
pub struct ParsedProgram {
    pub functions: Vec<Function>,
    pub structs: Vec<StructDef>,
    pub imports: Vec<String>,
}

/// Parse source code into a `ParsedProgram`.
pub fn parse(input: &str) -> Result<ParsedProgram, String> {
    let mut pairs =
        Lang1Parser::parse(Rule::program, input).map_err(|e| format!("parse error:\n{}", e))?;

    let program = pairs.next().unwrap(); // the single `program` pair

    let mut functions = Vec::new();
    let mut structs = Vec::new();
    let mut imports = Vec::new();

    for pair in program.into_inner() {
        match pair.as_rule() {
            Rule::top_level_item => {
                let inner = pair.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::fn_def => functions.push(parse_fn_def(inner)),
                    Rule::struct_def => structs.push(parse_struct_def(inner)),
                    Rule::import_stmt => {
                        let path = parse_string_literal(inner.into_inner().next().unwrap());
                        imports.push(path);
                    }
                    Rule::note_def => {
                        // note wraps another top-level item; for now just extract the inner item
                        let mut inner_pairs = inner.into_inner();
                        let note_str = parse_string_literal(inner_pairs.next().unwrap());
                        let wrapped = inner_pairs.next().unwrap(); // top_level_item
                        let wrapped_inner = wrapped.into_inner().next().unwrap();
                        match wrapped_inner.as_rule() {
                            Rule::fn_def => {
                                let f = parse_fn_def(wrapped_inner);
                                let _ = note_str;
                                functions.push(f);
                            }
                            Rule::struct_def => structs.push(parse_struct_def(wrapped_inner)),
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
            Rule::EOI => {}
            _ => {}
        }
    }

    Ok(ParsedProgram {
        functions,
        structs,
        imports,
    })
}

fn parse_fn_def(pair: Pair<Rule>) -> Function {
    let mut inner = pair.into_inner();

    let name = inner.next().unwrap().as_str().to_string();

    // Collect params
    let mut params: Vec<FunctionParam> = Vec::new();
    let mut return_type = Type::Void;
    let mut body: Vec<ASTNode> = Vec::new();

    for item in inner {
        match item.as_rule() {
            Rule::param => {
                let mut p = item.into_inner();
                let pname = p.next().unwrap().as_str().to_string();
                let ptype = parse_type_kind(p.next().unwrap());
                params.push(FunctionParam {
                    name: pname,
                    param_type: ptype,
                });
            }
            Rule::type_expr => {
                return_type = type_kind_to_default_type(parse_type_kind(item));
            }
            Rule::block => {
                body = parse_block(item);
            }
            _ => {}
        }
    }

    Function {
        name,
        return_type,
        args: params,
        body,
    }
}

fn parse_struct_def(pair: Pair<Rule>) -> StructDef {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();

    let mut def = StructDef::new(&name);

    for item in inner {
        if item.as_rule() == Rule::field_def {
            let mut f = item.into_inner();
            let fname = f.next().unwrap().as_str().to_string();
            let fkind = parse_type_kind(f.next().unwrap());
            def.add_field(&fname, fkind);
        }
    }

    def
}

fn parse_type_kind(pair: Pair<Rule>) -> TypeKind {
    // type_expr = { (array_type | primitive_type | type_identifier) ~ optional_marker? }
    let mut kind = TypeKind::Void;
    let mut _optional = false;

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::primitive_type => {
                kind = match inner.as_str() {
                    "i32" | "int" => TypeKind::Int32,
                    "i64" => TypeKind::Int64,
                    "f32" => TypeKind::Float32,
                    "f64" => TypeKind::Float64,
                    "bool" => TypeKind::Bool,
                    "str" => TypeKind::Str,
                    "void" => TypeKind::Void,
                    _ => TypeKind::Void,
                };
            }
            Rule::array_type => {
                kind = TypeKind::Array;
            }
            Rule::type_identifier => {
                kind = TypeKind::Object;
            }
            Rule::optional_marker => {
                _optional = true;
                kind = TypeKind::Optional;
            }
            _ => {}
        }
    }

    kind
}

fn type_kind_to_default_type(kind: TypeKind) -> Type {
    match kind {
        TypeKind::Int32 => Type::Int32(0),
        TypeKind::Int64 => Type::Int64(0),
        TypeKind::Float32 => Type::Float32(0.0),
        TypeKind::Float64 => Type::Float64(0.0),
        TypeKind::Str => Type::Str(String::new()),
        TypeKind::Bool => Type::Bool(false),
        TypeKind::Void => Type::Void,
        TypeKind::Null => Type::Null,
        TypeKind::Array => Type::Array(Vec::new()),
        TypeKind::Optional => Type::Optional(Box::new(Type::Null)),
        TypeKind::Object => Type::Object {
            type_name: String::new(),
            fields: std::collections::BTreeMap::new(),
        },
        _ => Type::Void,
    }
}

fn parse_block(pair: Pair<Rule>) -> Vec<ASTNode> {
    let mut stmts = Vec::new();
    for item in pair.into_inner() {
        if item.as_rule() == Rule::statement {
            stmts.push(parse_statement(item));
        }
    }
    stmts
}

fn parse_statement(pair: Pair<Rule>) -> ASTNode {
    let inner = pair.into_inner().next().unwrap();

    match inner.as_rule() {
        Rule::let_stmt => parse_let_stmt(inner),
        Rule::init_stmt => parse_init_stmt(inner),
        Rule::assign_stmt => parse_assign_stmt(inner),
        Rule::assign_index_stmt => parse_assign_index_stmt(inner),
        Rule::assign_field_stmt => parse_assign_field_stmt(inner),
        Rule::return_stmt => parse_return_stmt(inner),
        Rule::if_stmt => {
            let if_expr_pair = inner.into_inner().next().unwrap();
            ASTNode::Expr(parse_if_expr(if_expr_pair))
        }
        Rule::while_stmt => parse_while_stmt(inner),
        Rule::for_stmt => parse_for_stmt(inner),
        Rule::break_stmt => ASTNode::Expr(ExprNode::Literal(Type::Break)),
        Rule::continue_stmt => ASTNode::Expr(ExprNode::Literal(Type::Continue)),
        Rule::print_stmt => {
            let expr = parse_expr(inner.into_inner().next().unwrap());
            ASTNode::Print(expr)
        }
        Rule::println_stmt => {
            let expr = parse_expr(inner.into_inner().next().unwrap());
            ASTNode::Println(expr)
        }
        Rule::expr_stmt => {
            let expr = parse_expr(inner.into_inner().next().unwrap());
            ASTNode::Expr(expr)
        }
        _ => panic!("unexpected statement rule: {:?}", inner.as_rule()),
    }
}

fn parse_let_stmt(pair: Pair<Rule>) -> ASTNode {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let type_kind = parse_type_kind(inner.next().unwrap());
    let value_expr = parse_expr(inner.next().unwrap());

    ASTNode::DefineVar(DefineVariable::new(
        type_kind_to_default_type(type_kind),
        &name,
        Rc::new(value_expr),
    ))
}

fn parse_init_stmt(pair: Pair<Rule>) -> ASTNode {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let value_expr = parse_expr(inner.next().unwrap());

    ASTNode::DefineVar(DefineVariable::new(
        Type::Int32(0),
        &name,
        Rc::new(value_expr),
    ))
}

fn parse_assign_stmt(pair: Pair<Rule>) -> ASTNode {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let value_expr = parse_expr(inner.next().unwrap());

    ASTNode::SetVar(SetVariable::new(
        Type::Void, // runtime type check
        &name,
        Rc::new(value_expr),
    ))
}

fn parse_assign_index_stmt(pair: Pair<Rule>) -> ASTNode {
    let mut inner = pair.into_inner();
    let array_name = inner.next().unwrap().as_str().to_string();
    let index_expr = parse_expr(inner.next().unwrap());
    let value_expr = parse_expr(inner.next().unwrap());

    ASTNode::SetIndex {
        array_name,
        index: Rc::new(index_expr),
        value: Rc::new(value_expr),
    }
}

fn parse_assign_field_stmt(pair: Pair<Rule>) -> ASTNode {
    let mut inner = pair.into_inner();
    let object_name = inner.next().unwrap().as_str().to_string();
    let field_name = inner.next().unwrap().as_str().to_string();
    let value_expr = parse_expr(inner.next().unwrap());

    ASTNode::SetField {
        object_name,
        field_name,
        value: Rc::new(value_expr),
    }
}

fn parse_return_stmt(pair: Pair<Rule>) -> ASTNode {
    let mut inner = pair.into_inner();
    match inner.next() {
        Some(expr_pair) => ASTNode::Return(parse_expr(expr_pair)),
        None => ASTNode::Return(ExprNode::Literal(Type::Void)),
    }
}

fn parse_while_stmt(pair: Pair<Rule>) -> ASTNode {
    let mut inner = pair.into_inner();
    let condition = parse_expr(inner.next().unwrap());
    let body = parse_block(inner.next().unwrap());

    ASTNode::Expr(ExprNode::LoopNode {
        statement: Rc::new(condition),
        body: Rc::new(body),
    })
}

/// Desugars `for (init; cond; update) { body }` into two AST nodes
/// returned as a block: `[init, while cond { body...; update }]`.
/// Since a single ASTNode is expected, we wrap the init + loop in an
/// IfElseNode with a `true` condition (always-execute block).
fn parse_for_stmt(pair: Pair<Rule>) -> ASTNode {
    let mut inner = pair.into_inner();

    let init_pair = inner.next().unwrap(); // for_init
    let cond_pair = inner.next().unwrap(); // expr (condition)
    let update_pair = inner.next().unwrap(); // for_update
    let block_pair = inner.next().unwrap(); // block

    let init = parse_for_init(init_pair);
    let condition = parse_expr(cond_pair);
    let update = parse_for_update(update_pair);
    let mut body = parse_block(block_pair);

    // append the update statement to the end of the loop body
    body.push(update);

    // wrap as: init; while (cond) { body; update; }
    // we use an IfElseNode(true) to hold both statements in sequence.
    let loop_node = ASTNode::Expr(ExprNode::LoopNode {
        statement: Rc::new(condition),
        body: Rc::new(body),
    });

    // return as if(true) { init; while(...) { ... } } — a sequencing hack
    ASTNode::Expr(ExprNode::IfElseNode {
        statement: Rc::new(ExprNode::Literal(Type::Bool(true))),
        then_branch: Rc::new(vec![init, loop_node]),
        else_branch: None,
    })
}

fn parse_for_init(pair: Pair<Rule>) -> ASTNode {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::let_stmt_no_semi => {
            let mut parts = inner.into_inner();
            let name = parts.next().unwrap().as_str().to_string();
            let type_kind = parse_type_kind(parts.next().unwrap());
            let value_expr = parse_expr(parts.next().unwrap());
            ASTNode::DefineVar(DefineVariable::new(
                type_kind_to_default_type(type_kind),
                &name,
                Rc::new(value_expr),
            ))
        }
        Rule::init_stmt_no_semi => {
            let mut parts = inner.into_inner();
            let name = parts.next().unwrap().as_str().to_string();
            let value_expr = parse_expr(parts.next().unwrap());
            ASTNode::DefineVar(DefineVariable::new(
                Type::Int32(0),
                &name,
                Rc::new(value_expr),
            ))
        }
        Rule::assign_stmt_no_semi => {
            let mut parts = inner.into_inner();
            let name = parts.next().unwrap().as_str().to_string();
            let value_expr = parse_expr(parts.next().unwrap());
            ASTNode::SetVar(SetVariable::new(Type::Void, &name, Rc::new(value_expr)))
        }
        _ => panic!("unexpected for_init rule: {:?}", inner.as_rule()),
    }
}

fn parse_for_update(pair: Pair<Rule>) -> ASTNode {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::assign_stmt_no_semi => {
            let mut parts = inner.into_inner();
            let name = parts.next().unwrap().as_str().to_string();
            let value_expr = parse_expr(parts.next().unwrap());
            ASTNode::SetVar(SetVariable::new(Type::Void, &name, Rc::new(value_expr)))
        }
        Rule::expr => ASTNode::Expr(parse_expr(inner)),
        _ => panic!("unexpected for_update rule: {:?}", inner.as_rule()),
    }
}

fn parse_expr(pair: Pair<Rule>) -> ExprNode {
    let pairs = pair.into_inner();
    parse_expr_pratt(pairs)
}

fn parse_expr_pratt(pairs: Pairs<Rule>) -> ExprNode {
    PRATT
        .map_primary(|primary| parse_primary(primary))
        .map_infix(|lhs, op, rhs| {
            let symbol = match op.as_rule() {
                Rule::add_op => Symbol::Plus,
                Rule::sub_op => Symbol::Minus,
                Rule::mul_op => Symbol::Multiply,
                Rule::div_op => Symbol::Divide,
                Rule::modulo_op => Symbol::Modulo,
                Rule::power => Symbol::Power,
                Rule::and_op => Symbol::And,
                Rule::or_op => Symbol::Or,
                Rule::eq_op => Symbol::Eq,
                Rule::neq_op => Symbol::NotEq,
                Rule::lt_op => Symbol::Lt,
                Rule::gt_op => Symbol::Gt,
                Rule::lte_op => Symbol::LtEq,
                Rule::gte_op => Symbol::GtEq,
                Rule::bit_and_op => Symbol::BitAnd,
                Rule::bit_or_op => Symbol::BitOr,
                Rule::bit_xor_op => Symbol::BitXor,
                _ => unreachable!("unexpected infix rule: {:?}", op.as_rule()),
            };
            ExprNode::BinaryOp {
                op: symbol,
                left: Rc::new(lhs),
                right: Some(Rc::new(rhs)),
            }
        })
        .map_prefix(|op, rhs| {
            let symbol = match op.as_rule() {
                Rule::not_op => Symbol::Not,
                Rule::neg_op => Symbol::Minus,
                Rule::bit_not_op => Symbol::BitNot,
                _ => unreachable!("unexpected prefix rule: {:?}", op.as_rule()),
            };
            ExprNode::BinaryOp {
                op: symbol,
                left: Rc::new(rhs),
                right: None,
            }
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            Rule::postfix => {
                let inner = op.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::dot_access => {
                        let mut parts = inner.into_inner();
                        let name = parts.next().unwrap().as_str().to_string();
                        match parts.next() {
                            Some(call_args_pair) if call_args_pair.as_rule() == Rule::call_args => {
                                // .method(args) -> method(self, args)
                                let mut args = vec![Rc::new(lhs)];
                                args.extend(
                                    call_args_pair
                                        .into_inner()
                                        .filter(|p| p.as_rule() == Rule::expr)
                                        .map(|p| Rc::new(parse_expr(p))),
                                );
                                ExprNode::FunctionCall { name, args }
                            }
                            _ => {
                                // .field
                                ExprNode::Field {
                                    object: Rc::new(lhs),
                                    field_name: name,
                                }
                            }
                        }
                    }
                    Rule::index_access => {
                        let index_expr = parse_expr(inner.into_inner().next().unwrap());
                        ExprNode::Index {
                            array: Rc::new(lhs),
                            index: Rc::new(index_expr),
                        }
                    }
                    Rule::increment_op => ExprNode::BinaryOp {
                        op: Symbol::Increment,
                        left: Rc::new(lhs),
                        right: None,
                    },
                    Rule::decrement_op => ExprNode::BinaryOp {
                        op: Symbol::Decrement,
                        left: Rc::new(lhs),
                        right: None,
                    },
                    _ => unreachable!(),
                }
            }
            _ => unreachable!("unexpected postfix rule: {:?}", op.as_rule()),
        })
        .parse(pairs)
}

fn parse_primary(pair: Pair<Rule>) -> ExprNode {
    match pair.as_rule() {
        Rule::primary | Rule::cond_primary => {
            let mut inner = pair.into_inner();
            let first = inner.next().unwrap();

            // Handle `identifier ~ call_args?` — both are siblings under primary
            if first.as_rule() == Rule::identifier {
                let name = first.as_str().to_string();
                if let Some(next) = inner.next() {
                    if next.as_rule() == Rule::call_args {
                        let args: Vec<Rc<ExprNode>> = next
                            .into_inner()
                            .filter(|p| p.as_rule() == Rule::expr)
                            .map(|p| Rc::new(parse_expr(p)))
                            .collect();
                        return ExprNode::FunctionCall { name, args };
                    }
                }
                return ExprNode::Literal(Type::VarRef(name));
            }

            parse_primary(first)
        }
        Rule::integer => {
            let val: i32 = pair
                .as_str()
                .parse()
                .unwrap_or_else(|_| pair.as_str().parse::<i64>().unwrap_or(0) as i32);
            ExprNode::Literal(Type::Int32(val))
        }
        Rule::float => {
            let val: f64 = pair.as_str().parse().unwrap();
            ExprNode::Literal(Type::Float64(val))
        }
        Rule::string => ExprNode::Literal(Type::Str(parse_string_literal(pair))),
        Rule::boolean => ExprNode::Literal(Type::Bool(pair.as_str() == "true")),
        Rule::null_lit => ExprNode::Literal(Type::Null),
        Rule::identifier => {
            let name = pair.as_str().to_string();
            ExprNode::Literal(Type::VarRef(name))
        }
        Rule::if_expr => parse_if_expr(pair),
        Rule::len_expr => {
            let inner = pair.into_inner().next().unwrap();
            ExprNode::Len(Rc::new(parse_primary(inner)))
        }
        Rule::array_literal => {
            let inner = pair.into_inner().next().unwrap();
            match inner.as_rule() {
                Rule::array_sized => {
                    let mut parts = inner.into_inner();
                    let length = parse_expr(parts.next().unwrap());
                    let fill = parse_expr(parts.next().unwrap());
                    ExprNode::ArraySized {
                        length: Rc::new(length),
                        fill: Rc::new(fill),
                    }
                }
                Rule::array_elements => {
                    let elements: Vec<Rc<ExprNode>> = inner
                        .into_inner()
                        .filter(|p| p.as_rule() == Rule::expr)
                        .map(|p| Rc::new(parse_expr(p)))
                        .collect();
                    ExprNode::ArrayLiteral(elements)
                }
                _ => unreachable!(),
            }
        }
        Rule::struct_literal => parse_struct_literal(pair),
        Rule::expr => parse_expr(pair),
        _ => panic!("unexpected primary rule: {:?}", pair.as_rule()),
    }
}

fn parse_if_expr(pair: Pair<Rule>) -> ExprNode {
    let mut inner = pair.into_inner();

    // if_condition has the same structure as expr, reuse the Pratt parser
    let cond_pair = inner.next().unwrap();
    let condition = parse_expr_pratt(cond_pair.into_inner());
    let then_block = parse_block(inner.next().unwrap());

    let else_branch = inner.next().map(|else_part| {
        match else_part.as_rule() {
            Rule::if_expr => {
                // else if => wrap in a single-statement vec
                Rc::new(vec![ASTNode::Expr(parse_if_expr(else_part))])
            }
            Rule::block => Rc::new(parse_block(else_part)),
            _ => unreachable!(),
        }
    });

    ExprNode::IfElseNode {
        statement: Rc::new(condition),
        then_branch: Rc::new(then_block),
        else_branch,
    }
}

fn parse_struct_literal(pair: Pair<Rule>) -> ExprNode {
    let mut inner = pair.into_inner();
    let struct_name = inner.next().unwrap().as_str().to_string();

    let mut fields = Vec::new();
    for item in inner {
        if item.as_rule() == Rule::struct_field_init {
            let mut f = item.into_inner();
            let fname = f.next().unwrap().as_str().to_string();
            let fval = parse_expr(f.next().unwrap());
            fields.push((fname, Rc::new(fval)));
        }
    }

    ExprNode::StructLiteral {
        struct_name,
        fields,
    }
}

fn parse_string_literal(pair: Pair<Rule>) -> String {
    let raw = pair.as_str();
    // Strip surrounding quotes
    let inner = &raw[1..raw.len() - 1];
    // Unescape
    inner
        .replace("\\\\", "\x00") // placeholder
        .replace("\\\"", "\"")
        .replace("\\n", "\n")
        .replace("\\t", "\t")
        .replace("\\r", "\r")
        .replace("\\0", "\0")
        .replace("\x00", "\\") // restore real backslash
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_let() {
        let input = r#"
            fn main() -> void {
                let x: i32 = 5;
            }
        "#;
        let result = parse(input).unwrap();
        assert_eq!(result.functions.len(), 1);
        assert_eq!(result.functions[0].name, "main");
        assert_eq!(result.functions[0].body.len(), 1);
    }

    #[test]
    fn test_parse_arithmetic() {
        let input = r#"
            fn calc() -> i32 {
                return 2 + 3 * 4;
            }
        "#;
        let result = parse(input).unwrap();
        assert_eq!(result.functions.len(), 1);
    }

    #[test]
    fn test_parse_struct_def() {
        let input = r#"
            struct User {
                id: i32,
                name: str,
            }
        "#;
        let result = parse(input).unwrap();
        assert_eq!(result.structs.len(), 1);
        assert_eq!(result.structs[0].name, "User");
        assert_eq!(result.structs[0].fields.len(), 2);
    }

    #[test]
    fn test_parse_if_else() {
        let input = r#"
            fn check(x: i32) -> i32 {
                if x > 0 {
                    return 1;
                } else {
                    return 0;
                }
            }
        "#;
        let result = parse(input).unwrap();
        assert_eq!(result.functions.len(), 1);
    }

    #[test]
    fn test_parse_while_loop() {
        let input = r#"
            fn count() -> void {
                let i: i32 = 0;
                while i < 10 {
                    i = i + 1;
                }
            }
        "#;
        let result = parse(input).unwrap();
        assert_eq!(result.functions.len(), 1);
        assert_eq!(result.functions[0].body.len(), 2);
    }

    #[test]
    fn test_parse_function_call() {
        let input = r#"
            fn add(a: i32, b: i32) -> i32 {
                return a + b;
            }

            fn main() -> void {
                let result: i32 = add(1, 2);
            }
        "#;
        let result = parse(input).unwrap();
        assert_eq!(result.functions.len(), 2);
    }

    #[test]
    fn test_parse_struct_literal() {
        let input = r#"
            struct Point {
                x: f64,
                y: f64,
            }

            fn main() -> void {
                let p: Point = Point { x: 1.0, y: 2.0 };
            }
        "#;
        let result = parse(input).unwrap();
        assert_eq!(result.structs.len(), 1);
        assert_eq!(result.functions.len(), 1);
    }

    #[test]
    fn test_parse_field_access() {
        let input = r#"
            struct User {
                name: str,
            }

            fn main() -> void {
                let u: User = User { name: "Alice" };
                println(u.name);
            }
        "#;
        let result = parse(input).unwrap();
        assert_eq!(result.functions.len(), 1);
    }

    #[test]
    fn test_parse_nested_expr() {
        let input = r#"
            fn main() -> void {
                let x: i32 = (1 + 2) * (3 + 4);
            }
        "#;
        let result = parse(input).unwrap();
        assert_eq!(result.functions.len(), 1);
    }

    #[test]
    fn test_parse_for_loop() {
        let input = r#"
            fn main() -> void {
                for (let i: i32 = 0; i < 10; i = i + 1) {
                    println(i);
                }
            }
        "#;
        let result = parse(input).unwrap();
        assert_eq!(result.functions.len(), 1);
        // Desugared into a single if(true){init; while{...}} node
        assert_eq!(result.functions[0].body.len(), 1);
    }

    #[test]
    fn test_parse_for_loop_assign_init() {
        let input = r#"
            fn main() -> void {
                let i: i32 = 0;
                for (i = 0; i < 5; i++) {
                    println(i);
                }
            }
        "#;
        let result = parse(input).unwrap();
        assert_eq!(result.functions.len(), 1);
        assert_eq!(result.functions[0].body.len(), 2);
    }
}

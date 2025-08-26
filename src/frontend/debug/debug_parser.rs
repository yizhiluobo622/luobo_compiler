// src/frontend/debug/debug_parser.rs

use crate::frontend::ast::{Ast,AstKind,Statement,Expression,Literal,Type};
use std::fs::{File, create_dir_all};
use std::io::{self, Write};
use std::path::Path;

/// 生成 AST 的 Graphviz DOT 格式文件，用于可视化
///
/// # 参数
/// * `ast` - 程序的根 AST 节点
/// * `output_path` - 输出的 DOT 文件路径，比如 "ast.dot"
///
/// # 返回
/// 成功返回 `Ok(())`，失败返回 IO 错误
pub fn generate_ast_dot(ast: &Ast, output_path: &str) -> io::Result<()> {
    // 确保输出目录存在
    if let Some(parent) = Path::new(output_path).parent() {
        create_dir_all(parent)?;
    }
    
    let mut file = File::create(output_path)?;

    // 写入 DOT 文件头部
    writeln!(file, "digraph AST {{")?;
    writeln!(file, "  rankdir=TB;")?; // 从上到下布局
    writeln!(file, "  node [shape=box, fontname=\"Arial\", style=filled, fillcolor=white, fontcolor=black];")?;

    // 递归打印 AST 节点
    generate_ast_nodes_and_edges(ast, file.by_ref(), 0)?;

    // 写入 DOT 文件尾部
    writeln!(file, "}}")?;

    Ok(())
}

/// 递归遍历 AST，生成 DOT 节点和边（内部实现）
fn generate_ast_nodes_and_edges(ast: &Ast, writer: &mut impl Write, node_id: usize) -> io::Result<usize> {
    // 1. 打印当前节点
    let label = match &ast.kind {
        AstKind::Program { .. } => "Program".to_string(),
        AstKind::Function { function_name, .. } => format!("Function: {}", function_name),
        AstKind::VariableDeclaration { variable_name, variable_type, is_const, .. } => {
            let const_tag = if *is_const { "const " } else { "" };
            format!("VarDecl: {}{} ({:?})", const_tag, variable_name, variable_type)
        },
        AstKind::Statement(Statement::Compound { .. }) => "Compound".to_string(),
        AstKind::Statement(Statement::ExpressionStatement { .. }) => "ExprStmt".to_string(),
        AstKind::Statement(Statement::Return { .. }) => "Return".to_string(),
        AstKind::Statement(Statement::If { .. }) => "If".to_string(),
        AstKind::Statement(Statement::ElseIf { .. }) => "ElseIf".to_string(),
        AstKind::Statement(Statement::While { .. }) => "While".to_string(),
        AstKind::Statement(Statement::For { .. }) => "For".to_string(),
        AstKind::Statement(Statement::Break) => "Break".to_string(),
        AstKind::Statement(Statement::Continue) => "Continue".to_string(),
        AstKind::Statement(Statement::Empty) => "Empty".to_string(),
        AstKind::Expression(Expression::BinaryOperation { operator, .. }) => format!("BinaryOp: {:?}", operator),
        AstKind::Expression(Expression::UnaryOperation { operator, .. }) => format!("UnaryOp: {:?}", operator),
        AstKind::Expression(Expression::Literal(Literal::IntegerLiteral(val))) => format!("Literal(int: {})", val),
        AstKind::Expression(Expression::Literal(Literal::FloatLiteral(val))) => format!("Literal(float: {})", val),
        AstKind::Expression(Expression::Literal(Literal::StringLiteral(val))) => format!("Literal(str: \"{}\")", val),
        AstKind::Expression(Expression::Literal(Literal::BooleanLiteral(val))) => format!("Literal(bool: {})", val),
        AstKind::Expression(Expression::Identifier { name }) => format!("Identifier: {}", name),
        AstKind::Expression(Expression::FunctionCall { function_name, .. }) => format!("Call: {}", function_name),
        AstKind::Expression(Expression::Assignment { .. }) => "Assignment".to_string(),
        AstKind::Expression(Expression::InitializerList { .. }) => "InitList".to_string(),
        AstKind::Expression(Expression::ArrayAccess { .. }) => "ArrayAccess".to_string(),
        AstKind::Expression(Expression::MemberAccess { member_name, .. }) => format!("MemberAccess: {}", member_name),
        AstKind::Type(Type::IntType) => "Type: int".to_string(),
        AstKind::Type(Type::FloatType) => "Type: float".to_string(),
        AstKind::Type(Type::VoidType) => "Type: void".to_string(),
        AstKind::Type(Type::CharType) => "Type: char".to_string(),
        AstKind::Type(Type::BoolType) => "Type: bool".to_string(),
        AstKind::Type(Type::ArrayType { .. }) => "Type: array".to_string(),
        AstKind::Type(Type::PointerType { .. }) => "Type: pointer".to_string(),
        AstKind::Type(Type::FunctionType { .. }) => "Type: function".to_string(),
        _ => format!("{:?}", ast.kind),
    };
    writeln!(writer, "  node{} [label=\"{}\"];", node_id, label)?;

    // 2. 递归处理直接子节点
    let mut next_id = node_id + 1;
    for child in get_direct_children(ast) {
        writeln!(writer, "  node{} -> node{};", node_id, next_id)?;
        next_id = generate_ast_nodes_and_edges(child, writer, next_id)?;
    }
    Ok(next_id)
}

/// 获取AST节点的直接子节点（只返回直接语法子节点，顺序与语法结构一致）
fn get_direct_children<'a>(ast: &'a Ast) -> Vec<&'a Ast> {
    match &ast.kind {
        AstKind::Program { functions, global_variables } => {
            functions.iter().chain(global_variables.iter()).collect()
        }
        AstKind::Function { function_body, .. } => vec![function_body.as_ref()],
        AstKind::VariableDeclaration { initial_value, .. } => {
            initial_value.as_ref().map(|b| b.as_ref()).into_iter().collect()
        }
        AstKind::Statement(Statement::Compound { statements }) => statements.iter().collect(),
        AstKind::Statement(Statement::ExpressionStatement { expression }) => vec![expression.as_ref()],
        AstKind::Statement(Statement::Return { value }) => value.as_ref().map(|b| b.as_ref()).into_iter().collect(),
        AstKind::Statement(Statement::If { condition, then_branch, else_branch }) => {
            let mut v = vec![condition.as_ref(), then_branch.as_ref()];
            if let Some(e) = else_branch {
                v.push(e.as_ref());
            }
            v
        }
        AstKind::Statement(Statement::ElseIf { condition, then_branch, else_branch }) => {
            let mut v = vec![condition.as_ref(), then_branch.as_ref()];
            if let Some(e) = else_branch {
                v.push(e.as_ref());
            }
            v
        }
        AstKind::Statement(Statement::While { condition, body }) => vec![condition.as_ref(), body.as_ref()],
        AstKind::Statement(Statement::For { initialization, condition, update, body }) => {
            let mut v = Vec::new();
            if let Some(init) = initialization { v.push(init.as_ref()); }
            if let Some(cond) = condition { v.push(cond.as_ref()); }
            if let Some(upd) = update { v.push(upd.as_ref()); }
            v.push(body.as_ref());
            v
        }
        AstKind::Expression(Expression::Assignment { target, value }) => vec![target.as_ref(), value.as_ref()],
        AstKind::Expression(Expression::BinaryOperation { left_operand, right_operand, .. }) => vec![left_operand.as_ref(), right_operand.as_ref()],
        AstKind::Expression(Expression::UnaryOperation { operand, .. }) => vec![operand.as_ref()],
        AstKind::Expression(Expression::FunctionCall { arguments, .. }) => arguments.iter().collect(),
        AstKind::Expression(Expression::InitializerList { elements }) => elements.iter().collect(),
        AstKind::Expression(Expression::ArrayAccess { array, index }) => vec![array.as_ref(), index.as_ref()],
        AstKind::Expression(Expression::MemberAccess { object, .. }) => vec![object.as_ref()],
        _ => vec![],
    }
}

/// 调试工具：生成AST的DOT图并保存到graph文件夹
pub fn show_ast_dot(ast: &Ast, filename: &str) {
    println!("=== AST DOT Generation ===");
    
    // 构建完整的文件路径
    let output_path = format!("src/frontend/debug/graph/{}", filename);
    println!("Generating AST visualization to: {}", output_path);
    
    match generate_ast_dot(ast, &output_path) {
        Ok(()) => println!("✅ AST DOT file generated successfully!"),
        Err(e) => println!("❌ Failed to generate AST DOT file: {}", e),
    }
}
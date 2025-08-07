// src/frontend/debug/debug_semantic.rs

use crate::frontend::ast::{Ast, AstKind, Statement, Expression, Literal, Type};
use crate::frontend::semantic_analysis::analyze_ast_with_semantic_info;
use std::fs::{File, create_dir_all};
use std::io::{self, Write};
use std::path::Path;

/// 带语义信息的AST节点信息
#[derive(Debug)]
pub struct SemanticAstNode {
    /// 节点ID
    pub id: usize,
    /// 节点标签
    pub label: String,
    /// 节点类型
    pub node_type: String,
    /// 推导出的类型（如果有）
    pub deduced_type: Option<String>,
    /// 符号表信息（如果是标识符）
    pub symbol_info: Option<String>,
    /// 语义错误（如果有）
    pub semantic_errors: Vec<String>,
}

/// 生成带语义信息的AST的Graphviz DOT格式文件
///
/// # 参数
/// * `ast` - 程序的根AST节点
/// * `output_path` - 输出的DOT文件路径
///
/// # 返回
/// 成功返回`Ok(())`，失败返回IO错误
pub fn generate_semantic_ast_dot(
    ast: &Ast, 
    output_path: &str
) -> io::Result<()> {
    // 确保输出目录存在
    if let Some(parent) = Path::new(output_path).parent() {
        create_dir_all(parent)?;
    }
    
    let mut file = File::create(output_path)?;

    // 写入DOT文件头部
    writeln!(file, "digraph SemanticAST {{")?;
    writeln!(file, "  rankdir=TB;")?;
    writeln!(file, "  node [shape=box, fontname=\"Arial\", style=filled, fontcolor=black];")?;

    // 递归生成带语义信息的AST节点
    generate_semantic_ast_nodes_and_edges(ast, file.by_ref(), 0)?;

    // 写入DOT文件尾部
    writeln!(file, "}}")?;

    Ok(())
}

/// 递归遍历AST，生成带语义信息的DOT节点和边
fn generate_semantic_ast_nodes_and_edges(
    ast: &Ast, 
    writer: &mut impl Write, 
    node_id: usize
) -> io::Result<usize> {
    // 获取语义信息（直接从AST中获取）
    let semantic_info = get_semantic_info_from_ast(ast);
    
    // 生成节点标签
    let label = format_semantic_node_label(&semantic_info);
    
    // 根据是否有语义错误设置节点颜色
    let node_style = if semantic_info.semantic_errors.is_empty() {
        "style=filled, fillcolor=lightgreen"
    } else {
        "style=filled, fillcolor=lightcoral"
    };
    
    writeln!(writer, "  node{} [label=\"{}\", {}];", node_id, label, node_style)?;

    // 递归处理子节点
    let mut next_id = node_id + 1;
    for child in get_direct_children(ast) {
        writeln!(writer, "  node{} -> node{};", node_id, next_id)?;
        next_id = generate_semantic_ast_nodes_and_edges(child, writer, next_id)?;
    }
    
    Ok(next_id)
}

/// 从AST中获取语义信息（利用新的语义信息字段）
fn get_semantic_info_from_ast(ast: &Ast) -> SemanticAstNode {
    let mut semantic_info = SemanticAstNode {
        id: 0,
        label: get_simple_label(&ast.kind),
        node_type: get_node_type(&ast.kind),
        deduced_type: None,
        symbol_info: None,
        semantic_errors: Vec::new(),
    };
    
    // 直接从AST的语义信息字段获取数据
    if let Some(typ) = &ast.semantic_info.deduced_type {
        semantic_info.deduced_type = Some(format!("{:?}", typ));
    }
    
    if let Some(name) = &ast.semantic_info.symbol_name {
        semantic_info.symbol_info = Some(format!("符号: {}", name));
    }
    
    // 复制语义错误
    semantic_info.semantic_errors = ast.semantic_info.semantic_errors.clone();
    
    semantic_info
}

/// 获取简化的节点标签
fn get_simple_label(kind: &AstKind) -> String {
    match kind {
        AstKind::Program { .. } => "Program".to_string(),
        AstKind::Function { function_name, .. } => format!("Function: {}", function_name),
        AstKind::VariableDeclaration { variable_name, .. } => format!("VarDecl: {}", variable_name),
        AstKind::Statement(stmt) => match stmt {
            Statement::Compound { .. } => "Compound".to_string(),
            Statement::ExpressionStatement { .. } => "ExprStmt".to_string(),
            Statement::Return { .. } => "Return".to_string(),
            Statement::If { .. } => "If".to_string(),
            Statement::While { .. } => "While".to_string(),
            Statement::For { .. } => "For".to_string(),
            Statement::Break => "Break".to_string(),
            Statement::Continue => "Continue".to_string(),
            Statement::Empty => "Empty".to_string(),
        },
        AstKind::Expression(expr) => match expr {
            Expression::Literal(Literal::IntegerLiteral(val)) => format!("Literal(int: {})", val),
            Expression::Literal(Literal::FloatLiteral(val)) => format!("Literal(float: {})", val),
            Expression::Literal(Literal::StringLiteral(val)) => format!("Literal(str: \"{}\")", val),
            Expression::Literal(Literal::BooleanLiteral(val)) => format!("Literal(bool: {})", val),
            Expression::Identifier { name } => format!("Identifier: {}", name),
            Expression::BinaryOperation { operator, .. } => format!("BinaryOp: {:?}", operator),
            Expression::UnaryOperation { operator, .. } => format!("UnaryOp: {:?}", operator),
            Expression::FunctionCall { function_name, .. } => format!("Call: {}", function_name),
            Expression::Assignment { .. } => "Assignment".to_string(),
            Expression::ArrayAccess { .. } => "ArrayAccess".to_string(),
            Expression::MemberAccess { member_name, .. } => format!("MemberAccess: {}", member_name),
        },
        AstKind::Type(typ) => match typ {
            Type::IntType => "Type: int".to_string(),
            Type::FloatType => "Type: float".to_string(),
            Type::VoidType => "Type: void".to_string(),
            Type::CharType => "Type: char".to_string(),
            Type::BoolType => "Type: bool".to_string(),
            Type::ArrayType { .. } => "Type: array".to_string(),
            Type::PointerType { .. } => "Type: pointer".to_string(),
            Type::FunctionType { .. } => "Type: function".to_string(),
        },
    }
}

/// 获取节点类型描述
fn get_node_type(kind: &AstKind) -> String {
    match kind {
        AstKind::Program { .. } => "Program".to_string(),
        AstKind::Function { .. } => "Function".to_string(),
        AstKind::VariableDeclaration { .. } => "VariableDeclaration".to_string(),
        AstKind::Statement(stmt) => match stmt {
            Statement::Compound { .. } => "CompoundStatement".to_string(),
            Statement::ExpressionStatement { .. } => "ExpressionStatement".to_string(),
            Statement::Return { .. } => "ReturnStatement".to_string(),
            Statement::If { .. } => "IfStatement".to_string(),
            Statement::While { .. } => "WhileStatement".to_string(),
            Statement::For { .. } => "ForStatement".to_string(),
            Statement::Break => "BreakStatement".to_string(),
            Statement::Continue => "ContinueStatement".to_string(),
            Statement::Empty => "EmptyStatement".to_string(),
        },
        AstKind::Expression(expr) => match expr {
            Expression::Literal(_) => "Literal".to_string(),
            Expression::Identifier { .. } => "Identifier".to_string(),
            Expression::BinaryOperation { .. } => "BinaryOperation".to_string(),
            Expression::UnaryOperation { .. } => "UnaryOperation".to_string(),
            Expression::FunctionCall { .. } => "FunctionCall".to_string(),
            Expression::Assignment { .. } => "Assignment".to_string(),
            Expression::ArrayAccess { .. } => "ArrayAccess".to_string(),
            Expression::MemberAccess { .. } => "MemberAccess".to_string(),
        },
        AstKind::Type(_) => "Type".to_string(),
    }
}

/// 格式化带语义信息的节点标签
fn format_semantic_node_label(info: &SemanticAstNode) -> String {
    let mut label = format!("{}: {}", info.node_type, info.label);
    
    // 添加推导出的类型
    if let Some(typ) = &info.deduced_type {
        label.push_str(&format!("\\n类型: {}", typ));
    }
    
    // 添加符号表信息
    if let Some(symbol) = &info.symbol_info {
        label.push_str(&format!("\\n{}", symbol));
    }
    
    // 添加语义错误
    if !info.semantic_errors.is_empty() {
        label.push_str("\\n❌ 错误:");
        for error in &info.semantic_errors {
            label.push_str(&format!("\\n  {}", error));
        }
    }
    
    label
}

/// 获取AST节点的直接子节点
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
        AstKind::Expression(Expression::ArrayAccess { array, index }) => vec![array.as_ref(), index.as_ref()],
        AstKind::Expression(Expression::MemberAccess { object, .. }) => vec![object.as_ref()],
        _ => vec![],
    }
}

/// 调试工具：生成带语义信息的AST的DOT图并保存到graph文件夹
pub fn show_semantic_ast_dot(ast: &Ast, filename: &str) {
    println!("=== Semantic AST DOT Generation ===");
    
    // 构建完整的文件路径
    let output_path = format!("src/frontend/debug/graph/{}", filename);
    println!("Generating semantic AST visualization to: {}", output_path);
    
    // 直接生成可视化
    match generate_semantic_ast_dot(ast, &output_path) {
        Ok(()) => {
            println!("✅ Semantic AST DOT file generated successfully!");
            if ast.semantic_info.has_errors() {
                println!("⚠️  AST包含语义错误，错误节点将以红色显示");
            } else {
                println!("✅ AST语义分析通过，所有节点以绿色显示");
            }
        }
        Err(e) => println!("❌ Failed to generate semantic AST DOT file: {}", e),
    }
}

/// 打印AST的语义分析详细信息
pub fn print_semantic_analysis_details(ast: &Ast) {
    println!("=== Semantic Analysis Details ===");
    
    if ast.semantic_info.analyzed {
        println!("✅ 语义分析已完成");
        
        // 统计语义信息
        let mut total_nodes = 0;
        let mut nodes_with_type = 0;
        let mut nodes_with_symbol = 0;
        let mut nodes_with_errors = 0;
        let mut all_errors = Vec::new();
        
        count_semantic_info_with_errors(ast, &mut total_nodes, &mut nodes_with_type, &mut nodes_with_symbol, &mut nodes_with_errors, &mut all_errors);
        
        println!("📊 语义信息统计:");
        println!("  - 总节点数: {}", total_nodes);
        println!("  - 有类型信息的节点: {}", nodes_with_type);
        println!("  - 有符号信息的节点: {}", nodes_with_symbol);
        println!("  - 有错误的节点: {}", nodes_with_errors);
        
        if nodes_with_errors > 0 {
            println!("❌ 发现 {} 个语义错误:", nodes_with_errors);
            for (i, error) in all_errors.iter().enumerate() {
                println!("  {}. {}", i + 1, error);
            }
        } else {
            println!("✅ 没有发现语义错误");
        }
    } else {
        println!("❌ 语义分析未完成");
    }
}

/// 递归统计语义信息并收集错误
fn count_semantic_info_with_errors(
    ast: &Ast, 
    total_nodes: &mut usize, 
    nodes_with_type: &mut usize, 
    nodes_with_symbol: &mut usize, 
    nodes_with_errors: &mut usize,
    all_errors: &mut Vec<String>
) {
    *total_nodes += 1;
    
    if ast.semantic_info.deduced_type.is_some() {
        *nodes_with_type += 1;
    }
    
    if ast.semantic_info.symbol_name.is_some() {
        *nodes_with_symbol += 1;
    }
    
    if !ast.semantic_info.semantic_errors.is_empty() {
        *nodes_with_errors += 1;
        // 收集错误信息
        for error in &ast.semantic_info.semantic_errors {
            let node_label = get_simple_label(&ast.kind);
            let error_msg = format!("{} (位置: {:?}): {}", node_label, ast.span, error);
            all_errors.push(error_msg);
        }
    }
    
    // 递归处理子节点
    for child in get_direct_children(ast) {
        count_semantic_info_with_errors(child, total_nodes, nodes_with_type, nodes_with_symbol, nodes_with_errors, all_errors);
    }
}

/// 生成AST的语义分析报告
pub fn generate_semantic_report(ast: &Ast, output_path: &str) -> io::Result<()> {
    let mut file = File::create(output_path)?;
    
    writeln!(file, "# 语义分析报告")?;
    writeln!(file, "")?;
    
    if ast.semantic_info.analyzed {
        writeln!(file, "## 分析结果: ✅ 成功")?;
        
        // 统计语义信息
        let mut total_nodes = 0;
        let mut nodes_with_type = 0;
        let mut nodes_with_symbol = 0;
        let mut nodes_with_errors = 0;
        let mut all_errors = Vec::new();
        
        count_semantic_info_with_errors(ast, &mut total_nodes, &mut nodes_with_type, &mut nodes_with_symbol, &mut nodes_with_errors, &mut all_errors);
        
        writeln!(file, "### 语义信息统计")?;
        writeln!(file, "- 总节点数: {}", total_nodes)?;
        writeln!(file, "- 有类型信息的节点: {}", nodes_with_type)?;
        writeln!(file, "- 有符号信息的节点: {}", nodes_with_symbol)?;
        writeln!(file, "- 有错误的节点: {}", nodes_with_errors)?;
        
        if nodes_with_errors > 0 {
            writeln!(file, "### 错误列表 (共 {} 个)", nodes_with_errors)?;
            for (i, error) in all_errors.iter().enumerate() {
                writeln!(file, "{}. {}", i + 1, error)?;
            }
        }
    } else {
        writeln!(file, "## 分析结果: ❌ 失败")?;
        writeln!(file, "语义分析未完成")?;
    }
    
    Ok(())
}

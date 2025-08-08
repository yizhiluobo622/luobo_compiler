use std::fs;
use std::path::Path;
use std::collections::HashMap;

// 导入必要的模块
mod frontend;
use frontend::parser::Parser;
use frontend::lexer::Lexer;

fn main() {
    println!("🧪 AST构建批量测试开始...");
    
    let test_dir = "Code/sy/Perf/src";
    let mut results = HashMap::new();
    let mut total_tests = 0;
    let mut passed_tests = 0;
    
    // 检查测试目录是否存在
    if !Path::new(test_dir).exists() {
        println!("❌ 错误: 测试目录 {} 不存在", test_dir);
        return;
    }
    
    // 遍历测试文件
    if let Ok(entries) = fs::read_dir(test_dir) {
        for entry in entries {
            if let Ok(entry) = entry {
                let path = entry.path();
                if let Some(extension) = path.extension() {
                    if extension == "sy" {
                        total_tests += 1;
                        let file_name = path.file_name().unwrap().to_string_lossy();
                        
                        println!("📄 测试文件: {}", file_name);
                        
                        // 读取文件内容
                        match fs::read_to_string(&path) {
                            Ok(content) => {
                                // 创建词法分析器
                                let lexer = Lexer::new(&content);
                                let mut parser = Parser::new(lexer);
                                
                                // 尝试解析
                                match parser.parse() {
                                    Ok(ast) => {
                                        println!("  ✅ 解析成功 - AST节点数: {}", count_ast_nodes(&ast));
                                        results.insert(file_name.to_string(), "PASS".to_string());
                                        passed_tests += 1;
                                    }
                                    Err(errors) => {
                                        println!("  ❌ 解析失败 - {} 个错误", errors.len());
                                        for (i, error) in errors.iter().enumerate() {
                                            println!("    错误 {}: {} (位置: {:?})", 
                                                i + 1, error.message, error.span);
                                        }
                                        results.insert(file_name.to_string(), "FAIL".to_string());
                                    }
                                }
                            }
                            Err(e) => {
                                println!("  ❌ 读取文件失败: {}", e);
                                results.insert(file_name.to_string(), "ERROR".to_string());
                            }
                        }
                    }
                }
            }
        }
    }
    
    // 生成测试报告
    println!("\n📊 测试结果汇总:");
    println!("总测试数: {}", total_tests);
    println!("通过测试: {}", passed_tests);
    println!("失败测试: {}", total_tests - passed_tests);
    if total_tests > 0 {
        println!("成功率: {:.1}%", (passed_tests as f64 / total_tests as f64) * 100.0);
    }
    
    // 保存详细报告
    let mut report = String::new();
    report.push_str("AST构建批量测试报告\n");
    report.push_str("==================\n\n");
    report.push_str(&format!("总测试数: {}\n", total_tests));
    report.push_str(&format!("通过测试: {}\n", passed_tests));
    report.push_str(&format!("失败测试: {}\n", total_tests - passed_tests));
    if total_tests > 0 {
        report.push_str(&format!("成功率: {:.1}%\n\n", (passed_tests as f64 / total_tests as f64) * 100.0));
    }
    
    report.push_str("详细结果:\n");
    report.push_str("----------\n");
    for (file, result) in &results {
        report.push_str(&format!("{}: {}\n", file, result));
    }
    
    if let Err(e) = fs::write("test_ast_report.txt", report) {
        println!("❌ 保存报告失败: {}", e);
    } else {
        println!("📄 详细报告已保存到 test_ast_report.txt");
    }
}

fn count_ast_nodes(ast: &frontend::ast::Ast) -> usize {
    // 递归统计所有AST节点
    1 + match &ast.kind {
        frontend::ast::AstKind::Program { functions, global_variables } => {
            functions.iter().map(count_ast_nodes).sum::<usize>() +
            global_variables.iter().map(count_ast_nodes).sum::<usize>()
        }
        frontend::ast::AstKind::Function { function_body, .. } => {
            count_ast_nodes(function_body)
        }
        frontend::ast::AstKind::VariableDeclaration { initial_value, .. } => {
            initial_value.as_ref().map(|v| count_ast_nodes(v)).unwrap_or(0)
        }
        frontend::ast::AstKind::Statement(statement) => {
            match statement {
                frontend::ast::Statement::Compound { statements } => {
                    statements.iter().map(count_ast_nodes).sum::<usize>()
                }
                frontend::ast::Statement::ExpressionStatement { expression } => {
                    count_ast_nodes(expression)
                }
                frontend::ast::Statement::Return { value } => {
                    value.as_ref().map(|v| count_ast_nodes(v)).unwrap_or(0)
                }
                frontend::ast::Statement::If { condition, then_branch, else_branch } => {
                    count_ast_nodes(condition) + 
                    count_ast_nodes(then_branch) + 
                    else_branch.as_ref().map(|e| count_ast_nodes(e)).unwrap_or(0)
                }
                frontend::ast::Statement::While { condition, body } => {
                    count_ast_nodes(condition) + count_ast_nodes(body)
                }
                frontend::ast::Statement::For { initialization, condition, update, body } => {
                    initialization.as_ref().map(|i| count_ast_nodes(i)).unwrap_or(0) +
                    condition.as_ref().map(|c| count_ast_nodes(c)).unwrap_or(0) +
                    update.as_ref().map(|u| count_ast_nodes(u)).unwrap_or(0) +
                    count_ast_nodes(body)
                }
                frontend::ast::Statement::Break | frontend::ast::Statement::Continue | frontend::ast::Statement::Empty => 0,
            }
        }
        frontend::ast::AstKind::Expression(expression) => {
            match expression {
                frontend::ast::Expression::Assignment { target, value } => {
                    count_ast_nodes(target) + count_ast_nodes(value)
                }
                frontend::ast::Expression::BinaryOperation { left_operand, right_operand, .. } => {
                    count_ast_nodes(left_operand) + count_ast_nodes(right_operand)
                }
                frontend::ast::Expression::UnaryOperation { operand, .. } => {
                    count_ast_nodes(operand)
                }
                frontend::ast::Expression::FunctionCall { arguments, .. } => {
                    arguments.iter().map(count_ast_nodes).sum::<usize>()
                }
                frontend::ast::Expression::InitializerList { elements } => {
                    elements.iter().map(count_ast_nodes).sum::<usize>()
                }
                frontend::ast::Expression::ArrayAccess { array, index } => {
                    count_ast_nodes(array) + count_ast_nodes(index)
                }
                frontend::ast::Expression::MemberAccess { object, .. } => {
                    count_ast_nodes(object)
                }
                frontend::ast::Expression::Literal(_) | frontend::ast::Expression::Identifier { .. } => 0,
            }
        }
        frontend::ast::AstKind::Type(_) => 0,
    }
}

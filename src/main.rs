use std::path::PathBuf;
use std::path::Path;
use std::collections::HashMap;
use std::fs;
use clap::Parser as ClapParser;
use crate::frontend::lexer::Lexer;
use crate::frontend::parser::Parser;
mod frontend;
mod for_test;
mod TACIR;
#[derive(ClapParser, Debug)]
#[command(author, version, about)]

struct Args {
    /// Input source file
    input: PathBuf,
    
    /// Output file
    #[arg(short, long, default_value = "a.out")]
    output: PathBuf,
    
    /// Optimization level (0-3)
    #[arg(short, long, default_value = "0")]
    optimize: u8,
    
    /// Show structured IR example
    #[arg(long)]
    show_structured_ir: bool,
}

fn main() {
    let args = Args::parse();
    
    if args.show_structured_ir {
       
        return;
    }
    
   
}
#[test]
fn print_span_test(){
    
        println!("🧪 AST构建批量测试开始...");
        
        let test_dir = "Code/sy/HFunc/src";
        let mut results = HashMap::new();
        let mut total_tests = 0;
        let mut passed_tests = 0;
        
        // 检查测试目录是否存在
        if !Path::new(test_dir).exists() {
            println!("❌ 错误: 测试目录 {} 不存在", test_dir);
            return;
        }
        
        // 收集所有测试文件并按编号排序
        let mut test_files = Vec::new();
        
        if let Ok(entries) = fs::read_dir(test_dir) {
            for entry in entries {
                if let Ok(entry) = entry {
                    let path = entry.path();
                    if let Some(extension) = path.extension() {
                        if extension == "sy" {
                            if let Some(file_name) = path.file_name() {
                                let file_name_str = file_name.to_string_lossy().to_string();
                                test_files.push((path, file_name_str));
                            }
                        }
                    }
                }
            }
        }
        
        // 按文件名中的数字编号排序
        test_files.sort_by(|a, b| {
            let a_name = &a.1;
            let b_name = &b.1;
            
            // 提取文件名中的数字编号
            let a_num = extract_number(a_name);
            let b_num = extract_number(b_name);
            
            // 按数字编号排序
            a_num.cmp(&b_num)
        });
        
        println!("📋 找到 {} 个测试文件，按编号顺序测试:", test_files.len());
        
        // 按排序后的顺序运行测试
        for (path, file_name) in &test_files {
            total_tests += 1;
            
            println!("📄 测试文件 [{}/{}]: {}", total_tests, test_files.len(), file_name);
            
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
                            results.insert(file_name, "PASS".to_string());
                            passed_tests += 1;
                        }
                        Err(errors) => {
                            println!("  ❌ 解析失败 - {} 个错误", errors.len());
                            for (i, error) in errors.iter().enumerate() {
                                println!("    错误 {}: {} (位置: {:?})", 
                                    i + 1, error.message, error.span);
                            }
                            results.insert(file_name, "FAIL".to_string());
                        }
                    }
                }
                Err(e) => {
                    println!("  ❌ 读取文件失败: {}", e);
                    results.insert(file_name, "ERROR".to_string());
                }
            }
        }
        
        // 生成测试报告
        println!("\n📊 测试结果汇总:");
        println!("总测试数: {}", total_tests);
        println!("通过测试: {}", passed_tests);
        println!("失败测试: {}", total_tests - passed_tests);
        println!("成功率: {:.1}%", (passed_tests as f64 / total_tests as f64) * 100.0);
        
        // 保存详细报告
        let mut report = String::new();
        report.push_str("AST构建批量测试报告\n");
        report.push_str("==================\n\n");
        report.push_str(&format!("总测试数: {}\n", total_tests));
        report.push_str(&format!("通过测试: {}\n", passed_tests));
        report.push_str(&format!("失败测试: {}\n", total_tests - passed_tests));
        report.push_str(&format!("成功率: {:.1}%\n\n", (passed_tests as f64 / total_tests as f64) * 100.0));
        
        report.push_str("详细结果 (按编号顺序):\n");
        report.push_str("------------------------\n");
        
        // 按编号顺序输出结果
        let mut sorted_results: Vec<_> = results.iter().collect();
        sorted_results.sort_by(|a, b| {
            let a_num = extract_number(a.0);
            let b_num = extract_number(b.0);
            a_num.cmp(&b_num)
        });
        
        for (file, result) in sorted_results {
            report.push_str(&format!("{}: {}\n", file, result));
        }
        
        if let Err(e) = fs::write("test_ast_report.txt", report) {
            println!("❌ 保存报告失败: {}", e);
        } else {
            println!("📄 详细报告已保存到 test_ast_report.txt");
        }
    }
    
    /// 从文件名中提取数字编号
    fn extract_number(filename: &str) -> i32 {
        // 移除文件扩展名
        let name_without_ext = filename.replace(".sy", "");
        
        // 查找文件名开头的数字
        let mut num_str = String::new();
        for c in name_without_ext.chars() {
            if c.is_digit(10) {
                num_str.push(c);
            } else {
                break;
            }
        }
        
        // 解析数字，如果失败则返回0
        num_str.parse::<i32>().unwrap_or(0)
    }
    
    fn count_ast_nodes(ast: &crate::frontend::ast::Ast) -> usize {
        // 简单的AST节点计数函数
        // 这里可以根据需要实现更复杂的统计
        1 + match &ast.kind {
            crate::frontend::ast::AstKind::Program { functions, global_variables } => {
                functions.iter().map(count_ast_nodes).sum::<usize>() +
                global_variables.iter().map(count_ast_nodes).sum::<usize>()
            }
            crate::frontend::ast::AstKind::Function { function_body, .. } => {
                count_ast_nodes(function_body)
            }
            crate::frontend::ast::AstKind::Statement(statement) => {
                match statement {
                    crate::frontend::ast::Statement::Compound { statements } => {
                        statements.iter().map(count_ast_nodes).sum::<usize>()
                    }
                    crate::frontend::ast::Statement::ExpressionStatement { expression } => {
                        count_ast_nodes(expression)
                    }
                    _ => 0
                }
            }
            crate::frontend::ast::AstKind::Expression(_) => 0,
            _ => 0
        }

}


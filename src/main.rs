mod frontend;
use std::fs;
use frontend::debug::debug_lexer;
use frontend::debug::debug_parser;
use frontend::debug::debug_semantic;
use frontend::parser::Parser;
use frontend::lexer::Lexer;
use frontend::semantic_analysis::{analyze_ast_and_print, analyze_ast_with_semantic_info};

fn main() {
    println!("🦀 Welcome");

    let path = "src/target_code/sy/000_1.sy";
    let src = fs::read_to_string(path).expect("Cannot read file");
    
    println!("Analyzing");
    let lexer = Lexer::new(&src);
    let mut parser = Parser::new(lexer);
    
    let parse_result = parser.parse();
    println!("Analyzing");
    
    match parse_result {
        Ok(ast) => {
            println!("✅ Syntax analysis successful!");
            debug_parser::show_ast_dot(&ast, "real_ast.dot");
            
            // 恢复语义分析部分
            println!("\n🔍 Analyzing");
            
            // 使用新的带语义信息的AST功能
            match analyze_ast_with_semantic_info(ast.clone()) {
                Ok(semantic_ast) => {
                    println!("✅ Semantic analysis successful");
                    
                    // 生成带语义信息的AST可视化
                    println!("\n📊 Generating semantic AST visualization");
                    debug_semantic::show_semantic_ast_dot(&semantic_ast, "semantic_ast.dot");
                    
                    // 打印详细的语义分析信息
                    debug_semantic::print_semantic_analysis_details(&semantic_ast);
                    
                    
                    println!("\n🚀 Ready for IR generation with semantic AST");
                    
                    
                }
                Err(errors) => {
                    println!("❌ Semantic analysis failed with {} errors:", errors.len());
                    for (i, error) in errors.iter().enumerate() {
                        println!("  {}. {} (位置: {:?})", i + 1, error.message, error.span);
                    }
                    
                    // 即使有错误，也生成可视化（显示错误节点）
                    debug_semantic::show_semantic_ast_dot(&ast, "semantic_ast_with_errors.dot");
                    
                    // 打印详细的语义分析信息
                    debug_semantic::print_semantic_analysis_details(&ast);
                }
            }
        }
        Err(errors) => {
            println!("❌ Syntax analysis failed");
            println!("Error count: {}", errors.len());
            for (i, error) in errors.iter().enumerate() {
                println!("Error {}: {} Position: 第{}行第{}列", 
                    i + 1, 
                    error.message, 
                    error.span.line, 
                    error.span.column
                );
            }
        }
    }
}
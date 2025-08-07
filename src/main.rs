mod frontend;
use std::fs;
use frontend::debug::debug_lexer;
use frontend::debug::debug_parser;
use frontend::parser::Parser;
use frontend::lexer::Lexer;

fn main() {
    println!("🦀 Welcome");

    let path = "src/target_code/sy/000_1.sy";
    let src = fs::read_to_string(path).expect("Cannot read file");
    
    println!("Analyzing");
    let lexer = Lexer::new(&src);
    let mut parser = Parser::new(lexer);
    
    // 添加超时保护
    let parse_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        parser.parse()
    }));
    
    match parse_result {
        Ok(Ok(ast)) => {
            println!("✅ You are good to go !");
            debug_parser::show_ast_dot(&ast, "real_ast.dot");
        }
        Ok(Err(errors)) => {
            println!("❌ Error found");
            println!("Error: {}", errors.len());
            for (i, error) in errors.iter().enumerate() {
                println!("Error {}: {} Position: 第{}行第{}列", 
                    i + 1, 
                    error.message, 
                    error.span.line, 
                    error.span.column
                );
            }
        }
        Err(_) => {
            println!("❌ Panic!");
        }
    }
}
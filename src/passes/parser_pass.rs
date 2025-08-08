use crate::frontend::ast::Ast;
use crate::frontend::lexer::Lexer;
use crate::frontend::semantic_analysis::analyze_ast_with_semantic_info;

pub fn gen_sema_ast_pass(source: &str) -> Ast {
    // 词法分析
    let lexer = Lexer::new(source);
    
    // 语法分析
    let mut parser = crate::frontend::parser::Parser::new(lexer);
    let ast = parser.parse().expect("Syntax analysis failed");
    
    // 语义分析
    analyze_ast_with_semantic_info(ast).expect("Semantic analysis failed")
}

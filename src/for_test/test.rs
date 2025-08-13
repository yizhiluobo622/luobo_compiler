// 流程级测试：测试从源代码到SoN IR的完整流程
// 使用各模块的便利函数

pub mod tests {
    use crate::frontend::lexer::Lexer;
    use crate::frontend::parser::Parser;
    use crate::frontend::semantic_analysis::analyze_ast_with_semantic_info;


    #[test]
    pub fn test_complete_pipeline() {
        // 读取真实的测试文件
        let source_code = std::fs::read_to_string("/home/yizhiluobo/luobo_compiler/src/target_code/sy/000_1.sy")
            .expect("Failed to read test file");
        
        // Lexer: 两行
        let lexer = Lexer::new(&source_code);
        
        // Parser: 两行
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().expect("Parser failed");
        
        // Semantic Analysis: 两行
        let annotated_ast = analyze_ast_with_semantic_info(ast).expect("Semantic analysis failed");
        
        // AST to SoN IR: 使用带语义信息的转换函数
        let function_ast = extract_main_function(&annotated_ast);
        
        // 获取语义分析的结果
        let symbol_table = get_symbol_table_from_ast(&annotated_ast);
        let type_system = get_type_system_from_ast(&annotated_ast);
        
        
     
    }
    
    //以下是辅助函数
    
    fn extract_main_function(program_ast: &crate::frontend::ast::Ast) -> &crate::frontend::ast::Ast {
        match &program_ast.kind {
            crate::frontend::ast::AstKind::Program { functions, .. } => {
                functions.iter()
                    .find(|func| {
                        if let crate::frontend::ast::AstKind::Function { function_name, .. } = &func.kind {
                            function_name == "main"
                        } else {
                            false
                        }
                    })
                    .unwrap_or_else(|| functions.first().expect("No functions found in program"))
            }
            _ => panic!("Root AST is not a Program"),
        }
    }
    
    fn get_symbol_table_from_ast(ast: &crate::frontend::ast::Ast) -> crate::frontend::SemanticAnalyzer::symbol_table::SymbolTable {
        use crate::frontend::SemanticAnalyzer::symbol_table::SymbolTable;
        use crate::frontend::ast::{AstKind, Statement};
        
        let mut symbol_table = SymbolTable::new();
        
        fn extract_variables(ast: &crate::frontend::ast::Ast, symbol_table: &mut SymbolTable) {
            match &ast.kind {
                AstKind::VariableDeclaration { variable_name, variable_type, .. } => {
                    let _ = symbol_table.add_variable(
                        variable_name,
                        variable_type.clone(),
                        ast.span.clone(),
                        false,
                    );
                }
                AstKind::Function { function_name, return_type, parameters, .. } => {
                    if let Some(return_type) = return_type {
                        let param_types: Vec<_> = parameters.iter().map(|p| {
                            if let AstKind::VariableDeclaration { variable_type, .. } = &p.kind {
                                variable_type.clone()
                            } else {
                                crate::frontend::ast::Type::IntType // 默认类型
                            }
                        }).collect();
                        let _ = symbol_table.add_function(
                            function_name,
                            return_type.clone(),
                            param_types,
                            ast.span.clone(),
                        );
                    }
                    
                    for param in parameters {
                        if let AstKind::VariableDeclaration { variable_name, variable_type, .. } = &param.kind {
                            let _ = symbol_table.add_parameter(
                                variable_name,
                                variable_type.clone(),
                                param.span.clone(),
                            );
                        }
                    }
                }
                AstKind::Statement(stmt) => {
                    match stmt {
                        Statement::Compound { statements } => {
                            for stmt in statements {
                                extract_variables(stmt, symbol_table);
                            }
                        }
                        Statement::If { then_branch, else_branch, .. } => {
                            extract_variables(then_branch, symbol_table);
                            if let Some(else_branch) = else_branch {
                                extract_variables(else_branch, symbol_table);
                            }
                        }
                        Statement::While { body, .. } => {
                            extract_variables(body, symbol_table);
                        }
                        Statement::For { initialization, body, .. } => {
                            if let Some(init) = initialization {
                                extract_variables(init, symbol_table);
                            }
                            extract_variables(body, symbol_table);
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }
        
        extract_variables(ast, &mut symbol_table);
        symbol_table
    }
    
    fn get_type_system_from_ast(_ast: &crate::frontend::ast::Ast) -> crate::frontend::SemanticAnalyzer::type_system::TypeSystem {
        use crate::frontend::SemanticAnalyzer::type_system::TypeSystem;
        TypeSystem::new()
    }
}


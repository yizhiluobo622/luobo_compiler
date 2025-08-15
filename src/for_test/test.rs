// 流程级测试：测试从源代码到SoN IR的完整流程
// 使用各模块的便利函数

pub mod tests {
    use crate::frontend::lexer::Lexer;
    use crate::frontend::parser::Parser;
    use crate::frontend::semantic_analysis::analyze_ast_with_semantic_info;
    use crate::TACIR;
    use crate::TACIR::TAC_opt::OptimizationPass;

    #[test]
    pub fn test_complete_pipeline() {
        // 读取测试文件
        let source_code = std::fs::read_to_string("/home/yizhiluobo/luobo_compiler/src/target_code/sy/000_1.sy")
            .expect("Failed to read test file");
        
        // Lexer
        let lexer = Lexer::new(&source_code);
        
        // Parser
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().expect("Parser failed");
        
        // Semantic Analysis
        let annotated_ast = analyze_ast_with_semantic_info(ast).expect("Semantic analysis failed");
        
        // TAC IR转换
        println!("=== 开始TAC IR转换 ===");
        match TACIR::convert_ast_to_tac(&annotated_ast) {
            Ok(mut tac_program) => {
                println!("✅ TAC IR转换成功！");
                
                // 显示IR结构信息
                TACIR::debug_ir_structure(&tac_program);
                
                // 显示main函数的详细IR
                TACIR::debug_function_ir(&tac_program, "main");
                
                // 显示完整的TAC程序
                TACIR::print_tac_program(&tac_program);
                
                // 验证IR的正确性
                validate_tac_ir(&tac_program);
                
                // 运行常量传播优化
                println!("\n=== 运行常量传播优化 ===");
                let mut constant_opt = TACIR::TAC_opt::constant_opt::ConstantOptimizationPass::new();
                match constant_opt.run(&mut tac_program) {
                    Ok(result) => {
                        if result.optimized {
                            println!("✅ 常量传播优化成功！");
                            println!("   优化指令数: {}", result.instructions_optimized);
                            
                            // 显示优化后的IR
                            println!("\n=== 优化后的IR ===");
                            TACIR::debug_function_ir(&tac_program, "main");
                            TACIR::print_tac_program(&tac_program);
                        } else {
                            println!("ℹ️ 常量传播优化没有发现优化机会");
                        }
                    }
                    Err(e) => {
                        println!("❌ 常量传播优化失败: {}", e);
                    }
                }
            }
            Err(e) => {
                println!("❌ TAC IR转换失败: {}", e);
                panic!("TAC IR转换失败");
            }
        }
    }
    
    /// 简洁验证TAC IR的正确性
    fn validate_tac_ir(program: &TACIR::TACProgram) {
        if let Some(main_func) = program.get_main_function() {
            assert!(!main_func.basic_blocks.is_empty(), "main函数必须有基本块");
            assert!(!main_func.basic_blocks[0].instructions.is_empty(), "基本块必须有指令");
            println!("✅ IR验证通过");
        } else {
            panic!("未找到main函数");
        }
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


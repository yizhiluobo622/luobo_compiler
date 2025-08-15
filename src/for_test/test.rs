// 流程级测试：测试从源代码到SoN IR的完整流程
// 使用各模块的便利函数

pub mod tests {
    use crate::frontend::lexer::Lexer;
    use crate::frontend::parser::Parser;
    use crate::frontend::semantic_analysis::analyze_ast_with_semantic_info;
    use crate::ast_to_cfg::SoN_optimization::OptimizationPass;


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
        
        // AST to SoN IR: 转换并生成图
        let mut son_ir = crate::ast_to_cfg::ast_to_SoNir::converter::convert_ast_to_son(&annotated_ast)
            .expect("AST to SoN IR conversion failed");
        
        // 生成优化前的DOT图
        let dot_graph_before = crate::ast_to_cfg::dot_graph::generate_dot(&son_ir)
            .expect("Failed to generate DOT graph before optimization");
        
        // 保存优化前的图
        let output_dir = "src/ast_to_cfg/Debug/graph";
        std::fs::create_dir_all(output_dir).expect("Failed to create output directory");
        std::fs::write(format!("{}/sea_of_nodes_before.dot", output_dir), &dot_graph_before)
            .expect("Failed to write DOT file before optimization");
        
        // 执行常量传播优化
        let mut constant_propagation_pass = crate::ast_to_cfg::SoN_optimization::constant_propagation::ConstantPropagationPass::new();
        let optimization_result = constant_propagation_pass.run(&mut son_ir)
            .expect("Constant propagation optimization failed");
        
        // 打印优化统计信息
        constant_propagation_pass.print_stats();
        
        // 检查优化结果
        if optimization_result.optimized {
            println!("✅ 常量传播优化成功！");
            println!("   优化节点数: {}", optimization_result.nodes_optimized);
            println!("   删除节点数: {}", optimization_result.nodes_deleted);
        } else {
            println!("ℹ️  没有进行常量传播优化");
        }
        
        // 生成优化后的DOT图
        let dot_graph_after = crate::ast_to_cfg::dot_graph::generate_dot(&son_ir)
            .expect("Failed to generate DOT graph after optimization");
        
        // 保存优化后的图
        std::fs::write(format!("{}/sea_of_nodes_after.dot", output_dir), &dot_graph_after)
            .expect("Failed to write DOT file after optimization");
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
    
    /// 测试常量传播优化
    #[test]
    pub fn test_constant_propagation() {
        // 创建一个简单的测试用例：最简单的程序结构
        let source_code = "int main() { return 1 + 2; }";
        
        // Lexer
        let lexer = Lexer::new(source_code);
        
        // Parser
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().expect("Parser failed");
        
        // Semantic Analysis
        let annotated_ast = analyze_ast_with_semantic_info(ast).expect("Semantic analysis failed");
        
        // AST to SoN IR
        let mut son_ir = crate::ast_to_cfg::ast_to_SoNir::converter::convert_ast_to_son(&annotated_ast)
            .expect("AST to SoN IR conversion failed");
        
        println!("=== 优化前的图 ===");
        let node_count_before = son_ir.node_count();
        println!("节点数量: {}", node_count_before);
        
        // 执行常量传播优化
        let mut constant_propagation_pass = crate::ast_to_cfg::SoN_optimization::constant_propagation::ConstantPropagationPass::new();
        let optimization_result = constant_propagation_pass.run(&mut son_ir)
            .expect("Constant propagation optimization failed");
        
        println!("=== 优化后的图 ===");
        let node_count_after = son_ir.node_count();
        println!("节点数量: {}", node_count_after);
        
        // 打印优化统计信息
        constant_propagation_pass.print_stats();
        
        // 验证优化结果
        if optimization_result.optimized {
            println!("✅ 常量传播优化成功！");
            println!("   优化节点数: {}", optimization_result.nodes_optimized);
            println!("   删除节点数: {}", optimization_result.nodes_deleted);
            
            // 验证节点数量减少
            assert!(node_count_after < node_count_before, "优化后节点数量应该减少");
        } else {
            println!("ℹ️  没有进行常量传播优化");
        }
    }
}


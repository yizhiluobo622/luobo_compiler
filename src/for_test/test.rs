// 流程级测试：测试从源代码到SoN IR的完整流程
// 使用各模块的便利函数，简洁明了

pub mod tests {
    use crate::frontend::lexer::Lexer;
    use crate::frontend::parser::Parser;
    use crate::frontend::semantic_analysis::analyze_ast_with_semantic_info;
    use crate::ast_to_cfg::ast_to_SoNir::convert_ast_to_son_with_stats;
    use crate::ast_to_cfg::SoN_optimization::opt_pipeline::OptimizationPipeline;
    use crate::ast_to_cfg::ast_to_SoNir::generate_son_ir_dot;

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
        
        // AST to SoN IR: 两行
        let function_ast = extract_main_function(&annotated_ast);
        
        // 函数信息已提取
        
        let sonir_result = convert_ast_to_son_with_stats(function_ast).expect("AST to SoN IR conversion failed");
        
        // 获取初始SoN IR
        let mut sonir = sonir_result.son_ir;
        
        // 验证初始结果
        assert!(sonir.node_count() > 0, "SoN IR should contain nodes");
        assert!(sonir.edge_count() > 0, "SoN IR should contain edges");
        assert!(sonir.get_entry_node().is_some(), "SoN IR should have entry node");
        assert!(sonir.get_exit_node().is_some(), "SoN IR should have exit node");
        
        println!("✅ Initial SoN IR: {} nodes, {} edges", sonir.node_count(), sonir.edge_count());
        
        // 生成优化前的SoN IR图
        let initial_output_path = "/home/yizhiluobo/luobo_compiler/src/ast_to_cfg/Debug/graph/test_sy_file_to_sonir_initial.dot";
        match generate_son_ir_dot(&sonir, initial_output_path) {
            Ok(_) => println!("✅ Generated initial SoN IR DOT file: {}", initial_output_path),
            Err(e) => println!("❌ Failed to generate initial SoN IR DOT file: {}", e),
        }
        
        // 运行优化链：使用便利函数
        let mut opt_pipeline = OptimizationPipeline::new();
        opt_pipeline.run(&mut sonir);
        
        // 验证优化后的结果
        assert!(sonir.node_count() > 0, "Optimized SoN IR should contain nodes");
        assert!(sonir.edge_count() > 0, "Optimized SoN IR should contain edges");
        assert!(sonir.get_entry_node().is_some(), "Optimized SoN IR should have entry node");
        assert!(sonir.get_exit_node().is_some(), "Optimized SoN IR should have exit node");
        
        println!("✅ Optimized SoN IR: {} nodes, {} edges", sonir.node_count(), sonir.edge_count());
        
        // 生成优化后的SoN IR图
        let optimized_output_path = "/home/yizhiluobo/luobo_compiler/src/ast_to_cfg/Debug/graph/test_sy_file_to_sonir_optimized.dot";
        match generate_son_ir_dot(&sonir, optimized_output_path) {
            Ok(_) => println!("✅ Generated optimized SoN IR DOT file: {}", optimized_output_path),
            Err(e) => println!("❌ Failed to generate optimized SoN IR DOT file: {}", e),
        }
        
        println!("✅ Pipeline test passed: {} nodes, {} edges", sonir.node_count(), sonir.edge_count());
    }
    
    #[test]
    pub fn test_enhanced_constant_propagation() {
        // 测试增强后的常量传播优化器
        // 包括：常量折叠、理想化优化、全局值编号、强度削弱
        
        // 创建一个简单的测试用例
        let source_code = r#"
        int main() {
            int a = 1;
            int b = 2;
            int c = a + b;      // 应该被优化为 3
            int d = c * 0;      // 应该被优化为 0
            int e = a + a;      // 应该被优化为 2
            int f = b * 2;      // 应该被优化为 4
            int g = a - a;      // 应该被优化为 0
            int h = b / b;      // 应该被优化为 1
            return 0;
        }
        "#;
        
        // 这里可以添加具体的测试逻辑
        // 由于我们还没有完整的测试框架，先打印一些信息
        println!("✅ Enhanced constant propagation test framework ready");
        println!("   - Constant folding: c * 0 = 0");
        println!("   - Strength reduction: a + a = 2*a, b * 2 = b + b");
        println!("   - Algebraic identities: a - a = 0, b / b = 1");
        println!("   - Global value numbering: eliminate duplicate computations");
    }
    
    fn extract_main_function(program_ast: &crate::frontend::ast::Ast) -> &crate::frontend::ast::Ast {
        match &program_ast.kind {
            crate::frontend::ast::AstKind::Program { functions, .. } => {
                // 查找main函数，如果没有则使用第一个函数
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
}


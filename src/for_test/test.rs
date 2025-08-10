// 流程级测试：测试从源代码到SoN IR的完整流程
// 使用各模块的便利函数，简洁明了

pub mod tests {
    use crate::frontend::lexer::Lexer;
    use crate::frontend::parser::Parser;
    use crate::frontend::semantic_analysis::analyze_ast_with_semantic_info;
    use crate::ast_to_cfg::ast_to_SoNir::convert_ast_to_son_with_stats;
    //use crate::ast_to_cfg::SoN_optimization::constant_propagation::ConstantPropagation;
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
        
        // 常量传播优化: 两行
        //let mut constant_propagation = ConstantPropagation::new();
        //constant_propagation.run(&mut sonir);
        
        // 获取优化统计信息
        //let optimization_stats = constant_propagation.get_stats();
        //println!("✅ Constant propagation completed: {} constants identified, {} nodes processed", 
        //        optimization_stats.constants_identified, optimization_stats.nodes_processed);
        
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
    
    fn extract_main_function(program_ast: &crate::frontend::ast::Ast) -> &crate::frontend::ast::Ast {
        match &program_ast.kind {
            crate::frontend::ast::AstKind::Program { functions, .. } => {
                functions.first().expect("No functions found in program")
            }
            _ => panic!("Root AST is not a Program"),
        }
    }
}


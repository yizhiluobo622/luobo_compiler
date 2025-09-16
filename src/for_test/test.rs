// 流程级测试：测试从源代码到SoN IR的完整流程
// 使用各模块的便利函数

pub mod tests {
    use crate::frontend::lexer::Lexer;
    use crate::frontend::parser::Parser;
    use crate::frontend::ast::AstKind;
    use crate::frontend::semantic_analysis::analyze_ast_with_semantic_info;
    use crate::frontend::debug::debug_parser::show_ast_dot;
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
        
        // 生成AST DOT图来查看结构
        show_ast_dot(&ast, "test_000_1_ast.dot");
        
        // Semantic Analysis
        let annotated_ast = analyze_ast_with_semantic_info(ast).expect("Semantic analysis failed");
        
        // 生成语义分析后的AST DOT图
        show_ast_dot(&annotated_ast, "test_000_1_semantic_ast.dot");
        
        // TAC IR转换
        match TACIR::convert_ast_to_tac(&annotated_ast) {
            Ok(mut tac_program) => {
                // 验证IR的正确性
                validate_tac_ir(&tac_program);
                
                // 运行优化Pass
                let mut inline_pass = TACIR::TAC_opt::inline::InlineOptimizationPass::new();
                let _ = inline_pass.run(&mut tac_program);
                
                let mut constant_pass = TACIR::TAC_opt::constant_opt::ConstantOptimizationPass::new();
                let _ = constant_pass.run(&mut tac_program);
                
                let mut algebraic_pass = TACIR::TAC_opt::algebraic_opt::AlgebraicOptimizationPass::new();
                let _ = algebraic_pass.run(&mut tac_program);
                
                // 输出最终优化后的IR
                println!("{}", tac_program);
            }
            Err(e) => {
                panic!("TAC IR转换失败: {:?}", e);
            }
        }
    }
    
    /// 简洁验证TAC IR的正确性
    fn validate_tac_ir(program: &TACIR::TACProgram) {
        if let Some(main_func) = program.get_main_function() {
            assert!(!main_func.basic_blocks.is_empty(), "main函数必须有基本块");
            assert!(!main_func.basic_blocks[0].instructions.is_empty(), "基本块必须有指令");
        } else {
            panic!("未找到main函数");
        }
    }
}


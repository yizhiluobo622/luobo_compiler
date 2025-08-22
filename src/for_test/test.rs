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
                
                // 运行完整的优化流水线
                println!("\n=== 运行完整优化流水线 ===");
                match TACIR::TAC_opt::run_all_optimizations(&mut tac_program) {
                    Ok(results) => {
                        println!("✅ 优化流水线执行成功！");
                        for (i, result) in results.iter().enumerate() {
                            if result.optimized {
                                println!("   第{}个优化: 优化指令数 {}", i + 1, result.instructions_optimized);
                            } else {
                                println!("   第{}个优化: 无优化机会", i + 1);
                            }
                        }
                        
                        // 显示优化后的IR
                        println!("\n=== 优化后的IR ===");
                        TACIR::debug_function_ir(&tac_program, "main");
                        TACIR::print_tac_program(&tac_program);
                    }
                    Err(e) => {
                        println!("❌ 优化流水线失败: {}", e);
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
}


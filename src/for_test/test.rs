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
                
                // 单独运行内联优化进行详细分析
                println!("\n=== 单独测试内联优化 ===");
                let mut inline_opt = TACIR::TAC_opt::inline::InlineOptimizationPass::new();
                // 设置较高的阈值以便内联小函数
                inline_opt.set_threshold(100);
                match inline_opt.run(&mut tac_program) {
                    Ok(result) => {
                        if result.optimized {
                            println!("✅ 内联优化成功！");
                            println!("   优化指令数: {}", result.instructions_optimized);
                            
                            // 显示内联后的IR
                            println!("\n=== 内联优化后的IR ===");
                            TACIR::debug_function_ir(&tac_program, "main");
                            TACIR::print_tac_program(&tac_program);
                        } else {
                            println!("ℹ️ 内联优化没有发现优化机会");
                        }
                    }
                    Err(e) => {
                        println!("❌ 内联优化失败: {}", e);
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
    
    #[test]
    pub fn test_parser_array_dimensions() {
        // 读取测试文件
        let source_code = std::fs::read_to_string("/home/yizhiluobo/luobo_compiler/src/target_code/sy/000_1.sy")
            .expect("Failed to read test file");
        
        // Lexer
        let lexer = Lexer::new(&source_code);
        
        // Parser
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().expect("Parser failed");
        
        // 生成AST的DOT图用于可视化
        crate::frontend::debug::debug_parser::show_ast_dot(&ast, "000_1_parser_test.dot");
        
        // 检查解析器是否正确识别了数组维度
        check_array_dimensions(&ast);
        
        println!("✅ 解析器数组维度测试完成");
    }
    
    /// 检查AST中的数组维度是否正确识别
    fn check_array_dimensions(ast: &crate::frontend::ast::Ast) {
        use crate::frontend::ast::{AstKind, Type};
        
        match &ast.kind {
            AstKind::Program { global_variables, .. } => {
                println!("=== 检查全局变量声明中的数组维度 ===");
                
                for var_decl in global_variables {
                    if let AstKind::VariableDeclaration { variable_name, variable_type, .. } = &var_decl.kind {
                        println!("变量: {} -> {:?}", variable_name, variable_type);
                        
                        // 检查数组类型
                        if let Type::ArrayType { element_type, array_size } = variable_type {
                            println!("  📦 数组类型: element_type={:?}, array_size={:?}", element_type, array_size);
                            
                            // 检查嵌套数组
                            if let Type::ArrayType { element_type: inner_element, array_size: inner_size } = element_type.as_ref() {
                                println!("    🔄 嵌套数组: inner_element={:?}, inner_size={:?}", inner_element, inner_size);
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
   
  
    
    #[test]
    pub fn test_inline_constant_optimization() {
        println!("=== 测试内联+常量优化协同效果 ===");
        
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
        match TACIR::convert_ast_to_tac(&annotated_ast) {
            Ok(mut tac_program) => {
                println!("✅ TAC IR转换成功！");
                
                // 显示优化前的IR
                println!("\n=== 优化前的IR ===");
                TACIR::print_tac_program(&tac_program);
                analyze_optimization_opportunities(&tac_program);
                
                // 步骤1: 执行内联优化
                println!("\n=== 步骤1: 执行内联优化 ===");
                let mut inline_opt = TACIR::TAC_opt::inline::InlineOptimizationPass::new();
                inline_opt.set_threshold(100); // 高阈值，确保小函数被内联
                
                match inline_opt.run(&mut tac_program) {
                    Ok(result) => {
                        if result.optimized {
                            println!("✅ 内联优化成功！内联了 {} 个函数调用", result.instructions_optimized);
                            
                            // 显示内联后的IR
                            println!("\n=== 内联后的IR ===");
                            TACIR::print_tac_program(&tac_program);
                            analyze_optimization_opportunities(&tac_program);
                        } else {
                            println!("ℹ️ 内联优化没有发现优化机会");
                        }
                    }
                    Err(e) => {
                        println!("❌ 内联优化失败: {}", e);
                        return;
                    }
                }
                
                // 步骤2: 执行常量传播优化
                println!("\n=== 步骤2: 执行常量传播优化 ===");
                let mut constant_opt = TACIR::TAC_opt::constant_opt::ConstantOptimizationPass::new();
                
                match constant_opt.run(&mut tac_program) {
                    Ok(result) => {
                        if result.optimized {
                            println!("✅ 常量传播优化成功！优化了 {} 条指令", result.instructions_optimized);
                            
                            // 显示最终优化后的IR
                            println!("\n=== 最终优化后的IR ===");
                            TACIR::print_tac_program(&tac_program);
                            analyze_optimization_opportunities(&tac_program);
                        } else {
                            println!("ℹ️ 常量传播优化没有发现优化机会");
                        }
                    }
                    Err(e) => {
                        println!("❌ 常量传播优化失败: {}", e);
                    }
                }
                
                println!("✅ 内联+常量优化协同测试完成！");
            }
            Err(e) => {
                println!("❌ TAC IR转换失败: {}", e);
                panic!("TAC IR转换失败");
            }
        }
    }
    
    /// 分析当前IR中的优化机会
    fn analyze_optimization_opportunities(program: &TACIR::TACProgram) {
        println!("\n📊 优化机会分析:");
        
        let mut total_instructions = 0;
        let mut function_calls = 0;
        let mut constant_assignments = 0;
        let mut binary_ops_with_constants = 0;
        
        for function in &program.functions {
            println!("  函数 {}: {} 个基本块", function.name, function.basic_blocks.len());
            
            for block in &function.basic_blocks {
                total_instructions += block.instructions.len();
                
                for instruction in &block.instructions {
                    match instruction {
                        TACIR::TACInstruction::FunctionCall { .. } => {
                            function_calls += 1;
                            println!("    🔗 函数调用: {:?}", instruction);
                        }
                        TACIR::TACInstruction::Assign { source: TACIR::Operand::Constant(_), .. } => {
                            constant_assignments += 1;
                            println!("    📊 常量赋值: {:?}", instruction);
                        }
                        TACIR::TACInstruction::BinaryOp { left: TACIR::Operand::Constant(_), right: TACIR::Operand::Constant(_), .. } => {
                            binary_ops_with_constants += 1;
                            println!("    🧮 常量运算: {:?}", instruction);
                        }
                        TACIR::TACInstruction::BinaryOp { 
                            left: TACIR::Operand::Constant(_), .. 
                        } | TACIR::TACInstruction::BinaryOp { 
                            right: TACIR::Operand::Constant(_), .. 
                        } => {
                            println!("    🔢 部分常量运算: {:?}", instruction);
                        }
                        _ => {}
                    }
                }
            }
        }
        
        println!("  📈 总指令数: {}", total_instructions);
        println!("  🔗 函数调用数: {} (内联机会)", function_calls);
        println!("  📊 常量赋值数: {} (传播机会)", constant_assignments);
        println!("  🧮 常量运算数: {} (折叠机会)", binary_ops_with_constants);
    }

}


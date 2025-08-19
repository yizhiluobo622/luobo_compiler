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
    pub fn test_array_functionality() {
        // 读取测试文件
        let source_code = std::fs::read_to_string("/home/yizhiluobo/luobo_compiler/src/target_code/sy/000_1.sy")
            .expect("Failed to read test file");
        
        // Lexer
        let lexer = Lexer::new(&source_code);
        
        // Parser
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().expect("Parser failed");
        
        // 先检查AST中的数组维度
        println!("=== 检查AST中的数组维度 ===");
        check_ast_array_dimensions(&ast);
        
        // Semantic Analysis
        let annotated_ast = analyze_ast_with_semantic_info(ast).expect("Semantic analysis failed");
        
        // 检查语义分析后的数组维度
        println!("\n=== 检查语义分析后的数组维度 ===");
        check_ast_array_dimensions(&annotated_ast);
        
        // TAC IR转换
        println!("\n=== 开始数组功能测试 ===");
        match TACIR::convert_ast_to_tac(&annotated_ast) {
            Ok(tac_program) => {
                println!("✅ TAC IR转换成功！");
                
                // 专门测试数组功能
                test_global_arrays(&tac_program);
                test_array_instructions(&tac_program);
                
                println!("✅ 数组功能测试完成！");
            }
            Err(e) => {
                println!("❌ TAC IR转换失败: {}", e);
                panic!("TAC IR转换失败");
            }
        }
    }
    
    /// 检查AST中的数组维度是否正确识别
    fn check_ast_array_dimensions(ast: &crate::frontend::ast::Ast) {
        use crate::frontend::ast::{AstKind, Type};
        
        match &ast.kind {
            AstKind::Program { global_variables, .. } => {
                println!("全局变量数量: {}", global_variables.len());
                
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
    
    /// 测试全局数组变量是否正确添加
    fn test_global_arrays(program: &TACIR::TACProgram) {
        println!("\n=== 测试全局数组变量 ===");
        
        // 检查全局数组变量列表
        println!("全局数组变量数量: {}", program.global_array_variables.len());
        for (name, array_type, dimensions) in &program.global_array_variables {
            println!("  📦 {}: {:?} 维度: {:?}", name, array_type, dimensions);
        }
        
        // 检查全局变量列表中是否包含数组
        println!("\n全局变量列表中的数组:");
        for (name, var_type, initial_value) in &program.global_variables {
            // 使用字符串匹配来检查是否为数组类型
            let type_str = format!("{:?}", var_type);
            if type_str.contains("ArrayType") {
                println!("  📦 {}: {:?} 初始值: {:?}", name, var_type, initial_value);
            }
        }
        
        // 验证预期的数组变量
        let expected_arrays = vec![
            ("to", 5005),      // maxm
            ("nex", 5005),     // maxm
            ("head", 1005),    // maxn
            ("que", 1005),     // maxn
            ("inq", 1005),     // maxn
        ];
        
        for (expected_name, expected_size) in expected_arrays {
            let found = program.global_array_variables.iter()
                .find(|(name, _, _)| name == expected_name);
            
            match found {
                Some((name, array_type, dimensions)) => {
                    println!("✅ 找到数组 {}: {:?} 维度: {:?}", name, array_type, dimensions);
                    
                    // 验证维度是否正确
                    if let Some(first_dim) = dimensions.first() {
                        if *first_dim == expected_size {
                            println!("  ✅ 维度正确: {} = {}", expected_name, expected_size);
                        } else {
                            println!("  ❌ 维度错误: 期望 {}, 实际 {}", expected_size, first_dim);
                        }
                    }
                }
                None => {
                    println!("❌ 未找到预期的数组: {}", expected_name);
                }
            }
        }
    }
    
    /// 测试数组相关的IR指令是否正确生成
    fn test_array_instructions(program: &TACIR::TACProgram) {
        println!("\n=== 测试数组IR指令 ===");
        
        // 检查add_edge函数中的数组操作
        if let Some(add_edge_func) = program.functions.iter().find(|f| f.name == "add_edge") {
            println!("检查 add_edge 函数中的数组操作:");
            
            let mut array_ops_count = 0;
            for (i, instruction) in add_edge_func.basic_blocks[0].instructions.iter().enumerate() {
                match instruction {
                    TACIR::TACInstruction::GetElementPtr { target, base, indices } => {
                        println!("  {}: GetElementPtr {} = {}[{}]", i, target, base, indices[0]);
                        array_ops_count += 1;
                    }
                    TACIR::TACInstruction::Store { value, address } => {
                        println!("  {}: Store {} -> {}", i, value, address);
                        array_ops_count += 1;
                    }
                    TACIR::TACInstruction::Load { target, address } => {
                        println!("  {}: Load {} <- {}", i, target, address);
                        array_ops_count += 1;
                    }
                    _ => {}
                }
            }
            println!("  📊 add_edge函数中数组操作总数: {}", array_ops_count);
        }
        
        // 检查init函数中的数组操作
        if let Some(init_func) = program.functions.iter().find(|f| f.name == "init") {
            println!("\n检查 init 函数中的数组操作:");
            
            let mut array_ops_count = 0;
            for (i, instruction) in init_func.basic_blocks[0].instructions.iter().enumerate() {
                match instruction {
                    TACIR::TACInstruction::GetElementPtr { target, base, indices } => {
                        println!("  {}: GetElementPtr {} = {}[{}]", i, target, base, indices[0]);
                        array_ops_count += 1;
                    }
                    TACIR::TACInstruction::Store { value, address } => {
                        println!("  {}: Store {} -> {}", i, value, address);
                        array_ops_count += 1;
                    }
                    _ => {}
                }
            }
            println!("  📊 init函数中数组操作总数: {}", array_ops_count);
        }
        
        // 检查inqueue函数中的数组操作
        if let Some(inqueue_func) = program.functions.iter().find(|f| f.name == "inqueue") {
            println!("\n检查 inqueue 函数中的数组操作:");
            
            let mut array_ops_count = 0;
            for (i, instruction) in inqueue_func.basic_blocks[0].instructions.iter().enumerate() {
                match instruction {
                    TACIR::TACInstruction::GetElementPtr { target, base, indices } => {
                        println!("  {}: GetElementPtr {} = {}[{}]", i, target, base, indices[0]);
                        array_ops_count += 1;
                    }
                    TACIR::TACInstruction::Store { value, address } => {
                        println!("  {}: Store {} -> {}", i, value, address);
                        array_ops_count += 1;
                    }
                    _ => {}
                }
            }
            println!("  📊 inqueue函数中数组操作总数: {}", array_ops_count);
        }
    }

}


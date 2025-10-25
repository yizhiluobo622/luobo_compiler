pub mod tacir;
pub mod node_mapping;
pub mod ast_to_tacir;
pub mod TAC_opt;

pub use tacir::*;
pub use node_mapping::*;
pub use ast_to_tacir::*;
pub use crate::tacir::ast_to_tacir::ConversionError;

/// 便利函数：转换AST到三地址码IR
pub fn convert_ast_to_tac(ast: &crate::frontend::ast::Ast) -> Result<TACProgram, ConversionError> {
    let mut converter = ASTToTACConverter::new();
    let mut program = converter.convert(ast)?;
    
    // 处理全局数组初始化
    program.process_global_array_initializations(converter.get_mapper());
    
    Ok(program)
}

/// 便利函数：打印三地址码IR
pub fn print_tac_program(program: &TACProgram) {
    println!("=== 三地址码IR程序 ===");
    
    // 打印全局变量
    if !program.global_variables.is_empty() {
        println!("全局变量:");
        for (name, var_type, initial_value, is_const) in &program.global_variables {
            match initial_value {
                Some(value) => println!("  {}: {:?} = {:?}", name, var_type, value),
                None => println!("  {}: {:?}", name, var_type),
            }
        }
        println!();
    }
    
    // 打印函数
    for (i, function) in program.functions.iter().enumerate() {
        println!("函数 {}: {} -> {:?}", i, function.name, function.return_type);
        
        // 打印参数
        println!("  AST参数数量: {}, TAC IR参数数量: {}", function.ast_parameter_count, function.parameters.len());
        if !function.parameters.is_empty() {
            print!("  参数: ");
            for (j, (name, typ)) in function.parameters.iter().enumerate() {
                if j > 0 { print!(", "); }
                print!("{}: {:?}", name, typ);
            }
            println!();
        }
        
        // 打印基本块
        for (block_id, block) in function.basic_blocks.iter().enumerate() {
            if let Some(label) = &block.label {
                println!("  {}:", label);
            } else {
                println!("  基本块 {}:", block_id);
            }
            
            // 打印指令
            for (inst_id, instruction) in block.instructions.iter().enumerate() {
                println!("    {}: {:?}", inst_id, instruction);
            }
            
            // 打印控制流信息
            if !block.predecessors.is_empty() || !block.successors.is_empty() {
                print!("    前驱: {:?}, 后继: {:?}", block.predecessors, block.successors);
                println!();
            }
        }
        println!();
    }
}

/// 调试函数：快速查看IR结构
pub fn debug_ir_structure(program: &TACProgram) {
    println!("=== IR结构调试信息 ===");
    println!("程序包含 {} 个函数", program.functions.len());
    
    for (func_id, function) in program.functions.iter().enumerate() {
        println!("函数 {}: '{}'", func_id, function.name);
        println!("  返回类型: {:?}", function.return_type);
        println!("  AST参数数量: {}", function.ast_parameter_count);
        println!("  TAC IR参数数量: {}", function.parameters.len());
        println!("  基本块数量: {}", function.basic_blocks.len());
        println!("  临时变量数量: {}", function.temp_counter);
        println!("  标签数量: {}", function.label_counter);
        
        for (block_id, block) in function.basic_blocks.iter().enumerate() {
            println!("  基本块 {}: 标签={:?}, 指令数={}, 前驱={:?}, 后继={:?}", 
                block_id, block.label, block.instructions.len(), block.predecessors, block.successors);
        }
        println!();
    }
}

/// 调试函数：查看特定函数的IR
pub fn debug_function_ir(program: &TACProgram, function_name: &str) {
    if let Some(function) = program.functions.iter().find(|f| f.name == function_name) {
        println!("=== 函数 '{}' 的IR ===", function_name);
        println!("{}", function);
    } else {
        println!("未找到函数 '{}'", function_name);
    }
}

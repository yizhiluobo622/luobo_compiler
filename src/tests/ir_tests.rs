use crate::ir::high_level::{HighLevelIR, HighLevelType, HighLevelOperation, HighLevelValue};
use crate::ir::high_level::types::{IntType, FloatType};
use crate::passes::high_level::*;

#[test]
fn test_high_level_ir_creation() {
    let mut hir = HighLevelIR::new();
    
    // 测试创建高层IR
    assert_eq!(hir.functions.len(), 0);
    assert_eq!(hir.global_variables.len(), 0);
    
    // 添加一个函数
    let function = crate::ir::high_level::module::HighLevelFunction::new(
        "test_function".to_string(),
        Vec::new(),
        Vec::new(),
        HighLevelType::Int(IntType::I32),
    );
    
    hir.add_function(function);
    assert_eq!(hir.functions.len(), 1);
}

#[test]
fn test_type_system() {
    // 测试基本类型
    let int_type = HighLevelType::Int(IntType::I32);
    let float_type = HighLevelType::Float(FloatType::F32);
    
    assert_eq!(int_type.size(), 4);
    assert_eq!(float_type.size(), 4);
    assert!(int_type.is_primitive());
    assert!(float_type.is_primitive());
    
    // 测试复合类型
    let array_type = HighLevelType::Array {
        element_type: Box::new(HighLevelType::Int(IntType::I32)),
        size: Some(10),
    };
    
    assert_eq!(array_type.size(), 40); // 10 * 4
    assert!(!array_type.is_primitive());
}

#[test]
fn test_value_system() {
    let var_value = HighLevelValue::Variable("x".to_string());
    let lit_value = HighLevelValue::Literal(HighLevelType::Int(IntType::I32), "42".to_string());
    
    assert_eq!(var_value.name(), "x");
    assert_eq!(lit_value.name(), "42");
    assert!(!var_value.is_constant());
    assert!(lit_value.is_constant());
}

#[test]
fn test_pass_functions() {
    // 测试Pass函数（简化实现）
    let ast = crate::frontend::ast::Ast::new(
        crate::frontend::ast::AstKind::Program {
            functions: Vec::new(),
            global_variables: Vec::new(),
        },
        crate::frontend::span::Span::new(0, 0),
    );
    
    let hir = ast_to_hir_pass(&ast);
    let hir = type_inference_pass(&hir);
    let hir = ssa_conversion_pass(&hir);
    let hir = cfg_construction_pass(&hir);
    let hir = dead_code_elimination_pass(&hir);
    let hir = constant_folding_pass(&hir);
    
    // 所有Pass都应该返回有效的IR
    assert!(hir.functions.is_empty());
    assert!(hir.global_variables.is_empty());
}

#[test]
fn test_utils_cfg() {
    use crate::utils::cfg::{ControlFlowGraph, BasicBlock};
    
    let mut cfg = ControlFlowGraph::new();
    
    // 创建基本块
    let block1 = BasicBlock {
        name: "entry".to_string(),
        instructions: vec!["mov rax, 1".to_string()],
        predecessors: Vec::new(),
        successors: vec!["block2".to_string()],
    };
    
    let block2 = BasicBlock {
        name: "block2".to_string(),
        instructions: vec!["add rax, 1".to_string()],
        predecessors: vec!["entry".to_string()],
        successors: Vec::new(),
    };
    
    cfg.add_block(block1);
    cfg.add_block(block2);
    cfg.add_edge("entry", "block2");
    
    assert_eq!(cfg.blocks.len(), 2);
    assert!(cfg.get_block("entry").is_some());
    assert!(cfg.get_block("block2").is_some());
}

#[test]
fn test_utils_optimization() {
    use crate::utils::optimization::{OptimizationContext, DeadCodeEliminator, ConstantFolder};
    
    let mut context = OptimizationContext::new();
    context.add_variable("x".to_string(), "i32".to_string());
    context.mark_constant("x", "42".to_string());
    
    assert!(context.get_constant_value("x").is_some());
    assert_eq!(context.get_constant_value("x").unwrap(), "42");
    
    let mut eliminator = DeadCodeEliminator::new();
    let instructions = vec![
        "add rax, 0".to_string(),
        "mov rbx, 1".to_string(),
        "nop".to_string(),
    ];
    
    let optimized = eliminator.eliminate_dead_code(&instructions);
    assert!(!optimized.is_empty());
    
    let mut folder = ConstantFolder::new();
    let folded = folder.fold_constants(&instructions);
    assert!(!folded.is_empty());
}

#[test]
fn test_utils_codegen() {
    use crate::utils::codegen::CodeGenerator;
    
    let codegen = CodeGenerator::new("x86_64");
    assert_eq!(codegen.target_architecture, "x86_64");
    
    let instructions = vec![
        "add rax, rbx".to_string(),
        "mov rcx, 42".to_string(),
        "jmp label".to_string(),
    ];
    
    let assembly = codegen.generate_assembly(&instructions);
    assert!(assembly.contains("add"));
    assert!(assembly.contains("mov"));
    assert!(assembly.contains("jmp"));
    
    let reg = codegen.allocate_register("virtual_reg");
    assert!(!reg.is_empty());
}

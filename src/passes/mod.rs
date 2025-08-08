pub mod parser_pass;
pub mod mlir;
pub mod codegen_pass;
pub mod high_level;
pub mod mid_level;
pub mod low_level;
pub mod optimization;

// 重新导出各个Pass函数
pub use parser_pass::gen_sema_ast_pass;
pub use mlir::{
    high_level_mlir_pass,
    dialect_lowering_pass,
    mlir_optimization_pass,
    target_lowering_pass,
};
pub use codegen_pass::codegen_pass;

// 重新导出新的Pass函数
pub use high_level::{
    ast_to_hir_pass,
    type_inference_pass,
    ssa_conversion_pass,
    cfg_construction_pass,
    dead_code_elimination_pass,
    constant_folding_pass,
};

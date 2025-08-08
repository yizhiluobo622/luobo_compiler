pub mod ast_to_hir_pass;
pub mod type_inference_pass;
pub mod ssa_conversion_pass;
pub mod cfg_construction_pass;
pub mod dead_code_elimination_pass;
pub mod constant_folding_pass;

// 重新导出Pass函数
pub use ast_to_hir_pass::ast_to_hir_pass;
pub use type_inference_pass::type_inference_pass;
pub use ssa_conversion_pass::ssa_conversion_pass;
pub use cfg_construction_pass::cfg_construction_pass;
pub use dead_code_elimination_pass::dead_code_elimination_pass;
pub use constant_folding_pass::constant_folding_pass;

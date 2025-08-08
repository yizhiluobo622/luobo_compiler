pub mod parser_pass;
pub mod mlir_pass;
pub mod codegen_pass;

// 重新导出各个Pass函数
pub use parser_pass::gen_sema_ast_pass;
pub use mlir_pass::mlir_pass;
pub use codegen_pass::codegen_pass;

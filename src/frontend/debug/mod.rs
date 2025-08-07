// Debug模块统一导出
pub mod debug_lexer;
pub mod debug_parser;
pub mod debug_semantic;

// 重新导出词法分析器调试功能
pub use debug_lexer::*;
pub use debug_semantic::*;

#[cfg(test)]
mod test_debug;

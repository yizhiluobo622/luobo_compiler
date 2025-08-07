// Debug模块统一导出
pub mod debug_lexer;
pub mod debug_parser;

// 重新导出词法分析器调试功能
pub use debug_lexer::*;

#[cfg(test)]
mod test_debug;

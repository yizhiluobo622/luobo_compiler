// 调试模块测试文件
// 用于验证debug模块的功能

#[cfg(test)]
mod tests {
    use crate::frontend::debug::debug_lexer::*;

    #[test]
    fn test_lexer_debug_functions() {
        let input = "int main() { return 0; }";
        
        // 测试show_tokens函数
        show_tokens(input);
        
        // 测试show_located_tokens函数
        show_located_tokens(input);
        
        // 测试verify_lexer函数
        verify_lexer(input);
        
        // 测试reconstruct_source函数
        let reconstructed = reconstruct_source(input);
        assert!(!reconstructed.is_empty());
    }
}

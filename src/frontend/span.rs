use crate::frontend::lexer::Token;

/// 源代码位置信息
/// 设计为可扩展的，为未来多文件编译预留接口
#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub file_id: usize,    // 文件标识符，用于多文件编译，现在默认为 0
    pub line: usize,       // 行号，从 1 开始
    pub column: usize,     // 列号，从 1 开始
    pub start_pos: usize,  // 起始字节偏移
    pub end_pos: usize,    // 结束字节偏移
}

impl Span {
    /// 创建一个新的 Span
    pub fn new(file_id: usize, line: usize, column: usize, start_pos: usize, end_pos: usize) -> Self {
        Span {
            file_id,
            line,
            column,
            start_pos,
            end_pos,
        }
    }

    /// 创建一个只包含起始位置的 Span（用于错误提示）
    pub fn start_only(file_id: usize, line: usize, column: usize, pos: usize) -> Self {
        Span {
            file_id,
            line,
            column,
            start_pos: pos,
            end_pos: pos,
        }
    }

    /// 获取位置信息字符串（用于错误提示）
    pub fn to_string(&self) -> String {
        format!("{}:{}", self.line, self.column)
    }

    /// 获取文件位置信息字符串（用于多文件错误提示）
    pub fn to_file_string(&self) -> String {
        format!("file_{}:{}:{}", self.file_id, self.line, self.column)
    }
}

/// 带位置信息的 Token
#[derive(Debug, PartialEq)]
pub struct LocatedToken {
    pub token: Token,
    pub span: Span,
}

impl LocatedToken {
    pub fn new(token: Token, span: Span) -> Self {
        LocatedToken { token, span }
    }

    /// 获取 Token 类型
    pub fn token(&self) -> &Token {
        &self.token
    }

    /// 获取位置信息
    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    KeywordInt,
    KeywordFloat,
    KeywordConst,
    KeywordVoid,
    KeywordIf,
    KeywordElse,
    KeywordWhile,
    KeywordBreak,
    KeywordContinue,
    KeywordReturn,

    Identifier(String), // 如 "main"

    IntConst(i32),   // 如 0, 123
    FloatConst(f32),

    // 符号和运算符
    Plus,         // +
    Minus,        // -
    Star,         // *
    Slash,        // /
    Percent,      // %
    Equal,        // =
    DoubleEqual,  // ==
    NotEqual,     // !=
    Less,         // <
    Greater,      // >
    LessEqual,    // <=
    GreaterEqual, // >=
    AndAnd,       // &&
    OrOr,         // ||
    Bang,         // !

    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    LBracket,  // [
    RBracket,  // ]
    Semicolon, // ;
    Comma,     // ,
    EOF,       // 文件结束标记
}

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input,
            pos: 0,
        }
    }

    // 获取当前字符（可能为 None，只有在超出索引时）
    fn current_char(&self) -> Option<char> {
        self.input.chars().nth(self.pos)
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current_char() {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_single_line_comment(&mut self) {
        while let Some(c) = self.current_char() {
            if c == '\n' {
                break;
            }
            self.advance();
        }
    }

    fn skip_block_comment(&mut self) {
        let mut depth = 1; // 初始深度为 1（当前已读到 "/*"）
        while depth > 0 {
            let c = match self.current_char() {
                Some(c) => c,
                None => break, // 字符串意外结束（未闭合的注释）
            };

            // 检查 "/*"（嵌套注释开始）
            if c == '/' && self.pos + 1 < self.input.len() {
                let next_char = self.input.chars().nth(self.pos + 1).unwrap();
                if next_char == '*' {
                    self.advance(); // 跳过 '/'
                    self.advance(); // 跳过 '*'
                    depth += 1;
                    continue;
                }
            }

            // 检查 "*/"（注释结束）
            if c == '*' && self.pos + 1 < self.input.len() {
                let next_char = self.input.chars().nth(self.pos + 1).unwrap();
                if next_char == '/' {
                    self.advance(); // 跳过 '*'
                    self.advance(); // 跳过 '/'
                    depth -= 1;
                    continue;
                }
            }

            self.advance(); // 其他字符直接跳过
        }
    }

    fn skip_line_comment(&mut self) {
        // 检查是否真的是 "//"（避免误判单个 "/"）
        if let Some('/') = self.current_char() {
            if let Some('/') = self.peek_next_char() {
                // 检查下一个字符
                self.advance(); // 跳过第一个 '/'
                self.advance(); // 跳过第二个 '/'

                // 一直前进直到行尾（'\n'）或字符串结束
                while let Some(c) = self.current_char() {
                    if c == '\n' {
                        break; // 遇到换行符，注释结束
                    }
                    self.advance(); // 跳过注释内容
                }
            }
        }
    }

    /// 辅助方法：查看下一个字符（移动 pos）
    fn peek_next_char(&self) -> Option<char> {
        if self.pos + 1 < self.input.len() {
            Some(self.input.chars().nth(self.pos + 1).unwrap())
        } else {
            None
        }
    }

    /// 正式接口：给 Parser 使用，只返回 Token
    pub fn next_token(&mut self) -> Token {
        let (token, _) = self.next_token_with_text();
        token
    }

    /// 调试接口：返回 Token 和原文字符串，用于调试
    pub fn next_token_with_text(&mut self) -> (Token, String) {
        loop {
            self.skip_whitespace();

            // 如果遇到注释，跳过它们
            let c = match self.current_char() {
                Some(ch) => ch,
                None => return (Token::EOF, "".to_string()),
            };

            if c == '/' {
                if let Some(next) = self.input.chars().nth(self.pos + 1) {
                    if next == '/' {
                        self.skip_single_line_comment();
                        continue; // 继续下一轮循环，重新检测 token
                    } else if next == '*' {
                        self.skip_block_comment();
                        continue; // 继续下一轮循环，重新检测 token
                    }
                }
            }

            // 如果不是注释，跳出循环，开始真正解析 token
            break;
        }

        let start_pos = self.pos; // 记录当前 Token 的起始位置
        // 下面是原来的 token 识别逻辑
        let c = match self.current_char() {
            Some(ch) => ch,
            None => return (Token::EOF, "".to_string()),
        };

        match c {
            // 标识符或关键字
            'a'..='z' | 'A'..='Z' | '_' => {
                let tok = self.read_identifier_or_keyword();
                let text = self.input[start_pos..self.pos].to_string(); // 原文
                (tok, text)
            }

            // 数字
            '0'..='9' => {
                let tok = self.read_number();
                let text = self.input[start_pos..self.pos].to_string();
                (tok, text)
            }

            // 符号
            '+' => {
                self.advance();
                (Token::Plus, "+".to_string())
            }
            '-' => {
                self.advance();
                (Token::Minus, "-".to_string())
            }
            '*' => {
                self.advance();
                (Token::Star, "*".to_string())
            }
            '/' => {
                self.advance();
                (Token::Slash, "/".to_string())
            }
            '(' => {
                self.advance();
                (Token::LParen, "(".to_string())
            }
            ')' => {
                self.advance();
                (Token::RParen, ")".to_string())
            }
            '{' => {
                self.advance();
                (Token::LBrace, "{".to_string())
            }
            '}' => {
                self.advance();
                (Token::RBrace, "}".to_string())
            }
            ';' => {
                self.advance();
                (Token::Semicolon, ";".to_string())
            }

            '=' => {
                self.advance();
                if let Some(next) = self.current_char() {
                    if next == '=' {
                        self.advance();
                        (Token::DoubleEqual, "==".to_string())
                    } else {
                        (Token::Equal, "=".to_string())
                    }
                } else {
                    (Token::Equal, "=".to_string())
                }
            }
            '!' => {
                self.advance();
                if let Some(next) = self.current_char() {
                    if next == '=' {
                        self.advance();
                        (Token::NotEqual, "!=".to_string())
                    } else {
                        (Token::Bang, "!".to_string()) // 单独的 !
                    }
                } else {
                    (Token::Bang, "!".to_string())
                }
            }
            '<' => {
                self.advance();
                if let Some(next) = self.current_char() {
                    if next == '=' {
                        self.advance();
                        (Token::LessEqual, "<=".to_string())
                    } else {
                        (Token::Less, "<".to_string())
                    }
                } else {
                    (Token::Less, "<".to_string())
                }
            }
            '>' => {
                self.advance();
                if let Some(next) = self.current_char() {
                    if next == '=' {
                        self.advance();
                        (Token::GreaterEqual, ">=".to_string())
                    } else {
                        (Token::Greater, ">".to_string())
                    }
                } else {
                    (Token::Greater, ">".to_string())
                }
            }
            '&' => {
                self.advance();
                if let Some(next) = self.current_char() {
                    if next == '&' {
                        self.advance();
                        (Token::AndAnd, "&&".to_string())
                    } else {
                        panic!("Unexpected character after '&': {}", next)
                    }
                } else {
                    panic!("Unexpected character: '&'");
                }
            }
            '|' => {
                self.advance();
                if let Some(next) = self.current_char() {
                    if next == '|' {
                        self.advance();
                        (Token::OrOr, "||".to_string())
                    } else {
                        panic!("Unexpected character after '|': {}", next)
                    }
                } else {
                    panic!("Unexpected character: '|'");
                }
            }

            // 这里可以继续加其他字符实现

            _ => {
                self.advance();
                panic!("Unexpected character: {}", c)
            }
        }
    }

    fn read_identifier_or_keyword(&mut self) -> Token {
        let start = self.pos;
        while let Some(c) = self.current_char() {
            if c.is_ascii_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }
        let ident: String = self.input[start..self.pos].to_lowercase(); // SysY 应该是大小写敏感？通常关键字是小写

        match ident.as_str() {
            "int" => Token::KeywordInt,
            "float" => Token::KeywordFloat,
            "const" => Token::KeywordConst,
            "void" => Token::KeywordVoid,
            "if" => Token::KeywordIf,
            "else" => Token::KeywordElse,
            "while" => Token::KeywordWhile,
            "return" => Token::KeywordReturn,
            "break" => Token::KeywordBreak,
            "continue" => Token::KeywordContinue,
            _ => Token::Identifier(ident),
        }
    }

    fn read_number(&mut self) -> Token {
        let start = self.pos;
        let mut has_dot = false;

        while let Some(c) = self.current_char() {
            if c.is_ascii_digit() {
                self.advance();
            }else if c == '.' && !has_dot {
                has_dot = true;
                self.advance();
                // 继续读取数字部分
                while let Some(c) = self.current_char() {
                    if c.is_ascii_digit() {
                        self.advance();
                    } else {
                        break;
                    }
                }
            } else {
                break;
            }
        }
        let num_str: String = self.input[start..self.pos].to_string();

        if has_dot {
            // 尝试解析为浮点数 f32
            match num_str.parse::<f32>() {
                Ok(f) => Token::FloatConst(f),
                Err(_) => {
                    // 如果解析失败（理论上不应该，除非输入非法），返回默认 0.0
                    Token::FloatConst(0.0)
                }
            }
        } else {
            // 尝试解析为整数 i32
            match num_str.parse::<i32>() {
                Ok(i) => Token::IntConst(i),
                Err(_) => {
                    // 如果解析失败（比如数字太大），返回默认 0
                    Token::IntConst(0)
            }
            }
        }               
    }
}
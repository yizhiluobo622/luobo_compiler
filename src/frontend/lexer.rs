#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    KeywordInt,
    KeywordFloat,
    KeywordConst,
    KeywordVoid,
    KeywordIf,
    KeywordElse,
    KeywordWhile,
    KeywordFor,
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
    pos: usize,      // 当前字节索引
    line: usize,     // 当前行号，从 1 开始
    column: usize,   // 当前列号，从 1 开始
    file_id: usize,  // 文件标识符，用于多文件编译，现在默认为 0
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input,
            pos: 0,
            line: 1,     // 第一行
            column: 1,   // 第一列
            file_id: 0,  // 默认文件ID，单文件编译时为 0
        }
    }

    /// 创建带文件ID的 Lexer（为未来多文件编译预留）
    pub fn new_with_file_id(input: &'a str, file_id: usize) -> Self {
        Lexer {
            input: input,
            pos: 0,
            line: 1,
            column: 1,
            file_id,
        }
    }
    
    /// 获取源代码
    pub fn get_source(&self) -> &'a str {
        self.input
    }

    // 获取当前字符（可能为 None，只有在超出索引时）
    fn current_char(&self) -> Option<char> {
        if self.pos >= self.input.len() {
            return None;
        }
        self.input[self.pos..].chars().next()
    }

    fn advance(&mut self) {
        if let Some(c) = self.current_char() {
            if c == '\n' {
                self.line += 1;
                self.column = 1;  // 换行后列号重置为 1
            } else {
                self.column += 1; // 否则列号 +1
            }
            self.pos += c.len_utf8(); // 注意：移动的是 UTF-8 字符的字节长度
        }
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

    

    fn skip_block_comment(&mut self) {
        // 已经确认是 /* 开头，跳过这两个字符
        self.advance(); // 跳过 '/'
        self.advance(); // 跳过 '*'
         
        while let Some(c) = self.current_char() {
            if c == '*' {
                if let Some(next) = self.peek_next_char() {
                    if next == '/' {
                        // 遇到第一个 */，注释结束
                        self.advance(); // 跳过 '*'
                        self.advance(); // 跳过 '/'
                        return;
                    }
                }
            }
            // 跳过所有其他字符（包括内部的 /* 和 */）
            self.advance();
        }
    
        // 如果到达文件末尾还没找到 */, 说明注释未闭合
        panic!("Unclosed block comment");
    }

    

    fn skip_line_comment(&mut self) {
        // 检查是否真的是 "//"（避免误判单个 "/"）
        if let Some('/') = self.current_char() {
            if let Some(next) = self.peek_next_char() {
                if next == '/' {
                    // 跳过 "//"
                    self.advance(); // 跳过第一个 '/'
                    self.advance(); // 跳过第二个 '/'
                    // 一直前进直到行尾（'\n'）或字符串结束
                    while let Some(c) = self.current_char() {
                        if c == '\n' {
                            self.advance(); // 跳过换行符
                            break; // 遇到换行符，注释结束
                        }
                        self.advance(); // 跳过注释内容
                    }
                }
                // 如果不是 "//"，则继续后续的 Token 解析
            }
        }
    }



    /// 正式接口：给 Parser 使用，只返回 Token
    #[allow(dead_code)]
    pub fn next_token(&mut self) -> Token {
        let (token, _) = self.parse_token();
        token
    }

    /// 带位置信息的 Token 接口
    pub fn next_located_token(&mut self) -> crate::frontend::span::LocatedToken {
        
        loop {
            self.skip_whitespace();
            

            
            let c = match self.current_char() {
                Some(ch) => ch,
                None => {
                    let span = crate::frontend::span::Span::new(
                        self.file_id,
                        self.line,
                        self.column,
                        self.pos,
                        self.pos
                    );
                    return crate::frontend::span::LocatedToken::new(Token::EOF, span);
                }
            };

            if c == '/' {
                
                if let Some(next) = self.peek_next_char() {
                    
                    if next == '/' {
                        
                        self.skip_line_comment();
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

        // 现在记录 Token 的起始位置
        let start_line = self.line;
        let start_column = self.column;
        let start_pos = self.pos;
        
        let (token, _) = self.parse_token();
        let end_pos = self.pos;
        
        let span = crate::frontend::span::Span::new(
            self.file_id,
            start_line,
            start_column,
            start_pos,
            end_pos
        );
        crate::frontend::span::LocatedToken::new(token, span)
    }

    /// 解析单个 Token（不处理空白字符和注释）
    pub fn parse_token(&mut self) -> (Token, String) {
        let start_pos = self.pos;
        let c = match self.current_char() {
            Some(ch) => ch,
            None => return (Token::EOF, "".to_string()),
        };

        match c {
            // 标识符或关键字
            'a'..='z' | 'A'..='Z' | '_' => {
                let tok = self.read_identifier_or_keyword();
                let text: String = self.input.chars().skip(start_pos).take(self.pos - start_pos).collect();
                (tok, text)
            }

            // 数字
            '0'..='9' => {
                let tok = self.read_number();
                let text: String = self.input.chars().skip(start_pos).take(self.pos - start_pos).collect();
                (tok, text)
            }
            // 以点开头的浮点数
            '.' => {
                let tok = self.read_number();
                let text: String = self.input.chars().skip(start_pos).take(self.pos - start_pos).collect();
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
            '%' => {
                self.advance();
                (Token::Percent, "%".to_string())
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
            '[' => {
                self.advance();
                (Token::LBracket, "[".to_string())
            }
            ']' => {
                self.advance();
                (Token::RBracket, "]".to_string())
            }
            ',' => {
                self.advance();
                (Token::Comma, ",".to_string())
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

    /// 调试接口：返回 Token 和原文字符串，用于调试


    fn read_identifier_or_keyword(&mut self) -> Token {
        let mut chars = Vec::new();
        while let Some(c) = self.current_char() {
            if c.is_ascii_alphanumeric() || c == '_' {
                chars.push(c);
                self.advance();
            } else {
                break;
            }
        }
        let ident: String = chars.into_iter().collect::<String>();

        match ident.as_str() {
            "int" => Token::KeywordInt,
            "float" => Token::KeywordFloat,
            "const" => Token::KeywordConst,
            "void" => Token::KeywordVoid,
            "if" => Token::KeywordIf,
            "else" => Token::KeywordElse,
            "while" => Token::KeywordWhile,
            "for" => Token::KeywordFor,
            "return" => Token::KeywordReturn,
            "break" => Token::KeywordBreak,
            "continue" => Token::KeywordContinue,
            _ => Token::Identifier(ident),
        }
    }

    fn read_number(&mut self) -> Token {
        let start = self.pos;
        let mut has_dot = false;
        let mut has_e = false;
        let mut is_hex = false;
        let mut is_octal = false;

        // 检查是否以点开头（如 .0, .5）
        if let Some('.') = self.current_char() {
            has_dot = true;
            self.advance(); // 跳过 '.'
            
            // 读取小数部分
            while let Some(c) = self.current_char() {
                if c.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }
            
            // 检查是否有指数部分
            if let Some(c) = self.current_char() {
                if c == 'e' || c == 'E' {
                    has_e = true;
                    self.advance();
                    
                    // 读取指数符号
                    if let Some(sign) = self.current_char() {
                        if sign == '+' || sign == '-' {
                            self.advance();
                        }
                    }
                    
                    // 读取指数数字
                    while let Some(c) = self.current_char() {
                        if c.is_ascii_digit() {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
            }
            
            let num_str: &str = &self.input[start..self.pos];
            match num_str.parse::<f32>() {
                Ok(f) => return Token::FloatConst(f),
                Err(_) => return Token::FloatConst(0.0)
            }
        }

        // 检查是否是十六进制
        if let Some('0') = self.current_char() {
            if let Some(next) = self.peek_next_char() {
                if next == 'x' || next == 'X' {
                    is_hex = true;
                    self.advance(); // 跳过 '0'
                    self.advance(); // 跳过 'x' 或 'X'
                    
                    // 读取十六进制数字，支持浮点数格式
                    let mut has_dot = false;
                    let mut has_exponent = false;
                    
                    // 读取十六进制数字部分
                    while let Some(c) = self.current_char() {
                        if c.is_ascii_hexdigit() {
                            self.advance();
                        } else if c == '.' && !has_dot && !has_exponent {
                            has_dot = true;
                            self.advance();
                            // 继续读取小数点后的十六进制数字
                            while let Some(c) = self.current_char() {
                                if c.is_ascii_hexdigit() {
                                    self.advance();
                                } else {
                                    break;
                                }
                            }
                        } else if (c == 'p' || c == 'P') && !has_exponent {
                            has_exponent = true;
                            self.advance();
                            
                            // 读取指数符号
                            if let Some(sign) = self.current_char() {
                                if sign == '+' || sign == '-' {
                                    self.advance();
                                }
                            }
                            
                            // 读取指数数字（十进制）
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
                    
                    let hex_str: String = self.input.chars().skip(start).take(self.pos - start).collect();
                    
                    if has_dot || has_exponent {
                        // 十六进制浮点数
                        match hex_str.parse::<f32>() {
                            Ok(f) => return Token::FloatConst(f),
                            Err(_) => return Token::FloatConst(0.0)
                        }
                    } else {
                        // 十六进制整数
                        match i32::from_str_radix(&hex_str[2..], 16) {
                            Ok(i) => return Token::IntConst(i),
                            Err(_) => return Token::IntConst(0),
                        }
                    }
                } else if next.is_ascii_digit() && next != '0' {
                    // 检查是否后面跟着小数点，如果是则按十进制浮点数处理（基于字节安全的前瞻）
                    let mut temp_pos = self.pos + 1; // 跳过 '0'
                    let mut has_dot_after_zero = false;
                    while temp_pos < self.input.len() {
                        if let Some(ch) = self.input[temp_pos..].chars().next() {
                            if ch == '.' {
                                has_dot_after_zero = true;
                                break;
                            } else if ch.is_ascii_digit() {
                                temp_pos += ch.len_utf8();
                                continue;
                            }
                        }
                        break;
                    }
                    
                    if has_dot_after_zero {
                        // 按十进制浮点数处理，不在这里返回，继续到后面的通用处理逻辑
                        self.advance(); // 跳过 '0'
                    } else {
                        // 八进制
                        is_octal = true;
                        self.advance(); // 跳过 '0'
                        
                        while let Some(c) = self.current_char() {
                            if c >= '0' && c <= '7' {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                        
                        let oct_str: &str = &self.input[start..self.pos];
                        match i32::from_str_radix(&oct_str[1..], 8) {
                            Ok(i) => return Token::IntConst(i),
                            Err(_) => return Token::IntConst(0),
                        }
                    }
                }
            }
        }

        // 处理十进制整数和浮点数
        while let Some(c) = self.current_char() {
            if c.is_ascii_digit() {
                self.advance();
            } else if c == '.' && !has_dot && !has_e {
                has_dot = true;
                self.advance();
                // 继续读取小数部分
                while let Some(c) = self.current_char() {
                    if c.is_ascii_digit() {
                        self.advance();
                    } else {
                        break;
                    }
                }
            } else if (c == 'e' || c == 'E') && !has_e {
                has_e = true;
                self.advance();
                
                // 读取指数符号
                if let Some(sign) = self.current_char() {
                    if sign == '+' || sign == '-' {
                        self.advance();
                    }
                }
                
                // 读取指数数字
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
        
        let num_str: &str = &self.input[start..self.pos];

        if has_dot || has_e {
            // 尝试解析为浮点数 f32
            match num_str.parse::<f32>() {
                Ok(f) => Token::FloatConst(f),
                Err(_) => Token::FloatConst(0.0)
            }
        } else {
            // 尝试解析为整数 i32
            match num_str.parse::<i32>() {
                Ok(i) => Token::IntConst(i),
                Err(_) => Token::IntConst(0)
            }
        }
    }

    /// 辅助方法：查看下一个字符（不移动位置）
    fn peek_next_char(&self) -> Option<char> {
        // 直接使用字节位置来获取下一个字符
        if self.pos >= self.input.len() {
            return None;
        }
        
        // 从当前位置开始获取字符迭代器，跳过第一个字符
        let remaining = &self.input[self.pos..];
        let mut chars = remaining.chars();
        chars.next(); // 跳过当前字符
        chars.next()  // 返回下一个字符
    }
}
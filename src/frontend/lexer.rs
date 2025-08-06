#[derive(Debug,PartialEq)]
pub enum Token {
    KeywordInt,
    Identifier(String),   // 如 "main"
    IntConst(i32),        // 如 0, 123
    Plus,                 // +
    Minus,                // -
    Multiply,             // *
    Divide,               // /
    LParen,               // (
    RParen,               // )
    LBrace,               // {
    RBrace,               // }
    Semicolon,            // ;
    EOF,                  // 文件结束标记
}

pub struct Lexer<'a>{
    input : &'a str,
    pos: usize
}

impl<'a> Lexer<'a>{

    pub fn new(input:&'a str) -> Self{
        Lexer { input: input, pos: 0 }
    }

    // 获取当前字符（可能为 None，只有在超出索引时）
    fn current_char(&self) -> Option<char> {
        self.input.chars().nth(self.pos)
    }

    fn advance(&mut self){
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


    pub fn next_token(&mut self) -> Token{

        self.skip_whitespace();

        let c = match self.current_char() {
            Some(ch) => ch,
            None => return Token::EOF,
        };

        //把match的结果 也就是Token作为返回值
        match c {
            // 标识符或关键字
            'a'..='z' | 'A'..='Z' | '_' => self.read_identifier_or_keyword(),

            // 数字（简化处理，只处理整数）
            '0'..='9' => self.read_number(),

            // 符号
            '+' => { self.advance(); Token::Plus }
            '-' => { self.advance(); Token::Minus }
            '*' => { self.advance(); Token::Multiply }
            '/' => { self.advance(); Token::Divide }
            '(' => { self.advance(); Token::LParen }
            ')' => { self.advance(); Token::RParen }
            '{' => { self.advance(); Token::LBrace }
            '}' => { self.advance(); Token::RBrace }
            ';' => { self.advance(); Token::Semicolon }

            // 其它字符暂时不处理，可以 panic 或扩展
            _ => {
                self.advance();
                panic!("Unexpected character: {}", c)
            }
        }

    }

    // 读取标识符或关键字
    fn read_identifier_or_keyword(&mut self) -> Token {
        let start = self.pos;
        //一个一个读取字符 直到遇到空格
        while let Some(c) = self.current_char() {
            if c.is_ascii_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }
        //把字符收集起来合成字符串
        let ident: String = self.input[start..self.pos].to_string();
        //检查是不是关键字
        match ident.as_str() {
            "int" => Token::KeywordInt,
            _ => Token::Identifier(ident),
        }
    }


    fn read_number(&mut self) -> Token {
        let start = self.pos;
        while let Some(c) = self.current_char() {
            if c.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }
        let num_str: String = self.input[start..self.pos].to_string();
        let num = num_str.parse::<i32>().unwrap_or(0); // 简单处理，解析失败默认为 0
        Token::IntConst(num)
    }






}
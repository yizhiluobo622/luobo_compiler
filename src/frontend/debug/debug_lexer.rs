use crate::frontend::lexer::{Lexer, Token};
use crate::frontend::span::{LocatedToken, Span};

/// 调试工具：重构源码
/// 将 Token 流重新组合成接近源码的格式
pub fn reconstruct_source(input: &str) -> String {
    let mut lexer = Lexer::new(input);
    let mut result = String::new();
    
    loop {
        let (tok, text) = lexer.parse_token();
        
        match tok {
            Token::EOF => {
                break;
            }
            Token::Semicolon => {
                result.push_str(&text);
                result.push('\n'); // 分号后换行
            }
            Token::LBrace => {
                result.push_str(&text);
                result.push('\n'); // 左大括号后换行
            }
            Token::RBrace => {
                result.push_str(&text);
                result.push('\n'); // 右大括号后换行
            }
            Token::LParen => {
                result.push_str(&text);
            }
            Token::RParen => {
                result.push_str(&text);
            }
            Token::LBracket => {
                result.push_str(&text);
            }
            Token::RBracket => {
                result.push_str(&text);
            }
            Token::Comma => {
                result.push_str(&text);
            }
            _ => {
                result.push_str(&text);
            }
        }
    }
    
    result
}

/// 调试工具：显示详细的 Token 信息
pub fn show_tokens(input: &str) {
    let mut lexer = Lexer::new(input);
    
    println!("=== Token Details ===");
    loop {
        let (tok, text) = lexer.parse_token();
        
        match tok {
            Token::EOF => {
                println!("EOF");
                break;
            }
            _ => {
                println!("{:?} -> '{}'", tok, text);
            }
        }
    }
}

/// 调试工具：显示带位置信息的 Token
pub fn show_located_tokens(input: &str) {
    let mut lexer = Lexer::new(input);
    
    println!("=== Located Tokens ===");
    let mut token_count = 0;
    loop {
        let located_token = lexer.next_located_token();
        token_count += 1;
        
        match &located_token.token {
            Token::EOF => {
                println!("EOF (总共解析了 {} 个 Token)", token_count);
                break;
            }
            _ => {
                let span = &located_token.span;
                println!("[{}] {:?} -> '{}' at {}:{}", 
                    token_count,
                    located_token.token, 
                    get_token_text(&located_token.token),
                    span.line, 
                    span.column
                );
            }
        }
    }
}

/// 辅助函数：获取 Token 的文本表示
fn get_token_text(token: &Token) -> String {
    match token {
        Token::Identifier(name) => name.clone(),
        Token::IntConst(value) => value.to_string(),
        Token::FloatConst(value) => value.to_string(),
        Token::Plus => "+".to_string(),
        Token::Minus => "-".to_string(),
        Token::Star => "*".to_string(),
        Token::Slash => "/".to_string(),
        Token::Percent => "%".to_string(),
        Token::Equal => "=".to_string(),
        Token::DoubleEqual => "==".to_string(),
        Token::NotEqual => "!=".to_string(),
        Token::Less => "<".to_string(),
        Token::Greater => ">".to_string(),
        Token::LessEqual => "<=".to_string(),
        Token::GreaterEqual => ">=".to_string(),
        Token::AndAnd => "&&".to_string(),
        Token::OrOr => "||".to_string(),
        Token::Bang => "!".to_string(),
        Token::LParen => "(".to_string(),
        Token::RParen => ")".to_string(),
        Token::LBrace => "{".to_string(),
        Token::RBrace => "}".to_string(),
        Token::LBracket => "[".to_string(),
        Token::RBracket => "]".to_string(),
        Token::Semicolon => ";".to_string(),
        Token::Comma => ",".to_string(),
        Token::KeywordInt => "int".to_string(),
        Token::KeywordFloat => "float".to_string(),
        Token::KeywordConst => "const".to_string(),
        Token::KeywordVoid => "void".to_string(),
        Token::KeywordIf => "if".to_string(),
        Token::KeywordElse => "else".to_string(),
        Token::KeywordWhile => "while".to_string(),
        Token::KeywordFor => "for".to_string(),
        Token::KeywordBreak => "break".to_string(),
        Token::KeywordContinue => "continue".to_string(),
        Token::KeywordReturn => "return".to_string(),
        Token::EOF => "EOF".to_string(),
    }
}

/// 调试工具：验证词法分析器
/// 重构源码并与原源码比较
pub fn verify_lexer(input: &str) {
    println!("=== Lexer Verification ===");
    println!("Original Source:");
    println!("{}", input);
    println!();
    
    let reconstructed = reconstruct_source(input);
    println!("Reconstructed Source:");
    println!("{}", reconstructed);
    println!();
    
    // 简单比较（忽略空白字符差异）
    let original_clean: String = input.chars().filter(|c| !c.is_whitespace()).collect();
    let reconstructed_clean: String = reconstructed.chars().filter(|c| !c.is_whitespace()).collect();
    
    if original_clean == reconstructed_clean {
        println!("✅ Lexer is working correctly!");
    } else {
        println!("❌ Lexer may have issues");
        println!("Original source (cleaned): {}", original_clean);
        println!("Reconstructed source (cleaned): {}", reconstructed_clean);
    }
}
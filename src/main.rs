mod frontend;
use std::fs;

// 如果你要直接使用 frontend 下的 lexer 和 parser，可以这样引入
use frontend::lexer::Lexer;  // 假如你稍后在 lexer.rs 中定义了一个 Lexer 结构体
use frontend::lexer::Token;  // 假如你定义了一个 Token 枚举

fn main() {
    println!("🦀 欢迎使用 Luobo 编译器！");

    let path = "src/target_code/sy/00_main.sy";
    let src = fs::read_to_string(path).expect("无法读取文件");
    let mut lexer = Lexer::new(&src);

    loop {
        let (tok, text) = lexer.next_token_with_text();
        println!("{:?} -> '{}'", tok, text);
        if tok == Token::EOF {
            break;
        }
    }
}
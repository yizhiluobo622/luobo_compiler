mod frontend;
// 如果你要直接使用 frontend 下的 lexer 和 parser，可以这样引入
use frontend::lexer::Lexer;  // 假如你稍后在 lexer.rs 中定义了一个 Lexer 结构体
use frontend::lexer::Token;  // 假如你定义了一个 Token 枚举

fn main() {
    println!("🦀 欢迎使用 Luobo 编译器！");

    // 示例：读取一个 SysY 源码文件，进行词法分析
    let code = "int main() { return 0; }";  // 示例 SysY 代码

    // 创建 Lexer 并测试
    let mut lexer = Lexer::new(code);

    // 循环获取 Token（待实现）
    loop {
        let tok = lexer.next_token();
        println!("{:?}", tok);

        if tok == Token::EOF {
            break;
        }
    }
}
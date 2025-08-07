mod frontend;
use std::fs;
use frontend::debug;

fn main() {
    println!("🦀 欢迎使用 Luobo 编译器！");

    let path = "src/target_code/sy/test_comments.sy";
    let src = fs::read_to_string(path).expect("无法读取文件");
    
    println!("=== Source Reconstruction ===");
    let reconstructed = debug::reconstruct_source(&src);
    println!("{}", reconstructed);
    
    debug::show_located_tokens(&src);
}
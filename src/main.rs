mod frontend;
mod passes;

use std::path::PathBuf;
use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    /// Input source file
    input: PathBuf,
    
    /// Output file
    #[arg(short, long, default_value = "a.out")]
    output: PathBuf,
    
    /// Optimization level (0-3)
    #[arg(short, long, default_value = "0")]
    optimize: u8,
    
    /// Enable debug output
    #[arg(short, long)]
    verbose: bool,
}

fn main() {
    let args = Args::parse();
    let source = std::fs::read_to_string(&args.input).expect("Cannot read file");
    
    // 简洁的线性编译流程 - 一行一个Pass
    let semantic_ast = passes::gen_sema_ast_pass(&source);
    let mlir_module = passes::mlir_pass(&semantic_ast);
    let object_code = passes::codegen_pass(&mlir_module, args.optimize);
    
    std::fs::write(&args.output, object_code).expect("Cannot write file");
    
    if args.verbose {
        println!("✅ Compilation successful");
    }
}
mod frontend;
mod passes;
mod ir;
mod utils;

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
    
    /// Target architecture
    #[arg(short, long, default_value = "x86_64")]
    target: String,
}

fn main() {
    let args = Args::parse();
    let source = std::fs::read_to_string(&args.input).expect("Cannot read file");
    
    // 简洁的线性编译流程 - 一行一个Pass
    let semantic_ast = passes::gen_sema_ast_pass(&source);
    
    // 高层IR Pass
    let high_level_ir = passes::ast_to_hir_pass(&semantic_ast);
    let high_level_ir = passes::type_inference_pass(&high_level_ir);
    let high_level_ir = passes::ssa_conversion_pass(&high_level_ir);
    let high_level_ir = passes::cfg_construction_pass(&high_level_ir);
    let high_level_ir = passes::dead_code_elimination_pass(&high_level_ir);
    let high_level_ir = passes::constant_folding_pass(&high_level_ir);
    
    // 中层IR Pass (待实现)
    // let mid_level_ir = passes::hir_to_mir_pass(&high_level_ir);
    // let mid_level_ir = passes::loop_optimization_pass(&mid_level_ir);
    // let mid_level_ir = passes::vectorization_pass(&mid_level_ir);
    // let mid_level_ir = passes::function_inlining_pass(&mid_level_ir);
    
    // 底层IR Pass (待实现)
    // let low_level_ir = passes::register_allocation_pass(&mid_level_ir);
    // let low_level_ir = passes::instruction_selection_pass(&low_level_ir);
    // let low_level_ir = passes::instruction_scheduling_pass(&low_level_ir);
    
    // 代码生成 (待实现)
    // let object_code = passes::codegen_pass(&low_level_ir, args.optimize);
    
    // 临时实现：生成简单的汇编代码
    let object_code = generate_simple_assembly(&high_level_ir, &args.target);
    
    std::fs::write(&args.output, object_code).expect("Cannot write file");
    
    if args.verbose {
        println!("✅ Compilation successful");
        println!("📁 Input: {}", args.input.display());
        println!("📁 Output: {}", args.output.display());
        println!("🎯 Target: {}", args.target);
        println!("⚡ Optimization level: {}", args.optimize);
    }
}

fn generate_simple_assembly(high_level_ir: &ir::high_level::HighLevelIR, target: &str) -> Vec<u8> {
    // 临时实现：生成简单的汇编代码
    let mut assembly = String::new();
    
    match target {
        "x86_64" => {
            assembly.push_str("section .text\n");
            assembly.push_str("global _start\n\n");
            assembly.push_str("_start:\n");
            assembly.push_str("    mov rax, 60\n");
            assembly.push_str("    mov rdi, 0\n");
            assembly.push_str("    syscall\n");
        }
        "aarch64" => {
            assembly.push_str(".text\n");
            assembly.push_str(".global _start\n\n");
            assembly.push_str("_start:\n");
            assembly.push_str("    mov x0, #0\n");
            assembly.push_str("    mov x8, #93\n");
            assembly.push_str("    svc #0\n");
        }
        _ => {
            assembly.push_str("section .text\n");
            assembly.push_str("global _start\n\n");
            assembly.push_str("_start:\n");
            assembly.push_str("    mov rax, 60\n");
            assembly.push_str("    mov rdi, 0\n");
            assembly.push_str("    syscall\n");
        }
    }
    
    assembly.into_bytes()
}
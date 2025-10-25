use std::path::PathBuf;
use clap::Parser as ClapParser;
mod frontend;
mod for_test;
mod tacir;

// 使用示例:
// 基本用法:
//   cargo run -- input.sy
//   ./target/debug/luobo_compiler input.sy
//
// 指定输出文件:
//   cargo run -- -o my_program.out input.sy
//   ./target/debug/luobo_compiler -o my_program.out input.sy
//
// 指定优化级别:
//   cargo run -- -O 2 input.sy
//   ./target/debug/luobo_compiler -O 2 input.sy
//
// 组合使用:
//   cargo run -- -o optimized_program.out -O 3 input.sy
//   ./target/debug/luobo_compiler -o optimized_program.out -O 3 input.sy
//
// 显示结构化IR示例:
//   cargo run -- --show-structured-ir input.sy
//
// 查看帮助:
//   cargo run -- --help
//   ./target/debug/luobo_compiler --help

#[derive(ClapParser, Debug)]
#[command(author, version, about)]
struct Args {
    /// Input source file
    input: Option<PathBuf>,
    
    /// Output file
    #[arg(short, long, default_value = "a.out")]
    output: PathBuf,
    
    /// Optimization level (0-3)
    #[arg(short = 'O', long, default_value = "0")]
    optimize: u8,
    
    /// Show structured IR example
    #[arg(long)]
    show_structured_ir: bool,
}

fn main() {
    let args = Args::parse();
    
    if args.show_structured_ir {
        show_structured_ir_example();
        return;
    }
    
    // 检查是否提供了输入文件
    let input_path = match &args.input {
        Some(path) => path,
        None => {
            eprintln!("❌ 错误: 请提供输入源文件");
            eprintln!("使用 --help 查看使用方法");
            std::process::exit(1);
        }
    };
    
    // 执行完整的编译流程
    match compile(input_path, &args) {
        Ok(()) => {
            println!("✅ 编译完成: {} -> {}", input_path.display(), args.output.display());
        }
        Err(e) => {
            eprintln!("❌ 编译失败: {}", e);
            std::process::exit(1);
        }
    }
}

/// 完整的编译流程
fn compile(input_path: &std::path::Path, args: &Args) -> Result<(), String> {
    println!("🚀 开始编译: {}", input_path.display());
    
    // 1. 读取源文件
    let source_content = std::fs::read_to_string(input_path)
        .map_err(|e| format!("无法读取源文件 '{}': {}", input_path.display(), e))?;
    
    println!("📖 源文件读取完成，长度: {} 字节", source_content.len());
    
    // 2. 词法分析和语法分析
    println!("🔍 开始词法分析...");
    let lexer = frontend::lexer::Lexer::new(&source_content);
    println!("✅ 词法分析完成");
    
    // 3. 语法分析
    println!("🌳 开始语法分析...");
    let mut parser = frontend::parser::Parser::new(lexer);
    let ast = parser.parse()
        .map_err(|e| format!("语法分析失败: {:?}", e))?;
    println!("✅ 语法分析完成，生成AST");
    
    // 4. 语义分析
    println!("🔍 开始语义分析...");
    let semantic_ast = frontend::semantic_analysis::analyze_ast_with_semantic_info(ast)
        .map_err(|e| format!("语义分析失败: {:?}", e))?;
    println!("✅ 语义分析完成");
    
    // 5. TAC IR生成
    println!("🔧 开始TAC IR生成...");
    let mut tac_program = tacir::convert_ast_to_tac(&semantic_ast)
        .map_err(|e| format!("TAC IR生成失败: {:?}", e))?;
    println!("✅ TAC IR生成完成");
    
    // 6. 优化处理
    if args.optimize > 0 {
        println!("⚡ 开始优化处理 (级别: {})...", args.optimize);
        run_optimizations(&mut tac_program, args.optimize)?;
        println!("✅ 优化处理完成");
    } else {
        println!("⏭️ 跳过优化处理 (优化级别为0)");
    }
    
    // 7. 代码生成 (暂时输出TAC IR到文件)
    println!("📝 开始代码生成...");
    generate_output(&tac_program, &args.output)?;
    println!("✅ 代码生成完成");
    
    Ok(())
}

/// 运行优化处理
fn run_optimizations(program: &mut tacir::TACProgram, level: u8) -> Result<(), String> {
    match level {
        0 => {
            // 无优化
        }
        1 => {
            // 基本优化：常量折叠
            println!("🔧 运行基本优化...");
            let mut constant_pass = tacir::TAC_opt::ConstantOptimizationPass::new();
            constant_pass.run(program)?;
        }
        2 => {
            // 中等优化：常量 + 代数
            println!("🔧 运行中等优化...");
            let mut constant_pass = tacir::TAC_opt::ConstantOptimizationPass::new();
            constant_pass.run(program)?;
            
            let mut algebraic_pass = tacir::TAC_opt::AlgebraicOptimizationPass::new();
            algebraic_pass.run(program)?;
        }
        3 | _ => {
            // 完整优化：使用已有的优化管道
            println!("🔧 运行完整优化...");
            tacir::TAC_opt::run_all_optimizations(program)?;
        }
    }
    
    Ok(())
}

/// 生成输出代码
fn generate_output(program: &tacir::TACProgram, output_path: &std::path::Path) -> Result<(), String> {
    // 暂时输出TAC IR格式，未来可扩展为LLVM IR或汇编
    let tac_output = format!("{}", program);
    
    std::fs::write(output_path, tac_output)
        .map_err(|e| format!("无法写入输出文件 '{}': {}", output_path.display(), e))?;
    
    Ok(())
}

/// 显示结构化IR示例
fn show_structured_ir_example() {
    println!("📊 TAC IR 结构化示例:");
    println!("================================");
    println!("Global Variables:");
    println!("  n: IntType = 10");
    println!();
    println!("Functions:");
    println!("  Function 0: main");
    println!("  Return Type: IntType");
    println!("  Parameters: []");
    println!("  Basic Blocks:");
    println!("    Block 0:");
    println!("      0: t0 = call getint()");
    println!("      1: n = t0");
    println!("      2: t1 = call putint(n)");
    println!("      3: return 0");
    println!("================================");
}


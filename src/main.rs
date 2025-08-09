mod frontend;


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
    
    /// Show structured IR example
    #[arg(long)]
    show_structured_ir: bool,
}

fn main() {
    let args = Args::parse();
    
    if args.show_structured_ir {
       
        return;
    }
    
   
}
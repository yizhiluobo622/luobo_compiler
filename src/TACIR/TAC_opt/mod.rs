pub mod constant_opt;
pub mod algebraic_opt;
pub mod inline;
pub mod utils;

pub use constant_opt::*;
pub use algebraic_opt::*;
pub use inline::*;
pub use utils::*;

/// 优化Pass trait
pub trait OptimizationPass {
    /// 运行优化
    fn run(&mut self, program: &mut crate::TACIR::TACProgram) -> Result<OptimizationResult, String>;
    
    /// 获取Pass名称
    fn name(&self) -> &str;
    
    /// 获取优化统计
    fn get_stats(&self) -> &OptimizationStats;
}

/// 优化结果
#[derive(Debug, Clone)]
pub struct OptimizationResult {
    /// 是否进行了优化
    pub optimized: bool,
    /// 优化的节点数量
    pub nodes_optimized: usize,
    /// 优化的指令数量
    pub instructions_optimized: usize,
}

impl OptimizationResult {
    pub fn new() -> Self {
        Self {
            optimized: false,
            nodes_optimized: 0,
            instructions_optimized: 0,
        }
    }
    
    pub fn mark_optimized(&mut self) {
        self.optimized = true;
    }
}

/// 优化统计信息
#[derive(Debug, Clone)]
pub struct OptimizationStats {
    /// 优化轮次
    pub optimization_rounds: usize,
    /// 常量折叠次数
    pub constant_foldings: usize,
    /// 代数优化次数
    pub algebraic_optimizations: usize,
    /// 控制流优化次数
    pub control_flow_optimizations: usize,
    /// 死代码消除次数
    pub dead_code_eliminations: usize,
}

impl OptimizationStats {
    pub fn new() -> Self {
        Self {
            optimization_rounds: 0,
            constant_foldings: 0,
            algebraic_optimizations: 0,
            control_flow_optimizations: 0,
            dead_code_eliminations: 0,
        }
    }
}

/// 运行所有优化Pass
pub fn run_all_optimizations(program: &mut crate::TACIR::TACProgram) -> Result<Vec<OptimizationResult>, String> {
    let mut results = Vec::new();
    
    // 运行内联优化（最先执行）
    println!("=== 开始内联优化 ===");
    let mut inline_pass = inline::InlineOptimizationPass::new();
    let inline_result = inline_pass.run(program)?;
    results.push(inline_result);
    
    // 运行常量优化
    println!("=== 开始常量优化 ===");
    let mut constant_pass = constant_opt::ConstantOptimizationPass::new();
    let constant_result = constant_pass.run(program)?;
    results.push(constant_result);
    
    // 运行代数优化
    // let mut algebraic_pass = algebraic_opt::AlgebraicOptimizationPass::new();
    // let algebraic_result = algebraic_pass.run(program)?;
    // results.push(algebraic_result);
    

    
    Ok(results)
}

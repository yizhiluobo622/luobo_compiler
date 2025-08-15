pub mod constant_propagation;

/// 优化Pass的通用接口
pub trait OptimizationPass {
    /// 执行优化
    fn run(&mut self, son_ir: &mut crate::ast_to_cfg::ast_to_SoNir::son_ir::SonIr) -> Result<OptimizationResult, String>;
    
    /// 获取Pass名称
    fn name(&self) -> &str;
    
    /// 获取优化统计信息
    fn get_stats(&self) -> &OptimizationStats;
}

/// 优化结果
#[derive(Debug, Clone)]
pub struct OptimizationResult {
    /// 是否发生了优化
    pub optimized: bool,
    /// 优化的节点数量
    pub nodes_optimized: usize,
    /// 删除的节点数量
    pub nodes_deleted: usize,
    /// 新增的节点数量
    pub nodes_added: usize,
}

/// 优化统计信息
#[derive(Debug, Clone, Default)]
pub struct OptimizationStats {
    /// 常量传播次数
    pub constant_propagations: usize,
    /// 常量折叠次数
    pub constant_foldings: usize,
    /// 死代码消除次数
    pub dead_code_eliminations: usize,
    /// 优化轮次
    pub optimization_rounds: usize,
    /// 删除的单挂节点数量
    pub dangling_nodes_removed: usize,
}

impl OptimizationResult {
    /// 创建新的优化结果
    pub fn new() -> Self {
        Self {
            optimized: false,
            nodes_optimized: 0,
            nodes_deleted: 0,
            nodes_added: 0,
        }
    }
    
    /// 标记发生了优化
    pub fn mark_optimized(&mut self) {
        self.optimized = true;
    }
    
    /// 增加优化的节点数量
    pub fn increment_nodes_optimized(&mut self) {
        self.nodes_optimized += 1;
    }
    
    /// 增加删除的节点数量
    pub fn increment_nodes_deleted(&mut self) {
        self.nodes_deleted += 1;
    }
    
    /// 增加新增的节点数量
    pub fn increment_nodes_added(&mut self) {
        self.nodes_added += 1;
    }
}

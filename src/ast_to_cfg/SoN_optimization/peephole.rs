// Peephole optimization pass for SoN IR
// This pass implements the optimizations described in Chapter 3
// 参照 Simple-Rust 的实现设计

use crate::ast_to_cfg::ast_to_SoNir::son_ir::{
    SonIr, SonNode, SonNodeId, SonNodeKind, OpCode, NodeData, ConstantValue, EdgeType, SonEdge
};
use crate::frontend::ast::Type as AstType;
use std::collections::{HashMap, HashSet, VecDeque};

pub struct PeepholeOptimizer {
    /// 工作列表：需要重新优化的节点
    worklist: VecDeque<SonNodeId>,
    /// 已处理的节点：避免重复处理
    processed: HashSet<SonNodeId>,
    /// 统计信息
    stats: PeepholeStats,
    /// 优化配置
    config: PeepholeConfig,
}

#[derive(Debug, Default)]
pub struct PeepholeStats {
    pub nodes_processed: usize,
    pub optimizations_applied: usize,
    pub nodes_replaced: usize,
    pub dead_nodes_removed: usize,
    pub constant_foldings: usize,
    pub strength_reductions: usize,
    pub iterations: usize,
}

#[derive(Debug, Clone)]
pub struct PeepholeConfig {
    pub enable_constant_folding: bool,
    pub enable_strength_reduction: bool,
    pub enable_dead_code_elimination: bool,
    pub enable_identity_elimination: bool,
    pub max_iterations: usize,
    pub enable_debug: bool,
}

impl Default for PeepholeConfig {
    fn default() -> Self {
        Self {
            enable_constant_folding: true,
            enable_strength_reduction: true,
            enable_dead_code_elimination: true,
            enable_identity_elimination: true,
            max_iterations: 10,
            enable_debug: false,
        }
    }
}

impl PeepholeOptimizer {
    pub fn new() -> Self {
        Self {
            worklist: VecDeque::new(),
            processed: HashSet::new(),
            stats: PeepholeStats::default(),
            config: PeepholeConfig::default(),
        }
    }
    
    pub fn with_config(config: PeepholeConfig) -> Self {
        Self {
            worklist: VecDeque::new(),
            processed: HashSet::new(),
            stats: PeepholeStats::default(),
            config,
        }
    }
    
    /// 运行 peephole 优化
    pub fn run(&mut self, son_ir: &mut SonIr) {
        self.stats = PeepholeStats::default();
        
        if self.config.enable_debug {
            println!("Starting Peephole Optimization");
        }
        
        // 初始化工作列表：所有数据流节点
        self.initialize_worklist(son_ir);
        
        let mut iteration = 0;
        while !self.worklist.is_empty() && iteration < self.config.max_iterations {
            iteration += 1;
            
            if self.config.enable_debug {
                println!("  Peephole iteration {}", iteration);
            }
            
            // 处理工作列表中的所有节点
            let current_worklist: Vec<SonNodeId> = self.worklist.drain(..).collect();
            for node_id in current_worklist {
                if self.processed.contains(&node_id) {
                    continue;
                }
                
                self.stats.nodes_processed += 1;
                self.processed.insert(node_id);
                
                // 尝试优化节点
                if let Some(replacement_id) = self.try_peephole_opt(node_id, son_ir) {
                    if replacement_id != node_id {
                        self.stats.nodes_replaced += 1;
                        // 将相关节点加入工作列表
                        self.add_users_to_worklist(node_id, son_ir);
                    }
                }
            }
        }
        
        self.stats.iterations = iteration;
        
        if self.config.enable_debug {
            println!("Peephole optimization completed in {} iterations", iteration);
            println!("Stats: {:?}", self.stats);
        }
    }
    
    /// 初始化工作列表
    fn initialize_worklist(&mut self, son_ir: &SonIr) {
        for (node_id, node) in son_ir.get_all_nodes() {
            if node.is_data_flow() && !node.is_dead() {
                self.worklist.push_back(*node_id);
            }
        }
    }
    
    /// 尝试 peephole 优化
    fn try_peephole_opt(&mut self, node_id: SonNodeId, son_ir: &mut SonIr) -> Option<SonNodeId> {
        let node = son_ir.get_node(node_id)?;
        
        // 常量折叠
        if self.config.enable_constant_folding {
            if let Some(constant_id) = self.try_constant_fold(node_id, son_ir) {
                self.stats.constant_foldings += 1;
                return Some(constant_id);
            }
        }
        
        // 强度削弱
        if self.config.enable_strength_reduction {
            if let Some(replacement_id) = self.try_strength_reduction(node_id, son_ir) {
                self.stats.strength_reductions += 1;
                return Some(replacement_id);
            }
        }
        
        // 恒等消除
        if self.config.enable_identity_elimination {
            if let Some(replacement_id) = self.try_identity_elimination(node_id, son_ir) {
                return Some(replacement_id);
            }
        }
        
        // 死代码消除
        if self.config.enable_dead_code_elimination {
            if self.is_dead_code(node_id, son_ir) {
                self.stats.dead_nodes_removed += 1;
                // 标记为死亡，稍后清理
                if let Some(node) = son_ir.get_node_mut(node_id) {
                    node.mark_dead();
                }
            }
        }
        
        None
    }
    
    /// 尝试常量折叠
    fn try_constant_fold(&self, node_id: SonNodeId, son_ir: &mut SonIr) -> Option<SonNodeId> {
        let node = son_ir.get_node(node_id)?;
        
        match &node.kind.data {
            NodeData::BinaryOp { left, right } => {
                if let (Some(left_id), Some(right_id)) = (left, right) {
                    if let (Some(left_const), Some(right_const)) = (
                        self.get_constant_value(*left_id, son_ir),
                        self.get_constant_value(*right_id, son_ir)
                    ) {
                        if let Some(result) = self.fold_binary_operation(&node.kind.opcode, &left_const, &right_const) {
                            return self.create_constant_node(result, son_ir);
                        }
                    }
                }
            }
            NodeData::UnaryOp { operand } => {
                if let Some(operand_id) = operand {
                    if let Some(operand_const) = self.get_constant_value(*operand_id, son_ir) {
                        if let Some(result) = self.fold_unary_operation(&node.kind.opcode, &operand_const) {
                            return self.create_constant_node(result, son_ir);
                        }
                    }
                }
            }
            _ => {}
        }
        
        None
    }
    
    /// 尝试强度削弱
    fn try_strength_reduction(&self, node_id: SonNodeId, son_ir: &mut SonIr) -> Option<SonNodeId> {
        let node = son_ir.get_node(node_id)?;
        
        match &node.kind.data {
            NodeData::BinaryOp { left, right } => {
                match node.kind.opcode {
                    OpCode::Multiply => {
                        // x * 2^n -> x << n
                        if let Some(right_id) = right {
                            if let Some(power) = self.is_power_of_two(*right_id, son_ir) {
                                return self.create_shift_left_node(*left, power, son_ir);
                            }
                        }
                    }
                    OpCode::Divide => {
                        // x / 2^n -> x >> n (对于无符号整数)
                        if let Some(right_id) = right {
                            if let Some(power) = self.is_power_of_two(*right_id, son_ir) {
                                return self.create_shift_right_node(*left, power, son_ir);
                            }
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        
        None
    }
    
    /// 尝试恒等消除
    fn try_identity_elimination(&self, node_id: SonNodeId, son_ir: &mut SonIr) -> Option<SonNodeId> {
        let node = son_ir.get_node(node_id)?;
        
        match &node.kind.data {
            NodeData::BinaryOp { left, right } => {
                match node.kind.opcode {
                    OpCode::Add => {
                        // x + 0 -> x
                        if let Some(right_id) = right {
                            if self.is_zero(*right_id, son_ir) {
                                return *left;
                            }
                        }
                        // 0 + x -> x
                        if let Some(left_id) = left {
                            if self.is_zero(*left_id, son_ir) {
                                return *right;
                            }
                        }
                    }
                    OpCode::Multiply => {
                        // x * 1 -> x
                        if let Some(right_id) = right {
                            if self.is_one(*right_id, son_ir) {
                                return *left;
                            }
                        }
                        // 1 * x -> x
                        if let Some(left_id) = left {
                            if self.is_one(*left_id, son_ir) {
                                return *right;
                            }
                        }
                    }
                    _ => {}
                }
            }
            NodeData::UnaryOp { operand } => {
                match node.kind.opcode {
                    OpCode::Minus => {
                        // -(-x) -> x
                        if let Some(operand_id) = operand {
                            if let Some(operand_node) = son_ir.get_node(*operand_id) {
                                if matches!(operand_node.kind.opcode, OpCode::Minus) {
                                    if let NodeData::UnaryOp { operand: inner } = &operand_node.kind.data {
                                        if let Some(inner_id) = inner {
                                            return Some(*inner_id);
                                        }
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        
        None
    }
    
    /// 检查是否为死代码
    fn is_dead_code(&self, node_id: SonNodeId, son_ir: &SonIr) -> bool {
        if let Some(node) = son_ir.get_node(node_id) {
            // 检查是否有输出边
            if !node.outputs.is_empty() {
                return false;
            }
            
            // 检查是否为控制流节点
            if node.is_control_flow() {
                return false;
            }
            
            // 检查是否被标记为重要节点
            if node.is_keep() {
                return false;
            }
            
            true
        } else {
            false
        }
    }
    
    /// 获取常量值
    fn get_constant_value(&self, node_id: SonNodeId, son_ir: &SonIr) -> Option<ConstantValue> {
        let node = son_ir.get_node(node_id)?;
        
        if let NodeData::Constant { value, .. } = &node.kind.data {
            Some(value.clone())
        } else {
            None
        }
    }
    
    /// 折叠二元运算
    fn fold_binary_operation(&self, opcode: &OpCode, left: &ConstantValue, right: &ConstantValue) -> Option<ConstantValue> {
        match (opcode, left, right) {
            (OpCode::Add, ConstantValue::Integer(a), ConstantValue::Integer(b)) => {
                Some(ConstantValue::Integer(a + b))
            }
            (OpCode::Subtract, ConstantValue::Integer(a), ConstantValue::Integer(b)) => {
                Some(ConstantValue::Integer(a - b))
            }
            (OpCode::Multiply, ConstantValue::Integer(a), ConstantValue::Integer(b)) => {
                Some(ConstantValue::Integer(a * b))
            }
            (OpCode::Divide, ConstantValue::Integer(a), ConstantValue::Integer(b)) => {
                if *b != 0 {
                    Some(ConstantValue::Integer(a / b))
                } else {
                    None
                }
            }
            (OpCode::Modulo, ConstantValue::Integer(a), ConstantValue::Integer(b)) => {
                if *b != 0 {
                    Some(ConstantValue::Integer(a % b))
                } else {
                    None
                }
            }
            (OpCode::Equal, a, b) => {
                Some(ConstantValue::Boolean(a == b))
            }
            (OpCode::NotEqual, a, b) => {
                Some(ConstantValue::Boolean(a != b))
            }
            (OpCode::LessThan, ConstantValue::Integer(a), ConstantValue::Integer(b)) => {
                Some(ConstantValue::Boolean(a < b))
            }
            (OpCode::LessEqual, ConstantValue::Integer(a), ConstantValue::Integer(b)) => {
                Some(ConstantValue::Boolean(a <= b))
            }
            (OpCode::GreaterThan, ConstantValue::Integer(a), ConstantValue::Integer(b)) => {
                Some(ConstantValue::Boolean(a > b))
            }
            (OpCode::GreaterEqual, ConstantValue::Integer(a), ConstantValue::Integer(b)) => {
                Some(ConstantValue::Boolean(a >= b))
            }
            _ => None
        }
    }
    
    /// 折叠一元运算
    fn fold_unary_operation(&self, opcode: &OpCode, operand: &ConstantValue) -> Option<ConstantValue> {
        match (opcode, operand) {
            (OpCode::Minus, ConstantValue::Integer(a)) => {
                Some(ConstantValue::Integer(-a))
            }
            (OpCode::LogicalNot, ConstantValue::Boolean(a)) => {
                Some(ConstantValue::Boolean(!a))
            }
            _ => None
        }
    }
    
    /// 检查是否为 2 的幂
    fn is_power_of_two(&self, node_id: SonNodeId, son_ir: &SonIr) -> Option<u32> {
        if let Some(ConstantValue::Integer(value)) = self.get_constant_value(node_id, son_ir) {
            if value > 0 && (value & (value - 1)) == 0 {
                Some(value.trailing_zeros())
            } else {
                None
            }
        } else {
            None
        }
    }
    
    /// 检查是否为零
    fn is_zero(&self, node_id: SonNodeId, son_ir: &SonIr) -> bool {
        if let Some(ConstantValue::Integer(value)) = self.get_constant_value(node_id, son_ir) {
            value == 0
        } else {
            false
        }
    }
    
    /// 检查是否为一
    fn is_one(&self, node_id: SonNodeId, son_ir: &SonIr) -> bool {
        if let Some(ConstantValue::Integer(value)) = self.get_constant_value(node_id, son_ir) {
            value == 1
        } else {
            false
        }
    }
    
    /// 创建常量节点
    fn create_constant_node(&self, value: ConstantValue, son_ir: &mut SonIr) -> Option<SonNodeId> {
        let typ =         match &value {
            ConstantValue::Integer(_) => AstType::IntType,
            ConstantValue::Float(_) => AstType::FloatType,
            ConstantValue::Boolean(_) => AstType::BoolType,
            ConstantValue::String(_) => AstType::CharType, // 使用 CharType 替代 StringType
        };
        
        let constant_node = SonNode::new(
            son_ir.node_count(),
            SonNodeKind::with_data(
                OpCode::Constant,
                NodeData::Constant { value, typ }
            )
        );
        
        Some(son_ir.add_node(constant_node))
    }
    
    /// 创建左移节点
    fn create_shift_left_node(&self, operand: Option<SonNodeId>, shift: u32, son_ir: &mut SonIr) -> Option<SonNodeId> {
        let shift_node = SonNode::new(
            son_ir.node_count(),
            SonNodeKind::with_data(
                OpCode::ShiftLeft,
                NodeData::BinaryOp { left: operand, right: None }
            )
        );
        
        let shift_node_id = son_ir.add_node(shift_node);
        
        // 创建移位常量
        let shift_const = self.create_constant_node(ConstantValue::Integer(shift as i64), son_ir)?;
        
        // 连接操作数
        if let Some(operand_id) = operand {
            son_ir.add_edge(SonEdge::new(operand_id, shift_node_id, EdgeType::Data));
        }
        son_ir.add_edge(SonEdge::new(shift_const, shift_node_id, EdgeType::Data));
        
        Some(shift_node_id)
    }
    
    /// 创建右移节点
    fn create_shift_right_node(&self, operand: Option<SonNodeId>, shift: u32, son_ir: &mut SonIr) -> Option<SonNodeId> {
        let shift_node = SonNode::new(
            son_ir.node_count(),
            SonNodeKind::with_data(
                OpCode::ShiftRight,
                NodeData::BinaryOp { left: operand, right: None }
            )
        );
        
        let shift_node_id = son_ir.add_node(shift_node);
        
        // 创建移位常量
        let shift_const = self.create_constant_node(ConstantValue::Integer(shift as i64), son_ir)?;
        
        // 连接操作数
        if let Some(operand_id) = operand {
            son_ir.add_edge(SonEdge::new(operand_id, shift_node_id, EdgeType::Data));
        }
        son_ir.add_edge(SonEdge::new(shift_const, shift_node_id, EdgeType::Data));
        
        Some(shift_node_id)
    }
    
    /// 添加用户到工作列表
    fn add_users_to_worklist(&mut self, node_id: SonNodeId, son_ir: &SonIr) {
        if let Some(node) = son_ir.get_node(node_id) {
            for &output_id in &node.outputs {
                if !self.processed.contains(&output_id) {
                    self.worklist.push_back(output_id);
                }
            }
        }
    }
    
    /// 获取统计信息
    pub fn get_stats(&self) -> &PeepholeStats {
        &self.stats
    }
    
    /// 重置优化器
    pub fn reset(&mut self) {
        self.worklist.clear();
        self.processed.clear();
        self.stats = PeepholeStats::default();
    }
    
    /// 设置配置
    pub fn set_config(&mut self, config: PeepholeConfig) {
        self.config = config;
    }
}

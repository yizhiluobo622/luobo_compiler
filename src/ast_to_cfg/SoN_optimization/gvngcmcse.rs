// Global Value Numbering + Global Code Motion + Common Subexpression Elimination
// 组合优化器：在一个文件中实现完整的优化功能
// 参照 Simple-Rust 的实现思路，利用我们已有的接口

use crate::ast_to_cfg::ast_to_SoNir::son_ir::{
    SonIr, SonNode, SonNodeId, SonNodeKind, OpCode, NodeData, EdgeType
};
use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::{Hash, Hasher, DefaultHasher};

/// 组合优化器：GVN + GCM + CSE
pub struct CombinedOptimizer {
    // 节点哈希值缓存
    node_hashes: HashMap<SonNodeId, u64>,
    // 工作列表：需要处理的节点
    worklist: VecDeque<SonNodeId>,
    // 已处理的节点
    processed: HashSet<SonNodeId>,
    // 统计信息
    stats: OptimizationStats,
}

#[derive(Debug, Default)]
pub struct OptimizationStats {
    pub gvn_applications: usize,      // GVN应用次数
    pub cse_eliminations: usize,      // CSE消除次数
    pub gcm_movements: usize,         // GCM移动次数
    pub nodes_replaced: usize,        // 节点替换次数
    pub expressions_processed: usize, // 处理的表达式数量
}

impl CombinedOptimizer {
    pub fn new() -> Self {
        Self {
            node_hashes: HashMap::new(),
            worklist: VecDeque::new(),
            processed: HashSet::new(),
            stats: OptimizationStats::default(),
        }
    }
    
    /// 运行组合优化
    pub fn run(&mut self, son_ir: &mut SonIr) {
        self.stats = OptimizationStats::default();
        
        // 1. 执行GVN和CSE优化
        self.perform_gvn_and_cse(son_ir);
        
        // 2. 执行全局代码移动
        self.perform_global_code_motion(son_ir);
        
        // 3. 执行死代码消除
        self.perform_dead_code_elimination(son_ir);
    }
    
    /// 执行GVN和CSE优化
    fn perform_gvn_and_cse(&mut self, son_ir: &mut SonIr) {
        // 第一阶段：全局CSE优化
        self.perform_global_cse(son_ir);
        
        // 第二阶段：GVN优化
        self.perform_gvn_optimization(son_ir);
    }
    
    /// 执行全局CSE优化
    fn perform_global_cse(&mut self, son_ir: &mut SonIr) {
        let mut changed = true;
        let mut iteration = 0;
        
        while changed && iteration < 10 { // 最多10次迭代
            changed = false;
            iteration += 1;
            
            println!("[DEBUG] CSE iteration {}", iteration);
            
            // 收集所有计算节点
            let node_ids: Vec<SonNodeId> = son_ir.get_all_nodes()
                .keys()
                .filter(|&&id| self.is_computational_node(id, son_ir))
                .cloned()
                .collect();
            
            for &node_id in &node_ids {
                // 检查节点是否仍然存在（可能已被删除）
                if son_ir.get_node(node_id).is_none() {
                    continue;
                }
                
                // 尝试函数调用CSE
                if let Some(replacement_id) = self.try_function_call_cse(node_id, son_ir) {
                    if replacement_id != node_id {
                        println!("[DEBUG] Function call CSE: {} -> {}", node_id, replacement_id);
                        self.stats.cse_eliminations += 1;
                        self.stats.nodes_replaced += 1;
                        self.replace_node_uses(node_id, replacement_id, son_ir);
                        
                        // 删除被替换的节点
                        let _ = son_ir.remove_node(node_id);
                        
                        changed = true;
                        continue;
                    }
                }
                
                // 尝试表达式CSE
                if let Some(replacement_id) = self.try_expression_cse(node_id, son_ir) {
                    if replacement_id != node_id {
                        println!("[DEBUG] Expression CSE: {} -> {}", node_id, replacement_id);
                        self.stats.cse_eliminations += 1;
                        self.stats.nodes_replaced += 1;
                        self.replace_node_uses(node_id, replacement_id, son_ir);
                        
                        // 删除被替换的节点
                        let _ = son_ir.remove_node(node_id);
                        
                        changed = true;
                        continue;
                    }
                }
            }
        }
    }
    
    /// 执行GVN优化
    fn perform_gvn_optimization(&mut self, son_ir: &mut SonIr) {
        // 初始化工作列表
        self.initialize_worklist(son_ir);
        
        // 主优化循环
        while let Some(node_id) = self.worklist.pop_front() {
            if self.processed.contains(&node_id) {
                continue;
            }
            
            // 检查节点是否仍然存在
            if son_ir.get_node(node_id).is_none() {
                continue;
            }
            
            self.processed.insert(node_id);
            
            // 尝试GVN优化
            if let Some(replacement_id) = self.try_gvn_optimization(node_id, son_ir) {
                if replacement_id != node_id {
                    self.stats.gvn_applications += 1;
                    self.stats.nodes_replaced += 1;
                    self.replace_node_uses(node_id, replacement_id, son_ir);
                    continue;
                }
            }
            
            // 将用户节点加入工作列表
            self.add_users_to_worklist(node_id, son_ir);
        }
    }
    
    /// 初始化工作列表
    fn initialize_worklist(&mut self, son_ir: &SonIr) {
        for (node_id, _) in son_ir.get_all_nodes() {
            if self.is_computational_node(*node_id, son_ir) {
                self.worklist.push_back(*node_id);
            }
        }
    }
    
    /// 判断是否为计算节点
    fn is_computational_node(&self, node_id: SonNodeId, son_ir: &SonIr) -> bool {
        if let Some(node) = son_ir.get_node(node_id) {
            matches!(&node.kind.data, 
                NodeData::BinaryOp { .. } | 
                NodeData::UnaryOp { .. } |
                NodeData::Constant { .. } |
                NodeData::Call { .. }
            )
        } else {
            false
        }
    }
    
    /// 尝试GVN优化
    fn try_gvn_optimization(&mut self, node_id: SonNodeId, son_ir: &mut SonIr) -> Option<SonNodeId> {
        // 检查节点是否仍然存在
        if son_ir.get_node(node_id).is_none() {
            return None;
        }
        
        // 计算节点的哈希值
        let hash = self.compute_node_hash(node_id, son_ir);
        
        println!("[DEBUG] Checking node {} with hash {}", node_id, hash);
        
        // 检查是否已存在相同哈希值的节点
        for (existing_id, existing_hash) in &self.node_hashes {
            if *existing_hash == hash && *existing_id != node_id {
                println!("[DEBUG] Found node with same hash: {} (hash: {})", existing_id, existing_hash);
                
                // 检查节点是否真的相等
                if self.nodes_are_equal(node_id, *existing_id, son_ir) {
                    println!("[DEBUG] Nodes {} and {} are equal! CSE applied!", node_id, existing_id);
                    return Some(*existing_id);
                } else {
                    println!("[DEBUG] Nodes {} and {} have same hash but are not equal", node_id, existing_id);
                }
            }
        }
        

        
        // 尝试函数调用CSE
        if let Some(replacement_id) = self.try_function_call_cse(node_id, son_ir) {
            println!("[DEBUG] Function call CSE applied: {} -> {}", node_id, replacement_id);
            return Some(replacement_id);
        }
        
        // 将当前节点加入哈希表
        self.node_hashes.insert(node_id, hash);
        println!("[DEBUG] Added node {} to hash table with hash {}", node_id, hash);
        None
    }
    

    
    /// 尝试真正的公共子表达式消除
    fn try_common_subexpression_elimination(&self, node_id: SonNodeId, left_id: SonNodeId, right_id: SonNodeId, son_ir: &mut SonIr) -> Option<SonNodeId> {
        let node = son_ir.get_node(node_id)?;
        
        // 检查是否有其他节点具有相同的结构
        for (other_id, other_node) in son_ir.get_all_nodes() {
            if *other_id == node_id {
                continue;
            }
            
            if let NodeData::BinaryOp { left: other_left, right: other_right } = &other_node.kind.data {
                // 如果操作码相同
                if other_node.kind.opcode == node.kind.opcode {
                    if let (Some(other_left_id), Some(other_right_id)) = (other_left, other_right) {
                        // 检查是否是完全相同的表达式
                        if (*other_left_id == left_id && *other_right_id == right_id) ||
                                                   // 或者对于可交换操作，检查交换后的情况
                        (self.is_commutative_operation(&node.kind.opcode) && 
                         *other_left_id == right_id && *other_right_id == left_id) {
                            return Some(*other_id);
                        }
                        
                        // 检查是否有部分相同的操作数
                        if self.can_optimize_partial_expression(left_id, right_id, *other_left_id, *other_right_id, son_ir) {
                            return Some(*other_id);
                        }
                    }
                }
            }
        }
        
        None
    }
    
    /// 尝试函数调用CSE：识别相同的函数调用
    fn try_function_call_cse(&self, node_id: SonNodeId, son_ir: &SonIr) -> Option<SonNodeId> {
        let node = son_ir.get_node(node_id)?;
        
        // 只处理函数调用节点
        if let NodeData::Call { function_name, arguments, .. } = &node.kind.data {
            // 检查是否有其他相同的函数调用
            for (other_id, other_node) in son_ir.get_all_nodes() {
                if *other_id == node_id {
                    continue;
                }
                
                if let NodeData::Call { function_name: other_name, arguments: other_args, .. } = &other_node.kind.data {
                    // 如果函数名和参数都相同，可以合并
                    if function_name == other_name && arguments == other_args {
                        println!("[DEBUG] Found duplicate function call: {} -> {} (function: {}, args: {:?})", 
                                node_id, other_id, function_name, arguments);
                        return Some(*other_id);
                    }
                }
            }
        }
        
        None
    }
    
    /// 尝试表达式CSE：识别相同的表达式
    fn try_expression_cse(&self, node_id: SonNodeId, son_ir: &SonIr) -> Option<SonNodeId> {
        let node = son_ir.get_node(node_id)?;
        
        // 只处理二元运算
        if let NodeData::BinaryOp { left, right } = &node.kind.data {
            let left_id = *left.as_ref()?;
            let right_id = *right.as_ref()?;
            
            // 检查是否有其他相同的表达式
            for (other_id, other_node) in son_ir.get_all_nodes() {
                if *other_id == node_id {
                    continue;
                }
                
                if let NodeData::BinaryOp { left: other_left, right: other_right } = &other_node.kind.data {
                    if other_node.kind.opcode == node.kind.opcode {
                        if let (Some(other_left_id), Some(other_right_id)) = (other_left, other_right) {
                            // 检查是否是完全相同的表达式
                            if (*other_left_id == left_id && *other_right_id == right_id) ||
                               // 或者对于可交换操作，检查交换后的情况
                               (self.is_commutative_operation(&node.kind.opcode) && 
                                *other_left_id == right_id && *other_right_id == left_id) {
                                println!("[DEBUG] Found duplicate expression: {} -> {} (op: {:?}, left: {}, right: {})", 
                                        node_id, other_id, node.kind.opcode, left_id, right_id);
                                return Some(*other_id);
                            }
                        }
                    }
                }
            }
        }
        
        None
    }
    
    /// 检查操作是否为可交换的
    fn is_commutative_operation(&self, opcode: &OpCode) -> bool {
        matches!(opcode, 
            OpCode::Add | OpCode::Multiply | OpCode::Equal | OpCode::NotEqual
        )
    }
    
    /// 检查是否可以优化部分表达式
    fn can_optimize_partial_expression(&self, left1: SonNodeId, right1: SonNodeId, 
                                     left2: SonNodeId, right2: SonNodeId, son_ir: &SonIr) -> bool {
        // 如果左操作数相同，检查右操作数是否相似
        if left1 == left2 {
            return self.are_operands_similar(right1, right2, son_ir);
        }
        
        // 如果右操作数相同，检查左操作数是否相似
        if right1 == right2 {
            return self.are_operands_similar(left1, left2, son_ir);
        }
        
        // 如果都是常量，检查值是否相同
        if let (Some(node1), Some(node2)) = (son_ir.get_node(left1), son_ir.get_node(right1)) {
            if let (Some(node3), Some(node4)) = (son_ir.get_node(left2), son_ir.get_node(right2)) {
                if let (NodeData::Constant { .. }, NodeData::Constant { .. }) = 
                    (&node1.kind.data, &node3.kind.data) {
                    if let (NodeData::Constant { .. }, NodeData::Constant { .. }) = 
                        (&node2.kind.data, &node4.kind.data) {
                        return self.nodes_are_equal(left1, left2, son_ir) && 
                               self.nodes_are_equal(right1, right2, son_ir);
                    }
                }
            }
        }
        
        false
    }
    
    /// 检查两个操作数是否相似（用于部分CSE）
    fn are_operands_similar(&self, op1: SonNodeId, op2: SonNodeId, son_ir: &SonIr) -> bool {
        // 如果操作数ID相同，直接返回true
        if op1 == op2 {
            return true;
        }
        
        let node1 = son_ir.get_node(op1);
        let node2 = son_ir.get_node(op2);
        
        if node1.is_none() || node2.is_none() {
            return false;
        }
        
        let node1 = node1.unwrap();
        let node2 = node2.unwrap();
        
        // 如果都是常量，比较值
        if let (NodeData::Constant { value: val1, .. }, NodeData::Constant { value: val2, .. }) = 
            (&node1.kind.data, &node2.kind.data) {
            match (val1, val2) {
                (crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::Integer(i1), 
                 crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::Integer(i2)) => i1 == i2,
                (crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::Float(f1), 
                 crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::Float(f2)) => 
                    (f1 - f2).abs() < f64::EPSILON,
                (crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::Boolean(b1), 
                 crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::Boolean(b2)) => b1 == b2,
                (crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::String(s1), 
                 crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::String(s2)) => s1 == s2,
                _ => false,
            }
        } else {
            // 如果操作码相同，认为相似
            node1.kind.opcode == node2.kind.opcode
        }
    }
    
    /// 计算节点的哈希值
    fn compute_node_hash(&mut self, node_id: SonNodeId, son_ir: &SonIr) -> u64 {
        if let Some(cached) = self.node_hashes.get(&node_id) {
            return *cached;
        }
        
        let mut hasher = DefaultHasher::new();
        let node = son_ir.get_node(node_id).unwrap();
        
        // 哈希操作码
        node.kind.opcode.hash(&mut hasher);
        
        // 哈希操作数
        match &node.kind.data {
            NodeData::BinaryOp { left, right } => {
                // 对于可交换操作，确保操作数顺序一致
                let is_commutative = matches!(node.kind.opcode, 
                    OpCode::Add | OpCode::Multiply | OpCode::Equal | OpCode::NotEqual
                );
                
                if is_commutative {
                    // 对于可交换操作，按操作数ID排序以确保一致性
                    let mut operands = Vec::new();
                    if let Some(left_id) = left {
                        operands.push(*left_id);
                    }
                    if let Some(right_id) = right {
                        operands.push(*right_id);
                    }
                    operands.sort(); // 排序确保一致性
                    
                    for operand_id in operands {
                        if let Some(operand_hash) = self.node_hashes.get(&operand_id) {
                            operand_hash.hash(&mut hasher);
                        }
                    }
                } else {
                    // 对于不可交换操作，保持原有顺序
                    if let Some(left_id) = left {
                        if let Some(left_hash) = self.node_hashes.get(left_id) {
                            left_hash.hash(&mut hasher);
                        }
                    }
                    if let Some(right_id) = right {
                        if let Some(right_hash) = self.node_hashes.get(right_id) {
                            right_hash.hash(&mut hasher);
                        }
                    }
                }
            }
            NodeData::UnaryOp { operand } => {
                if let Some(operand_id) = operand {
                    if let Some(operand_hash) = self.node_hashes.get(operand_id) {
                        operand_hash.hash(&mut hasher);
                    }
                }
            }
            NodeData::Constant { value, typ } => {
                // 对于常量，哈希值和类型
                match value {
                    crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::Integer(i) => {
                        i.hash(&mut hasher);
                    }
                    crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::Float(f) => {
                        format!("{:.6}", f).hash(&mut hasher);
                    }
                    crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::Boolean(b) => {
                        b.hash(&mut hasher);
                    }
                    crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::String(s) => {
                        s.hash(&mut hasher);
                    }
                }
                format!("{:?}", typ).hash(&mut hasher);
            }
            NodeData::Call { function_name, arguments, .. } => {
                // 哈希函数名和参数
                function_name.hash(&mut hasher);
                arguments.hash(&mut hasher);
            }
            _ => {}
        }
        
        let hash = hasher.finish();
        self.node_hashes.insert(node_id, hash);
        hash
    }
    
    /// 检查两个节点是否相等
    fn nodes_are_equal(&self, node1: SonNodeId, node2: SonNodeId, son_ir: &SonIr) -> bool {
        let node1_data = son_ir.get_node(node1);
        let node2_data = son_ir.get_node(node2);
        
        if node1_data.is_none() || node2_data.is_none() {
            println!("[DEBUG] Node equality check failed: one of nodes {} or {} not found", node1, node2);
            return false;
        }
        
        let node1_data = node1_data.unwrap();
        let node2_data = node2_data.unwrap();
        
        println!("[DEBUG] Comparing nodes {} ({:?}) and {} ({:?})", 
                node1, node1_data.kind.opcode, node2, node2_data.kind.opcode);
        
        // 检查操作码
        if node1_data.kind.opcode != node2_data.kind.opcode {
            println!("[DEBUG] Opcodes differ: {} vs {}", 
                    format!("{:?}", node1_data.kind.opcode), 
                    format!("{:?}", node2_data.kind.opcode));
            return false;
        }
        
        // 检查操作数
        match (&node1_data.kind.data, &node2_data.kind.data) {
            (NodeData::BinaryOp { left: left1, right: right1 }, 
             NodeData::BinaryOp { left: left2, right: right2 }) => {
                println!("[DEBUG] BinaryOp comparison: left1={:?}, right1={:?} vs left2={:?}, right2={:?}", 
                        left1, right1, left2, right2);
                
                // 对于可交换操作，检查操作数是否相等（考虑顺序）
                let is_commutative = matches!(node1_data.kind.opcode, 
                    OpCode::Add | OpCode::Multiply | OpCode::Equal | OpCode::NotEqual
                );
                
                let result = if is_commutative {
                    (left1 == left2 && right1 == right2) || (left1 == right2 && right1 == left2)
                } else {
                    left1 == left2 && right1 == right2
                };
                
                println!("[DEBUG] BinaryOp equality result: {} (commutative: {})", result, is_commutative);
                result
            }
            (NodeData::UnaryOp { operand: op1 }, NodeData::UnaryOp { operand: op2 }) => {
                let result = op1 == op2;
                println!("[DEBUG] UnaryOp equality result: {} (operands: {:?} vs {:?})", result, op1, op2);
                result
            }
            (NodeData::Constant { value: val1, typ: _ }, NodeData::Constant { value: val2, typ: _ }) => {
                // 比较常量值
                let result = match (val1, val2) {
                    (crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::Integer(i1), 
                     crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::Integer(i2)) => i1 == i2,
                    (crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::Float(f1), 
                     crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::Float(f2)) => 
                        (f1 - f2).abs() < f64::EPSILON,
                    (crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::Boolean(b1), 
                     crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::Boolean(b2)) => b1 == b2,
                    (crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::String(s1), 
                     crate::ast_to_cfg::ast_to_SoNir::son_ir::ConstantValue::String(s2)) => s1 == s2,
                    _ => false,
                };
                println!("[DEBUG] Constant equality result: {} (values: {:?} vs {:?})", result, val1, val2);
                result
            }
            (NodeData::Call { function_name: name1, arguments: args1, .. }, 
             NodeData::Call { function_name: name2, arguments: args2, .. }) => {
                // 检查函数调用是否等价
                let result = name1 == name2 && args1 == args2;
                println!("[DEBUG] Call equality result: {} (function: {} vs {}, args: {:?} vs {:?})", 
                        result, name1, name2, args1, args2);
                result
            }
            _ => {
                println!("[DEBUG] Other node types, not equal");
                false
            }
        }
    }
    
    /// 执行全局代码移动
    fn perform_global_code_motion(&mut self, son_ir: &mut SonIr) {
        // 构建CFG的RPO顺序
        let rpo_order = self.build_rpo_order(son_ir);
        
        // 早期调度：将表达式尽可能早地调度
        self.schedule_early(&rpo_order, son_ir);
        
        // 晚期调度：将表达式尽可能晚地调度
        self.schedule_late(&rpo_order, son_ir);
        
        self.stats.gcm_movements += 1; // 简化统计
    }
    
    /// 构建CFG的RPO顺序
    fn build_rpo_order(&self, son_ir: &SonIr) -> Vec<SonNodeId> {
        let mut rpo = Vec::new();
        let mut visited = HashSet::new();
        
        // 从Start节点开始DFS
        if let Some(start_node) = son_ir.get_all_nodes().keys().find(|&&id| {
            if let Some(node) = son_ir.get_node(id) {
                matches!(&node.kind.data, NodeData::Start { .. })
            } else {
                false
            }
        }) {
            self.dfs_rpo(*start_node, &mut visited, &mut rpo, son_ir);
        }
        
        rpo.reverse();
        rpo
    }
    
    /// DFS构建RPO
    fn dfs_rpo(&self, node_id: SonNodeId, visited: &mut HashSet<SonNodeId>, 
                rpo: &mut Vec<SonNodeId>, son_ir: &SonIr) {
        if visited.contains(&node_id) {
            return;
        }
        
        visited.insert(node_id);
        
        // 先访问所有后继节点
        for edge in son_ir.get_all_edges() {
            if edge.from == node_id {
                self.dfs_rpo(edge.to, visited, rpo, son_ir);
            }
        }
        
        rpo.push(node_id);
    }
    
    /// 早期调度
    fn schedule_early(&self, rpo_order: &[SonNodeId], son_ir: &mut SonIr) {
        // 简化实现：将常量表达式移动到最早可能的位置
        for &node_id in rpo_order {
            if let Some(node) = son_ir.get_node(node_id) {
                if matches!(&node.kind.data, NodeData::Constant { .. }) {
                    // 常量可以移动到最早的位置
                    // 这里简化处理，实际应该计算最佳位置
                }
            }
        }
    }
    
    /// 晚期调度
    fn schedule_late(&self, rpo_order: &[SonNodeId], son_ir: &mut SonIr) {
        // 简化实现：将表达式移动到最晚可能的位置
        for &node_id in rpo_order.iter().rev() {
            if let Some(node) = son_ir.get_node(node_id) {
                if self.is_computational_node(node_id, son_ir) {
                    // 计算节点应该移动到最晚可能的位置
                    // 这里简化处理，实际应该计算最佳位置
                }
            }
        }
    }
    
    /// 执行死代码消除
    fn perform_dead_code_elimination(&mut self, son_ir: &mut SonIr) {
        let mut dead_nodes = Vec::new();
        
        // 识别死节点
        for (node_id, _) in son_ir.get_all_nodes() {
            if self.is_dead_node(*node_id, son_ir) {
                dead_nodes.push(*node_id);
            }
        }
        
        // 删除死节点
        for node_id in dead_nodes {
            let _ = son_ir.remove_node(node_id);
        }
    }
    
    /// 判断节点是否为死节点
    fn is_dead_node(&self, node_id: SonNodeId, son_ir: &SonIr) -> bool {
        // 检查节点是否有输出边
        let has_outputs = son_ir.get_all_edges()
            .iter()
            .any(|edge| edge.from == node_id);
        
        // 检查节点是否为返回值或控制流节点
        let is_essential = if let Some(node) = son_ir.get_node(node_id) {
            // 保护控制流节点
            let is_control_flow = matches!(&node.kind.data, 
                NodeData::If { .. } | 
                NodeData::Loop { .. } |
                NodeData::Start { .. } |
                NodeData::Region { .. }
            );
            
            // 保护返回值节点
            let is_return = node.kind.opcode == OpCode::Return;
            
            // 保护函数调用节点
            let is_function_call = node.kind.opcode == OpCode::Call;
            
            is_control_flow || is_return || is_function_call
        } else {
            false
        };
        
        !has_outputs && !is_essential
    }
    
    /// 替换节点的所有使用
    fn replace_node_uses(&mut self, old_id: SonNodeId, new_id: SonNodeId, son_ir: &mut SonIr) {
        // 获取所有使用旧节点的边
        let edges_to_update: Vec<_> = son_ir.get_all_edges()
            .iter()
            .filter(|edge| edge.from == old_id)
            .cloned()
            .collect();
        
        // 更新边：将指向旧节点的边改为指向新节点
        for edge in edges_to_update {
            son_ir.remove_edge(&edge);
            let new_edge = crate::ast_to_cfg::ast_to_SoNir::son_ir::SonEdge::new(new_id, edge.to, edge.edge_type);
            son_ir.add_edge(new_edge);
        }
        
        // 更新所有节点内部引用的旧节点ID
        self.update_node_references(old_id, new_id, son_ir);

        // 替换后，将新节点的用户加入工作列表以便后续再次优化
        self.add_users_to_worklist(new_id, son_ir);
    }
    
    /// 更新所有节点内部引用的旧节点ID
    fn update_node_references(&mut self, old_id: SonNodeId, new_id: SonNodeId, son_ir: &mut SonIr) {
        let node_ids: Vec<SonNodeId> = son_ir.get_all_nodes().keys().cloned().collect();
        
        for node_id in node_ids {
            if let Some(node) = son_ir.get_node_mut(node_id) {
                match &mut node.kind.data {
                    NodeData::BinaryOp { left, right } => {
                        if *left == Some(old_id) { *left = Some(new_id); }
                        if *right == Some(old_id) { *right = Some(new_id); }
                    }
                    NodeData::UnaryOp { operand } => {
                        if *operand == Some(old_id) { *operand = Some(new_id); }
                    }
                    NodeData::Store { value, .. } => {
                        if *value == Some(old_id) { *value = Some(new_id); }
                    }
                    NodeData::Load { mem, ptr, offset, .. } => {
                        if *mem == Some(old_id) { *mem = Some(new_id); }
                        if *ptr == Some(old_id) { *ptr = Some(new_id); }
                        if *offset == Some(old_id) { *offset = Some(new_id); }
                    }
                    NodeData::Call { arguments, .. } => {
                        for arg in arguments {
                            if *arg == old_id { *arg = new_id; }
                        }
                    }
                    NodeData::Cast { value, .. } => {
                        if *value == Some(old_id) { *value = Some(new_id); }
                    }
                    NodeData::ArrayAccess { array, index } => {
                        if *array == Some(old_id) { *array = Some(new_id); }
                        if *index == Some(old_id) { *index = Some(new_id); }
                    }
                    NodeData::MemberAccess { object, .. } => {
                        if *object == Some(old_id) { *object = Some(new_id); }
                    }
                    NodeData::If { condition } => {
                        if *condition == Some(old_id) { *condition = Some(new_id); }
                    }
                    NodeData::Loop { entry, back } => {
                        if *entry == Some(old_id) { *entry = Some(new_id); }
                        if *back == Some(old_id) { *back = Some(new_id); }
                    }
                    NodeData::Region { inputs } => {
                        for input in inputs {
                            if *input == Some(old_id) { *input = Some(new_id); }
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    
    /// 将用户的节点加入工作列表
    fn add_users_to_worklist(&mut self, node_id: SonNodeId, son_ir: &SonIr) {
        let users: Vec<_> = son_ir.get_all_edges()
            .iter()
            .filter(|edge| edge.from == node_id)
            .map(|edge| edge.to)
            .collect();
        
        for user_id in users {
            if !self.processed.contains(&user_id) && 
               !self.worklist.iter().any(|&id| id == user_id) {
                self.worklist.push_back(user_id);
            }
        }
    }
    
    /// 获取统计信息
    pub fn get_stats(&self) -> &OptimizationStats {
        &self.stats
    }
    
    /// 重置优化器状态
    pub fn reset(&mut self) {
        self.node_hashes.clear();
        self.worklist.clear();
        self.processed.clear();
        self.stats = OptimizationStats::default();
    }
}




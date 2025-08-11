// Constant Propagation optimization pass
// This pass propagates constant values through the SoN IR to enable further optimizations
// 参照 Simple-Rust 的实现设计

use crate::ast_to_cfg::ast_to_SoNir::son_ir::{
    SonIr, SonNode, SonNodeId, SonNodeKind, OpCode, NodeData, ConstantValue, EdgeType, SonEdge
};
use crate::frontend::ast::Type;
use std::collections::{HashMap, HashSet, VecDeque};

pub struct ConstantPropagation {
    // 工作列表：需要重新优化的节点
    worklist: VecDeque<SonNodeId>,
    // 节点类型缓存：避免重复计算
    type_cache: HashMap<SonNodeId, Option<Type>>,
    // 常量值缓存：跟踪每个节点的常量值
    constant_cache: HashMap<SonNodeId, Option<ConstantValue>>,
    // 变量值映射：跟踪每个变量的当前值
    variable_values: HashMap<String, ConstantValue>,
    // 已处理的节点：避免重复处理
    processed: HashSet<SonNodeId>,
    // 已识别的变量：避免重复识别
    identified_variables: HashSet<String>,
    // 统计信息
    stats: ConstantPropagationStats,
}

#[derive(Debug, Default)]
pub struct ConstantPropagationStats {
    pub nodes_processed: usize,
    pub constants_folded: usize,
    pub nodes_replaced: usize,
    pub phi_nodes_optimized: usize,
    pub strength_reduction_applied: usize,  // 强度削弱优化次数
    pub global_value_numbering_applied: usize,  // 全局值编号优化次数
    pub iterations: usize,
}

impl ConstantPropagation {
    pub fn new() -> Self {
        Self {
            worklist: VecDeque::new(),
            type_cache: HashMap::new(),
            constant_cache: HashMap::new(),
            variable_values: HashMap::new(),
            processed: HashSet::new(),
            identified_variables: HashSet::new(),
            stats: ConstantPropagationStats::default(),
        }
    }
    
    /// 运行常量传播优化
    pub fn run(&mut self, son_ir: &mut SonIr) {
        self.stats = ConstantPropagationStats::default();
        
        // 初始化工作列表：所有数据流节点
        self.initialize_worklist(son_ir);
        
        // 第一轮：识别和传播变量值（只执行一次）
        self.identify_variable_values(son_ir);
        
        // 第二轮：传播变量值到使用点
        self.propagate_variable_values(son_ir);
        
        // 第三轮：迭代优化直到工作列表为空
        while let Some(node_id) = self.worklist.pop_front() {
            // 如果节点已经被处理过，跳过
            if self.processed.contains(&node_id) {
                continue;
            }
            
            self.stats.nodes_processed += 1;
            self.processed.insert(node_id);
            
            // 尝试优化节点
            if let Some(replacement_id) = self.try_peephole_opt(node_id, son_ir) {
                if replacement_id != node_id {
                    self.stats.nodes_replaced += 1;
                    // 将相关节点加入工作列表，但避免重复
                    self.add_users_to_worklist(node_id, son_ir);
                }
            }
        }
        
        self.stats.iterations = 3; // 三轮优化
    }
    
    /// 识别变量值
    fn identify_variable_values(&mut self, son_ir: &mut SonIr) {
        // 只识别原始Store节点，避免识别新创建的常量节点
        for (node_id, node) in son_ir.get_all_nodes() {
            if let NodeData::Store { name, value, .. } = &node.kind.data {
                if let Some(value_id) = value {
                    // 避免重复识别同一个变量
                    if !self.identified_variables.contains(name) {
                        // 检查存储的值是否是常量
                        if let Some(constant_value) = self.get_constant_value(*value_id, son_ir) {
                            // 更新变量值映射
                            self.update_variable_value(name, constant_value.clone());
                            self.identified_variables.insert(name.clone());
                        }
                    }
                }
            }
        }
    }
    
    /// 初始化工作列表
    fn initialize_worklist(&mut self, son_ir: &SonIr) {
        for (node_id, node) in son_ir.get_all_nodes() {
            if node.kind.is_data_flow() {
                self.worklist.push_back(*node_id);
            }
        }
    }
    
    /// 尝试窥孔优化节点
    fn try_peephole_opt(&mut self, node_id: SonNodeId, son_ir: &mut SonIr) -> Option<SonNodeId> {
        let node = son_ir.get_node(node_id)?;
        
        // 1. 计算节点类型
        let node_type = self.compute_node_type(node_id, son_ir);
        self.type_cache.insert(node_id, node_type.clone());
        
        // 2. 检查是否为常量计算
        // 注意：常量折叠现在在理想化优化之后进行
        // if let Some(constant_value) = self.try_constant_fold(node_id, son_ir) {
        //     self.constant_cache.insert(node_id, Some(constant_value.clone()));
        //     self.stats.constants_folded += 1;
        //     
        //     // 用常量节点替换
        //     let constant_node_id = self.create_constant_node(constant_value, node_type.unwrap_or(Type::IntType), son_ir);
        //     
        //     // 替换节点的所有使用，包括输入和输出边
        //     self.replace_node_uses(node_id, constant_node_id, son_ir);
        //     
        //     // 不要将新常量节点加入工作列表，避免无限循环
        //     // self.worklist.push_back(constant_node_id);
        //     
        //     return Some(constant_node_id);
        // }
        
        // 3. 尝试理想化优化
        if let Some(idealized_id) = self.try_idealize(node_id, son_ir) {
            if idealized_id != node_id {
                // 替换节点的所有使用，包括输入和输出边
                self.replace_node_uses(node_id, idealized_id, son_ir);
                return Some(idealized_id);
            }
        }
        
        // 4. 尝试常量折叠
        if let Some(constant_value) = self.try_constant_fold(node_id, son_ir) {
            self.constant_cache.insert(node_id, Some(constant_value.clone()));
            self.stats.constants_folded += 1;
            
            // 用常量节点替换
            let constant_node_id = self.create_constant_node(constant_value, node_type.unwrap_or(Type::IntType), son_ir);
            
            // 将新创建的常量节点标记为已处理，避免重复处理
            self.processed.insert(constant_node_id);
            
            // 替换节点的所有使用，包括输入和输出边
            self.replace_node_uses(node_id, constant_node_id, son_ir);
            
            return Some(constant_node_id);
        }
        
        // 5. 全局值编号：识别和消除重复计算
        if let Some(replacement_id) = self.global_value_numbering(node_id, son_ir) {
            if replacement_id != node_id {
                self.stats.nodes_replaced += 1;
                self.stats.global_value_numbering_applied += 1;
                // 替换节点的所有使用
                self.replace_node_uses(node_id, replacement_id, son_ir);
                return Some(replacement_id);
            }
        }
        
        Some(node_id)
    }
    
    /// 计算节点类型
    fn compute_node_type(&self, node_id: SonNodeId, son_ir: &SonIr) -> Option<Type> {
        let node = son_ir.get_node(node_id)?;
        
        match &node.kind.data {
            NodeData::Constant { typ, .. } => Some(typ.clone()),
            NodeData::Parameter { typ, .. } => Some(typ.clone()),
            NodeData::Local { typ, .. } => Some(typ.clone()),
            NodeData::BinaryOp { left, right } => {
                // 根据操作符和操作数类型推断结果类型
                let left_type = left.and_then(|id| self.compute_node_type(id, son_ir));
                let right_type = right.and_then(|id| self.compute_node_type(id, son_ir));
                
                match node.kind.opcode {
                    OpCode::Add | OpCode::Subtract | OpCode::Multiply => {
                        // 算术运算：如果两个操作数都是整数，结果是整数
                        if left_type.as_ref().map_or(false, |t| matches!(t, Type::IntType)) &&
                           right_type.as_ref().map_or(false, |t| matches!(t, Type::IntType)) {
                            Some(Type::IntType)
                        } else {
                            left_type.or(right_type)
                        }
                    }
                    OpCode::Equal | OpCode::NotEqual | OpCode::LessThan | OpCode::LessEqual |
                    OpCode::GreaterThan | OpCode::GreaterEqual => {
                        // 比较运算：结果总是布尔值
                        Some(Type::BoolType)
                    }
                    _ => left_type.or(right_type)
                }
            }
            _ => node.node_type.clone()
        }
    }
    
    /// 计算节点的实际值（类型计算系统）
    fn compute_node_value(&self, node_id: SonNodeId, son_ir: &SonIr) -> Option<ConstantValue> {
        let node = son_ir.get_node(node_id)?;
        
        match &node.kind.data {
            NodeData::Constant { value, .. } => Some(value.clone()),
            NodeData::BinaryOp { left, right } => {
                // 计算二元运算的实际值
                let left_value = left.and_then(|id| self.compute_node_value(id, son_ir));
                let right_value = right.and_then(|id| self.compute_node_value(id, son_ir));
                
                if let (Some(left_val), Some(right_val)) = (left_value, right_value) {
                    // 如果两个操作数都是常量，进行常量折叠
                    self.fold_binary_operation(&node.kind.opcode, &left_val, &right_val)
                } else {
                    // 如果只有一个操作数是常量，尝试理想化优化
                    self.try_idealize_binary_op(node_id, &node.kind.opcode, *left, *right, son_ir)
                }
            }
            NodeData::UnaryOp { operand } => {
                let operand_value = operand.and_then(|id| self.compute_node_value(id, son_ir));
                operand_value.and_then(|val| self.fold_unary_operation(&node.kind.opcode, &val))
            }
            NodeData::Local { name, .. } => {
                // 检查变量是否有已知的常量值
                self.get_variable_value(name).cloned()
            }
            NodeData::Parameter { name, .. } => {
                // 检查参数是否有已知的常量值
                self.get_variable_value(name).cloned()
            }
            _ => None
        }
    }
    
    /// 增强的常量折叠：支持变量到常量的传播
    fn try_constant_fold(&self, node_id: SonNodeId, son_ir: &SonIr) -> Option<ConstantValue> {
        // 首先尝试直接计算节点值
        if let Some(value) = self.compute_node_value(node_id, son_ir) {
            return Some(value);
        }
        
        // 如果无法直接计算，尝试理想化优化
        let node = son_ir.get_node(node_id)?;
        
        match &node.kind.data {
            NodeData::BinaryOp { left, right } => {
                // 尝试理想化优化
                if let Some(value) = self.try_idealize_binary_op(node_id, &node.kind.opcode, *left, *right, son_ir) {
                    return Some(value);
                }
                
                // 尝试直接常量折叠
                if let (Some(left_id), Some(right_id)) = (left, right) {
                    let left_value = self.get_constant_value(*left_id, son_ir);
                    let right_value = self.get_constant_value(*right_id, son_ir);
                    
                    if let (Some(left_val), Some(right_val)) = (left_value, right_value) {
                        return self.fold_binary_operation(&node.kind.opcode, &left_val, &right_val);
                    }
                }
            }
            _ => {}
        }
        
        None
    }
    
    /// 尝试理想化二元运算优化
    fn try_idealize_binary_op(&self, node_id: SonNodeId, opcode: &OpCode, left: Option<SonNodeId>, right: Option<SonNodeId>, son_ir: &SonIr) -> Option<ConstantValue> {
        let node = son_ir.get_node(node_id)?;
        
        if let NodeData::BinaryOp { left, right } = &node.kind.data {
            match opcode {
                OpCode::Add => {
                    // x + 0 = x
                    if let Some(right_id) = right {
                        if let Some(constant) = self.get_constant_value(*right_id, son_ir) {
                            if let ConstantValue::Integer(0) = constant {
                                return left.and_then(|id| self.get_constant_value(id, son_ir));
                            }
                        }
                    }
                    // 0 + x = x
                    if let Some(left_id) = left {
                        if let Some(constant) = self.get_constant_value(*left_id, son_ir) {
                            if let ConstantValue::Integer(0) = constant {
                                return right.and_then(|id| self.get_constant_value(id, son_ir));
                            }
                        }
                    }
                    // x + x = 2 * x (强度削弱)
                    if left == right {
                        if let Some(left_id) = left {
                            if let Some(left_value) = self.get_constant_value(*left_id, son_ir) {
                                if let ConstantValue::Integer(val) = left_value {
                                    return Some(ConstantValue::Integer(val * 2));
                                }
                            }
                        }
                    }
                }
                OpCode::Multiply => {
                    // x * 0 = 0
                    if let Some(right_id) = right {
                        if let Some(constant) = self.get_constant_value(*right_id, son_ir) {
                            if let ConstantValue::Integer(0) = constant {
                                return Some(ConstantValue::Integer(0));
                            }
                        }
                    }
                    if let Some(left_id) = left {
                        if let Some(constant) = self.get_constant_value(*left_id, son_ir) {
                            if let ConstantValue::Integer(0) = constant {
                                return Some(ConstantValue::Integer(0));
                            }
                        }
                    }
                    // x * 1 = x
                    if let Some(right_id) = right {
                        if let Some(constant) = self.get_constant_value(*right_id, son_ir) {
                            if let ConstantValue::Integer(1) = constant {
                                return left.and_then(|id| self.get_constant_value(id, son_ir));
                            }
                        }
                    }
                    // 1 * x = x
                    if let Some(left_id) = left {
                        if let Some(constant) = self.get_constant_value(*left_id, son_ir) {
                            if let ConstantValue::Integer(1) = constant {
                                return right.and_then(|id| self.get_constant_value(id, son_ir));
                            }
                        }
                    }
                    // x * 2 = x + x (强度削弱)
                    if let Some(right_id) = right {
                        if let Some(constant) = self.get_constant_value(*right_id, son_ir) {
                            if let ConstantValue::Integer(2) = constant {
                                if let Some(left_id) = left {
                                    if let Some(left_value) = self.get_constant_value(*left_id, son_ir) {
                                        if let ConstantValue::Integer(val) = left_value {
                                            return Some(ConstantValue::Integer(val + val));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                OpCode::Subtract => {
                    // x - 0 = x
                    if let Some(right_id) = right {
                        if let Some(constant) = self.get_constant_value(*right_id, son_ir) {
                            if let ConstantValue::Integer(0) = constant {
                                return left.and_then(|id| self.get_constant_value(id, son_ir));
                            }
                        }
                    }
                    // x - x = 0
                    if left == right {
                        return Some(ConstantValue::Integer(0));
                    }
                    // 0 - x = -x
                    if let Some(left_id) = left {
                        if let Some(constant) = self.get_constant_value(*left_id, son_ir) {
                            if let ConstantValue::Integer(0) = constant {
                                if let Some(right_id) = right {
                                    if let Some(right_value) = self.get_constant_value(*right_id, son_ir) {
                                        if let ConstantValue::Integer(val) = right_value {
                                            return Some(ConstantValue::Integer(-val));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                OpCode::Divide => {
                    // 0 / x = 0 (x != 0)
                    if let Some(left_id) = left {
                        if let Some(constant) = self.get_constant_value(*left_id, son_ir) {
                            if let ConstantValue::Integer(0) = constant {
                                return Some(ConstantValue::Integer(0));
                            }
                        }
                    }
                    // x / 1 = x
                    if let Some(right_id) = right {
                        if let Some(constant) = self.get_constant_value(*right_id, son_ir) {
                            if let ConstantValue::Integer(1) = constant {
                                return left.and_then(|id| self.get_constant_value(id, son_ir));
                            }
                        }
                    }
                    // x / x = 1
                    if left == right {
                        return Some(ConstantValue::Integer(1));
                    }
                }
                _ => {}
            }
        }
        
        None
    }
    
    /// 获取节点的常量值
    fn get_constant_value(&self, node_id: SonNodeId, son_ir: &SonIr) -> Option<ConstantValue> {
        // 首先检查缓存
        if let Some(cached) = self.constant_cache.get(&node_id) {
            return cached.clone();
        }
        
        let node = son_ir.get_node(node_id)?;
        
        match &node.kind.data {
            NodeData::Constant { value, .. } => {
                // 常量节点直接返回值
                Some(value.clone())
            }
            NodeData::Local { name, .. } => {
                // 检查变量是否有已知的常量值
                self.get_variable_value(name).cloned()
            }
            NodeData::Parameter { name, .. } => {
                // 检查参数是否有已知的常量值
                self.get_variable_value(name).cloned()
            }
            _ => {
                // 递归计算
                self.try_constant_fold(node_id, son_ir)
            }
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
                    None // 除零错误
                }
            }
            (OpCode::Equal, a, b) => Some(ConstantValue::Boolean(a == b)),
            (OpCode::NotEqual, a, b) => Some(ConstantValue::Boolean(a != b)),
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
            (OpCode::LogicalAnd, ConstantValue::Boolean(a), ConstantValue::Boolean(b)) => {
                Some(ConstantValue::Boolean(*a && *b))
            }
            (OpCode::LogicalOr, ConstantValue::Boolean(a), ConstantValue::Boolean(b)) => {
                Some(ConstantValue::Boolean(*a || *b))
            }
            _ => None
        }
    }
    
    /// 折叠一元运算
    fn fold_unary_operation(&self, opcode: &OpCode, operand: &ConstantValue) -> Option<ConstantValue> {
        match (opcode, operand) {
            (OpCode::Minus, ConstantValue::Integer(a)) => Some(ConstantValue::Integer(-a)),
            (OpCode::LogicalNot, ConstantValue::Boolean(a)) => Some(ConstantValue::Boolean(!a)),
            (OpCode::BitwiseNot, ConstantValue::Integer(a)) => Some(ConstantValue::Integer(!a)),
            _ => None
        }
    }
    
    /// 尝试理想化优化
    fn try_idealize(&mut self, node_id: SonNodeId, son_ir: &mut SonIr) -> Option<SonNodeId> {
        let node = son_ir.get_node(node_id)?;
        
        match &node.kind.data {
            NodeData::BinaryOp { left, right } => {
                // 先尝试理想化优化
                if let Some(idealized_id) = self.idealize_binary_op(node_id, &node.kind.opcode, *left, *right, son_ir) {
                    return Some(idealized_id);
                }
                
                // 如果理想化没有效果，尝试常量折叠
                if let Some(constant_value) = self.try_constant_fold(node_id, son_ir) {
                    // 创建常量节点
                    let constant_node_id = self.create_constant_node(
                        constant_value, 
                        self.compute_node_type(node_id, son_ir).unwrap_or(Type::IntType), 
                        son_ir
                    );
                    
                    // 替换节点
                    self.replace_node_uses(node_id, constant_node_id, son_ir);
                    return Some(constant_node_id);
                }
                
                None
            }
            NodeData::Phi { inputs, .. } => {
                self.idealize_phi(node_id, son_ir)
            }
            _ => None
        }
    }
    
    /// 理想化二元运算
    fn idealize_binary_op(&self, node_id: SonNodeId, opcode: &OpCode, left: Option<SonNodeId>, right: Option<SonNodeId>, son_ir: &SonIr) -> Option<SonNodeId> {
        match opcode {
            OpCode::Add => {
                // x + 0 = x
                if let Some(right_id) = right {
                    if let Some(constant) = self.get_constant_value(right_id, son_ir) {
                        if let ConstantValue::Integer(0) = constant {
                            return left;
                        }
                    }
                }
                // 0 + x = x
                if let Some(left_id) = left {
                    if let Some(constant) = self.get_constant_value(left_id, son_ir) {
                        if let ConstantValue::Integer(0) = constant {
                            return right;
                        }
                    }
                }
            }
            OpCode::Multiply => {
                // x * 1 = x
                if let Some(right_id) = right {
                    if let Some(constant) = self.get_constant_value(right_id, son_ir) {
                        if let ConstantValue::Integer(1) = constant {
                            return left;
                        }
                    }
                }
                // 1 * x = x
                if let Some(left_id) = left {
                    if let Some(constant) = self.get_constant_value(left_id, son_ir) {
                        if let ConstantValue::Integer(1) = constant {
                            return right;
                        }
                    }
                }
            }
            OpCode::Subtract => {
                // x - 0 = x
                if let Some(right_id) = right {
                    if let Some(constant) = self.get_constant_value(right_id, son_ir) {
                        if let ConstantValue::Integer(0) = constant {
                            return left;
                        }
                    }
                }
            }
            OpCode::Divide => {
                // x / 1 = x
                if let Some(right_id) = right {
                    if let Some(constant) = self.get_constant_value(right_id, son_ir) {
                        if let ConstantValue::Integer(1) = constant {
                            return left;
                        }
                    }
                }
            }
            _ => {}
        }
        
        None
    }
    
    /// 理想化Phi节点
    fn idealize_phi(&mut self, node_id: SonNodeId, son_ir: &mut SonIr) -> Option<SonNodeId> {
        let node = son_ir.get_node(node_id)?;
        
        if let NodeData::Phi { inputs, .. } = &node.kind.data {
            // 检查是否所有输入都是相同的常量
            let mut first_constant: Option<ConstantValue> = None;
            let mut all_same = true;
            
            for input in inputs {
                if let Some(input_id) = input {
                    if let Some(constant) = self.get_constant_value(*input_id, son_ir) {
                        if let Some(ref first) = first_constant {
                            if !self.constants_equal(first, &constant) {
                                all_same = false;
                                break;
                            }
                        } else {
                            first_constant = Some(constant);
                        }
                    } else {
                        return None; // 有非常量输入
                    }
                }
            }
            
            // 如果所有输入都是相同的常量，可以替换为常量节点
            if all_same && first_constant.is_some() {
                if let Some(constant_value) = first_constant {
                    self.stats.phi_nodes_optimized += 1;
                    let constant_node_id = self.create_constant_node(
                        constant_value,
                        Type::IntType, // 简化类型推断，使用默认类型
                        son_ir
                    );
                    
                    // 替换节点的所有使用，包括输入和输出边
                    self.replace_node_uses(node_id, constant_node_id, son_ir);
                    
                    return Some(constant_node_id);
                }
            }
        }
        
        None
    }
    
    /// 比较两个常量值是否相等
    fn constants_equal(&self, a: &ConstantValue, b: &ConstantValue) -> bool {
        match (a, b) {
            (ConstantValue::Integer(x), ConstantValue::Integer(y)) => x == y,
            (ConstantValue::Float(x), ConstantValue::Float(y)) => (x - y).abs() < f64::EPSILON,
            (ConstantValue::Boolean(x), ConstantValue::Boolean(y)) => x == y,
            (ConstantValue::String(x), ConstantValue::String(y)) => x == y,
            _ => false
        }
    }
    
    /// 创建常量节点
    fn create_constant_node(&self, value: ConstantValue, typ: Type, son_ir: &mut SonIr) -> SonNodeId {
        let kind = SonNodeKind::with_data(OpCode::Constant, NodeData::Constant { value, typ });
        let node = SonNode::new(0, kind);
        son_ir.add_node(node)
    }
    
    /// 替换节点的所有使用
    fn replace_node_uses(&self, old_id: SonNodeId, new_id: SonNodeId, son_ir: &mut SonIr) {
        // 获取所有使用旧节点的边
        let edges_to_update: Vec<_> = son_ir.get_all_edges()
            .iter()
            .filter(|edge| edge.to == old_id)
            .cloned()
            .collect();
        
        // 更新边：将指向旧节点的边改为指向新节点
        for edge in edges_to_update {
            son_ir.remove_edge(&edge);
            let new_edge = SonEdge::new(edge.from, new_id, edge.edge_type);
            son_ir.add_edge(new_edge);
        }
        
        // 获取所有从旧节点出发的边
        let edges_from_old: Vec<_> = son_ir.get_all_edges()
            .iter()
            .filter(|edge| edge.from == old_id)
            .cloned()
            .collect();
        
        // 更新边：将旧节点作为源节点的边改为新节点作为源节点
        for edge in edges_from_old {
            son_ir.remove_edge(&edge);
            let new_edge = SonEdge::new(new_id, edge.to, edge.edge_type);
            son_ir.add_edge(new_edge);
        }
        
        // 更新所有节点内部引用的旧节点ID
        self.update_node_references(old_id, new_id, son_ir);
        
        // 删除孤立的旧节点（如果它不再有任何连接）
        if son_ir.get_all_edges()
            .iter()
            .all(|edge| edge.from != old_id && edge.to != old_id) {
            son_ir.remove_node(old_id);
        }
    }
    
    /// 更新所有节点内部引用的旧节点ID
    fn update_node_references(&self, old_id: SonNodeId, new_id: SonNodeId, son_ir: &mut SonIr) {
        let node_ids: Vec<SonNodeId> = son_ir.get_all_nodes().keys().cloned().collect();
        
        for node_id in node_ids {
            if let Some(node) = son_ir.get_node_mut(node_id) {
                let mut updated = false;
                
                // 根据节点类型更新引用
                match &mut node.kind.data {
                    NodeData::BinaryOp { left, right } => {
                        if *left == Some(old_id) {
                            *left = Some(new_id);
                            updated = true;
                        }
                        if *right == Some(old_id) {
                            *right = Some(new_id);
                            updated = true;
                        }
                    }
                    NodeData::UnaryOp { operand } => {
                        if *operand == Some(old_id) {
                            *operand = Some(new_id);
                            updated = true;
                        }
                    }
                    NodeData::Phi { inputs, .. } => {
                        for input in inputs {
                            if *input == Some(old_id) {
                                *input = Some(new_id);
                                updated = true;
                            }
                        }
                    }
                    NodeData::Store { value, .. } => {
                        if *value == Some(old_id) {
                            *value = Some(new_id);
                            updated = true;
                        }
                    }
                    NodeData::Load { mem, ptr, offset, .. } => {
                        if *mem == Some(old_id) {
                            *mem = Some(new_id);
                            updated = true;
                        }
                        if *ptr == Some(old_id) {
                            *ptr = Some(new_id);
                            updated = true;
                        }
                        if *offset == Some(old_id) {
                            *offset = Some(new_id);
                            updated = true;
                        }
                    }
                    NodeData::Call { arguments, .. } => {
                        for arg in arguments {
                            if *arg == old_id {
                                *arg = new_id;
                                updated = true;
                            }
                        }
                    }
                    NodeData::Cast { value, .. } => {
                        if *value == Some(old_id) {
                            *value = Some(new_id);
                            updated = true;
                        }
                    }
                    NodeData::ArrayAccess { array, index } => {
                        if *array == Some(old_id) {
                            *array = Some(new_id);
                            updated = true;
                        }
                        if *index == Some(old_id) {
                            *index = Some(new_id);
                            updated = true;
                        }
                    }
                    NodeData::MemberAccess { object, .. } => {
                        if *object == Some(old_id) {
                            *object = Some(new_id);
                            updated = true;
                        }
                    }
                    NodeData::If { condition } => {
                        if *condition == Some(old_id) {
                            *condition = Some(new_id);
                            updated = true;
                        }
                    }
                    NodeData::Loop { entry, back } => {
                        if *entry == Some(old_id) {
                            *entry = Some(new_id);
                            updated = true;
                        }
                        if *back == Some(old_id) {
                            *back = Some(new_id);
                            updated = true;
                        }
                    }
                    NodeData::Region { inputs } => {
                        for input in inputs {
                            if *input == Some(old_id) {
                                *input = Some(new_id);
                                updated = true;
                            }
                        }
                    }
                    NodeData::Constant { .. } | NodeData::Local { .. } | NodeData::Parameter { .. } | 
                    NodeData::Start { .. } | NodeData::Proj { .. } | NodeData::CProj { .. } | NodeData::None => {
                        // 这些节点不引用其他节点ID
                    }
                }
                
                // 如果节点被更新，将其加入工作列表以便重新处理
                if updated {
                    // 注意：这里我们不能直接修改工作列表，因为我们在遍历节点
                    // 我们将在主循环中处理这个情况
                }
            }
        }
    }
    
    /// 将节点的用户加入工作列表
    fn add_users_to_worklist(&mut self, node_id: SonNodeId, son_ir: &SonIr) {
        let users: Vec<_> = son_ir.get_all_edges()
            .iter()
            .filter(|edge| edge.from == node_id)
            .map(|edge| edge.to)
            .collect();
        
        for user_id in users {
            // 只添加未处理且不在工作列表中的节点
            if !self.processed.contains(&user_id) && 
               !self.worklist.iter().any(|&id| id == user_id) {
                self.worklist.push_back(user_id);
            }
        }
    }
    
    /// 获取统计信息
    pub fn get_stats(&self) -> &ConstantPropagationStats {
        &self.stats
    }
    
    /// 重置优化器状态
    pub fn reset(&mut self) {
        self.worklist.clear();
        self.type_cache.clear();
        self.constant_cache.clear();
        self.variable_values.clear();
        self.processed.clear();
        self.identified_variables.clear();
        self.stats = ConstantPropagationStats::default();
    }

    /// 全局值编号：识别和消除重复计算
    fn global_value_numbering(&self, node_id: SonNodeId, son_ir: &SonIr) -> Option<SonNodeId> {
        let node = son_ir.get_node(node_id)?;
        
        // 只对计算节点进行值编号
        if !node.kind.is_data_flow() {
            return None;
        }
        
        // 查找具有相同操作和操作数的其他节点
        for (other_id, other_node) in son_ir.get_all_nodes() {
            if *other_id == node_id {
                continue; // 跳过自己
            }
            
            if other_node.kind.opcode == node.kind.opcode {
                // 检查操作数是否相同
                if self.nodes_have_same_inputs(node_id, *other_id, son_ir) {
                    return Some(*other_id); // 找到重复节点
                }
            }
        }
        
        None
    }
    
    /// 检查两个节点是否具有相同的输入
    fn nodes_have_same_inputs(&self, node1_id: SonNodeId, node2_id: SonNodeId, son_ir: &SonIr) -> bool {
        let node1 = son_ir.get_node(node1_id);
        let node2 = son_ir.get_node(node2_id);
        
        if let (Some(node1), Some(node2)) = (node1, node2) {
            match (&node1.kind.data, &node2.kind.data) {
                (NodeData::BinaryOp { left: l1, right: r1 }, NodeData::BinaryOp { left: l2, right: r2 }) => {
                    l1 == l2 && r1 == r2
                }
                (NodeData::UnaryOp { operand: o1 }, NodeData::UnaryOp { operand: o2 }) => {
                    o1 == o2
                }
                (NodeData::Constant { value: v1, .. }, NodeData::Constant { value: v2, .. }) => {
                    v1 == v2
                }
                _ => false
            }
        } else {
            false
        }
    }

    /// 更新变量的值
    fn update_variable_value(&mut self, var_name: &str, value: ConstantValue) {
        self.variable_values.insert(var_name.to_string(), value);
    }
    
    /// 获取变量的当前值
    fn get_variable_value(&self, var_name: &str) -> Option<&ConstantValue> {
        self.variable_values.get(var_name)
    }
    
    /// 传播变量值到使用该变量的节点
    fn propagate_variable_values(&mut self, son_ir: &mut SonIr) {
        // 收集所有需要优化的变量使用
        let mut optimizations = Vec::new();
        
        // 遍历所有节点，查找变量使用
        let node_ids: Vec<SonNodeId> = son_ir.get_all_nodes().keys().cloned().collect();
        
        for node_id in node_ids {
            if let Some(node) = son_ir.get_node(node_id) {
                // 检查节点是否使用了变量
                if let Some(var_name) = self.extract_variable_name(node) {
                    if let Some(var_value) = self.get_variable_value(&var_name) {
                        // 收集优化信息，稍后执行
                        optimizations.push((node_id, var_name, var_value.clone()));
                    }
                }
            }
        }
        
        // 执行收集到的优化
        for (node_id, var_name, var_value) in optimizations {
            self.try_optimize_variable_usage(node_id, &var_name, &var_value, son_ir);
        }
    }
    
    /// 尝试优化变量使用
    fn try_optimize_variable_usage(&mut self, node_id: SonNodeId, var_name: &str, var_value: &ConstantValue, son_ir: &mut SonIr) {
        // 查找所有使用该变量的节点
        let users = self.find_variable_users(node_id, son_ir);
        
        for user_id in users {
            if let Some(user_node) = son_ir.get_node(user_id) {
                // 尝试将变量替换为常量值
                if let Some(optimized_id) = self.try_replace_variable_with_constant(user_id, var_name, var_value, son_ir) {
                    if optimized_id != user_id {
                        // 替换成功，更新统计
                        self.stats.nodes_replaced += 1;
                        self.stats.constants_folded += 1;
                        
                        // 将受影响的节点加入工作列表
                        self.add_users_to_worklist(user_id, son_ir);
                    }
                }
            }
        }
    }
    
    /// 查找变量的所有用户
    fn find_variable_users(&self, var_node_id: SonNodeId, son_ir: &SonIr) -> Vec<SonNodeId> {
        let mut users = Vec::new();
        
        for (node_id, node) in son_ir.get_all_nodes() {
            if self.node_uses_variable(*node_id, var_node_id, son_ir) {
                users.push(*node_id);
            }
        }
        
        users
    }
    
    /// 检查节点是否使用了指定的变量
    fn node_uses_variable(&self, node_id: SonNodeId, var_node_id: SonNodeId, son_ir: &SonIr) -> bool {
        if let Some(node) = son_ir.get_node(node_id) {
            match &node.kind.data {
                NodeData::BinaryOp { left, right } => {
                    *left == Some(var_node_id) || *right == Some(var_node_id)
                }
                NodeData::UnaryOp { operand } => {
                    *operand == Some(var_node_id)
                }
                NodeData::Store { value, .. } => {
                    *value == Some(var_node_id)
                }
                NodeData::Load { .. } => false,
                _ => false
            }
        } else {
            false
        }
    }
    
    /// 尝试将变量替换为常量
    fn try_replace_variable_with_constant(&mut self, node_id: SonNodeId, var_name: &str, var_value: &ConstantValue, son_ir: &mut SonIr) -> Option<SonNodeId> {
        let node = son_ir.get_node(node_id)?;
        
        match &node.kind.data {
            NodeData::BinaryOp { left, right } => {
                // 检查是否可以直接替换为常量计算
                if let Some(result) = self.compute_binary_with_constant(left, right, &node.kind.opcode, var_name, var_value, son_ir) {
                    // 创建常量节点
                    let constant_node_id = self.create_constant_node(
                        result,
                        self.compute_node_type(node_id, son_ir).unwrap_or(Type::IntType),
                        son_ir
                    );
                    
                    // 替换节点
                    self.replace_node_uses(node_id, constant_node_id, son_ir);
                    return Some(constant_node_id);
                }
            }
            NodeData::UnaryOp { operand } => {
                // 检查是否可以直接替换为常量计算
                if let Some(result) = self.compute_unary_with_constant(operand, &node.kind.opcode, var_name, var_value, son_ir) {
                    // 创建常量节点
                    let constant_node_id = self.create_constant_node(
                        result,
                        self.compute_node_type(node_id, son_ir).unwrap_or(Type::IntType),
                        son_ir
                    );
                    
                    // 替换节点
                    self.replace_node_uses(node_id, constant_node_id, son_ir);
                    return Some(constant_node_id);
                }
            }
            _ => {}
        }
        
        None
    }
    
    /// 计算二元运算与常量的结果
    fn compute_binary_with_constant(&self, left: &Option<SonNodeId>, right: &Option<SonNodeId>, opcode: &OpCode, var_name: &str, var_value: &ConstantValue, son_ir: &SonIr) -> Option<ConstantValue> {
        // 检查左操作数是否是目标变量
        if let Some(left_id) = left {
            if let Some(left_node) = son_ir.get_node(*left_id) {
                if let NodeData::Local { name, .. } = &left_node.kind.data {
                    if name == var_name {
                        // 左操作数是目标变量，右操作数是常量
                        if let Some(right_id) = right {
                            if let Some(right_value) = self.get_constant_value(*right_id, son_ir) {
                                return self.fold_binary_operation(opcode, var_value, &right_value);
                            }
                        }
                    }
                }
            }
        }
        
        // 检查右操作数是否是目标变量
        if let Some(right_id) = right {
            if let Some(right_node) = son_ir.get_node(*right_id) {
                if let NodeData::Local { name, .. } = &right_node.kind.data {
                    if name == var_name {
                        // 右操作数是目标变量，左操作数是常量
                        if let Some(left_id) = left {
                            if let Some(left_value) = self.get_constant_value(*left_id, son_ir) {
                                return self.fold_binary_operation(opcode, &left_value, var_value);
                            }
                        }
                    }
                }
            }
        }
        
        None
    }
    
    /// 计算一元运算与常量的结果
    fn compute_unary_with_constant(&self, operand: &Option<SonNodeId>, opcode: &OpCode, var_name: &str, var_value: &ConstantValue, son_ir: &SonIr) -> Option<ConstantValue> {
        if let Some(operand_id) = operand {
            if let Some(operand_node) = son_ir.get_node(*operand_id) {
                if let NodeData::Local { name, .. } = &operand_node.kind.data {
                    if name == var_name {
                        return self.fold_unary_operation(opcode, var_value);
                    }
                }
            }
        }
        
        None
    }

    /// 提取节点中的变量名
    fn extract_variable_name(&self, node: &SonNode) -> Option<String> {
        match &node.kind.data {
            NodeData::Local { name, .. } => Some(name.clone()),
            NodeData::Parameter { name, .. } => Some(name.clone()),
            _ => None
        }
    }
}


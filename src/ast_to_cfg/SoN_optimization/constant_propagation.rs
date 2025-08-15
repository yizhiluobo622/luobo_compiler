use crate::ast_to_cfg::SoN_optimization::{OptimizationPass, OptimizationResult, OptimizationStats};
use crate::ast_to_cfg::ast_to_SoNir::son_ir::{SonIr, SonNode, OpCode, NodeData, ConstantValue, SonNodeId, EdgeType, SonNodeKind, SonEdge};
use crate::frontend::ast::Type;
use std::collections::HashSet;

/// 常量传播优化Pass
pub struct ConstantPropagationPass {
    stats: OptimizationStats,
}

impl ConstantPropagationPass {
    pub fn new() -> Self {
        Self {
            stats: OptimizationStats {
                constant_propagations: 0,
                constant_foldings: 0,
                dead_code_eliminations: 0,
                optimization_rounds: 0,
                dangling_nodes_removed: 0,
            },
        }
    }
    
    /// 检查节点是否可以进行常量折叠
    fn can_constant_fold(&self, son_ir: &SonIr, node_id: SonNodeId) -> bool {
        if let Some(node) = son_ir.get_node(node_id) {
            match node.kind.opcode {
                OpCode::Add | OpCode::Subtract | OpCode::Multiply | OpCode::Divide | OpCode::Minus => {
                    // 检查所有输入是否都是常量
                    node.inputs.iter().all(|&input_id| {
                        if let Some(input_node) = son_ir.get_node(input_id) {
                            matches!(input_node.kind.opcode, OpCode::Constant)
                        } else {
                            false
                        }
                    })
                }
                _ => false,
            }
        } else {
            false
        }
    }
    
    /// 执行常量折叠
    fn perform_constant_folding(&mut self, son_ir: &mut SonIr, node_id: SonNodeId) -> Result<(), String> {
        // 1. 计算常量值
        let constant_value = self.compute_constant_value(son_ir, node_id)?;
        
        // 2. 创建新的常量节点
        let new_constant_id = self.create_constant_node(son_ir, constant_value)?;
        
        // 3. 在常量节点到Start节点之间添加虚拟边
        self.add_virtual_edge_to_start(son_ir, new_constant_id)?;
        
        // 4. 重定向所有依赖关系（父节点指向新常量）
        self.redirect_all_dependencies(son_ir, node_id, new_constant_id)?;
        
        // 5. 断开操作节点与父节点的连接
        self.disconnect_from_parents(son_ir, node_id)?;
        
        // 6. 删除操作节点（边会自动清理）
        son_ir.remove_node(node_id)?;
        
        // 更新统计
        self.stats.constant_foldings += 1;
        
        Ok(())
    }
    
    /// 计算节点的常量值
    fn compute_constant_value(&self, son_ir: &SonIr, node_id: SonNodeId) -> Result<ConstantValue, String> {
        let node = son_ir.get_node(node_id).ok_or("节点不存在")?;
        
        match node.kind.opcode {
            OpCode::Add => {
                // 加法：左操作数 + 右操作数
                let left_value = self.get_constant_value(son_ir, node.inputs[0])?;
                let right_value = self.get_constant_value(son_ir, node.inputs[1])?;
                match (left_value, right_value) {
                    (ConstantValue::Integer(a), ConstantValue::Integer(b)) => Ok(ConstantValue::Integer(a + b)),
                    (ConstantValue::Float(a), ConstantValue::Float(b)) => Ok(ConstantValue::Float(a + b)),
                    _ => Err("类型不匹配".to_string()),
                }
            }
            OpCode::Subtract => {
                // 减法：左操作数 - 右操作数
                // 注意：node.inputs[0] 是左操作数，node.inputs[1] 是右操作数
                let left_value = self.get_constant_value(son_ir, node.inputs[0])?;
                let right_value = self.get_constant_value(son_ir, node.inputs[1])?;
                match (left_value, right_value) {
                    (ConstantValue::Integer(a), ConstantValue::Integer(b)) => Ok(ConstantValue::Integer(a - b)),
                    (ConstantValue::Float(a), ConstantValue::Float(b)) => Ok(ConstantValue::Float(a - b)),
                    _ => Err("类型不匹配".to_string()),
                }
            }
            OpCode::Multiply => {
                // 乘法：左操作数 * 右操作数
                let left_value = self.get_constant_value(son_ir, node.inputs[0])?;
                let right_value = self.get_constant_value(son_ir, node.inputs[1])?;
                match (left_value, right_value) {
                    (ConstantValue::Integer(a), ConstantValue::Integer(b)) => Ok(ConstantValue::Integer(a * b)),
                    (ConstantValue::Float(a), ConstantValue::Float(b)) => Ok(ConstantValue::Float(a * b)),
                    _ => Err("类型不匹配".to_string()),
                }
            }
            OpCode::Divide => {
                // 除法：左操作数 / 右操作数
                let left_value = self.get_constant_value(son_ir, node.inputs[0])?;
                let right_value = self.get_constant_value(son_ir, node.inputs[1])?;
                match (left_value, right_value) {
                    (ConstantValue::Integer(a), ConstantValue::Integer(b)) => {
                        if b == 0 { return Err("除零错误".to_string()); }
                        Ok(ConstantValue::Integer(a / b))
                    }
                    (ConstantValue::Float(a), ConstantValue::Float(b)) => {
                        if b == 0.0 { return Err("除零错误".to_string()); }
                        Ok(ConstantValue::Float(a / b))
                    }
                    _ => Err("类型不匹配".to_string()),
                }
            }
            OpCode::Minus => {
                // 一元负号：-操作数
                let operand_value = self.get_constant_value(son_ir, node.inputs[0])?;
                match operand_value {
                    ConstantValue::Integer(a) => Ok(ConstantValue::Integer(-a)),
                    ConstantValue::Float(a) => Ok(ConstantValue::Float(-a)),
                    _ => Err("类型不匹配".to_string()),
                }
            }
            _ => Err("不支持的节点类型".to_string()),
        }
    }
    
    /// 获取常量节点的值
    fn get_constant_value(&self, son_ir: &SonIr, node_id: SonNodeId) -> Result<ConstantValue, String> {
        if let Some(node) = son_ir.get_node(node_id) {
            if let NodeData::Constant { value, .. } = &node.kind.data {
                Ok(value.clone())
            } else {
                Err("节点不是常量".to_string())
            }
        } else {
            Err("节点不存在".to_string())
        }
    }
    
    /// 创建常量节点
    fn create_constant_node(&self, son_ir: &mut SonIr, value: ConstantValue) -> Result<SonNodeId, String> {
        // 根据常量值类型确定类型
        let typ = match &value {
            ConstantValue::Integer(_) => Type::IntType,
            ConstantValue::Float(_) => Type::FloatType,
            ConstantValue::Boolean(_) => Type::BoolType,
            ConstantValue::String(_) => Type::CharType,
            ConstantValue::DeadControl => Type::VoidType,
        };
        
        // 创建常量节点数据
        let constant_data = NodeData::Constant {
            value: value.clone(),
            typ,
            start_input: None,
        };
        
        // 创建节点类型
        let constant_kind = SonNodeKind::with_data(OpCode::Constant, constant_data);
        
        // 创建节点并添加到图中
        let constant_node = SonNode::new(0, constant_kind); // ID 会被 SonIr::add_node 自动设置
        let constant_id = son_ir.add_node(constant_node);
        
        Ok(constant_id)
    }
    
    /// 在常量节点到Start节点之间添加虚拟边
    fn add_virtual_edge_to_start(&self, son_ir: &mut SonIr, constant_node_id: SonNodeId) -> Result<(), String> {
        let start_node_id = son_ir.get_entry_node().ok_or("未找到Start节点")?;
        let virtual_edge = SonEdge::new(start_node_id, constant_node_id, EdgeType::Virtual);
        son_ir.add_edge(virtual_edge);
        Ok(())
    }
    
    /// 重定向所有依赖关系（父节点指向新常量）
    fn redirect_all_dependencies(&self, son_ir: &mut SonIr, old_node_id: SonNodeId, new_node_id: SonNodeId) -> Result<(), String> {
        // 先收集所有需要的信息，避免借用冲突
        let (output_nodes, control_output_nodes) = {
            let old_node = son_ir.get_node(old_node_id).ok_or("节点不存在")?;
            (old_node.outputs.clone(), old_node.control_outputs.clone())
        };
        
        // 重定向所有数据输出边
        for output_id in output_nodes {
            // 删除旧的数据边
            let old_edge = SonEdge::new(old_node_id, output_id, EdgeType::Data);
            son_ir.remove_edge(&old_edge);
            
            // 创建新的数据边
            let new_edge = SonEdge::new(new_node_id, output_id, EdgeType::Data);
            son_ir.add_edge(new_edge);
        }
        
        // 重定向所有控制流输出边
        for output_id in control_output_nodes {
            // 删除旧的控制边
            let old_edge = SonEdge::new(old_node_id, output_id, EdgeType::Control);
            son_ir.remove_edge(&old_edge);
            
            // 创建新的控制边
            let new_edge = SonEdge::new(new_node_id, output_id, EdgeType::Control);
            son_ir.add_edge(new_edge);
        }
        
        Ok(())
    }
    
    /// 断开操作节点与父节点的连接
    fn disconnect_from_parents(&self, son_ir: &mut SonIr, node_id: SonNodeId) -> Result<(), String> {
        // 先收集所有需要的信息，避免借用冲突
        let (input_nodes, parent_nodes) = {
            let node = son_ir.get_node(node_id).ok_or("节点不存在")?;
            (node.inputs.clone(), node.parent_nodes.clone())
        };
        
        // 断开所有数据输入边
        for input_id in input_nodes {
            let edge = SonEdge::new(input_id, node_id, EdgeType::Data);
            son_ir.remove_edge(&edge);
        }
        
        // 断开所有控制流输入边
        for parent_id in parent_nodes {
            let edge = SonEdge::new(parent_id, node_id, EdgeType::Control);
            son_ir.remove_edge(&edge);
        }
        
        Ok(())
    }
    
    /// 清理无用的常量节点（只处理常量节点，检查是否还有使用者）
    fn cleanup_unused_constants(&mut self, son_ir: &mut SonIr) -> Result<usize, String> {
        let mut cleaned_count = 0;
        
        // 只检查常量节点，不遍历其他节点
        let nodes_to_clean: Vec<_> = son_ir.get_all_nodes()
            .iter()
            .filter_map(|(id, node)| {
                // 只处理常量节点
                if matches!(node.kind.opcode, OpCode::Constant) {
                    // 检查是否还有运算符在使用这个常量
                    let has_users = !node.outputs.is_empty();
                    if !has_users {
                        Some(*id)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();
        
        // 只删除真正无用的常量节点
        for node_id in nodes_to_clean {
            if let Ok(()) = son_ir.remove_node(node_id) {
                cleaned_count += 1;
            }
        }
        
        self.stats.dangling_nodes_removed += cleaned_count;
        Ok(cleaned_count)
    }
    
    /// 发现新的优化机会
    fn find_new_optimization_opportunities(&self, son_ir: &SonIr) -> Result<Vec<SonNodeId>, String> {
        let mut new_opportunities = Vec::new();
        
        // 遍历所有节点，寻找新产生的优化机会
        for (id, node) in son_ir.get_all_nodes() {
            // 只检查运算符节点
            if matches!(node.kind.opcode, OpCode::Add | OpCode::Subtract | OpCode::Multiply | OpCode::Divide | OpCode::Minus) {
                // 检查是否可以进行常量折叠
                if self.can_constant_fold(son_ir, *id) {
                    new_opportunities.push(*id);
                }
            }
        }
        
        Ok(new_opportunities)
    }
    
    /// 验证优化结果
    fn validate_optimization(&self, son_ir: &SonIr) -> bool {
        // 检查是否还有可以进行常量折叠的节点
        let remaining_optimizable = son_ir.get_all_nodes()
            .iter()
            .any(|(_, node)| self.can_constant_fold(son_ir, node.id));
        
        !remaining_optimizable
    }
    
    /// 打印优化统计信息
    pub fn print_stats(&self) {
        println!("=== 常量传播优化统计 ===");
        println!("优化轮次: {}", self.stats.optimization_rounds);
        println!("常量折叠次数: {}", self.stats.constant_foldings);
        println!("删除的无用常量节点: {}", self.stats.dangling_nodes_removed);
        println!("常量传播次数: {}", self.stats.constant_propagations);
    }
}

impl OptimizationPass for ConstantPropagationPass {
    fn run(&mut self, son_ir: &mut SonIr) -> Result<OptimizationResult, String> {
        let mut worklist: Vec<SonNodeId> = Vec::new();
        let mut processed_nodes: HashSet<SonNodeId> = HashSet::new();
        let mut total_optimizations = 0;
        
        // 初始：找到所有可优化的节点
        for (id, _) in son_ir.get_all_nodes() {
            if self.can_constant_fold(son_ir, *id) {
                worklist.push(*id);
            }
        }
        
        // 真正的worklist循环
        while let Some(node_id) = worklist.pop() {
            if processed_nodes.contains(&node_id) {
                continue; // 避免重复处理
            }
            
            // 检查节点是否还存在（可能在之前的优化中被删除了）
            if son_ir.get_node(node_id).is_none() {
                continue;
            }
            
            // 再次检查是否可以进行常量折叠（状态可能已经改变）
            if !self.can_constant_fold(son_ir, node_id) {
                continue;
            }
            
            // 执行优化
            if let Ok(()) = self.perform_constant_folding(son_ir, node_id) {
                processed_nodes.insert(node_id);
                total_optimizations += 1;
                
                // 检查是否产生了新的优化机会
                let new_opportunities = self.find_new_optimization_opportunities(son_ir)?;
                for new_node_id in new_opportunities {
                    if !processed_nodes.contains(&new_node_id) && son_ir.get_node(new_node_id).is_some() {
                        worklist.push(new_node_id);
                    }
                }
            }
        }
        
        // 最后统一清理无用节点
        let cleaned_count = self.cleanup_unused_constants(son_ir)?;
        
        // 更新统计信息
        self.stats.optimization_rounds = 1; // worklist算法只需要一轮
        self.stats.constant_propagations = total_optimizations;
        self.stats.dangling_nodes_removed = cleaned_count;
        
        // 创建优化结果
        let mut result = OptimizationResult::new();
        if total_optimizations > 0 {
            result.mark_optimized();
            result.nodes_optimized = total_optimizations;
        }
        
        Ok(result)
    }
    
    fn name(&self) -> &str {
        "ConstantPropagationPass"
    }
    
    fn get_stats(&self) -> &OptimizationStats {
        &self.stats
    }
}

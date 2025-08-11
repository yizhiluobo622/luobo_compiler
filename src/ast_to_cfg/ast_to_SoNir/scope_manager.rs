use std::collections::{HashMap, VecDeque};
use crate::ast_to_cfg::ast_to_SoNir::son_ir::{
    SonIr, SonNode, SonNodeId, SonNodeKind, OpCode, NodeData
};
use crate::frontend::ast::Type as AstType;

/// 作用域管理器
/// 实现第三章中描述的 ScopeNode 功能
pub struct ScopeManager {
    /// 当前作用域栈
    scope_stack: VecDeque<ScopeInfo>,
    /// 作用域节点映射
    scope_nodes: HashMap<usize, SonNodeId>,
    /// 变量名到作用域索引的映射
    variable_scope_map: HashMap<String, usize>,
    /// 下一个作用域 ID
    next_scope_id: usize,
}

/// 作用域信息
#[derive(Debug, Clone)]
pub struct ScopeInfo {
    /// 作用域 ID
    pub id: usize,
    /// 作用域名称
    pub name: String,
    /// 作用域级别
    pub level: usize,
    /// 父作用域 ID
    pub parent_id: Option<usize>,
    /// 符号表：变量名 -> 节点输入索引
    pub symbol_table: HashMap<String, usize>,
    /// 作用域节点 ID
    pub scope_node_id: Option<SonNodeId>,
    /// 控制流节点 ($ctrl) - 存储在索引 0
    pub ctrl_node: Option<SonNodeId>,
    /// 内存节点 ($mem) - 存储在索引 1
    pub mem_node: Option<SonNodeId>,
    /// 参数节点 (arg) - 存储在索引 2
    pub arg_node: Option<SonNodeId>,
}

impl ScopeManager {
    // 特殊名称常量
    pub const CTRL: &'static str = "$ctrl";
    pub const ARG0: &'static str = "arg";
    pub const MEM0: &'static str = "$mem";
    
    /// 创建新的作用域管理器
    pub fn new() -> Self {
        let mut manager = Self {
            scope_stack: VecDeque::new(),
            scope_nodes: HashMap::new(),
            variable_scope_map: HashMap::new(),
            next_scope_id: 0,
        };
        
        // 创建全局作用域
        manager.enter_scope("global");
        manager
    }
    
    /// 进入新作用域
    pub fn enter_scope(&mut self, name: &str) -> usize {
        let scope_id = self.next_scope_id;
        self.next_scope_id += 1;
        
        let parent_id = self.scope_stack.back().map(|scope| scope.id);
        let level = self.scope_stack.len();
        
        let scope_info = ScopeInfo {
            id: scope_id,
            name: name.to_string(),
            level,
            parent_id,
            symbol_table: HashMap::new(),
            scope_node_id: None,
            ctrl_node: None,
            mem_node: None,
            arg_node: None,
        };
        
        self.scope_stack.push_back(scope_info);
        scope_id
    }
    
    /// 退出当前作用域
    pub fn exit_scope(&mut self, son_ir: &mut SonIr) -> Option<ScopeInfo> {
        if let Some(scope_info) = self.scope_stack.pop_back() {
            // 不要立即清理作用域，只是从栈中移除
            // 让调用者决定何时真正清理
            Some(scope_info)
        } else {
            None
        }
    }
    
    /// 获取当前作用域
    pub fn current_scope(&self) -> Option<&ScopeInfo> {
        self.scope_stack.back()
    }
    
    /// 获取当前作用域 ID
    pub fn current_scope_id(&self) -> Option<usize> {
        self.scope_stack.back().map(|scope| scope.id)
    }
    
    /// 获取指定作用域的节点ID
    pub fn get_scope_node_id(&self, scope_id: usize) -> Option<&SonNodeId> {
        self.scope_nodes.get(&scope_id)
    }
    
    /// 获取当前控制流节点 ($ctrl)
    pub fn get_ctrl(&self) -> Option<SonNodeId> {
        if let Some(current_scope) = self.current_scope() {
            current_scope.ctrl_node
        } else {
            None
        }
    }
    
    /// 设置当前控制流节点 ($ctrl)
    pub fn set_ctrl(&mut self, ctrl_node: SonNodeId) -> Result<(), String> {
        if let Some(current_scope) = self.scope_stack.back_mut() {
            current_scope.ctrl_node = Some(ctrl_node);
            current_scope.symbol_table.insert(Self::CTRL.to_string(), 0); // 使用索引0表示$ctrl
            Ok(())
        } else {
            Err("没有活动的作用域".to_string())
        }
    }
    
    /// 获取参数节点 (arg)
    pub fn get_arg(&self) -> Option<SonNodeId> {
        if let Some(current_scope) = self.current_scope() {
            current_scope.arg_node
        } else {
            None
        }
    }
    
    /// 设置参数节点 (arg)
    pub fn set_arg(&mut self, arg_node: SonNodeId) -> Result<(), String> {
        if let Some(current_scope) = self.scope_stack.back_mut() {
            current_scope.arg_node = Some(arg_node);
            current_scope.symbol_table.insert(Self::ARG0.to_string(), 2); // 使用索引2表示arg
            Ok(())
        } else {
            Err("没有活动的作用域".to_string())
        }
    }
    
    /// 获取内存节点 ($mem)
    pub fn get_mem(&self) -> Option<SonNodeId> {
        if let Some(current_scope) = self.current_scope() {
            current_scope.mem_node
        } else {
            None
        }
    }
    
    /// 设置内存节点 ($mem)
    pub fn set_mem(&mut self, mem_node: SonNodeId) -> Result<(), String> {
        if let Some(current_scope) = self.scope_stack.back_mut() {
            current_scope.mem_node = Some(mem_node);
            current_scope.symbol_table.insert(Self::MEM0.to_string(), 1); // 使用索引1表示$mem
            Ok(())
        } else {
            Err("没有活动的作用域".to_string())
        }
    }
    
    /// 声明变量
    pub fn declare_variable(&mut self, name: &str, typ: AstType, init_value: Option<SonNodeId>, son_ir: &mut SonIr) -> Result<SonNodeId, String> {
        let scope_id = self.current_scope_id().ok_or("No active scope")?;
        
        // 先获取作用域信息的副本，避免借用冲突
        let scope_info = {
            let scope = self.scope_stack.iter().find(|s| s.id == scope_id)
                .ok_or("Scope not found")?;
            scope.clone()
        };
        
        // 检查变量是否已声明
        if scope_info.symbol_table.contains_key(name) {
            return Err(format!("Variable '{}' already declared in scope '{}'", name, scope_info.name));
        }
        
        // 创建 Local 节点
        let local_node = SonNode::new(
            son_ir.node_count(),
            SonNodeKind::with_data(
                OpCode::Local,
                NodeData::Local {
                    name: name.to_string(),
                    typ: typ.clone().into(), // 使用传入的 AST 类型
                }
            )
        );
        
        let local_node_id = son_ir.add_node(local_node);
        
        // 更新作用域信息
        if let Some(scope) = self.scope_stack.iter_mut().find(|s| s.id == scope_id) {
            // 添加到符号表
            let input_index = scope.symbol_table.len();
            scope.symbol_table.insert(name.to_string(), input_index);
        }
        self.variable_scope_map.insert(name.to_string(), scope_id);
        
        // 如果有初始值，创建 Store 节点
        if let Some(value_id) = init_value {
            let store_node = SonNode::new(
                son_ir.node_count(),
                SonNodeKind::with_data(
                    OpCode::Store,
                    NodeData::Store {
                        name: name.to_string(),
                        alias: 0,
                        declared_type: typ.into(), // 使用传入的 AST 类型
                        mem: None,
                        ptr: None,
                        offset: None,
                        value: Some(value_id),
                        init: true,
                    }
                )
            );
            
            let store_node_id = son_ir.add_node(store_node);
            
            // 连接 Local 和 Store
            son_ir.add_edge(crate::ast_to_cfg::ast_to_SoNir::son_ir::SonEdge::new(
                local_node_id, store_node_id, crate::ast_to_cfg::ast_to_SoNir::son_ir::EdgeType::Data
            ));
            
            // 更新作用域节点
            self.update_scope_node(scope_id, son_ir);
            
            Ok(store_node_id)
        } else {
            // 更新作用域节点
            self.update_scope_node(scope_id, son_ir);
            
            Ok(local_node_id)
        }
    }
    
    /// 查找变量
    pub fn lookup_variable(&self, name: &str) -> Option<(usize, usize)> {
        // 从内到外查找变量
        for scope in self.scope_stack.iter().rev() {
            if let Some(&input_index) = scope.symbol_table.get(name) {
                return Some((scope.id, input_index));
            }
        }
        None
    }
    
    /// 更新变量值
    pub fn update_variable(&mut self, name: &str, new_value: SonNodeId, son_ir: &mut SonIr) -> Result<SonNodeId, String> {
        let (scope_id, _input_index) = self.lookup_variable(name)
            .ok_or_else(|| format!("Variable '{}' not found", name))?;
        
        // 创建 Store 节点
        let store_node = SonNode::new(
            son_ir.node_count(),
            SonNodeKind::with_data(
                OpCode::Store,
                NodeData::Store {
                    name: name.to_string(),
                    alias: 0,
                    declared_type: AstType::IntType.into(), // 这里应该从符号表获取实际类型
                    mem: None,
                    ptr: None,
                    offset: None,
                    value: Some(new_value),
                    init: false,
                }
            )
        );
        
        let store_node_id = son_ir.add_node(store_node);
        
        // 更新作用域节点
        self.update_scope_node(scope_id, son_ir);
        
        Ok(store_node_id)
    }
    
    /// 创建或更新作用域节点
    fn update_scope_node(&mut self, scope_id: usize, son_ir: &mut SonIr) {
        // 先获取作用域信息的副本，避免借用冲突
        let scope_info = {
            let scope = self.scope_stack.iter().find(|s| s.id == scope_id)
                .expect("Scope not found");
            scope.clone()
        };
        
        if let Some(existing_node_id) = scope_info.scope_node_id {
            // 更新现有作用域节点
            if let Some(node) = son_ir.get_node_mut(existing_node_id) {
                // 更新输入
                self.update_scope_node_inputs(node, &scope_info);
            }
        } else {
            // 创建新的作用域节点 - 先创建，再更新
            let scope_node = self.create_scope_node(&scope_info, son_ir);
            
            // 更新作用域信息
            for scope in &mut self.scope_stack {
                if scope.id == scope_id {
                    scope.scope_node_id = Some(scope_node);
                    break;
                }
            }
            
            // 插入到映射中
            self.scope_nodes.insert(scope_id, scope_node);
        }
    }
    
    /// 创建作用域节点
    fn create_scope_node(&self, scope_info: &ScopeInfo, son_ir: &mut SonIr) -> SonNodeId {
        let parent_scope = scope_info.parent_id.and_then(|id| self.scope_nodes.get(&id).copied());
        
        let scope_node = SonNode::new(
            son_ir.node_count(),
            SonNodeKind::with_data(
                OpCode::Scope,
                NodeData::Scope {
                    symbol_tables: vec![scope_info.symbol_table.clone()],
                    scope_level: scope_info.level,
                    parent_scope,
                }
            )
        );
        
        let scope_node_id = son_ir.add_node(scope_node);
        
        // 添加父作用域作为输入
        if let Some(parent_id) = parent_scope {
            son_ir.add_edge(crate::ast_to_cfg::ast_to_SoNir::son_ir::SonEdge::new(
                parent_id, scope_node_id, crate::ast_to_cfg::ast_to_SoNir::son_ir::EdgeType::Control
            ));
        }
        
        scope_node_id
    }
    
    /// 更新作用域节点的输入
    fn update_scope_node_inputs(&self, node: &mut SonNode, scope_info: &ScopeInfo) {
        // 清空现有输入
        node.inputs.clear();
        
        // 添加父作用域作为第一个输入
        if let Some(parent_id) = scope_info.parent_id {
            if let Some(&parent_node_id) = self.scope_nodes.get(&parent_id) {
                node.inputs.push(parent_node_id);
            }
        }
        
        // 添加所有变量节点作为输入
        // 注意：这里我们需要从外部传入变量节点ID，因为ScopeManager不直接管理变量节点
        // 实际的变量节点连接在converter中处理
    }
    
    /// 清理作用域
    fn cleanup_scope(&mut self, scope_id: usize, son_ir: &mut SonIr) {
        // 实现死代码清理逻辑
        if let Some(scope_info) = self.scope_stack.iter().find(|s| s.id == scope_id) {
            // 标记作用域内的节点为死亡状态
            if let Some(scope_node_id) = scope_info.scope_node_id {
                if let Some(node) = son_ir.get_node_mut(scope_node_id) {
                    node.mark_dead();
                }
            }
        }
    }

    /// 手动清理指定作用域
    pub fn cleanup_scope_manually(&mut self, scope_id: usize, son_ir: &mut SonIr) {
        // 从栈中移除作用域
        self.scope_stack.retain(|s| s.id != scope_id);
        
        // 清理作用域节点
        if let Some(scope_node_id) = self.scope_nodes.remove(&scope_id) {
            if let Some(node) = son_ir.get_node_mut(scope_node_id) {
                node.mark_dead();
            }
        }
        
        // 清理变量映射
        self.variable_scope_map.retain(|_, &mut scope_id_map| scope_id_map != scope_id);
    }

    /// 复制当前作用域（用于if语句分支）
    pub fn duplicate_current_scope(&mut self, son_ir: &mut SonIr) -> Result<usize, String> {
        let current_scope = self.current_scope()
            .ok_or("没有当前作用域")?
            .clone();
        
        // 创建新的作用域ID
        let new_scope_id = self.next_scope_id;
        self.next_scope_id += 1;
        
        // 复制作用域信息
        let mut new_scope = current_scope.clone();
        new_scope.id = new_scope_id;
        new_scope.name = format!("{}_dup", current_scope.name);
        new_scope.level = current_scope.level + 1;
        new_scope.parent_id = Some(current_scope.id);
        
        // 创建新的作用域节点
        let new_scope_node_id = self.create_scope_node(&new_scope, son_ir);
        new_scope.scope_node_id = Some(new_scope_node_id);
        
        // 将新作用域添加到栈中
        self.scope_stack.push_back(new_scope);
        
        // 记录作用域节点映射
        self.scope_nodes.insert(new_scope_id, new_scope_node_id);
        
        Ok(new_scope_id)
    }

    /// 合并两个作用域（用于if语句分支合并）
    pub fn merge_scopes(&mut self, scope1_id: usize, scope2_id: usize, son_ir: &mut SonIr) -> Result<usize, String> {
        // 首先检查作用域是否在栈中
        let scope1_in_stack = self.scope_stack.iter().any(|s| s.id == scope1_id);
        let scope2_in_stack = self.scope_stack.iter().any(|s| s.id == scope2_id);
        
        // 如果作用域不在栈中，尝试从节点映射中恢复
        let mut scope1 = None;
        let mut scope2 = None;
        
        if scope1_in_stack {
            scope1 = self.scope_stack.iter().find(|s| s.id == scope1_id).cloned();
        } else {
            // 尝试从节点映射中恢复作用域信息
            if let Some(&node_id) = self.scope_nodes.get(&scope1_id) {
                // 创建一个基本的作用域信息
                scope1 = Some(ScopeInfo {
                    id: scope1_id,
                    name: format!("recovered_scope_{}", scope1_id),
                    level: 0,
                    parent_id: None,
                    symbol_table: HashMap::new(),
                    scope_node_id: Some(node_id),
                    ctrl_node: None,
                    mem_node: None,
                    arg_node: None,
                });
            }
        }
        
        if scope2_in_stack {
            scope2 = self.scope_stack.iter().find(|s| s.id == scope2_id).cloned();
        } else {
            // 尝试从节点映射中恢复作用域信息
            if let Some(&node_id) = self.scope_nodes.get(&scope2_id) {
                // 创建一个基本的作用域信息
                scope2 = Some(ScopeInfo {
                    id: scope2_id,
                    name: format!("recovered_scope_{}", scope2_id),
                    level: 0,
                    parent_id: None,
                    symbol_table: HashMap::new(),
                    scope_node_id: Some(node_id),
                    ctrl_node: None,
                    mem_node: None,
                    arg_node: None,
                });
            }
        }
        
        let scope1 = scope1.ok_or_else(|| format!("作用域1 (ID: {}) 不存在且无法恢复", scope1_id))?;
        let scope2 = scope2.ok_or_else(|| format!("作用域2 (ID: {}) 不存在且无法恢复", scope2_id))?;
        
        // 创建Region节点来合并控制流
        let region_node_id = self.create_region_node(&[scope1.ctrl_node, scope2.ctrl_node], son_ir);
        
        // 创建Phi节点来合并不同的变量值
        let merged_vars = self.create_phi_nodes_for_scope_merge(&scope1, &scope2, region_node_id, son_ir)?;
        
        // 创建合并后的作用域
        let merged_scope_id = self.next_scope_id;
        self.next_scope_id += 1;
        
        let mut merged_scope = scope1.clone();
        merged_scope.id = merged_scope_id;
        merged_scope.name = format!("{}_merged", scope1.name);
        merged_scope.ctrl_node = Some(region_node_id);
        
        // 更新变量绑定，使用Phi节点
        for (var_name, phi_node_id) in merged_vars {
            merged_scope.symbol_table.insert(var_name, phi_node_id);
        }
        
        // 创建合并后的作用域节点
        let merged_scope_node_id = self.create_scope_node(&merged_scope, son_ir);
        merged_scope.scope_node_id = Some(merged_scope_node_id);
        
        // 将合并后的作用域添加到栈中
        self.scope_stack.push_back(merged_scope);
        
        // 记录作用域节点映射
        self.scope_nodes.insert(merged_scope_id, merged_scope_node_id);
        
        // 清理原始作用域（从栈中移除，但保留节点映射直到真正需要清理时）
        self.scope_stack.retain(|s| s.id != scope1_id && s.id != scope2_id);
        
        Ok(merged_scope_id)
    }

    /// 为作用域合并创建Phi节点
    fn create_phi_nodes_for_scope_merge(
        &self,
        scope1: &ScopeInfo,
        scope2: &ScopeInfo,
        region_node_id: SonNodeId,
        son_ir: &mut SonIr
    ) -> Result<HashMap<String, SonNodeId>, String> {
        let mut merged_vars = HashMap::new();
        
        // 遍历所有变量，检查是否需要创建Phi节点
        let all_vars: std::collections::HashSet<_> = scope1.symbol_table.keys()
            .chain(scope2.symbol_table.keys())
            .collect();
        
        for var_name in all_vars {
            let scope1_value = scope1.symbol_table.get(var_name);
            let scope2_value = scope2.symbol_table.get(var_name);
            
            // 如果两个作用域中变量的值不同，创建Phi节点
            if scope1_value != scope2_value {
                let phi_node_id = self.create_phi_node(
                    var_name,
                    region_node_id,
                    scope1_value,
                    scope2_value,
                    son_ir
                )?;
                merged_vars.insert(var_name.clone(), phi_node_id);
            } else if let Some(value) = scope1_value {
                // 如果值相同，直接使用原值
                merged_vars.insert(var_name.clone(), *value);
            }
        }
        
        Ok(merged_vars)
    }

    /// 创建Phi节点
    fn create_phi_node(
        &self,
        var_name: &str,
        region_node_id: SonNodeId,
        value1: Option<&SonNodeId>,
        value2: Option<&SonNodeId>,
        son_ir: &mut SonIr
    ) -> Result<SonNodeId, String> {
        let mut inputs = vec![Some(region_node_id)]; // 第一个输入是Region节点
        
        // 添加数据值输入
        if let Some(v1) = value1 {
            inputs.push(Some(*v1));
        }
        if let Some(v2) = value2 {
            inputs.push(Some(*v2));
        }
        
        // 推断变量类型（简化实现，假设为IntType）
        let var_type = AstType::IntType;
        
        let phi_data = NodeData::Phi {
            label: var_name.to_string(),
            typ: var_type,
            inputs: inputs.clone(),
            region: Some(region_node_id),
        };
        
        let phi_kind = SonNodeKind::with_data(OpCode::Phi, phi_data);
        let phi_node = SonNode::new(0, phi_kind);
        let phi_node_id = son_ir.add_node(phi_node);
        
        // 添加边
        for input_id in inputs.iter().flatten() {
            son_ir.add_edge(crate::ast_to_cfg::ast_to_SoNir::son_ir::SonEdge::new(
                *input_id,
                phi_node_id,
                crate::ast_to_cfg::ast_to_SoNir::son_ir::EdgeType::Data
            ));
        }
        
        Ok(phi_node_id)
    }

    /// 创建Region节点
    fn create_region_node(
        &self,
        control_inputs: &[Option<SonNodeId>],
        son_ir: &mut SonIr
    ) -> SonNodeId {
        let region_data = NodeData::Region {
            inputs: control_inputs.to_vec(),
        };
        
        let region_kind = SonNodeKind::with_data(OpCode::Region, region_data);
        let region_node = SonNode::new(0, region_kind);
        let region_node_id = son_ir.add_node(region_node);
        
        // 添加控制流边
        for input_id in control_inputs.iter().flatten() {
            son_ir.add_edge(crate::ast_to_cfg::ast_to_SoNir::son_ir::SonEdge::new(
                *input_id,
                region_node_id,
                crate::ast_to_cfg::ast_to_SoNir::son_ir::EdgeType::Control
            ));
        }
        
        region_node_id
    }
    
    /// 获取作用域统计信息
    pub fn get_scope_stats(&self) -> ScopeStats {
        ScopeStats {
            total_scopes: self.scope_stack.len(),
            current_level: self.scope_stack.len().saturating_sub(1),
            total_variables: self.variable_scope_map.len(),
            scope_stack_depth: self.scope_stack.len(),
        }
    }
    
    /// 打印作用域信息（用于调试）
    pub fn print_scope_info(&self) {
        println!("=== Scope Manager Info ===");
        println!("Total scopes: {}", self.scope_stack.len());
        println!("Current level: {}", self.scope_stack.len().saturating_sub(1));
        
        for (i, scope) in self.scope_stack.iter().enumerate() {
            println!("Scope {}: {} (ID: {}, Level: {})", 
                i, scope.name, scope.id, scope.level);
            println!("  Variables: {:?}", scope.symbol_table.keys().collect::<Vec<_>>());
            if let Some(parent) = scope.parent_id {
                println!("  Parent: {}", parent);
            }
        }
        println!("========================");
    }

    /// 打印作用域状态（调试用）
    pub fn debug_print_scopes(&self) {
        println!("=== 作用域管理器状态 ===");
        println!("作用域栈深度: {}", self.scope_stack.len());
        println!("下一个作用域ID: {}", self.next_scope_id);
        println!("作用域节点映射数量: {}", self.scope_nodes.len());
        
        for (i, scope) in self.scope_stack.iter().enumerate() {
            println!("  作用域 {}: ID={}, 名称='{}', 级别={}, 父ID={:?}", 
                    i, scope.id, scope.name, scope.level, scope.parent_id);
        }
        
        println!("作用域节点映射:");
        for (scope_id, node_id) in &self.scope_nodes {
            println!("  作用域ID {} -> 节点ID {}", scope_id, node_id);
        }
        println!("========================");
    }
}

/// 作用域统计信息
#[derive(Debug)]
pub struct ScopeStats {
    pub total_scopes: usize,
    pub current_level: usize,
    pub total_variables: usize,
    pub scope_stack_depth: usize,
}

impl Default for ScopeStats {
    fn default() -> Self {
        Self {
            total_scopes: 0,
            current_level: 0,
            total_variables: 0,
            scope_stack_depth: 0,
        }
    }
}

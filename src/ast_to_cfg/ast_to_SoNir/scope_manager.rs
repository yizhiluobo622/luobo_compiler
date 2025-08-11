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
}

impl ScopeManager {
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
        };
        
        self.scope_stack.push_back(scope_info);
        scope_id
    }
    
    /// 退出当前作用域
    pub fn exit_scope(&mut self, son_ir: &mut SonIr) -> Option<ScopeInfo> {
        if let Some(scope_info) = self.scope_stack.pop_back() {
            // 清理作用域内的死代码
            self.cleanup_scope(scope_info.id, son_ir);
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
        // 移除作用域节点
        if let Some(node_id) = self.scope_nodes.remove(&scope_id) {
            // 标记节点为死亡（在实际实现中应该更智能地处理）
            if let Some(node) = son_ir.get_node_mut(node_id) {
                node.mark_dead();
            }
        }
        
        // 清理变量映射
        self.variable_scope_map.retain(|_, &mut scope_id_map| scope_id_map != scope_id);
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

use std::collections::HashMap;
use crate::frontend::ast::Ast;
use super::tacir::{Operand, TACInstruction};

/// 全局数组初始化信息
#[derive(Debug, Clone)]
pub struct GlobalArrayInit {
    pub array_name: String,
    pub offset: usize,
    pub value: Operand,
}

/// 节点映射器
/// 负责管理AST节点到IR节点的映射关系
pub struct NodeMapper {
    /// AST节点ID到IR操作数的映射
    ast_to_ir: HashMap<usize, Operand>,
    /// 变量名到IR操作数的映射
    variable_map: HashMap<String, Operand>,
    /// 标签名到IR标签的映射
    label_map: HashMap<String, String>,
    /// 数组变量名到维度的映射
    array_info: HashMap<String, Vec<usize>>,
    /// 全局数组初始化信息
    global_array_inits: Vec<GlobalArrayInit>,
}

impl NodeMapper {
    pub fn new() -> Self {
        Self {
            ast_to_ir: HashMap::new(),
            variable_map: HashMap::new(),
            label_map: HashMap::new(),
            array_info: HashMap::new(),
            global_array_inits: Vec::new(),
        }
    }
    
    /// 记录AST节点到IR操作数的映射
    pub fn map_ast_to_ir(&mut self, ast_id: usize, ir_operand: Operand) {
        self.ast_to_ir.insert(ast_id, ir_operand);
    }
    
    /// 获取AST节点对应的IR操作数
    pub fn get_ir_operand(&self, ast_id: usize) -> Option<&Operand> {
        self.ast_to_ir.get(&ast_id)
    }
    

    
    /// 记录变量名到IR操作数的映射
    pub fn map_variable(&mut self, var_name: &str, ir_operand: Operand) {
        self.variable_map.insert(var_name.to_string(), ir_operand);
    }
    
    /// 获取变量名对应的IR操作数
    pub fn get_variable(&self, var_name: &str) -> Option<&Operand> {
        self.variable_map.get(var_name)
    }
    
    /// 记录标签名到IR标签的映射
    pub fn map_label(&mut self, label_name: &str, ir_label: String) {
        self.label_map.insert(label_name.to_string(), ir_label);
    }
    
    /// 获取标签名对应的IR标签
    pub fn get_label(&self, label_name: &str) -> Option<&String> {
        self.label_map.get(label_name)
    }
    
    /// 记录数组变量名到维度的映射
    pub fn map_array_info(&mut self, var_name: &str, dims: Vec<usize>) {
        self.array_info.insert(var_name.to_string(), dims);
    }

    /// 获取数组变量名的维度信息
    pub fn get_array_info(&self, var_name: &str) -> Option<&Vec<usize>> {
        self.array_info.get(var_name)
    }
    
    /// 检查AST节点是否已经映射
    pub fn has_mapping(&self, ast_id: usize) -> bool {
        self.ast_to_ir.contains_key(&ast_id)
    }
    
    /// 检查变量是否已经映射
    pub fn has_variable(&self, var_name: &str) -> bool {
        self.variable_map.contains_key(var_name)
    }
    
    /// 清空所有映射（用于函数作用域切换）
    pub fn clear_mappings(&mut self) {
        self.ast_to_ir.clear();
        self.variable_map.clear();
        self.label_map.clear();
        self.array_info.clear();
        // 注意：不清空全局数组初始化信息，因为它们是全局的
    }
    
    /// 获取映射统计信息
    pub fn get_mapping_stats(&self) -> MappingStats {
        MappingStats {
            ast_mappings: self.ast_to_ir.len(),
            variable_mappings: self.variable_map.len(),
            label_mappings: self.label_map.len(),
        }
    }
    
    /// 获取所有变量映射
    pub fn get_all_variables(&self) -> HashMap<String, Operand> {
        self.variable_map.clone()
    }
    
    /// 添加全局数组初始化信息
    pub fn add_global_array_init(&mut self, array_name: String, offset: usize, value: Operand) {
        self.global_array_inits.push(GlobalArrayInit {
            array_name,
            offset,
            value,
        });
    }
    
    /// 获取所有全局数组初始化信息
    pub fn get_global_array_inits(&self) -> &Vec<GlobalArrayInit> {
        &self.global_array_inits
    }
    
    /// 清空全局数组初始化信息
    pub fn clear_global_array_inits(&mut self) {
        self.global_array_inits.clear();
    }
    
    /// 恢复变量映射
    pub fn restore_variables(&mut self, variables: HashMap<String, Operand>) {
        self.variable_map = variables;
    }
}

/// 映射统计信息
#[derive(Debug)]
pub struct MappingStats {
    pub ast_mappings: usize,
    pub variable_mappings: usize,
    pub label_mappings: usize,
}

/// 表达式结果缓存
/// 用于缓存复杂表达式的计算结果
pub struct ExpressionCache {
    /// 表达式到结果的映射
    cache: HashMap<String, Operand>,
}

impl ExpressionCache {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
        }
    }
    
    /// 缓存表达式结果
    pub fn cache_result(&mut self, expression: String, result: Operand) {
        self.cache.insert(expression, result);
    }
    
    /// 获取缓存的表达式结果
    pub fn get_cached_result(&self, expression: &str) -> Option<&Operand> {
        self.cache.get(expression)
    }
    
    /// 检查表达式是否已缓存
    pub fn is_cached(&self, expression: &str) -> bool {
        self.cache.contains_key(expression)
    }
    
    /// 清空缓存
    pub fn clear(&mut self) {
        self.cache.clear();
    }
}

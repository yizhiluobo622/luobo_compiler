use std::collections::HashMap;
use crate::frontend::ast::{Ast, AstKind, Expression, Statement, BinaryOperator, UnaryOperator, Literal, Type};
use crate::frontend::span::Span;
use super::son_ir::{
    SonIr, SonNode, SonNodeKind, SonNodeId, SonEdge, OpCode, NodeData, 
    ConstantValue, EdgeType
};
use super::scope_manager::ScopeManager;

/// AST 到 Sea of Nodes IR 转换器
pub struct AstToSonConverter;

/// 转换结果
#[derive(Debug)]
pub struct ConversionResult {
    pub son_ir: SonIr,
    pub stats: ConversionStats,
    pub scope_stats: Option<crate::ast_to_cfg::ast_to_SoNir::scope_manager::ScopeStats>,
}

/// 转换统计信息
#[derive(Debug)]
pub struct ConversionStats {
    pub node_count: usize,
    pub edge_count: usize,
    pub function_count: usize,
    pub variable_count: usize,
}

/// 转换错误
#[derive(Debug)]
pub enum ConversionError {
    UnsupportedNodeType(String),
    TypeConversionError(String),
    UndefinedVariable(String),
    TypeMismatch { expected: String, actual: String },
    ControlFlowError(String),
    InternalError(String),
}

/// Sea of Nodes IR 构建器
/// 用于维护转换过程中的状态和构建IR图
pub struct SonIrBuilder<'a> {
    /// 目标SoN IR图
    son_ir: &'a mut SonIr,
    /// 当前函数的返回类型
    current_function_return_type: Option<Type>,
    /// 变量名到节点ID的映射
    variable_map: HashMap<String, SonNodeId>,
    /// 变量声明状态：true表示已声明，false表示未声明
    variable_declared: HashMap<String, bool>,
    /// 当前控制流节点
    current_control_flow: Option<SonNodeId>,
    /// 当前基本块的入口节点
    current_block_entry: Option<SonNodeId>,
    /// 临时节点缓存
    temp_node_cache: HashMap<String, SonNodeId>,
    /// 类型检查模式：true为严格模式（错误），false为宽松模式（警告）
    strict_type_checking: bool,
    /// 收集的类型错误和警告
    type_errors: Vec<String>,
    /// 符号表引用，用于类型查询和变量查找
    symbol_table: Option<&'a crate::frontend::SemanticAnalyzer::symbol_table::SymbolTable>,
    /// 类型系统引用，用于类型兼容性检查
    type_system: Option<&'a crate::frontend::SemanticAnalyzer::type_system::TypeSystem>,
    /// 作用域管理器
    scope_manager: ScopeManager,
}
impl<'a> SonIrBuilder<'a> {
    /// 创建新的构建器
    pub fn new(son_ir: &'a mut SonIr) -> Self {
        Self {
            son_ir,
            current_function_return_type: None,
            variable_map: HashMap::new(),
            variable_declared: HashMap::new(),
            current_control_flow: None,
            current_block_entry: None,
            temp_node_cache: HashMap::new(),
            strict_type_checking: true, // 默认严格模式
            type_errors: Vec::new(),
            symbol_table: None,
            type_system: None,
            scope_manager: ScopeManager::new(),
        }
    }

    /// 创建新的构建器（可配置类型检查模式）
    pub fn with_type_checking(son_ir: &'a mut SonIr, strict: bool) -> Self {
        Self {
            son_ir,
            current_function_return_type: None,
            variable_map: HashMap::new(),
            variable_declared: HashMap::new(),
            current_control_flow: None,
            current_block_entry: None,
            temp_node_cache: HashMap::new(),
            strict_type_checking: strict,
            type_errors: Vec::new(),
            symbol_table: None,
            type_system: None,
            scope_manager: ScopeManager::new(),
        }
    }

    /// 创建新的构建器（包含符号表和类型系统）
    pub fn with_semantic_info(
        son_ir: &'a mut SonIr,
        symbol_table: &'a crate::frontend::SemanticAnalyzer::symbol_table::SymbolTable,
        type_system: &'a crate::frontend::SemanticAnalyzer::type_system::TypeSystem,
        strict: bool,
    ) -> Self {
        Self {
            son_ir,
            current_function_return_type: None,
            variable_map: HashMap::new(),
            variable_declared: HashMap::new(),
            current_control_flow: None,
            current_block_entry: None,
            temp_node_cache: HashMap::new(),
            strict_type_checking: strict,
            type_errors: Vec::new(),
            symbol_table: Some(symbol_table),
            type_system: Some(type_system),
            scope_manager: ScopeManager::new(),
        }
    }

    /// 构建函数
    pub fn build_function(&mut self, ast: &Ast) -> Result<SonNodeId, ConversionError> {
        match &ast.kind {
            AstKind::Function { function_name, return_type, parameters, function_body } => {
                // 设置当前函数返回类型
                self.current_function_return_type = return_type.clone();
                
                // 进入函数作用域
                self.scope_manager.enter_scope(&format!("function_{}", function_name));
                
                // 创建Start节点
                let start_id = self.create_start_node();
                
                // 创建参数节点
                let mut param_nodes = Vec::new();
                for param in parameters {
                    if let AstKind::VariableDeclaration { variable_name, variable_type, .. } = &param.kind {
                        let param_id = self.create_parameter_node(
                            variable_name.clone(),
                            variable_type.clone()
                        );
                        param_nodes.push(param_id);
                        
                        // 将参数添加到变量映射
                        self.variable_map.insert(variable_name.clone(), param_id);
                        self.variable_declared.insert(variable_name.clone(), true);
                        
                        // 使用 ScopeManager 声明参数变量
                        if let Err(e) = self.scope_manager.declare_variable(
                            variable_name, 
                            variable_type.clone(), 
                            Some(param_id), 
                            self.son_ir
                        ) {
                            // 如果 ScopeManager 失败，记录错误但继续
                            if self.strict_type_checking {
                                self.type_errors.push(format!("参数声明失败: {}", e));
                            }
                        }
                        
                        // 将参数节点连接到当前作用域的Scope节点
                        if let Some(scope_id) = self.scope_manager.current_scope_id() {
                            if let Some(&scope_node_id) = self.scope_manager.get_scope_node_id(scope_id) {
                                self.son_ir.add_edge(SonEdge::new(param_id, scope_node_id, EdgeType::Data));
                            }
                        }
                    }
                }
                
                // 设置当前控制流为Start节点
                self.current_control_flow = Some(start_id);
                self.current_block_entry = Some(start_id);
        
                
                // 构建函数体
                let body_result = if let AstKind::Statement(stmt) = &function_body.kind {
                    self.build_statement(stmt)? // ✅ 返回 SonNodeId
                } else {
                    return Err(ConversionError::UnsupportedNodeType("函数体必须是语句".to_string()));
                };
                
                // 🔍 调试：打印节点ID
        
                
                // ✅ 设置入口和出口节点
                self.son_ir.set_entry_node(start_id);
                self.son_ir.set_exit_node(body_result);
                
                // 移除手动添加的控制流边，让控制流通过正常的语句构建流程来连接
                // self.son_ir.add_edge(SonEdge::new(start_id, body_result, EdgeType::Control));
        
                
                // 🔍 调试：显示最终的图结构信息
                
                // 退出函数作用域
                self.scope_manager.exit_scope(self.son_ir);
                
                Ok(body_result)
            }
            _ => Err(ConversionError::UnsupportedNodeType(
                format!("期望Function节点，得到: {:?}", ast.kind)
            )),
        }
    }

    /// 创建节点并自动添加数据依赖边
    fn create_node_with_data_edges(&mut self, opcode: OpCode, data: NodeData, input_nodes: Vec<SonNodeId>) -> SonNodeId {
        let node_id = self.son_ir.add_node(SonNode::new(0, SonNodeKind::with_data(opcode, data)));
        
        // 自动添加数据依赖边
        for input_id in input_nodes {
            self.son_ir.add_edge(SonEdge::new(input_id, node_id, EdgeType::Data));
        }
        
        node_id
    }

    /// 构建语句
    fn build_statement(&mut self, stmt: &Statement) -> Result<SonNodeId, ConversionError> {

        match stmt {
            Statement::Compound { statements } => {
                if statements.is_empty() {
                    // 空复合语句，创建Region节点
                    let region_id = self.create_region_node();
                    if let Some(control) = self.current_control_flow {
                        self.son_ir.add_edge(SonEdge::new(control, region_id, EdgeType::Control));
                        self.current_control_flow = Some(region_id);
                    }
                    return Ok(region_id);
                }
                
                // 进入复合语句作用域
                let scope_id = self.scope_manager.enter_scope("compound");
                
                let mut last_result = None;
                let mut previous_control = self.current_control_flow;
                
                for (i, stmt) in statements.iter().enumerate() {
                    let stmt_result = match &stmt.kind {
                        AstKind::Statement(stmt_kind) => {
                            self.build_statement(stmt_kind)?
                        }
                        AstKind::VariableDeclaration { variable_name, variable_type, initial_value, is_const } => {
                            // 使用 ScopeManager 声明变量
                            let var_id = match self.scope_manager.declare_variable(
                                variable_name,
                                variable_type.clone(),
                                None, // 先声明，稍后设置初始值
                                self.son_ir
                            ) {
                                Ok(node_id) => node_id,
                                Err(e) => {
                                    // 如果 ScopeManager 失败，降级到原来的实现
                                    if self.strict_type_checking {
                                        self.type_errors.push(format!("变量声明失败: {}", e));
                                    }
                                    self.create_local_node(variable_name.clone(), variable_type.clone())
                                }
                            };
                            
                            // 将变量节点连接到当前作用域的Scope节点
                            if let Some(scope_id) = self.scope_manager.current_scope_id() {
                                if let Some(&scope_node_id) = self.scope_manager.get_scope_node_id(scope_id) {
                                    self.son_ir.add_edge(SonEdge::new(var_id, scope_node_id, EdgeType::Data));
                                }
                            }
                            
                            // 将变量添加到变量映射（保持向后兼容）
                            self.variable_map.insert(variable_name.clone(), var_id);
                            self.variable_declared.insert(variable_name.clone(), true);
                            
                            // 如果有初始值，构建表达式并创建Store节点
                            if let Some(init_expr) = initial_value {
                                if let AstKind::Expression(expr_kind) = &init_expr.kind {
                                    let init_result = self.build_expression(expr_kind)?;
                                    
                                    // 创建Store节点来实际存储初始值
                                    let store_id = self.create_store_node(
                                        variable_name.clone(),
                                        0, // 默认别名
                                        variable_type.clone(),
                                        None, // mem
                                        Some(var_id), // ptr - 指向变量节点
                                        None, // offset
                                        Some(init_result), // value - 初始值
                                        true // init - 标记为初始化
                                    );
                                    
                                    // 添加数据流边：从初始值到Store（Store节点只有输入边，没有输出数据边）
                                    self.son_ir.add_edge(SonEdge::new(init_result, store_id, EdgeType::Data));
                                    // Store节点不应该有输出数据边，因为它是副作用操作
                                    
                                    // 更新 ScopeManager 中的变量值
                                    if let Err(e) = self.scope_manager.update_variable(
                                        variable_name,
                                        store_id,
                                        self.son_ir
                                    ) {
                                        if self.strict_type_checking {
                                            self.type_errors.push(format!("变量更新失败: {}", e));
                                        }
                                    }
                                    
                                    // Store节点的控制流连接由复合语句处理逻辑统一管理
                                    // 这里不需要手动添加控制边
                                    
                                    // 返回Store节点ID，这样控制流会经过初始化
                                    store_id
                                } else {
                                    var_id
                                }
                            } else {
                                var_id
                            }
                        }
                        _ => {
                            return Err(ConversionError::UnsupportedNodeType(
                                format!("复合语句中的元素必须是语句或变量声明，得到: {:?}", stmt.kind)
                            ));
                        }
                    };
                    
                    // 连接控制流：从上一个控制流节点到当前语句
                    if let Some(control) = previous_control {
                        self.son_ir.add_edge(SonEdge::new(control, stmt_result, EdgeType::Control));
                    }
                    
                    // 更新控制流状态
                    previous_control = Some(stmt_result);
                    last_result = Some(stmt_result);
                }
                
                // 更新当前控制流为最后一个语句的结果
                if let Some(result) = last_result {
                    self.current_control_flow = Some(result);
                }
                
                // 退出复合语句作用域
                self.scope_manager.exit_scope(self.son_ir);
                
                Ok(last_result.unwrap_or_else(|| self.create_region_node()))
            }
            
            Statement::ExpressionStatement { expression } => {
                let expr_result = if let AstKind::Expression(expr_kind) = &expression.kind {
                    self.build_expression(expr_kind)?
                } else {
                    return Err(ConversionError::UnsupportedNodeType("表达式语句必须是表达式".to_string()));
                };
                
                // 更新控制流
                if let Some(control) = self.current_control_flow {
                    self.son_ir.add_edge(SonEdge::new(control, expr_result, EdgeType::Control));
                    self.current_control_flow = Some(expr_result);
                }
                
                Ok(expr_result)
            }
            
            Statement::Return { value } => {
                let return_value = if let Some(expr) = value {
                    if let AstKind::Expression(expr_kind) = &expr.kind {
                        Some(self.build_expression(expr_kind)?)
                    } else {
                        return Err(ConversionError::UnsupportedNodeType("return值必须是表达式".to_string()));
                    }
                } else {
                    None
                };
                
                let return_id = self.create_return_node(return_value);
        
                
                // 更新控制流
                if let Some(control) = self.current_control_flow {
                            self.son_ir.add_edge(SonEdge::new(control, return_id, EdgeType::Control));
                        }
                
                Ok(return_id)
            }
            
            Statement::If { condition, then_branch, else_branch } => {
                let condition_id = if let AstKind::Expression(expr_kind) = &condition.kind {
                    self.build_expression(expr_kind)?
                } else {
                    return Err(ConversionError::UnsupportedNodeType("if条件必须是表达式".to_string()));
                };
                
                // 创建If节点
                let if_id = self.create_if_node(Some(condition_id));
                
                // 构建then分支
                let then_id = if let AstKind::Statement(stmt_kind) = &then_branch.kind {
                    self.build_statement(stmt_kind)?
                } else {
                    return Err(ConversionError::UnsupportedNodeType("then分支必须是语句".to_string()));
                };
                
                // 构建else分支（如果存在）
                let else_id = if let Some(else_stmt) = else_branch {
                    if let AstKind::Statement(stmt_kind) = &else_stmt.kind {
                        self.build_statement(stmt_kind)?
                    } else {
                        return Err(ConversionError::UnsupportedNodeType("else分支必须是语句".to_string()));
                    }
                } else {
                    self.create_region_node()
                };
                
                // 创建Region节点
                let merge_id = self.create_region_node();
                
                // 连接控制流
                if let Some(control) = self.current_control_flow {
                    self.son_ir.add_edge(SonEdge::new(control, if_id, EdgeType::Control));
                }
                
                self.son_ir.add_edge(SonEdge::new(if_id, then_id, EdgeType::Control));
                self.son_ir.add_edge(SonEdge::new(if_id, else_id, EdgeType::Control));
                self.son_ir.add_edge(SonEdge::new(then_id, merge_id, EdgeType::Control));
                self.son_ir.add_edge(SonEdge::new(else_id, merge_id, EdgeType::Control));
                
                self.current_control_flow = Some(merge_id);
                Ok(merge_id)
            }
            
            Statement::While { condition, body } => {
                // 创建Loop节点
                let loop_id = self.create_loop_node();
                
                // 构建条件表达式
                let condition_id = if let AstKind::Expression(expr_kind) = &condition.kind {
                    self.build_expression(expr_kind)?
                } else {
                    return Err(ConversionError::UnsupportedNodeType("while条件必须是表达式".to_string()));
                };
                
                // 构建循环体
                let body_id = if let AstKind::Statement(stmt_kind) = &body.kind {
                    self.build_statement(stmt_kind)?
                } else {
                    return Err(ConversionError::UnsupportedNodeType("while循环体必须是语句".to_string()));
                };
                
                // 创建Region节点
                let merge_id = self.create_region_node();
                
                // 连接控制流
                if let Some(control) = self.current_control_flow {
                    self.son_ir.add_edge(SonEdge::new(control, loop_id, EdgeType::Control));
                }
                
                self.son_ir.add_edge(SonEdge::new(loop_id, condition_id, EdgeType::Control));
                self.son_ir.add_edge(SonEdge::new(condition_id, body_id, EdgeType::Control));
                self.son_ir.add_edge(SonEdge::new(body_id, loop_id, EdgeType::Control)); // 回边
                self.son_ir.add_edge(SonEdge::new(condition_id, merge_id, EdgeType::Control)); // 退出边
                
                self.current_control_flow = Some(merge_id);
                Ok(merge_id)
            }
            
            Statement::For { initialization, condition, update, body } => {
                // 创建Loop节点
                let loop_id = self.create_loop_node();
                
                // 构建初始化语句
                if let Some(init_stmt) = initialization {
                    let init_id = if let AstKind::Statement(stmt_kind) = &init_stmt.kind {
                        self.build_statement(stmt_kind)?
                    } else {
                        return Err(ConversionError::UnsupportedNodeType("for初始化必须是语句".to_string()));
                    };
                    if let Some(control) = self.current_control_flow {
                        self.son_ir.add_edge(SonEdge::new(control, init_id, EdgeType::Control));
                    }
                    self.current_control_flow = Some(init_id);
                }
                
                // 构建条件表达式
                let condition_id = if let Some(condition_ast) = condition {
                    if let AstKind::Expression(expr_kind) = &condition_ast.kind {
                        self.build_expression(expr_kind)?
                    } else {
                        return Err(ConversionError::UnsupportedNodeType("for条件必须是表达式".to_string()));
                    }
                } else {
                    // 没有条件表达式，创建一个始终为true的常量节点
                    self.create_constant_node(ConstantValue::Boolean(true), Type::BoolType)
                };
                
                // 构建循环体
                let body_id = if let AstKind::Statement(stmt_kind) = &body.kind {
                    self.build_statement(stmt_kind)?
                } else {
                    return Err(ConversionError::UnsupportedNodeType("for循环体必须是语句".to_string()));
                };
                
                // 构建更新语句
                if let Some(update_expr) = update {
                    let update_id = if let AstKind::Expression(expr_kind) = &update_expr.kind {
                        self.build_expression(expr_kind)?
                    } else {
                        return Err(ConversionError::UnsupportedNodeType("for更新必须是表达式".to_string()));
                    };
                    self.son_ir.add_edge(SonEdge::new(body_id, update_id, EdgeType::Control));
                    self.son_ir.add_edge(SonEdge::new(update_id, loop_id, EdgeType::Control));
                } else {
                    self.son_ir.add_edge(SonEdge::new(body_id, loop_id, EdgeType::Control));
                }
                
                // 创建Region节点
                let merge_id = self.create_region_node();
                
                // 连接控制流
                if let Some(control) = self.current_control_flow {
                    self.son_ir.add_edge(SonEdge::new(control, loop_id, EdgeType::Control));
                }
                
                self.son_ir.add_edge(SonEdge::new(loop_id, condition_id, EdgeType::Control));
                self.son_ir.add_edge(SonEdge::new(condition_id, body_id, EdgeType::Control));
                self.son_ir.add_edge(SonEdge::new(condition_id, merge_id, EdgeType::Control)); // 退出边
                
                self.current_control_flow = Some(merge_id);
                Ok(merge_id)
            }
            
            Statement::Break => {
                let break_id = self.create_break_node();
                
                // 更新控制流
                if let Some(control) = self.current_control_flow {
                    self.son_ir.add_edge(SonEdge::new(control, break_id, EdgeType::Control));
                }
                
                Ok(break_id)
            }
            
            Statement::Continue => {
                let continue_id = self.create_continue_node();
                
                // 更新控制流
                if let Some(control) = self.current_control_flow {
                    self.son_ir.add_edge(SonEdge::new(control, continue_id, EdgeType::Control));
                }
                
                Ok(continue_id)
            }
            
            Statement::Empty => {
                let region_id = self.create_region_node();
                
                // 更新控制流
                if let Some(control) = self.current_control_flow {
                    self.son_ir.add_edge(SonEdge::new(control, region_id, EdgeType::Control));
                    self.current_control_flow = Some(region_id);
                }
                
                Ok(region_id)
            }
        }
    }

    /// 构建表达式
    fn build_expression(&mut self, expr: &Expression) -> Result<SonNodeId, ConversionError> {
        match expr {
            Expression::Literal(literal) => {
                Ok(self.build_literal(literal))
            }
            
            Expression::Identifier { name } => {
                // 查找变量
                if let Some(&node_id) = self.variable_map.get(name) {
                    Ok(node_id)
                } else {
                    // 检查变量是否已声明
                    if !self.variable_declared.get(name).copied().unwrap_or(false) {
                        // 尝试从上下文推断类型，如果无法推断则使用默认类型
                        let inferred_type = self.infer_variable_type_from_context(name);
                        
                        // 创建Local节点
                        let local_id = self.create_local_node(name.clone(), inferred_type);
                        self.variable_map.insert(name.clone(), local_id);
                        self.variable_declared.insert(name.clone(), true);
                        Ok(local_id)
                    } else {
                        // 变量已声明但未找到节点，这是内部错误
                        Err(ConversionError::InternalError(
                            format!("变量 '{}' 已声明但未找到对应节点", name)
                        ))
                    }
                }
            }
            
            Expression::BinaryOperation { operator, left_operand, right_operand } => {
                let left_id = if let AstKind::Expression(expr_kind) = &left_operand.kind {
                    self.build_expression(expr_kind)?
                } else {
                    return Err(ConversionError::UnsupportedNodeType("二元运算左操作数必须是表达式".to_string()));
                };
                let right_id = if let AstKind::Expression(expr_kind) = &right_operand.kind {
                    self.build_expression(expr_kind)?
                } else {
                    return Err(ConversionError::UnsupportedNodeType("二元运算右操作数必须是表达式".to_string()));
                };
                
                let opcode = match operator {
                    BinaryOperator::Add => OpCode::Add,
                    BinaryOperator::Subtract => OpCode::Subtract,
                    BinaryOperator::Multiply => OpCode::Multiply,
                    BinaryOperator::Divide => OpCode::Divide,
                    BinaryOperator::Modulo => OpCode::Modulo,
                    BinaryOperator::Equal => OpCode::Equal,
                    BinaryOperator::NotEqual => OpCode::NotEqual,
                    BinaryOperator::LessThan => OpCode::LessThan,
                    BinaryOperator::LessEqual => OpCode::LessEqual,
                    BinaryOperator::GreaterThan => OpCode::GreaterThan,
                    BinaryOperator::GreaterEqual => OpCode::GreaterEqual,
                    BinaryOperator::LogicalAnd => OpCode::LogicalAnd,
                    BinaryOperator::LogicalOr => OpCode::LogicalOr,
                    BinaryOperator::Assign => OpCode::Store,
                    BinaryOperator::AddAssign => OpCode::Add,
                    BinaryOperator::SubtractAssign => OpCode::Subtract,
                    BinaryOperator::MultiplyAssign => OpCode::Multiply,
                    BinaryOperator::DivideAssign => OpCode::Divide,
                    BinaryOperator::ModuloAssign => OpCode::Modulo,
                };
                
                Ok(self.create_binary_op_node(opcode, Some(left_id), Some(right_id)))
            }
            
            Expression::UnaryOperation { operator, operand } => {
                let operand_id = if let AstKind::Expression(expr_kind) = &operand.kind {
                    self.build_expression(expr_kind)?
                } else {
                    return Err(ConversionError::UnsupportedNodeType("一元运算操作数必须是表达式".to_string()));
                };
                
                let opcode = match operator {
                    UnaryOperator::Minus => OpCode::Minus,
                    UnaryOperator::Plus => OpCode::Constant, // 一元正号操作，直接返回操作数
                    UnaryOperator::LogicalNot => OpCode::LogicalNot,
                    UnaryOperator::BitwiseNot => OpCode::BitwiseNot,
                    UnaryOperator::Dereference => OpCode::Load,
                    UnaryOperator::AddressOf => OpCode::Load, // 使用Load作为地址操作的替代
                    UnaryOperator::Increment => OpCode::Add,
                    UnaryOperator::Decrement => OpCode::Subtract,
                };
                
                Ok(self.create_unary_op_node(opcode, Some(operand_id)))
            }
            
            Expression::Assignment { target, value } => {
                // 提取值表达式
                let value_id = if let AstKind::Expression(expr) = &value.kind {
                    self.build_expression(expr)?
                } else {
                    return Err(ConversionError::UnsupportedNodeType("赋值值必须是表达式".to_string()));
                };
                
                // 处理目标变量
                let target_name = if let AstKind::Expression(Expression::Identifier { name }) = &target.kind {
                    name.clone()
                } else {
                    return Err(ConversionError::UnsupportedNodeType(
                        "复杂的赋值目标暂不支持".to_string()
                    ));
                };
                
                // 检查变量是否已声明
                if !self.variable_declared.get(&target_name).copied().unwrap_or(false) {
                    return Err(ConversionError::UndefinedVariable(format!("变量 '{}' 未声明", target_name)));
                }

                // 创建Store节点
                let store_id = self.create_store_node(
                    target_name.clone(),
                    0, // 默认别名
                    Type::IntType, // 默认类型
                    None, // mem
                    None, // ptr
                    None, // offset
                    Some(value_id),
                    false // init
                );
                
                // 更新变量映射
                self.variable_map.insert(target_name, store_id);
                
                Ok(store_id)
            }
            
            Expression::FunctionCall { function_name, arguments } => {
                let mut arg_ids = Vec::new();
                let mut arg_types = Vec::new();
                
                // 构建参数表达式并收集类型信息
                for arg in arguments {
                    let arg_id = if let AstKind::Expression(expr_kind) = &arg.kind {
                        self.build_expression(expr_kind)?
                    } else {
                        return Err(ConversionError::UnsupportedNodeType("函数调用参数必须是表达式".to_string()));
                    };
                    arg_ids.push(arg_id);
                    
                    // 尝试推断参数类型
                    if let Some(type_system) = self.type_system {
                        // 这里可以进一步优化：从表达式中提取类型信息
                        // 目前使用启发式推断，未来可以从AST中获取类型注解
                        arg_types.push(Type::IntType); // 默认类型
                    }
                }
                
                // 尝试从符号表获取函数返回类型
                let return_type = if let Some(symbol_table) = self.symbol_table {
                    symbol_table.get_function_return_type(function_name)
                        .unwrap_or_else(|| self.current_function_return_type.clone().unwrap_or(Type::VoidType))
                } else {
                    self.current_function_return_type.clone().unwrap_or(Type::VoidType)
                };
                
                // 检查函数参数类型匹配（如果可用）
                if let Some(symbol_table) = self.symbol_table {
                    if let Err(err) = self.check_function_arguments(function_name, &arg_types) {
                        if self.strict_type_checking {
                            return Err(ConversionError::TypeMismatch {
                                expected: "函数参数类型匹配".to_string(),
                                actual: err,
                            });
                        } else {
                            self.type_errors.push(format!(
                                "函数 '{}' 参数类型不匹配: {}", function_name, err
                            ));
                        }
                    }
                }
                
                Ok(self.create_call_node(function_name.clone(), return_type, arg_ids))
            }
            
            Expression::ArrayAccess { array, index } => {
                let array_id = if let AstKind::Expression(expr_kind) = &array.kind {
                    self.build_expression(expr_kind)?
                } else {
                    return Err(ConversionError::UnsupportedNodeType("数组访问的数组必须是表达式".to_string()));
                };
                let index_id = if let AstKind::Expression(expr_kind) = &index.kind {
                    self.build_expression(expr_kind)?
                } else {
                    return Err(ConversionError::UnsupportedNodeType("数组访问的索引必须是表达式".to_string()));
                };
                
                Ok(self.create_array_access_node(Some(array_id), Some(index_id)))
            }
            
            Expression::MemberAccess { object, member_name } => {
                let object_id = if let AstKind::Expression(expr_kind) = &object.kind {
                    self.build_expression(expr_kind)?
                } else {
                    return Err(ConversionError::UnsupportedNodeType("成员访问的对象必须是表达式".to_string()));
                };
                
                Ok(self.create_member_access_node(Some(object_id), member_name.clone()))
            }
            Expression::InitializerList { elements } => {
                // 处理初始化列表
                if elements.is_empty() {
                    // 空初始化列表，创建一个默认值节点
                    let default_value = self.create_constant_node(ConstantValue::Integer(0), Type::IntType);
                    Ok(default_value)
                } else {
                    // 对于非空初始化列表，我们创建一个Region节点来收集所有元素
                    let region_id = self.create_region_node();
                    
                    // 处理每个初始化元素，只处理实际存在的元素
                    for element in elements {
                        // 跳过空的初始化元素（如 {} 或 7 这样的不完整初始化）
                        if let AstKind::Expression(Expression::InitializerList { elements: nested_elements }) = &element.kind {
                            if nested_elements.is_empty() {
                                // 跳过空的嵌套初始化列表
                                continue;
                            }
                        }
                        
                        let element_id = if let AstKind::Expression(expr_kind) = &element.kind {
                            self.build_expression(expr_kind)?
                        } else {
                            // 如果不是表达式，尝试作为语句处理
                            if let AstKind::Statement(stmt_kind) = &element.kind {
                                self.build_statement(stmt_kind)?
                            } else {
                                // 跳过不支持的初始化元素类型，而不是报错
                                continue;
                            }
                        };
                        
                        // 添加数据流边：从元素到Region
                        self.son_ir.add_edge(SonEdge::new(element_id, region_id, EdgeType::Data));
                    }
                    
                    Ok(region_id)
                }
            }
        }
    }

    /// 构建字面量
    fn build_literal(&mut self, literal: &Literal) -> SonNodeId {
        let (value, typ) = match literal {
            Literal::IntegerLiteral(value) => (ConstantValue::Integer(*value as i64), Type::IntType),
            Literal::FloatLiteral(value) => (ConstantValue::Float(*value as f64), Type::FloatType),
            Literal::BooleanLiteral(value) => (ConstantValue::Boolean(*value), Type::BoolType),
            Literal::StringLiteral(value) => (ConstantValue::String(value.clone()), Type::CharType), // 使用CharType作为字符串的替代
        };
        
        self.create_constant_node(value, typ)
    }

    /// 声明变量
    fn declare_variable(&mut self, name: String, typ: Type) -> SonNodeId {
        // 使用 ScopeManager 声明变量
        match self.scope_manager.declare_variable(&name, typ.clone(), None, self.son_ir) {
            Ok(node_id) => {
                // 添加到变量映射（保持向后兼容）
                self.variable_map.insert(name.clone(), node_id);
                self.variable_declared.insert(name, true);
                node_id
            }
            Err(e) => {
                // 如果 ScopeManager 失败，降级到原来的实现
                if self.strict_type_checking {
                    self.type_errors.push(format!("变量声明失败: {}", e));
                }
                let local_id = self.create_local_node(name.clone(), typ);
                self.variable_map.insert(name.clone(), local_id);
                self.variable_declared.insert(name, true);
                local_id
            }
        }
    }

    /// 从上下文推断变量类型
    /// 
    /// 优先使用符号表进行准确类型推断，后备使用启发式推断
    fn infer_variable_type_from_context(&mut self, name: &str) -> Type {
        // 首先检查是否是函数参数（这些在 build_function 中已经处理）
        if let Some(return_type) = &self.current_function_return_type {
            // 如果是函数名，返回函数返回类型
            if name == "return" {
                return return_type.clone();
            }
        }
        
        // 优先使用符号表进行准确类型推断
        if let Some(symbol_table) = self.symbol_table {
            if let Some(symbol) = symbol_table.lookup_symbol(name) {
                match symbol.kind {
                    crate::frontend::SemanticAnalyzer::symbol_table::SymbolKind::Variable |
                    crate::frontend::SemanticAnalyzer::symbol_table::SymbolKind::Parameter => {
                        return symbol.data_type.clone();
                    }
                    crate::frontend::SemanticAnalyzer::symbol_table::SymbolKind::Function => {
                        return symbol.data_type.clone();
                    }
                }
            }
        }
        
        // 如果符号表中没有找到，使用启发式推断（仅作为后备）
        // 注意：这应该在实际编译器中尽量避免，因为不够准确
        if matches!(name, "i" | "j" | "k" | "index" | "count") {
            if self.strict_type_checking {
                self.type_errors.push(format!(
                    "警告：变量 '{}' 类型推断不准确，建议显式声明类型", name
                ));
            }
            return Type::IntType;
        }
        
        if matches!(name, "flag" | "done" | "found" | "valid") {
            if self.strict_type_checking {
                self.type_errors.push(format!(
                    "警告：变量 '{}' 类型推断不准确，建议显式声明类型", name
                ));
            }
            return Type::BoolType;
        }
        
        // 默认返回 IntType，但记录警告
        if self.strict_type_checking {
            self.type_errors.push(format!(
                "警告：变量 '{}' 类型推断失败，使用默认类型 IntType，建议显式声明类型", name
            ));
        }
        Type::IntType
    }

    /// 检查两个类型是否兼容
    /// 
    /// 优先使用类型系统进行准确检查，后备使用基本规则
    fn types_are_compatible(&self, actual: &Type, expected: &Type) -> bool {
        // 优先使用类型系统进行准确检查
        if let Some(type_system) = self.type_system {
            if type_system.is_type_compatible(actual, expected) {
                return true;
            }
        }
        
        // 后备使用基本类型兼容性规则
        match (actual, expected) {
            // 相同类型
            (Type::IntType, Type::IntType) => true,
            (Type::FloatType, Type::FloatType) => true,
            (Type::BoolType, Type::BoolType) => true,
            (Type::CharType, Type::CharType) => true, // 使用CharType替代StringType
            (Type::VoidType, Type::VoidType) => true,
            
            // 类型提升：int -> float
            (Type::IntType, Type::FloatType) => true,
            
            // 其他类型转换（可以根据语言语义进一步细化）
            _ => false,
        }
    }

    /// 查找所有return语句
    fn find_all_return_statements(&mut self, stmt: &'a Statement) -> Vec<&'a Statement> {
        let mut returns = Vec::new();
        self.collect_return_statements(stmt, &mut returns);
        returns
    }

    /// 递归收集 return 语句
    fn collect_return_statements(&self, stmt: &'a Statement, returns: &mut Vec<&'a Statement>) {
        match stmt {
            Statement::Return { .. } => {
                returns.push(stmt);
            }
            Statement::Compound { statements } => {
                for s in statements {
                    if let AstKind::Statement(stmt) = &s.kind {
                        self.collect_return_statements(stmt, returns);
                    }
                }
            }
            Statement::If { then_branch, else_branch, .. } => {
                if let AstKind::Statement(stmt) = &then_branch.kind {
                    self.collect_return_statements(stmt, returns);
                }
                if let Some(else_stmt) = else_branch {
                    if let AstKind::Statement(stmt) = &else_stmt.kind {
                        self.collect_return_statements(stmt, returns);
                    }
                }
            }
            Statement::While { body, .. } => {
                if let AstKind::Statement(stmt) = &body.kind {
                    self.collect_return_statements(stmt, returns);
                }
            }
            Statement::For { body, .. } => {
                if let AstKind::Statement(stmt) = &body.kind {
                    self.collect_return_statements(stmt, returns);
                }
            }
            _ => {}
        }
    }

    /// 查找return语句
    fn find_return_statement<'b>(&self, stmt: &'b Statement) -> Option<&'b Statement> {
        match stmt {
            Statement::Return { .. } => Some(stmt),
            Statement::Compound { statements } => {
                for stmt in statements.iter().rev() {
                    if let AstKind::Statement(stmt) = &stmt.kind {
                        if let Some(found) = self.find_return_statement(stmt) {
                            return Some(found);
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }

    // ========== 节点创建方法 ==========

    /// 创建Start节点
    fn create_start_node(&mut self) -> SonNodeId {
        let kind = SonNodeKind::with_data(
            OpCode::Start,
            NodeData::Start { args: Vec::new() }
        );
        // 让 SonIr 自动分配节点 ID，传入 0 作为占位符
        let node = SonNode::new(0, kind);
        let node_id = self.son_ir.add_node(node);

        node_id
    }

    /// 创建常量节点
    fn create_constant_node(&mut self, value: ConstantValue, typ: Type) -> SonNodeId {
        let kind = SonNodeKind::with_data(
            OpCode::Constant,
            NodeData::Constant { value, typ }
        );
        self.son_ir.add_node(SonNode::new(0, kind))
    }

    /// 创建参数节点
    fn create_parameter_node(&mut self, name: String, typ: Type) -> SonNodeId {
        let name_clone = name.clone();
        let kind = SonNodeKind::with_data(
            OpCode::Parameter,
            NodeData::Parameter { name, typ }
        );
        let node_id = self.son_ir.add_node(SonNode::new(0, kind));

        node_id
    }

    /// 创建局部变量节点
    fn create_local_node(&mut self, name: String, typ: Type) -> SonNodeId {
        let kind = SonNodeKind::with_data(
            OpCode::Local,
            NodeData::Local { name, typ }
        );
        self.son_ir.add_node(SonNode::new(0, kind))
    }

    /// 创建二元运算节点
    fn create_binary_op_node(&mut self, opcode: OpCode, left: Option<SonNodeId>, right: Option<SonNodeId>) -> SonNodeId {
        let mut input_nodes = Vec::new();
        if let Some(left_id) = left {
            input_nodes.push(left_id);
        }
        if let Some(right_id) = right {
            input_nodes.push(right_id);
        }
        
        self.create_node_with_data_edges(
            opcode,
            NodeData::BinaryOp { left, right },
            input_nodes
        )
    }

    /// 创建一元运算节点
    fn create_unary_op_node(&mut self, opcode: OpCode, operand: Option<SonNodeId>) -> SonNodeId {
        let input_nodes = operand.map_or(Vec::new(), |id| vec![id]);
        
        self.create_node_with_data_edges(
            opcode,
            NodeData::UnaryOp { operand },
            input_nodes
        )
    }

    /// 创建函数调用节点
    fn create_call_node(&mut self, function_name: String, return_type: Type, arguments: Vec<SonNodeId>) -> SonNodeId {
        self.create_node_with_data_edges(
            OpCode::Call,
            NodeData::Call { function_name, return_type, arguments: arguments.clone() },
            arguments
        )
    }

    /// 创建Return节点
    fn create_return_node(&mut self, value: Option<SonNodeId>) -> SonNodeId {
        let kind = SonNodeKind::new(OpCode::Return);
        let node_id = self.son_ir.add_node(SonNode::new(0, kind));

        
        // 添加数据依赖边
        if let Some(value_id) = value {
            self.son_ir.add_edge(SonEdge::new(value_id, node_id, EdgeType::Data));
            
            // 类型检查：验证返回值类型与函数返回类型是否匹配
            if let Some(expected_type) = &self.current_function_return_type {
                if let Some(value_node) = self.son_ir.get_node(value_id) {
                    if let Some(value_type) = &value_node.node_type {
                        if !self.types_are_compatible(value_type, expected_type) {
                            let error_msg = format!(
                                "返回值类型 {:?} 与函数返回类型 {:?} 不匹配", 
                                value_type, expected_type
                            );
                            
                            if self.strict_type_checking {
                                // 严格模式：记录错误
                                self.type_errors.push(error_msg);
                            }
                        }
                    }
                }
            }
        } else {
            // 无返回值，检查函数是否应该返回 void
            if let Some(expected_type) = &self.current_function_return_type {
                if !matches!(expected_type, Type::VoidType) {
                    let error_msg = format!(
                        "函数期望返回 {:?} 类型，但没有返回值", 
                        expected_type
                    );
                    
                    if self.strict_type_checking {
                        // 严格模式：记录错误
                        self.type_errors.push(error_msg);
                    }
                }
            }
        }
        
        node_id
    }

    /// 创建If节点
    fn create_if_node(&mut self, condition: Option<SonNodeId>) -> SonNodeId {
        let input_nodes = condition.map_or(Vec::new(), |id| vec![id]);
        
        self.create_node_with_data_edges(
            OpCode::If,
            NodeData::If { condition },
            input_nodes
        )
    }

    /// 创建Loop节点
    fn create_loop_node(&mut self) -> SonNodeId {
        let kind = SonNodeKind::new(OpCode::Loop);
        self.son_ir.add_node(SonNode::new(0, kind))
    }

    /// 创建Region节点
    fn create_region_node(&mut self) -> SonNodeId {
        let kind = SonNodeKind::new(OpCode::Region);
        self.son_ir.add_node(SonNode::new(0, kind))
    }


    
    fn create_phi_node(&mut self, label: String, typ: Type, inputs: Vec<Option<SonNodeId>>) -> SonNodeId {
        let kind = SonNodeKind::with_data(OpCode::Phi, NodeData::Phi { label, typ, inputs: inputs.clone() });
        let phi_id = self.son_ir.add_node(SonNode::new(0, kind));
        
        // 添加数据依赖边
        for input in &inputs {
            if let Some(input_id) = input {
                self.son_ir.add_edge(SonEdge::new(*input_id, phi_id, EdgeType::Data));
            }
        }
        
        phi_id
    }

    /// 创建Break节点
    fn create_break_node(&mut self) -> SonNodeId {
        let kind = SonNodeKind::new(OpCode::Break);
        self.son_ir.add_node(SonNode::new(0, kind))
    }

    /// 创建Continue节点
    fn create_continue_node(&mut self) -> SonNodeId {
        let kind = SonNodeKind::new(OpCode::Continue);
        self.son_ir.add_node(SonNode::new(0, kind))
    }

    /// 创建Store节点
    fn create_store_node(&mut self, name: String, alias: u32, declared_type: Type, 
                        mem: Option<SonNodeId>, ptr: Option<SonNodeId>, offset: Option<SonNodeId>, 
                        value: Option<SonNodeId>, init: bool) -> SonNodeId {
        // 创建Store节点
        let kind = SonNodeKind::with_data(
            OpCode::Store,
            NodeData::Store { name, alias, declared_type, mem, ptr, offset, value, init }
        );
        let store_id = self.son_ir.add_node(SonNode::new(0, kind));
        
        // Store节点只添加输入数据边，不添加输出数据边
        // 因为Store是副作用操作，不产生值
        if let Some(mem_id) = mem {
            self.son_ir.add_edge(SonEdge::new(mem_id, store_id, EdgeType::Data));
        }
        if let Some(ptr_id) = ptr {
            self.son_ir.add_edge(SonEdge::new(ptr_id, store_id, EdgeType::Data));
        }
        if let Some(offset_id) = offset {
            self.son_ir.add_edge(SonEdge::new(offset_id, store_id, EdgeType::Data));
        }
        if let Some(value_id) = value {
            self.son_ir.add_edge(SonEdge::new(value_id, store_id, EdgeType::Data));
        }
        
        store_id
    }

    /// 创建数组访问节点
    fn create_array_access_node(&mut self, array: Option<SonNodeId>, index: Option<SonNodeId>) -> SonNodeId {
        let mut input_nodes = Vec::new();
        if let Some(array_id) = array {
            input_nodes.push(array_id);
        }
        if let Some(index_id) = index {
            input_nodes.push(index_id);
        }
        
        self.create_node_with_data_edges(
            OpCode::ArrayAccess,
            NodeData::ArrayAccess { array, index },
            input_nodes
        )
    }

    /// 创建成员访问节点
    fn create_member_access_node(&mut self, object: Option<SonNodeId>, field: String) -> SonNodeId {
        let input_nodes = object.map_or(Vec::new(), |id| vec![id]);
        
        self.create_node_with_data_edges(
            OpCode::MemberAccess,
            NodeData::MemberAccess { object, field },
            input_nodes
        )
    }

    /// 验证当前构建状态
    fn validate_build_state(&self) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();
        

        
        // 检查所有声明的变量都有对应的节点
        for (var_name, is_declared) in &self.variable_declared {
            if *is_declared && !self.variable_map.contains_key(var_name) {
                let error_msg = format!("变量 '{}' 已声明但未找到对应节点", var_name);
                errors.push(error_msg);
            }
        }
        
        // 检查所有变量映射中的变量都已声明
        for var_name in self.variable_map.keys() {
            if !self.variable_declared.get(var_name).copied().unwrap_or(false) {
                let error_msg = format!("变量 '{}' 有节点但未声明", var_name);
                errors.push(error_msg);
            }
        }
        
        // 检查类型错误
        if !self.type_errors.is_empty() {
            errors.extend(self.type_errors.iter().map(|e| format!("类型错误: {}", e)));
        }
        
        // 检查控制流可达性
        if let Err(control_flow_errors) = self.validate_control_flow() {
            errors.extend(control_flow_errors);
        }
        
        // 检查函数参数类型匹配
        if let Err(param_errors) = self.validate_function_parameters() {
            errors.extend(param_errors);
        }
        
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// 验证控制流可达性
    fn validate_control_flow(&self) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();
        
        let entry_node = self.son_ir.get_entry_node().unwrap_or(0);
        
        // 检查是否有孤立节点（没有输入边）
        for (node_id, node) in self.son_ir.get_all_nodes() {
            if *node_id != entry_node {
                // 检查节点类型，某些节点不需要输入边
                let node_kind = &node.kind;
                let is_parameter_node = matches!(node_kind.opcode, OpCode::Parameter);
                let is_constant_node = matches!(node_kind.opcode, OpCode::Constant);
                let is_start_node = matches!(node_kind.opcode, OpCode::Start);
                let is_local_node = matches!(node_kind.opcode, OpCode::Local);
                let is_scope_node = matches!(node_kind.opcode, OpCode::Scope);
                let is_store_node = matches!(node_kind.opcode, OpCode::Store);
                
                // 这些节点类型不需要输入边
                let needs_inputs = !is_parameter_node && !is_constant_node && !is_start_node && !is_local_node && !is_scope_node && !is_store_node;
                
                if needs_inputs && node.inputs.is_empty() && node.control_inputs.is_empty() {
                    let error_msg = format!("节点 {} ({:?}) 没有输入边，可能不可达", node_id, node_kind.opcode);
                    errors.push(error_msg);
                }
            }
        }
        
        // 检查 break/continue 是否在循环内（简化检查）
        // 注意：完整的检查需要更复杂的控制流分析
        
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// 验证函数参数类型匹配
    fn validate_function_parameters(&self) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();
        
        // 这里可以添加更复杂的参数类型检查
        // 例如：检查参数使用时的类型是否与声明匹配
        
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// 获取构建统计信息
    fn get_build_stats(&self) -> ConversionStats {
        ConversionStats {
            node_count: self.son_ir.node_count(),
            edge_count: self.son_ir.edge_count(),
            function_count: 1,
            variable_count: self.variable_declared.len(),
        }
    }

    /// 获取类型错误列表
    fn get_type_errors(&self) -> &[String] {
        &self.type_errors
    }

    /// 设置类型检查模式
    fn set_type_checking_mode(&mut self, strict: bool) {
        self.strict_type_checking = strict;
    }

    /// 检查是否有类型错误
    fn has_type_errors(&self) -> bool {
        !self.type_errors.is_empty()
    }

    /// 清除类型错误
    fn clear_type_errors(&mut self) {
        self.type_errors.clear();
    }
    
    /// 获取作用域统计信息
    pub fn get_scope_stats(&self) -> crate::ast_to_cfg::ast_to_SoNir::scope_manager::ScopeStats {
        self.scope_manager.get_scope_stats()
    }
    
    /// 打印作用域信息（用于调试）
    pub fn print_scope_info(&self) {
        self.scope_manager.print_scope_info();
    }

    /// 设置语义信息（符号表和类型系统）
    pub fn set_semantic_info(
        &mut self,
        symbol_table: &'a crate::frontend::SemanticAnalyzer::symbol_table::SymbolTable,
        type_system: &'a crate::frontend::SemanticAnalyzer::type_system::TypeSystem,
    ) {
        self.symbol_table = Some(symbol_table);
        self.type_system = Some(type_system);
    }

    /// 从符号表获取变量类型
    pub fn get_variable_type_from_symbol_table(&self, name: &str) -> Option<Type> {
        self.symbol_table
            .and_then(|st| st.get_variable_type(name))
    }

    /// 从符号表获取函数返回类型
    pub fn get_function_return_type_from_symbol_table(&self, name: &str) -> Option<Type> {
        self.symbol_table
            .and_then(|st| st.get_function_return_type(name))
    }

    /// 检查函数参数类型是否匹配
    pub fn check_function_arguments(
        &self,
        function_name: &str,
        arguments: &[Type],
    ) -> Result<(), String> {
        if let Some(type_system) = self.type_system {
            if let Some(symbol) = self.symbol_table.and_then(|st| st.lookup_symbol(function_name)) {
                if let Some(expected_params) = &symbol.parameters {
                    return type_system.check_function_arguments(expected_params, arguments);
                }
            }
        }
        Ok(()) // 如果没有类型系统或符号表，跳过检查
    }
}

impl AstToSonConverter {
    /// 转换AST到Sea of Nodes IR
    pub fn convert(ast: &Ast) -> Result<ConversionResult, ConversionError> {
        Self::convert_with_options(ast, true)
    }

    /// 转换AST到Sea of Nodes IR（可配置类型检查模式）
    pub fn convert_with_options(ast: &Ast, strict_type_checking: bool) -> Result<ConversionResult, ConversionError> {
        let mut son_ir = SonIr::new();
        let mut builder = SonIrBuilder::with_type_checking(&mut son_ir, strict_type_checking);
        
        // 构建函数
        let _function_id = builder.build_function(ast)?;
        
        // 验证构建状态
        let validation_result = builder.validate_build_state();
        if let Err(errors) = validation_result {
            return Err(ConversionError::InternalError(format!("构建状态验证失败: {:?}", errors)));
        }
        
        let stats = builder.get_build_stats();
        let scope_stats = builder.get_scope_stats();
        
        Ok(ConversionResult {
            son_ir,
            stats,
            scope_stats: Some(scope_stats),
        })
    }

    /// 转换AST到Sea of Nodes IR（包含语义分析结果）
    pub fn convert_with_semantic_info(
        ast: &Ast,
        symbol_table: &crate::frontend::SemanticAnalyzer::symbol_table::SymbolTable,
        type_system: &crate::frontend::SemanticAnalyzer::type_system::TypeSystem,
        strict_type_checking: bool,
    ) -> Result<ConversionResult, ConversionError> {
        let mut son_ir = SonIr::new();
        let mut builder = SonIrBuilder::with_semantic_info(
            &mut son_ir,
            symbol_table,
            type_system,
            strict_type_checking,
        );
        
        // 构建函数
        let _function_id = builder.build_function(ast)?;
        
        // 验证构建状态
        let validation_result = builder.validate_build_state();
        if let Err(errors) = validation_result {
            return Err(ConversionError::InternalError(format!("构建状态验证失败: {:?}", errors)));
        }
        
        let stats = builder.get_build_stats();
        let scope_stats = builder.get_scope_stats();
        
        Ok(ConversionResult {
            son_ir,
            stats,
            scope_stats: Some(scope_stats),
        })
    }

    /// 将AST转换为Sea of Nodes IR（带验证）
    pub fn convert_with_validation(ast: &Ast) -> Result<ConversionResult, ConversionError> {
        Self::convert_with_options(ast, true)
    }
}

/// 便利函数：直接转换AST到SoN IR
pub fn convert_ast_to_son(ast: &Ast) -> Result<SonIr, ConversionError> {
    let result = AstToSonConverter::convert(ast)?;
    Ok(result.son_ir)
}

/// 便利函数：转换AST到SoN IR并返回结果
pub fn convert_ast_to_son_with_stats(ast: &Ast) -> Result<ConversionResult, ConversionError> {
    AstToSonConverter::convert_with_validation(ast)
}

/// 验证AST是否适合转换
pub fn validate_ast_for_conversion(ast: &Ast) -> Result<(), Vec<String>> {
    let mut errors = Vec::new();
    
    // 检查是否包含函数定义
    match &ast.kind {
        AstKind::Function { .. } => {
            // 函数定义，可以转换
        }
        _ => {
            errors.push("AST必须包含函数定义".to_string());
        }
    }
    
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// 估算转换统计信息
pub fn estimate_conversion_stats(ast: &Ast) -> ConversionStats {
    // 简单的估算，基于AST结构
    let mut node_count = 0;
    let mut variable_count = 0;
    
    // 递归计算节点数量
    fn count_nodes(ast: &Ast, count: &mut usize) {
        *count += 1;
        match &ast.kind {
            AstKind::Function { parameters, function_body, .. } => {
                *count += parameters.len();
                count_statement_nodes(function_body, count);
            }
            _ => {}
        }
    }
    
    fn count_statement_nodes(stmt: &Ast, count: &mut usize) {
        *count += 1;
        match &stmt.kind {
            AstKind::Statement(Statement::Compound { statements }) => {
                for stmt in statements {
                    count_statement_nodes(stmt, count);
                }
            }
            AstKind::Statement(Statement::ExpressionStatement { expression }) => {
                count_expression_nodes(expression, count);
            }
            AstKind::Statement(Statement::If { condition, then_branch, else_branch }) => {
                count_expression_nodes(condition, count);
                count_statement_nodes(then_branch, count);
                if let Some(else_stmt) = else_branch {
                    count_statement_nodes(else_stmt, count);
                }
            }
            AstKind::Statement(Statement::While { condition, body }) => {
                count_expression_nodes(condition, count);
                count_statement_nodes(body, count);
            }
            AstKind::Statement(Statement::For { initialization, condition, update, body }) => {
                if let Some(init_stmt) = initialization {
                    count_statement_nodes(init_stmt, count);
                }
                if let Some(cond_expr) = condition {
                    count_expression_nodes(cond_expr, count);
                }
                if let Some(update_expr) = update {
                    count_expression_nodes(update_expr, count);
                }
                count_statement_nodes(body, count);
            }
            AstKind::Statement(Statement::Return { value }) => {
                if let Some(expr) = value {
                    count_expression_nodes(expr, count);
                }
            }
            _ => {}
        }
    }
    
    fn count_expression_nodes(expr: &Ast, count: &mut usize) {
        *count += 1;
        match &expr.kind {
            AstKind::Expression(Expression::BinaryOperation { left_operand, right_operand, .. }) => {
                count_expression_nodes(left_operand, count);
                count_expression_nodes(right_operand, count);
            }
            AstKind::Expression(Expression::UnaryOperation { operand, .. }) => {
                count_expression_nodes(operand, count);
            }
            AstKind::Expression(Expression::Assignment { target, value, .. }) => {
                count_expression_nodes(target, count);
                count_expression_nodes(value, count);
            }
            AstKind::Expression(Expression::FunctionCall { arguments, .. }) => {
                for arg in arguments {
                    count_expression_nodes(arg, count);
                }
            }
            AstKind::Expression(Expression::ArrayAccess { array, index }) => {
                count_expression_nodes(array, count);
                count_expression_nodes(index, count);
            }
            AstKind::Expression(Expression::MemberAccess { object, .. }) => {
                count_expression_nodes(object, count);
            }
            _ => {}
        }
    }
    
    count_nodes(ast, &mut node_count);
    
    // 估算边数量（通常是节点数量的1.5倍）
    let edge_count = (node_count as f64 * 1.5) as usize;
    
    ConversionStats {
        node_count,
        edge_count,
        function_count: 1,
        variable_count,
    }
}




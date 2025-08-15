use crate::frontend::ast::{Ast, AstKind, Expression, Statement, Literal, Type, BinaryOperator, UnaryOperator};
use super::son_ir::{SonIr, SonNode, SonNodeKind, SonNodeId, SonEdge, OpCode, NodeData, ConstantValue, EdgeType};
use std::collections::HashMap;

/// AST到Sea of Nodes转换器
pub struct Converter {
    /// AST节点ID到IR节点ID的映射
    ast_to_ir: HashMap<usize, SonNodeId>,
    /// IR节点ID到AST节点ID的映射
    ir_to_ast: HashMap<SonNodeId, usize>,
    /// 当前函数的Start节点ID（用于连接Constant节点）
    current_start_id: Option<SonNodeId>,
}

impl Converter {
    /// 临时节点ID（会被SonIr::add_node自动分配）
    const TEMP_NODE_ID: usize = 0;
    
    /// 创建新的转换器
    pub fn new() -> Self {
        Self {
            ast_to_ir: HashMap::new(),
            ir_to_ast: HashMap::new(),
            current_start_id: None,
        }
    }

    /// 转换AST到Sea of Nodes IR
    pub fn convert(ast: &Ast) -> Result<SonIr, String> {
        let mut converter = Self::new();
        let mut son_ir = SonIr::new();
        
        // 递归下降处理所有AST类型
        let result_id = converter.convert_ast_node(ast, &mut son_ir)?;
        
        // 设置入口和出口节点
        son_ir.set_entry_node(result_id);
        son_ir.set_exit_node(result_id);
        
        Ok(son_ir)
    }
    
    /// 获取或创建IR节点（节点重用）
    fn get_or_create_ir_node(&mut self, ast: &Ast, son_ir: &mut SonIr) -> Result<SonNodeId, String> {
        // 检查是否已经转换过这个AST节点
        if let Some(existing_id) = self.ast_to_ir.get(&ast.span.start_pos) {
            return Ok(*existing_id);
        }
        
        // 创建新的IR节点
        let ir_id = self.create_ir_node(ast, son_ir)?;
        
        // 记录映射关系
        self.ast_to_ir.insert(ast.span.start_pos, ir_id);
        self.ir_to_ast.insert(ir_id, ast.span.start_pos);
        
        Ok(ir_id)
    }
    
    /// 创建IR节点
    fn create_ir_node(&mut self, ast: &Ast, son_ir: &mut SonIr) -> Result<SonNodeId, String> {
        // 检查语义分析是否完成
        if !ast.semantic_info.analyzed {
            return Err("AST节点未完成语义分析".to_string());
        }
        
        // 检查是否有语义错误
        if ast.semantic_info.has_errors() {
            return Err(format!("AST节点存在语义错误: {:?}", ast.semantic_info.semantic_errors));
        }
        
        match &ast.kind {
            AstKind::Program { .. } => {
                // 程序节点不创建IR节点，只处理子节点
                Err("程序节点不创建IR节点".to_string())
            }
            
            AstKind::Function { function_name: _, return_type: _, parameters, function_body } => {
                // 函数：创建Start节点
                // 注意：SonNode::new的第一个参数是临时ID，会被SonIr::add_node自动分配
                let start_node = SonNode::new(Self::TEMP_NODE_ID, SonNodeKind::new(OpCode::Start));
                let start_id = son_ir.add_node(start_node);
                
                // 设置当前函数的Start节点ID，供表达式转换使用
                let old_start_id = self.current_start_id;
                self.current_start_id = Some(start_id);
                
                // 符合第一章设计：Start节点没有输入
                // 参数作为独立节点存在，不连接到Start节点
                for param in parameters {
                    self.convert_ast_node(param, son_ir)?;
                }
                
                // 处理函数体（语句）
                let body_id = self.convert_ast_node(function_body, son_ir)?;
                
                // 符合第一章设计：Return节点使用Start节点作为控制输入
                // 不需要Start指向Return的边，避免形成环
                
                // 恢复之前的Start节点ID
                self.current_start_id = old_start_id;
                
                Ok(start_id)
            }
            
            AstKind::VariableDeclaration { variable_name, variable_type, initial_value, is_const: _ } => {
                // 变量声明：使用推导的类型，而不是声明的类型
                let actual_type = ast.semantic_info.deduced_type
                    .as_ref()
                    .unwrap_or(variable_type);
                
                // 创建Local节点
                let local_id = son_ir.add_node(SonNode::new(Self::TEMP_NODE_ID, SonNodeKind::with_data(
                    OpCode::Local,
                    NodeData::Local {
                        name: variable_name.clone(),
                        typ: actual_type.clone(),
                    }
                )));
                
                // 如果有初始值，处理表达式
                if let Some(init_expr) = initial_value {
                    let init_id = self.get_or_create_ir_node(init_expr, son_ir)?;
                    // 连接初始值到Local节点
                    son_ir.add_edge(SonEdge::new(init_id, local_id, EdgeType::Data));
                }
                
                Ok(local_id)
            }
            
            AstKind::Statement(stmt) => {
                // 使用当前函数的Start节点ID作为控制流起点
                let start_id = self.current_start_id
                    .ok_or_else(|| "语句必须在函数内部使用".to_string())?;
                self.convert_statement(stmt, son_ir, start_id)
            }
            
            AstKind::Expression(expr) => {
                // 使用当前函数的Start节点ID来连接Constant节点
                let start_id = self.current_start_id
                    .ok_or_else(|| "表达式必须在函数内部使用".to_string())?;
                self.convert_expression_with_ast(expr, ast, son_ir, start_id)
            }
            
            AstKind::Type(_) => {
                // 类型节点不产生IR节点
                Err("类型节点不产生IR节点".to_string())
            }
        }
    }
    
    /// 递归下降：处理所有AST节点类型
    fn convert_ast_node(&mut self, ast: &Ast, son_ir: &mut SonIr) -> Result<SonNodeId, String> {
        match &ast.kind {
            AstKind::Program { functions, global_variables } => {
                // 检查函数列表是否为空
                if functions.is_empty() {
                    return Err("程序必须包含至少一个函数".to_string());
                }
                
                // 程序：寻找主函数作为入口点
                let main_function = functions.iter()
                    .find(|func| {
                        if let AstKind::Function { function_name, .. } = &func.kind {
                            function_name == "main"  // 寻找名为main的函数
                        } else {
                            false
                        }
                    })
                    .ok_or_else(|| {
                        format!("程序必须包含main函数作为入口点，当前函数列表: [{}]", 
                            functions.iter()
                                .filter_map(|f| {
                                    if let AstKind::Function { function_name, .. } = &f.kind {
                                        Some(function_name.as_str())
                                    } else {
                                        None
                                    }
                                })
                                .collect::<Vec<_>>()
                                .join(", "))
                    })?;
                
                // 处理全局变量
                for var_decl in global_variables {
                    self.convert_ast_node(var_decl, son_ir)?;
                }
                
                // 处理所有函数（不仅仅是main）
                for func in functions {
                    self.convert_ast_node(func, son_ir)?;
                }
                
                // 返回main函数的ID
                self.get_or_create_ir_node(main_function, son_ir)
            }
            
            _ => {
                // 对于其他节点类型，使用节点重用机制
                self.get_or_create_ir_node(ast, son_ir)
            }
        }
    }
    
    /// 转换语句
    fn convert_statement(&mut self, stmt: &Statement, son_ir: &mut SonIr, control_from: SonNodeId) -> Result<SonNodeId, String> {
        match stmt {
            Statement::Return { value } => {
                if let Some(expr) = value {
                    // 有返回值的return，需要从AST中提取表达式
                    if let AstKind::Expression(expr_kind) = &expr.kind {
                        let value_id = self.convert_expression_with_ast(expr_kind, expr, son_ir, control_from)?;
                        
                        // 创建Return节点
                        let return_node = SonNode::new(Self::TEMP_NODE_ID, SonNodeKind::new(OpCode::Return));
                        let return_id = son_ir.add_node(return_node);
                        
                        // 连接控制流：Start -> Return
                        son_ir.add_edge(SonEdge::new(control_from, return_id, EdgeType::Control));
                        
                        // 连接数据流：Constant -> Return
                        son_ir.add_edge(SonEdge::new(value_id, return_id, EdgeType::Data));
                        
                        Ok(return_id)
                    } else {
                        Err("return值必须是表达式".to_string())
                    }
                } else {
                    // 无返回值的return
                    let return_node = SonNode::new(Self::TEMP_NODE_ID, SonNodeKind::new(OpCode::Return));
                    let return_id = son_ir.add_node(return_node);
                    
                    // 连接控制流：Start -> Return
                    son_ir.add_edge(SonEdge::new(control_from, return_id, EdgeType::Control));
                    
                    Ok(return_id)
                }
            }
            Statement::Compound { statements } => {
                // 复合语句：递归处理其中的语句
                // 第一章只支持return语句，所以复合语句中应该只有return语句
                if statements.len() != 1 {
                    return Err("第一章只支持单个return语句".to_string());
                }
                
                // 处理第一个（也是唯一的）语句
                let first_stmt = &statements[0];
                if let AstKind::Statement(stmt_kind) = &first_stmt.kind {
                    self.convert_statement(stmt_kind, son_ir, control_from)
                } else {
                    Err("复合语句中的项目必须是语句".to_string())
                }
            }
            _ => Err("第一章只支持return语句".to_string()),
        }
    }
    
    /// 转换表达式（带AST语义信息）
    /// start_id: Start节点的ID，用于连接Constant节点（符合第一章设计要求）
    fn convert_expression_with_ast(&mut self, expr: &Expression, ast: &Ast, son_ir: &mut SonIr, start_id: SonNodeId) -> Result<SonNodeId, String> {
        match expr {
            Expression::Literal(literal) => {
                // 使用语义分析推导的类型，而不是硬编码
                let deduced_type = ast.semantic_info.deduced_type
                    .as_ref()
                    .unwrap_or_else(|| {
                        // 如果没有推导类型，使用字面量的自然类型
                        match literal {
                            Literal::IntegerLiteral(_) => &Type::IntType,
                            Literal::FloatLiteral(_) => &Type::FloatType,
                            Literal::BooleanLiteral(_) => &Type::BoolType,
                            _ => &Type::IntType, // 默认类型
                        }
                    });
                
                match literal {
                    Literal::IntegerLiteral(value) => {
                        // i32的所有值都在i64范围内，直接转换即可
                        let int_value = *value as i64;
                        
                        let constant_id = son_ir.add_node(SonNode::new(Self::TEMP_NODE_ID, SonNodeKind::with_data(
                            OpCode::Constant,
                            NodeData::Constant {
                                value: ConstantValue::Integer(int_value),
                                typ: deduced_type.clone(),
                                start_input: None,  // 初始为 None
                            }
                        )));
                        
                        // 创建 Start -> Constant 的虚拟边，仅用于图遍历，不携带语义信息
                        son_ir.add_edge(SonEdge::new(start_id, constant_id, EdgeType::Virtual));
                        
                        Ok(constant_id)
                    }
                    Literal::FloatLiteral(value) => {
                        let constant_id = son_ir.add_node(SonNode::new(Self::TEMP_NODE_ID, SonNodeKind::with_data(
                            OpCode::Constant,
                            NodeData::Constant {
                                value: ConstantValue::Float(*value as f64),
                                typ: deduced_type.clone(),
                                start_input: None,  // 初始为 None
                            }
                        )));
                        
                        // 创建 Start -> Constant 的虚拟边，仅用于图遍历，不携带语义信息
                        son_ir.add_edge(SonEdge::new(start_id, constant_id, EdgeType::Virtual));
                        
                        Ok(constant_id)
                    }
                    Literal::BooleanLiteral(value) => {
                        let constant_id = son_ir.add_node(SonNode::new(Self::TEMP_NODE_ID, SonNodeKind::with_data(
                            OpCode::Constant,
                            NodeData::Constant {
                                value: ConstantValue::Boolean(*value),
                                typ: deduced_type.clone(),
                                start_input: None,  // 初始为 None
                            }
                        )));
                        
                        // 创建 Start -> Constant 的虚拟边，仅用于图遍历，不携带语义信息
                        son_ir.add_edge(SonEdge::new(start_id, constant_id, EdgeType::Virtual));
                        
                        Ok(constant_id)
                    }
                    _ => Err("不支持的常量类型".to_string()),
                }
            }
            Expression::Identifier { name } => {
                // 使用语义分析推导的类型
                let deduced_type = ast.semantic_info.deduced_type
                    .as_ref()
                    .unwrap_or_else(|| {
                        // 如果没有推导类型，这是一个错误，因为标识符必须有类型
                        eprintln!("警告：标识符 '{}' 没有推导类型，使用默认类型 IntType", name);
                        &Type::IntType
                    });
                
                let data = NodeData::Local {
                    name: name.clone(),
                    typ: deduced_type.clone(),
                };
                let local_id = son_ir.add_node(SonNode::new(Self::TEMP_NODE_ID, SonNodeKind::with_data(
                    OpCode::Local,
                    data
                )));
                
                Ok(local_id)
            }
            Expression::BinaryOperation { operator, left_operand, right_operand } => {
                // 递归转换左右操作数
                let left_id = self.convert_ast_node(left_operand, son_ir)?;
                let right_id = self.convert_ast_node(right_operand, son_ir)?;
                
                // 根据操作符创建相应的节点
                let opcode = match operator {
                    BinaryOperator::Add => OpCode::Add,
                    BinaryOperator::Subtract => OpCode::Subtract,
                    BinaryOperator::Multiply => OpCode::Multiply,
                    BinaryOperator::Divide => OpCode::Divide,
                    _ => return Err(format!("不支持的二元操作符: {:?}", operator)),
                };
                
                // 创建二元运算节点
                let binary_node = SonNode::new(Self::TEMP_NODE_ID, SonNodeKind::new(opcode));
                let binary_op_id = son_ir.add_node(binary_node);
                
                // 连接数据流：左操作数 -> 二元运算节点
                son_ir.add_edge(SonEdge::new(left_id, binary_op_id, EdgeType::Data));
                // 连接数据流：右操作数 -> 二元运算节点
                son_ir.add_edge(SonEdge::new(right_id, binary_op_id, EdgeType::Data));
                
                Ok(binary_op_id)
            }
            Expression::UnaryOperation { operator, operand } => {
                // 递归转换操作数
                let operand_id = self.convert_ast_node(operand, son_ir)?;
                
                // 根据操作符创建相应的节点
                let opcode = match operator {
                    UnaryOperator::Minus => OpCode::Minus,
                    _ => return Err(format!("不支持的一元操作符: {:?}", operator)),
                };
                
                // 创建一元运算节点
                let unary_node = SonNode::new(Self::TEMP_NODE_ID, SonNodeKind::new(opcode));
                let unary_op_id = son_ir.add_node(unary_node);
                
                // 连接数据流：操作数 -> 一元运算节点
                son_ir.add_edge(SonEdge::new(operand_id, unary_op_id, EdgeType::Data));
                
                Ok(unary_op_id)
            }
            _ => Err("第一章只支持字面量、标识符和基本算术表达式".to_string()),
        }
    }
    
    /// 获取AST节点对应的IR节点ID
    pub fn get_ir_node(&self, ast_id: usize) -> Option<SonNodeId> {
        self.ast_to_ir.get(&ast_id).copied()
    }
    
    /// 获取IR节点对应的AST节点ID
    pub fn get_ast_node(&self, ir_id: SonNodeId) -> Option<usize> {
        self.ir_to_ast.get(&ir_id).copied()
    }
    
    /// 检查是否已经转换过某个AST节点
    pub fn has_converted(&self, ast_id: usize) -> bool {
        self.ast_to_ir.contains_key(&ast_id)
    }
}

/// 便利函数
pub fn convert_ast_to_son(ast: &Ast) -> Result<SonIr, String> {
    Converter::convert(ast)
}

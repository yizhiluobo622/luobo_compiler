use crate::frontend::ast::{Ast, AstKind, Expression, Statement, Literal, Type, BinaryOperator, UnaryOperator};
use super::tacir::{TACProgram, TACFunction, BasicBlock, TACInstruction, Operand, ConstantValue, BinaryOperator as TACBinaryOp, UnaryOperator as TACUnaryOp};
use super::node_mapping::NodeMapper;

/// 转换上下文，管理转换过程中的可变状态
pub struct ConversionContext {
    /// 当前函数
    pub current_function: Option<TACFunction>,
    /// 当前基本块
    pub current_block: Option<BasicBlock>,
    /// 基本块ID计数器
    pub block_counter: usize,
}

impl ConversionContext {
    pub fn new() -> Self {
        Self {
            current_function: None,
            current_block: None,
            block_counter: 0,
        }
    }
    
    /// 设置当前函数
    pub fn set_current_function(&mut self, function: TACFunction) {
        self.current_function = Some(function);
    }
    
    /// 获取当前函数的可变引用
    pub fn get_current_function_mut(&mut self) -> Option<&mut TACFunction> {
        self.current_function.as_mut()
    }
    
    /// 获取当前函数的不可变引用
    pub fn get_current_function(&self) -> Option<&TACFunction> {
        self.current_function.as_ref()
    }
    
    /// 设置当前基本块
    pub fn set_current_block(&mut self, block: BasicBlock) {
        self.current_block = Some(block);
    }
    
    /// 获取当前基本块的可变引用
    pub fn get_current_block_mut(&mut self) -> Option<&mut BasicBlock> {
        self.current_block.as_mut()
    }
    
    /// 获取当前基本块的不可变引用
    pub fn get_current_block(&self) -> Option<&BasicBlock> {
        self.current_block.as_ref()
    }
    
    /// 获取下一个基本块ID
    pub fn next_block_id(&mut self) -> usize {
        let id = self.block_counter;
        self.block_counter += 1;
        id
    }
    
    /// 获取下一个临时变量ID
    pub fn next_temp_id(&mut self) -> usize {
        if let Some(func) = &mut self.current_function {
            let temp_id = func.temp_counter;
            func.temp_counter += 1;
            temp_id
        } else {
            0
        }
    }
}

/// AST到三地址码IR转换器
pub struct ASTToTACConverter {
    /// 节点映射器
    mapper: NodeMapper,
    /// 转换上下文
    context: ConversionContext,
}

impl ASTToTACConverter {
    pub fn new() -> Self {
        Self {
            mapper: NodeMapper::new(),
            context: ConversionContext::new(),
        }
    }
    
    /// 转换AST到三地址码IR
    pub fn convert(&mut self, ast: &Ast) -> Result<TACProgram, String> {
        let mut program = TACProgram::new();
        
        // 递归下降处理所有AST类型
        self.convert_ast_node(ast, &mut program)?;
        
        Ok(program)
    }
    
    /// 转换AST节点
    fn convert_ast_node(&mut self, ast: &Ast, program: &mut TACProgram) -> Result<Operand, String> {
        // 检查是否已经转换过这个AST节点
        if self.mapper.has_mapping(ast.span.start_pos) {
            return Ok(self.mapper.get_ir_operand(ast.span.start_pos).unwrap().clone());
        }
        
        match &ast.kind {
            AstKind::Program { functions, global_variables } => {
                // 处理全局变量
                for var_decl in global_variables {
                    self.convert_variable_declaration(var_decl, program)?;
                }
                
                // 处理所有函数
                for func in functions {
                    self.convert_ast_node(func, program)?;
                }
                
                // 返回一个虚拟操作数（程序节点不产生值）
                Ok(Operand::Constant(ConstantValue::Integer(0)))
            }
            
            AstKind::Function { function_name, return_type, parameters, function_body } => {
                if let Some(return_type) = return_type {
                    self.convert_function_definition(ast, program)
                } else {
                    Err("函数缺少返回类型".to_string())
                }
            }
            
            AstKind::VariableDeclaration { variable_name, variable_type, initial_value, is_const: _ } => {
                self.convert_variable_declaration(ast, program)
            }
            
            AstKind::Statement(stmt) => {
                self.convert_statement(stmt, program)
            }
            
            AstKind::Expression(expr) => {
                self.convert_expression(expr)
            }
            
            AstKind::Type(_) => {
                Err("类型节点不产生IR".to_string())
            }
        }
    }
    
    /// 转换函数定义
    fn convert_function_definition(&mut self, func_def: &Ast, program: &mut TACProgram) -> Result<Operand, String> {
        if let AstKind::Function { function_name, parameters, return_type, function_body } = &func_def.kind {
            // 创建新函数
            let mut function = TACFunction::new(
                function_name.clone(),
                return_type.clone().unwrap_or(Type::IntType),
            );
            
            // 设置AST参数数量
            function.ast_parameter_count = parameters.len();
            
            // 创建入口基本块
            let entry_block = BasicBlock::new(0);
            function.add_basic_block(entry_block);
            
            // 转换参数
            self.process_function_parameters(parameters, &mut function)?;
            
            // 设置当前函数和基本块
            self.context.set_current_function(function);
            
            // 设置当前基本块 - 避免借用冲突
            self.setup_current_block_from_function()?;
            
            // 转换函数体
            self.convert_ast_node(function_body, program)?;
            
            // 获取并更新函数中的基本块
            self.finalize_function_blocks(program)?;
            
            // 返回一个虚拟操作数（函数定义不产生值）
            Ok(Operand::Constant(ConstantValue::Integer(0)))
        } else {
            Err("不是函数定义".to_string())
        }
    }
    
    /// 处理函数参数，避免借用冲突
    fn process_function_parameters(&mut self, parameters: &[Ast], function: &mut TACFunction) -> Result<(), String> {
        for param in parameters {
            if let AstKind::VariableDeclaration { variable_name, variable_type, .. } = &param.kind {
                // 添加参数到函数
                function.parameters.push((variable_name.clone(), variable_type.clone()));
                
                // 如果是数组类型，添加到数组变量列表
                if let Type::ArrayType { element_type, array_size } = variable_type {
                    if let Some(size) = array_size {
                        // 计算数组的总大小
                        let mut dimensions = vec![*size];
                        let mut current_type = element_type;
                        
                        // 处理多维数组
                        while let Type::ArrayType { element_type: inner_type, array_size: inner_size } = current_type.as_ref() {
                            if let Some(inner_size_val) = inner_size {
                                dimensions.push(*inner_size_val);
                                current_type = inner_type;
                            } else {
                                break;
                            }
                        }
                        
                        function.add_array_variable(
                            variable_name.clone(),
                            variable_type.clone(),
                            dimensions
                        );
                    }
                }
            }
        }
        Ok(())
    }
    
    /// 最终化函数基本块，避免借用冲突
    fn finalize_function_blocks(&mut self, program: &mut TACProgram) -> Result<(), String> {
        // 获取当前函数和基本块，避免同时借用
        let mut function = None;
        let current_block = self.context.get_current_block_mut().cloned();
        
        // 先获取函数
        if let Some(func) = self.context.get_current_function_mut() {
            function = Some(func.clone());
        }
        
        // 如果有当前基本块，更新到函数中
        if let (Some(mut func), Some(block)) = (function, current_block) {
            // 更新第一个基本块
            if let Some(existing_block) = func.get_basic_block(0) {
                let block_index = func.basic_blocks.iter().position(|b| b.id == 0).unwrap_or(0);
                func.basic_blocks[block_index] = block;
            }
            
            // 检查是否需要默认返回语句
            if let Some(block) = func.get_basic_block(0) {
                if !block.instructions.iter().any(|inst| matches!(inst, TACInstruction::Return { .. })) {
                    let mut updated_block = block.clone();
                    updated_block.add_instruction(TACInstruction::Return { value: None });
                    
                    let block_index = func.basic_blocks.iter().position(|b| b.id == 0).unwrap_or(0);
                    func.basic_blocks[block_index] = updated_block;
                }
            }
            
            // 将函数添加到程序
            program.add_function(func);
        }
        
        Ok(())
    }
    
    /// 转换变量声明
    fn convert_variable_declaration(&mut self, ast: &Ast, program: &mut TACProgram) -> Result<Operand, String> {
        if let AstKind::VariableDeclaration { variable_name, variable_type, initial_value, is_const: _ } = &ast.kind {
            // 创建变量操作数
            let var_operand = Operand::Variable(variable_name.clone());
            
            // 记录变量映射
            self.mapper.map_variable(variable_name, var_operand.clone());
            
            // 处理数组类型
            if let Type::ArrayType { element_type, array_size } = variable_type {
                // 如果是数组类型，添加到数组变量列表
                if let Some(size) = array_size {
                    // 计算数组的总大小
                    let mut dimensions = vec![*size];
                    let mut current_type = element_type;
                    
                    // 处理多维数组
                    while let Type::ArrayType { element_type: inner_type, array_size: inner_size } = current_type.as_ref() {
                        if let Some(inner_size_val) = inner_size {
                            dimensions.push(*inner_size_val);
                            current_type = inner_type;
                        } else {
                            break;
                        }
                    }
                    
                    // 如果有当前函数，添加到函数中；否则添加到全局程序
                    if let Some(func) = self.context.get_current_function_mut() {
                        func.add_array_variable(
                            variable_name.clone(),
                            variable_type.clone(),
                            dimensions.clone()
                        );
                    } else {
                        // 全局数组变量，添加到程序的全局数组列表中
                        // 这里需要扩展TACProgram来支持全局数组
                        println!("全局数组变量: {} 维度: {:?}", variable_name, dimensions);
                    }
                    
                    // 保存数组信息到mapper中，供后续使用
                    self.mapper.map_array_info(variable_name, dimensions);
                }
                
                // 为数组分配内存
                // 先计算数组总大小（字节数），避免借用冲突
                let total_elements = self.calculate_array_total_size(variable_type);
                let element_size = self.get_type_size(element_type);
                let total_bytes = total_elements * element_size;
                
                if let Some(block) = self.context.get_current_block_mut() {
                    // 生成内存分配指令
                    block.add_instruction(TACInstruction::Allocate {
                        target: var_operand.clone(),
                        size: Operand::Constant(ConstantValue::Integer(total_bytes as i64)),
                    });
                    
                    println!("为数组 {} 分配 {} 字节内存", variable_name, total_bytes);
                }
            } else {
                // 为普通变量声明生成IR指令（即使没有初始值）
                if let Some(block) = self.context.get_current_block_mut() {
                    // 如果没有初始值，使用默认值
                    let default_value = match variable_type {
                        Type::IntType => Operand::Constant(ConstantValue::Integer(0)),
                        Type::FloatType => Operand::Constant(ConstantValue::Float(0.0)),
                        Type::BoolType => Operand::Constant(ConstantValue::Boolean(false)),
                        Type::CharType => Operand::Constant(ConstantValue::Integer(0)),
                        _ => Operand::Constant(ConstantValue::Integer(0)),
                    };
                    
                    block.add_instruction(TACInstruction::Assign {
                        target: var_operand.clone(),
                        source: default_value,
                    });
                }
            }
            
            // 如果有初始值，转换它
            if let Some(init_value) = initial_value {
                // 对于数组，这里需要特殊处理
                if let Type::ArrayType { .. } = variable_type {
                    // 数组初始化
                    // 如果初始值是初始化列表，需要特殊处理
                    if let AstKind::Expression(Expression::InitializerList { elements }) = &init_value.kind {
                        // 递归处理嵌套初始化列表
                        self.process_array_initialization(variable_name, variable_type, elements, program)?;
                    } else {
                        // 单个值初始化，暂时不处理
                    }
                } else {
                    // 普通变量赋值
                    let init_result = self.convert_ast_node(init_value, program)?;
                    
                    // 使用独立方法存储赋值，避免借用冲突
                    self.store_variable_assignment(&var_operand, init_result)?;
                }
            }
            
            // 记录AST到IR的映射
            self.mapper.map_ast_to_ir(ast.span.start_pos, var_operand.clone());
            
            Ok(var_operand)
        } else {
            Err("不是变量声明".to_string())
        }
    }
    
    /// 转换语句
    fn convert_statement(&mut self, stmt: &Statement, program: &mut TACProgram) -> Result<Operand, String> {
        match stmt {
            Statement::Return { value } => {
                if let Some(expr) = value {
                    // 有返回值的return
                    let value_result = self.convert_ast_node(expr, &mut TACProgram::new())?;
                    
                    // 使用独立方法添加return指令，避免借用冲突
                    self.add_return_instruction(Some(value_result.clone()))?;
                    
                    Ok(value_result)
                } else {
                    // 无返回值的return
                    self.add_return_instruction(None)?;
                    
                    Ok(Operand::Constant(ConstantValue::Integer(0)))
                }
            }
            
            Statement::Compound { statements } => {
                let mut last_result = Operand::Constant(ConstantValue::Integer(0));
                
                // 处理所有语句
                for stmt in statements {
                    // 直接转换AST节点，而不是只处理Statement类型
                    last_result = self.convert_ast_node(stmt, program)?;
                }
                
                Ok(last_result)
            }
            
            Statement::ExpressionStatement { expression } => {
                // 转换表达式语句
                self.convert_ast_node(expression, program)
            }
            
            Statement::If { condition, then_branch, else_branch } => {
                // 转换if语句
                let condition_result = self.convert_ast_node(condition, program)?;
                
                // 创建then和else标签
                let then_label = if let Some(func) = self.context.get_current_function_mut() {
                    func.new_label()
                } else {
                    "then".to_string()
                };
                
                let else_label = if let Some(func) = self.context.get_current_function_mut() {
                    func.new_label()
                } else {
                    "else".to_string()
                };
                
                let end_label = if let Some(func) = self.context.get_current_function_mut() {
                    func.new_label()
                } else {
                    "end".to_string()
                };
                
                // 添加条件跳转指令
                if let Some(block) = self.context.get_current_block_mut() {
                    block.add_instruction(TACInstruction::ConditionalJump {
                        condition: condition_result,
                        true_label: then_label.clone(),
                        false_label: else_label.clone(),
                    });
                }
                
                // 处理then分支
                let then_result = self.convert_ast_node(then_branch, program)?;
                
                // 添加跳转到结束标签的指令
                if let Some(block) = self.context.get_current_block_mut() {
                    block.add_instruction(TACInstruction::Jump {
                        label: end_label.clone(),
                    });
                }
                
                // 处理else分支（如果存在）
                if let Some(else_branch) = else_branch {
                    let else_result = self.convert_ast_node(else_branch, program)?;
                    // 这里可以合并结果，暂时返回then的结果
                }
                
                // 添加结束标签
                if let Some(block) = self.context.get_current_block_mut() {
                    block.add_instruction(TACInstruction::Label {
                        name: end_label,
                    });
                }
                
                Ok(then_result)
            }
            
            Statement::While { condition, body } => {
                // 转换while语句
                let loop_start_label = if let Some(func) = self.context.get_current_function_mut() {
                    func.new_label()
                } else {
                    "loop_start".to_string()
                };
                
                let loop_body_label = if let Some(func) = self.context.get_current_function_mut() {
                    func.new_label()
                } else {
                    "loop_body".to_string()
                };
                
                let loop_end_label = if let Some(func) = self.context.get_current_function_mut() {
                    func.new_label()
                } else {
                    "loop_end".to_string()
                };
                
                // 添加循环开始标签
                if let Some(block) = self.context.get_current_block_mut() {
                    block.add_instruction(TACInstruction::Label {
                        name: loop_start_label.clone(),
                    });
                }
                
                // 转换循环条件
                let condition_result = self.convert_ast_node(condition, program)?;
                
                // 添加条件跳转指令
                if let Some(block) = self.context.get_current_block_mut() {
                    block.add_instruction(TACInstruction::ConditionalJump {
                        condition: condition_result,
                        true_label: loop_body_label.clone(),
                        false_label: loop_end_label.clone(),
                    });
                }
                
                // 处理循环体
                let body_result = self.convert_ast_node(body, program)?;
                
                // 添加跳转回循环开始标签的指令
                if let Some(block) = self.context.get_current_block_mut() {
                    block.add_instruction(TACInstruction::Jump {
                        label: loop_start_label,
                    });
                }
                
                // 添加循环结束标签
                if let Some(block) = self.context.get_current_block_mut() {
                    block.add_instruction(TACInstruction::Label {
                        name: loop_end_label,
                    });
                }
                
                Ok(body_result)
            }
            
            Statement::Break => {
                // 转换break语句
                // 注意：这里需要知道break应该跳转到哪个标签
                // 暂时使用一个占位符标签，实际实现需要更复杂的标签管理
                let break_label = "break_target".to_string();
                
                if let Some(block) = self.context.get_current_block_mut() {
                    block.add_instruction(TACInstruction::Jump {
                        label: break_label,
                    });
                }
                
                Ok(Operand::Constant(ConstantValue::Integer(0)))
            }
            
            Statement::Continue => {
                // 转换continue语句
                // 注意：这里需要知道continue应该跳转到哪个标签
                // 暂时使用一个占位符标签，实际实现需要更复杂的标签管理
                let continue_label = "continue_target".to_string();
                
                if let Some(block) = self.context.get_current_block_mut() {
                    block.add_instruction(TACInstruction::Jump {
                        label: continue_label,
                    });
                }
                
                Ok(Operand::Constant(ConstantValue::Integer(0)))
            }
            
            Statement::Empty => {
                // 空语句，不生成任何指令
                Ok(Operand::Constant(ConstantValue::Integer(0)))
            }
            
            _ => Err(format!("不支持的语句类型: {:?}", stmt)),
        }
    }
    
    /// 转换表达式
    fn convert_expression(&mut self, expr: &Expression) -> Result<Operand, String> {
        match expr {
            Expression::Literal(literal) => {
                let constant_value = match literal {
                    Literal::IntegerLiteral(value) => ConstantValue::Integer(*value as i64),
                    Literal::FloatLiteral(value) => ConstantValue::Float(*value as f64),
                    Literal::BooleanLiteral(value) => ConstantValue::Boolean(*value),
                    _ => return Err("不支持的常量类型".to_string()),
                };
                
                let operand = Operand::Constant(constant_value);
                
                // 记录AST到IR的映射
                if let Some(block) = self.context.get_current_block() {
                    // 这里需要获取AST节点ID，暂时用0
                    self.mapper.map_ast_to_ir(0, operand.clone());
                }
                
                Ok(operand)
            }
            
            Expression::Identifier { name } => {
                // 查找变量
                if let Some(var_operand) = self.mapper.get_variable(name) {
                    Ok(var_operand.clone())
                } else {
                    Err(format!("变量 '{}' 未声明", name))
                }
            }
            
            Expression::BinaryOperation { operator, left_operand, right_operand } => {
                // 递归转换左右操作数
                let left_result = self.convert_ast_node(left_operand, &mut TACProgram::new())?;
                let right_result = self.convert_ast_node(right_operand, &mut TACProgram::new())?;
                
                // 创建临时变量存储结果
                let temp_operand = if let Some(func) = self.context.get_current_function_mut() {
                    func.new_temp()
                } else {
                    Operand::Temp(0) // 临时值
                };
                
                // 转换操作符
                let tac_op = match operator {
                    BinaryOperator::Add => TACBinaryOp::Add,
                    BinaryOperator::Subtract => TACBinaryOp::Subtract,
                    BinaryOperator::Multiply => TACBinaryOp::Multiply,
                    BinaryOperator::Divide => TACBinaryOp::Divide,
                    BinaryOperator::Modulo => TACBinaryOp::Modulo,
                    BinaryOperator::Equal => TACBinaryOp::Equal,
                    BinaryOperator::NotEqual => TACBinaryOp::NotEqual,
                    BinaryOperator::LessThan => TACBinaryOp::Less,
                    BinaryOperator::GreaterThan => TACBinaryOp::Greater,
                    BinaryOperator::LessEqual => TACBinaryOp::LessEqual,
                    BinaryOperator::GreaterEqual => TACBinaryOp::GreaterEqual,
                    BinaryOperator::LogicalAnd => TACBinaryOp::And,
                    BinaryOperator::LogicalOr => TACBinaryOp::Or,
                    _ => return Err(format!("不支持的二元操作符: {:?}", operator)),
                };
                
                // 添加二元运算指令
                if let Some(block) = self.context.get_current_block_mut() {
                    block.add_instruction(TACInstruction::BinaryOp {
                        target: temp_operand.clone(),
                        left: left_result,
                        op: tac_op,
                        right: right_result,
                    });
                }
                
                Ok(temp_operand)
            }
            
            Expression::UnaryOperation { operator, operand } => {
                // 递归转换操作数
                let operand_result = self.convert_ast_node(operand, &mut TACProgram::new())?;
                
                // 创建临时变量存储结果
                let temp_operand = if let Some(func) = self.context.get_current_function_mut() {
                    func.new_temp()
                } else {
                    Operand::Temp(0) // 临时值
                };
                
                // 转换操作符
                let tac_op = match operator {
                    UnaryOperator::Minus => TACUnaryOp::Minus,
                    UnaryOperator::LogicalNot => TACUnaryOp::Not,
                    _ => return Err(format!("不支持的一元操作符: {:?}", operator)),
                };
                
                // 添加一元运算指令
                if let Some(block) = self.context.get_current_block_mut() {
                    block.add_instruction(TACInstruction::UnaryOp {
                        target: temp_operand.clone(),
                        op: tac_op,
                        operand: operand_result,
                    });
                }
                
                Ok(temp_operand)
            }
            
            Expression::Assignment { target, value } => {
                // 转换目标变量
                let target_result = self.convert_ast_node(target, &mut TACProgram::new())?;
                
                // 转换赋值表达式
                let value_result = self.convert_ast_node(value, &mut TACProgram::new())?;
                
                // 添加赋值指令
                if let Some(block) = self.context.get_current_block_mut() {
                    block.add_instruction(TACInstruction::Assign {
                        target: target_result,
                        source: value_result.clone(),
                    });
                }
                
                Ok(value_result)
            }
            
            Expression::FunctionCall { function_name, arguments } => {
                // 转换所有参数
                let mut arg_operands = Vec::new();
                for arg in arguments {
                    let arg_result = self.convert_ast_node(arg, &mut TACProgram::new())?;
                    arg_operands.push(arg_result);
                }
                
                // 创建临时变量存储函数调用结果
                let temp_operand = if let Some(func) = self.context.get_current_function_mut() {
                    func.new_temp()
                } else {
                    Operand::Temp(0) // 临时值
                };
                
                // 添加函数调用指令
                if let Some(block) = self.context.get_current_block_mut() {
                    block.add_instruction(TACInstruction::FunctionCall {
                        target: temp_operand.clone(),
                        function_name: function_name.clone(),
                        arguments: arg_operands,
                    });
                }
                
                Ok(temp_operand)
            }
            
            Expression::InitializerList { elements } => {
                // 初始化列表，需要递归处理嵌套结构
                if elements.is_empty() {
                    // 空初始化列表，返回0
                    Ok(Operand::Constant(ConstantValue::Integer(0)))
                } else {
                    // 处理初始化列表
                    // 只转换第一个元素作为返回值，不生成额外的临时变量
                    let first_element = self.convert_ast_node(&elements[0], &mut TACProgram::new())?;
                    
                    // 其他元素暂时不处理，避免生成过多临时变量
                    // 在实际实现中，这些值应该直接存储到数组中
                    
                    Ok(first_element)
                }
            }
            
            Expression::ArrayAccess { array, index } => {
                // 数组访问，需要计算地址偏移
                let array_result = self.convert_ast_node(array, &mut TACProgram::new())?;
                let index_result = self.convert_ast_node(index, &mut TACProgram::new())?;
                
                // 创建临时变量存储数组访问结果
                let temp_operand = if let Some(func) = self.context.get_current_function_mut() {
                    func.new_temp()
                } else {
                    Operand::Temp(0)
                };
                
                // 生成数组访问指令
                if let Some(block) = self.context.get_current_block_mut() {
                    // 这里应该生成数组访问的指令
                    // 暂时使用简单的赋值，实际实现需要地址计算
                    block.add_instruction(TACInstruction::Assign {
                        target: temp_operand.clone(),
                        source: Operand::Constant(ConstantValue::Integer(0)), // 暂时使用0
                    });
                }
                
                Ok(temp_operand)
            }
            
            Expression::MemberAccess { object, member_name: _ } => {
                // 成员访问，暂时返回对象本身
                self.convert_ast_node(object, &mut TACProgram::new())
            }
            
            _ => Err(format!("不支持的表达式类型: {:?}", expr)),
        }
    }

    /// 递归处理数组初始化列表
    fn process_array_initialization(
        &mut self,
        variable_name: &str,
        variable_type: &Type,
        elements: &[Ast],
        program: &mut TACProgram,
    ) -> Result<(), String> {
        if elements.is_empty() {
            // 空初始化列表，不需要处理
            Ok(())
        } else {
            // 获取数组维度信息
            let array_info = self.get_array_info_cloned(variable_name);
            let dimensions = if let Some((_, dims, _)) = array_info {
                dims
            } else {
                // 如果没有维度信息，回退到简单处理
                return self.process_simple_array_initialization(variable_name, elements, program);
            };
            
            // 处理初始化列表
            for (i, element) in elements.iter().enumerate() {
                // 检查元素是否是嵌套的初始化列表
                if let AstKind::Expression(Expression::InitializerList { elements: nested_elements }) = &element.kind {
                    // 递归处理嵌套的初始化列表
                    // 对于嵌套列表，需要计算正确的多维索引
                    self.process_nested_array_initialization_simple(variable_name, variable_type, nested_elements, i, &dimensions, program)?;
                } else {
                    // 单个值，生成数组元素存储指令
                    let element_result = self.convert_ast_node(element, program)?;
                    
                    // 计算正确的索引
                    let indices = self.calculate_array_indices(i, &dimensions);
                    self.store_array_element(variable_name, &indices, element_result)?;
                }
            }
            Ok(())
        }
    }
    
    /// 处理嵌套的数组初始化列表（修复版本）
    fn process_nested_array_initialization_simple(
        &mut self,
        variable_name: &str,
        variable_type: &Type,
        elements: &[Ast],
        outer_index: usize,
        dimensions: &[usize],
        program: &mut TACProgram,
    ) -> Result<(), String> {
        if elements.is_empty() {
            Ok(())
        } else {
            for (j, element) in elements.iter().enumerate() {
                let element_result = self.convert_ast_node(element, program)?;
                
                // 根据数组维度计算正确的索引
                let indices = if dimensions.len() >= 2 {
                    if dimensions.len() == 2 {
                        vec![outer_index, j]
                    } else if dimensions.len() == 3 {
                        // 三维数组：outer_index 是第一维，j 是第二维
                        vec![outer_index, j, 0] // 第三维默认为0
                    } else {
                        vec![outer_index, j]
                    }
                } else {
                    vec![outer_index]
                };
                
                self.store_array_element(variable_name, &indices, element_result)?;
            }
            Ok(())
        }
    }
    
    /// 计算一维数组的索引
    fn calculate_array_indices(&self, linear_index: usize, dimensions: &[usize]) -> Vec<usize> {
        if dimensions.len() == 1 {
            vec![linear_index]
        } else if dimensions.len() == 2 {
            // 二维数组：将线性索引转换为二维索引
            let row = linear_index / dimensions[1];
            let col = linear_index % dimensions[1];
            vec![row, col]
        } else if dimensions.len() == 3 {
            // 三维数组：将线性索引转换为三维索引
            let total_size_2d = dimensions[1] * dimensions[2];
            let i = linear_index / total_size_2d;
            let remaining = linear_index % total_size_2d;
            let j = remaining / dimensions[2];
            let k = remaining % dimensions[2];
            
            // 确保索引在有效范围内
            if i < dimensions[0] && j < dimensions[1] && k < dimensions[2] {
                vec![i, j, k]
            } else {
                // 如果索引超出范围，使用回退策略
                vec![linear_index]
            }
        } else {
            // 多维数组：暂时使用线性索引
            vec![linear_index]
        }
    }
    
    /// 计算二维数组的索引
    fn calculate_array_indices_2d(&self, row: usize, col: usize, dimensions: &[usize]) -> Vec<usize> {
        if dimensions.len() >= 2 {
            vec![row, col]
        } else {
            vec![row]
        }
    }
    
    /// 简单的数组初始化处理（回退方案）
    fn process_simple_array_initialization(
        &mut self,
        variable_name: &str,
        elements: &[Ast],
        program: &mut TACProgram,
    ) -> Result<(), String> {
        for (i, element) in elements.iter().enumerate() {
            let element_result = self.convert_ast_node(element, program)?;
            self.store_array_element(variable_name, &[i], element_result)?;
        }
        Ok(())
    }
    
    /// 存储数组元素，避免借用冲突
    fn store_array_element(&mut self, variable_name: &str, indices: &[usize], value: Operand) -> Result<(), String> {
        // 先获取当前基本块，避免同时借用
        let block = self.context.get_current_block_mut().cloned();
        
        if let Some(mut block) = block {
            self.generate_array_store_simple(variable_name, indices, value, &mut block)?;
            
            // 更新基本块
            self.context.set_current_block(block);
        }
        Ok(())
    }
    
    /// 生成数组存储指令（修复借用冲突版本）
    fn generate_array_store_simple(
        &mut self,
        variable_name: &str,
        indices: &[usize],
        value: Operand,
        block: &mut BasicBlock,
    ) -> Result<(), String> {
        // 先获取数组信息，避免后续多次借用
        let array_info = self.get_array_info_cloned(variable_name);
        
        if let Some((_, dimensions, _)) = array_info {
            // 计算数组元素的地址偏移
            let offset_operand = self.calculate_array_offset_simple(variable_name, &dimensions, indices, block)?;
            
            // 生成存储指令：store(value, base + offset)
            let base_operand = Operand::Variable(variable_name.to_string());
            
            // 生成临时变量用于地址计算
            let temp_id = self.context.next_temp_id();
            
            let addr_operand = Operand::Temp(temp_id);
            
            // addr = base + offset
            block.add_instruction(TACInstruction::BinaryOp {
                target: addr_operand.clone(),
                left: base_operand,
                op: TACBinaryOp::Add,
                right: offset_operand,
            });
            
            // 生成真正的存储指令
            block.add_instruction(TACInstruction::Store {
                value: value,
                address: addr_operand,
            });
            
            return Ok(());
        }
        
        // 如果没有维度信息，回退到简单的索引形式
        let array_element = Operand::Variable(format!("{}[{}]", variable_name, indices.iter().map(|i| i.to_string()).collect::<Vec<_>>().join("][")));
        block.add_instruction(TACInstruction::Assign {
            target: array_element,
            source: value,
        });
        
        Ok(())
    }
    
    /// 获取数组信息的克隆版本，避免借用冲突
    fn get_array_info_cloned(&self, variable_name: &str) -> Option<(Type, Vec<usize>, usize)> {
        // 首先尝试从NodeMapper获取数组信息
        if let Some(dimensions) = self.mapper.get_array_info(variable_name) {
            // 从NodeMapper获取维度信息，类型和大小暂时使用默认值
            return Some((Type::IntType, dimensions.clone(), 0));
        }
        
        // 如果NodeMapper中没有，尝试从当前函数获取
        if let Some(func) = self.context.get_current_function() {
            func.get_array_info(variable_name).cloned()
        } else {
            None
        }
    }
    
    /// 获取下一个临时变量ID，避免借用冲突
    fn get_next_temp_id(&mut self) -> usize {
        if let Some(func) = &mut self.context.get_current_function_mut() {
            let temp_id = func.temp_counter;
            func.temp_counter += 1;
            temp_id
        } else {
            // 如果没有当前函数，使用简单的ID生成策略
            0
        }
    }
    
    /// 计算数组元素的地址偏移（修复版本）
    fn calculate_array_offset_simple(
        &mut self,
        variable_name: &str,
        dimensions: &[usize],
        indices: &[usize],
        block: &mut BasicBlock,
    ) -> Result<Operand, String> {
        if indices.len() != dimensions.len() {
            return Err(format!("索引数量与数组维度不匹配: 索引数量={}, 维度数量={}", 
                indices.len(), dimensions.len()));
        }
        
        // 计算偏移：offset = i0 * stride0 + i1 * stride1 + i2 * stride2 + ...
        let mut offset_operand = Operand::Constant(ConstantValue::Integer(0));
        
        for (i, (index, dimension)) in indices.iter().zip(dimensions.iter()).enumerate() {
            let index_operand = Operand::Constant(ConstantValue::Integer(*index as i64));
            let stride_operand = Operand::Constant(ConstantValue::Integer(*dimension as i64));
            
            // 计算当前维度的偏移：index * stride
            let temp_offset = Operand::Temp(self.context.next_temp_id());
            
            block.add_instruction(TACInstruction::BinaryOp {
                target: temp_offset.clone(),
                left: index_operand,
                op: TACBinaryOp::Multiply,
                right: stride_operand,
            });
            
            // 累加到总偏移
            let new_offset = Operand::Temp(self.context.next_temp_id());
            
            block.add_instruction(TACInstruction::BinaryOp {
                target: new_offset.clone(),
                left: offset_operand,
                op: TACBinaryOp::Add,
                right: temp_offset,
            });
            
            offset_operand = new_offset;
        }
        
        Ok(offset_operand)
    }

    /// 生成简单的数组存储指令（避免借用问题）
    fn generate_simple_array_store(
        &mut self,
        variable_name: &str,
        indices: &[usize],
        value: Operand,
        block: &mut BasicBlock,
    ) -> Result<(), String> {
        // 生成简单的数组索引形式，但保持标准三地址码结构
        let array_element = Operand::Variable(format!("{}[{}]", variable_name, indices.iter().map(|i| i.to_string()).collect::<Vec<_>>().join("][")));
        
        // 使用标准的Assign指令
        block.add_instruction(TACInstruction::Assign {
            target: array_element,
            source: value,
        });
        
        Ok(())
    }

    /// 存储变量赋值，避免借用冲突
    fn store_variable_assignment(&mut self, target: &Operand, value: Operand) -> Result<(), String> {
        // 先获取当前基本块，避免同时借用
        let block = self.context.get_current_block_mut().cloned();
        
        if let Some(mut block) = block {
            block.add_instruction(TACInstruction::Assign {
                target: target.clone(),
                source: value.clone(),
            });
            
            // 更新基本块
            self.context.set_current_block(block);
        }
        Ok(())
    }

    /// 添加返回指令，避免借用冲突
    fn add_return_instruction(&mut self, value: Option<Operand>) -> Result<(), String> {
        // 先获取当前基本块，避免同时借用
        let block = self.context.get_current_block_mut().cloned();
        
        if let Some(mut block) = block {
            block.add_instruction(TACInstruction::Return { value });
            
            // 更新基本块
            self.context.set_current_block(block);
        }
        Ok(())
    }

    /// 设置当前基本块，避免借用冲突
    fn setup_current_block_from_function(&mut self) -> Result<(), String> {
        // 先获取函数，避免同时借用
        let function = self.context.get_current_function_mut().cloned();
        
        if let Some(func) = function {
            if let Some(block) = func.get_basic_block(0) {
                self.context.set_current_block(block.clone());
                return Ok(());
            }
        }
        Err("无法获取函数入口基本块".to_string())
    }

    /// 计算数组总大小（字节数）
    fn calculate_array_total_size(&self, array_type: &Type) -> usize {
        let mut total_elements = 1;
        let mut current_type = array_type;

        while let Type::ArrayType { element_type, array_size: size } = current_type {
            if let Some(size_val) = size {
                total_elements *= *size_val;
            } else {
                // 如果数组大小未知，假设为1
                total_elements *= 1;
            }
            current_type = element_type;
        }
        total_elements
    }

    /// 获取类型大小（字节数）
    fn get_type_size(&self, t: &Type) -> usize {
        match t {
            Type::IntType => 4,
            Type::FloatType => 4,
            Type::BoolType => 1,
            Type::CharType => 1,
            Type::ArrayType { element_type, array_size: _ } => self.get_type_size(element_type),
            _ => 1, // 默认大小
        }
    }
    
    /// 生成数组元素加载指令
    fn generate_array_load_simple(
        &mut self,
        variable_name: &str,
        indices: &[usize],
        target: Operand,
        block: &mut BasicBlock,
    ) -> Result<(), String> {
        // 获取数组信息
        let array_info = self.get_array_info_cloned(variable_name);
        
        if let Some((_, dimensions, _)) = array_info {
            // 计算数组元素的地址偏移
            let offset_operand = self.calculate_array_offset_simple(variable_name, &dimensions, indices, block)?;
            
            // 生成地址计算指令
            let base_operand = Operand::Variable(variable_name.to_string());
            let temp_id = self.context.next_temp_id();
            let addr_operand = Operand::Temp(temp_id);
            
            // addr = base + offset
            block.add_instruction(TACInstruction::BinaryOp {
                target: addr_operand.clone(),
                left: base_operand,
                op: TACBinaryOp::Add,
                right: offset_operand,
            });
            
            // 生成加载指令
            block.add_instruction(TACInstruction::Load {
                target: target,
                address: addr_operand,
            });
            
            Ok(())
        } else {
            // 回退到简单形式
            let array_element = Operand::Variable(format!("{}[{}]", variable_name, indices.iter().map(|i| i.to_string()).collect::<Vec<_>>().join("][")));
            block.add_instruction(TACInstruction::Assign {
                target: target,
                source: array_element,
            });
            Ok(())
        }
    }
}

use crate::frontend::ast::{Ast, AstKind, Expression, Statement, Literal, Type, BinaryOperator, UnaryOperator};
use super::tacir::{TACProgram, TACFunction, BasicBlock, TACInstruction, Operand, ConstantValue, BinaryOperator as TACBinaryOp, UnaryOperator as TACUnaryOp};
use super::node_mapping::NodeMapper;

/// AST到三地址码IR转换器
pub struct ASTToTACConverter {
    /// 节点映射器
    mapper: NodeMapper,
    /// 当前函数
    current_function: Option<TACFunction>,
    /// 当前基本块
    current_block: Option<BasicBlock>,
    /// 基本块ID计数器
    block_counter: usize,
}

impl ASTToTACConverter {
    pub fn new() -> Self {
        Self {
            mapper: NodeMapper::new(),
            current_function: None,
            current_block: None,
            block_counter: 0,
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
                    self.convert_function_definition(function_name, return_type, parameters, function_body, program)
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
    fn convert_function_definition(&mut self, function_name: &str, return_type: &Type, parameters: &[Ast], function_body: &Ast, program: &mut TACProgram) -> Result<Operand, String> {
        // 创建新函数
        let mut function = TACFunction::new(function_name.to_string(), return_type.clone());
        
        // 设置AST参数数量
        function.ast_parameter_count = parameters.len();
        
        // 创建入口基本块
        let entry_block = BasicBlock::new(self.block_counter);
        self.block_counter += 1;
        let entry_block_id = function.add_basic_block(entry_block);
        
        // 设置当前函数和基本块
        self.current_function = Some(function.clone());
        self.current_block = Some(function.get_basic_block(entry_block_id).unwrap().clone());
        
        // 清空映射器（新函数作用域）
        self.mapper.clear_mappings();
        
        // 设置参数（在清空映射器之后）
        for param in parameters {
            if let AstKind::VariableDeclaration { variable_name, variable_type, .. } = &param.kind {
                // 直接修改current_function中的参数
                if let Some(func) = &mut self.current_function {
                    func.parameters.push((variable_name.clone(), variable_type.clone()));
                }
                
                // 将参数添加到变量映射器（作为局部变量）
                let param_operand = Operand::Variable(variable_name.clone());
                self.mapper.map_variable(variable_name, param_operand.clone());
                
                // 记录AST到IR的映射
                self.mapper.map_ast_to_ir(param.span.start_pos, param_operand);
            }
        }
        
        // 处理函数体
        let body_result = self.convert_ast_node(function_body, program)?;
        
        // 如果函数体没有显式return，添加一个
        if let Some(block) = &mut self.current_block {
            if !block.instructions.iter().any(|inst| matches!(inst, TACInstruction::Return { .. })) {
                block.add_instruction(TACInstruction::Return { value: None });
            }
        }
        
        // 将当前基本块添加到函数中
        if let Some(block) = self.current_block.take() {
            if let Some(func) = &mut self.current_function {
                // 使用数组索引而不是block.id来访问
                let block_index = func.basic_blocks.iter().position(|b| b.id == entry_block_id).unwrap_or(0);
                func.basic_blocks[block_index] = block;
            }
        }
        
        // 将函数添加到程序中
        let function_id = program.add_function(self.current_function.take().unwrap());
        
        // 返回函数ID（作为操作数）
        Ok(Operand::Constant(ConstantValue::Integer(function_id as i64)))
    }
    
    /// 转换变量声明
    fn convert_variable_declaration(&mut self, ast: &Ast, program: &mut TACProgram) -> Result<Operand, String> {
        if let AstKind::VariableDeclaration { variable_name, variable_type, initial_value, is_const: _ } = &ast.kind {
            // 创建变量操作数
            let var_operand = Operand::Variable(variable_name.clone());
            
            // 记录变量映射
            self.mapper.map_variable(variable_name, var_operand.clone());
            
            // 为变量声明生成IR指令（即使没有初始值）
            if let Some(block) = &mut self.current_block {
                // 如果没有初始值，使用默认值
                let default_value = match variable_type {
                    Type::IntType => Operand::Constant(ConstantValue::Integer(0)),
                    Type::FloatType => Operand::Constant(ConstantValue::Float(0.0)),
                    Type::BoolType => Operand::Constant(ConstantValue::Boolean(false)),
                    _ => Operand::Constant(ConstantValue::Integer(0)), // 默认
                };
                
                // 添加变量声明指令
                block.add_instruction(TACInstruction::Assign {
                    target: var_operand.clone(),
                    source: default_value,
                });
            }
            
            // 如果有初始值，覆盖默认值
            if let Some(init_expr) = initial_value {
                let init_result = self.convert_ast_node(init_expr, program)?;
                
                // 添加赋值指令
                if let Some(block) = &mut self.current_block {
                    block.add_instruction(TACInstruction::Assign {
                        target: var_operand.clone(),
                        source: init_result,
                    });
                }
            }
            
            // 记录AST到IR的映射
            self.mapper.map_ast_to_ir(ast.span.start_pos, var_operand.clone());
            
            Ok(var_operand)
        } else {
            Err("不是变量声明节点".to_string())
        }
    }
    
    /// 转换语句
    fn convert_statement(&mut self, stmt: &Statement, program: &mut TACProgram) -> Result<Operand, String> {
        match stmt {
            Statement::Return { value } => {
                if let Some(expr) = value {
                    // 有返回值的return
                    let value_result = self.convert_ast_node(expr, &mut TACProgram::new())?;
                    
                    // 添加return指令
                    if let Some(block) = &mut self.current_block {
                        block.add_instruction(TACInstruction::Return {
                            value: Some(value_result.clone()),
                        });
                    }
                    
                    Ok(value_result)
                } else {
                    // 无返回值的return
                    if let Some(block) = &mut self.current_block {
                        block.add_instruction(TACInstruction::Return { value: None });
                    }
                    
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
            
            _ => Err("不支持的语句类型".to_string()),
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
                if let Some(block) = &self.current_block {
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
                let temp_operand = if let Some(func) = &mut self.current_function {
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
                    _ => return Err(format!("不支持的二元操作符: {:?}", operator)),
                };
                
                // 添加二元运算指令
                if let Some(block) = &mut self.current_block {
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
                let temp_operand = if let Some(func) = &mut self.current_function {
                    func.new_temp()
                } else {
                    Operand::Temp(0) // 临时值
                };
                
                // 转换操作符
                let tac_op = match operator {
                    UnaryOperator::Minus => TACUnaryOp::Minus,
                    _ => return Err(format!("不支持的一元操作符: {:?}", operator)),
                };
                
                // 添加一元运算指令
                if let Some(block) = &mut self.current_block {
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
                if let Some(block) = &mut self.current_block {
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
                let temp_operand = if let Some(func) = &mut self.current_function {
                    func.new_temp()
                } else {
                    Operand::Temp(0) // 临时值
                };
                
                // 添加函数调用指令
                if let Some(block) = &mut self.current_block {
                    block.add_instruction(TACInstruction::FunctionCall {
                        target: temp_operand.clone(),
                        function_name: function_name.clone(),
                        arguments: arg_operands,
                    });
                }
                
                Ok(temp_operand)
            }
            
            _ => Err("不支持的表达式类型".to_string()),
        }
    }
}

use crate::frontend::ast::{Ast, AstKind, Expression, Statement, Literal, Type, BinaryOperator, UnaryOperator};
use super::tacir::{TACProgram, TACFunction, BasicBlock, TACInstruction, Operand, ConstantValue, BinaryOperator as TACBinaryOp, UnaryOperator as TACUnaryOp};
use super::node_mapping::NodeMapper;
use std::collections::HashMap;

/// 转换错误类型
#[derive(Debug, Clone)]
pub struct ConversionError {
    pub message: String,
}

impl ConversionError {
    pub fn new(message: String) -> Self {
        Self { message }
    }
}

impl std::fmt::Display for ConversionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Conversion error: {}", self.message)
    }
}

impl std::error::Error for ConversionError {}

impl From<String> for ConversionError {
    fn from(message: String) -> Self {
        ConversionError::new(message)
    }
}

/// 转换上下文，管理转换过程中的可变状态
pub struct ConversionContext {
    /// 当前函数
    pub current_function: Option<TACFunction>,
    /// 当前基本块
    pub current_block: Option<BasicBlock>,
    /// 基本块ID计数器
    pub block_counter: usize,
    /// 标签到基本块的映射
    pub label_to_block: HashMap<String, usize>,
    /// 当前基本块的ID
    pub current_block_id: usize,
}

impl ConversionContext {
    pub fn new() -> Self {
        Self {
            current_function: None,
            current_block: None,
            block_counter: 0,
            label_to_block: HashMap::new(),
            current_block_id: 0,
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
    
    /// 注册标签到基本块的映射
    pub fn register_label(&mut self, label: String, block_id: usize) {
        self.label_to_block.insert(label, block_id);
    }
    
    /// 获取标签对应的基本块ID
    pub fn get_block_for_label(&self, label: &str) -> Option<usize> {
        self.label_to_block.get(label).copied()
    }
    
    /// 设置当前基本块ID
    pub fn set_current_block_id(&mut self, block_id: usize) {
        self.current_block_id = block_id;
    }
    
    /// 获取当前基本块ID
    pub fn get_current_block_id(&self) -> usize {
        self.current_block_id
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
    
    /// 获取mapper的引用
    pub fn get_mapper(&self) -> &NodeMapper {
        &self.mapper
    }
    
    /// 创建新的基本块
    fn create_new_block(&mut self) -> Result<BasicBlock, String> {
        let block_id = self.context.next_block_id();
        let mut new_block = BasicBlock::new(block_id);
        
        // 设置前驱和后继（暂时为空，后续填充）
        new_block.predecessors = Vec::new();
        new_block.successors = Vec::new();
        
        Ok(new_block)
    }
    
    /// 结束当前基本块
    fn finalize_current_block(&mut self) -> Result<(), String> {
        // 先获取当前块，避免同时借用
        let current_block = self.context.get_current_block().cloned();
        
        if let Some(block) = current_block {
            // 将当前块添加到函数中
            if let Some(func) = self.context.get_current_function_mut() {
                func.add_basic_block(block);
            }
        }
        Ok(())
    }
    
    /// 切换到新的基本块
    fn switch_to_new_block(&mut self) -> Result<(), String> {
        // 结束当前块
        self.finalize_current_block()?;
        
        // 创建新块
        let new_block = self.create_new_block()?;
        let block_id = new_block.id;
        
        // 设置新块为当前块
        self.context.set_current_block(new_block);
        self.context.set_current_block_id(block_id);
        
        Ok(())
    }
    
    /// 获取或创建标签对应的基本块
    fn get_or_create_block_for_label(&mut self, label: &str) -> Result<BasicBlock, String> {
        // 检查标签是否已有对应的块
        if let Some(block_id) = self.context.get_block_for_label(label) {
            // 如果标签已存在，获取对应的块
            if let Some(func) = self.context.get_current_function_mut() {
                if let Some(existing_block) = func.get_basic_block(block_id) {
                    return Ok(existing_block.clone());
                }
            }
        }
        
        // 创建新块
        let new_block = self.create_new_block()?;
        let block_id = new_block.id;
        
        // 注册标签到块的映射
        self.context.register_label(label.to_string(), block_id);
        
        Ok(new_block)
    }
    
    /// 处理标签指令，创建新块
    fn handle_label_instruction(&mut self, label_name: &str) -> Result<(), String> {
        // 切换到新块
        self.switch_to_new_block()?;
        
        // 获取或创建标签对应的块
        let label_block = self.get_or_create_block_for_label(label_name)?;
        let block_id = label_block.id;
        
        // 设置为当前块
        self.context.set_current_block(label_block);
        self.context.set_current_block_id(block_id);
        
        Ok(())
    }
    
    /// 处理跳转指令，建立块间关系
    fn handle_jump_instruction(&mut self, target_label: &str) -> Result<(), String> {
        // 获取目标块ID
        let target_block_id = if let Some(block_id) = self.context.get_block_for_label(target_label) {
            block_id
        } else {
            // 如果目标标签还没有对应的块，创建一个占位块
            let placeholder_block = self.create_new_block()?;
            let placeholder_id = placeholder_block.id;
            self.context.register_label(target_label.to_string(), placeholder_id);
            
            // 将占位块添加到函数中
            if let Some(func) = self.context.get_current_function_mut() {
                func.add_basic_block(placeholder_block);
            }
            
            placeholder_id
        };
        
        // 建立当前块到目标块的后继关系
        if let Some(block) = self.context.get_current_block_mut() {
            if !block.successors.contains(&target_block_id) {
                block.successors.push(target_block_id);
            }
        }
        
        // 建立目标块到当前块的前驱关系
        let current_block_id = self.context.get_current_block_id();
        if let Some(func) = self.context.get_current_function_mut() {
            if let Some(target_block) = func.get_basic_block_mut(target_block_id) {
                if !target_block.predecessors.contains(&current_block_id) {
                    target_block.predecessors.push(current_block_id);
                }
            }
        }
        
        Ok(())
    }
    
    /// 从AST推断数组维度
    /// 仅依赖语义分析阶段填充的 deduced_type，不做变量名启发式
    fn infer_array_dimensions_from_ast(&self, ast: &crate::frontend::ast::Ast, _variable_name: &str) -> Vec<usize> {
        if let Some(deduced_type) = &ast.semantic_info.deduced_type {
            let mut dims: Vec<usize> = Vec::new();
            let mut ty = deduced_type.clone();
            loop {
                match ty {
                    crate::frontend::ast::Type::ArrayType { element_type, array_size } => {
                        match array_size {
                            crate::frontend::ast::ArraySize::Fixed(n) => dims.push(n),
                            _ => return Vec::new(),
                        }
                        ty = *element_type;
                    }
                    _ => break,
                }
            }
            return dims;
        }
        Vec::new()
    }
    
    /// 转换AST到三地址码IR
    pub fn convert(&mut self, ast: &Ast) -> Result<TACProgram, ConversionError> {
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
                    if let AstKind::VariableDeclaration { variable_name, variable_type, is_const, .. } = &var_decl.kind {
                    }
                    if let AstKind::VariableDeclaration { is_const, .. } = &var_decl.kind {
                        self.convert_variable_declaration_with_const_info(var_decl, program, *is_const)?;
                    }
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
            
            AstKind::VariableDeclaration { variable_name, variable_type, initial_value, is_const } => {
                self.convert_variable_declaration_with_const_info(ast, program, *is_const)
            }
            
            AstKind::Statement(stmt) => {
                self.convert_statement(stmt, program)
            }
            
            AstKind::Expression(expr) => {
                self.convert_expression(expr, program)
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
            
            // 设置当前基本块ID
            self.context.set_current_block_id(0);
            
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
                
                // 映射参数到变量映射表，这样在函数体中就能找到参数了
                let var_operand = Operand::Variable(variable_name.clone());
                self.mapper.map_variable(variable_name, var_operand);
                
                // 如果是数组类型，添加到数组变量列表
                if let Type::ArrayType { element_type, array_size } = variable_type {
                    if let crate::frontend::ast::ArraySize::Fixed(size) = array_size {
                        // 计算数组的总大小
                        let mut dimensions = vec![size];
                        let mut current_type = element_type;
                        
                        // 处理多维数组
                        while let Type::ArrayType { element_type: inner_type, array_size: inner_size } = current_type.as_ref() {
                            if let crate::frontend::ast::ArraySize::Fixed(inner_size_val) = inner_size {
                                dimensions.push(inner_size_val);
                                current_type = inner_type;
                            } else {
                                break;
                            }
                        }
                        
                        function.add_array_variable(
                            variable_name.clone(),
                            variable_type.clone(),
                            dimensions.into_iter().map(|&x| x).collect()
                        );
                    }
                }
            }
        }
        Ok(())
    }
    
    /// 最终化函数基本块，避免借用冲突
    fn finalize_function_blocks(&mut self, program: &mut TACProgram) -> Result<(), String> {
        // 首先同步当前基本块到函数中
        self.sync_current_block_to_function()?;
        
        // 获取当前函数
        if let Some(mut func) = self.context.current_function.take() {
            // 检查是否需要默认返回语句
            let current_block_id = self.context.get_current_block_id();
            if let Some(block) = func.get_basic_block_mut(current_block_id) {
                // 确保基本块至少有一条指令
                if block.instructions.is_empty() {
                    // 添加一个空操作指令，确保基本块不为空
                    // 使用函数获取临时变量ID,避免可变借用冲突
                    let temp_id = self.context.next_temp_id();
                    // 使用context的next_temp_id方法来增加临时变量计数器,避免可变借用冲突
                    let _ = self.context.next_temp_id();
                    let temp = Operand::Temp(temp_id);
                    block.add_instruction(TACInstruction::Assign {
                        target: temp.clone(),
                        source: Operand::Constant(ConstantValue::Integer(0)),
                    });
                }
                
                if !block.instructions.iter().any(|inst| matches!(inst, TACInstruction::Return { .. })) {
                    block.add_instruction(TACInstruction::Return { value: None });
                }
            }
            
            // 检查所有基本块，确保每个基本块都有指令
            for block in func.basic_blocks.iter_mut() {
                if block.instructions.is_empty() {
                    // 添加一个空操作指令，确保基本块不为空
                    let temp_id = func.temp_counter;
                    func.temp_counter += 1;
                    let temp = Operand::Temp(temp_id);
                    block.add_instruction(TACInstruction::Assign {
                        target: temp.clone(),
                        source: Operand::Constant(ConstantValue::Integer(0)),
                    });
                }
            }
            
            // 将函数添加到程序
            program.add_function(func);
        }
        
        Ok(())
    }
    
    /// 同步当前基本块到函数中
    fn sync_current_block_to_function(&mut self) -> Result<(), String> {
        if let Some(current_block) = self.context.current_block.take() {
            let current_block_id = self.context.current_block_id;
            
            if let Some(current_function) = self.context.current_function.as_mut() {
                // 查找并更新现有基本块，或添加新基本块
                if let Some(block_index) = current_function.basic_blocks.iter().position(|b| b.id == current_block_id) {
                    current_function.basic_blocks[block_index] = current_block;
                } else {
                    current_function.add_basic_block(current_block);
                }
            }
        }
        
        Ok(())
    }
    
    /// 转换变量声明（带常量信息）
    fn convert_variable_declaration_with_const_info(&mut self, ast: &Ast, program: &mut TACProgram, is_const: bool) -> Result<Operand, String> {
        if let AstKind::VariableDeclaration { variable_name, variable_type, initial_value, .. } = &ast.kind {
            // 创建变量操作数
            let var_operand = Operand::Variable(variable_name.clone());
            
            // 记录变量映射
            self.mapper.map_variable(variable_name, var_operand.clone());
            
            // 处理数组类型
            if let crate::frontend::ast::Type::ArrayType { element_type, array_size } = variable_type {
                // 如果是数组类型，添加到数组变量列表
                let dimensions = if let crate::frontend::ast::ArraySize::Fixed(size) = array_size {
                    // 如果array_size是确定值，直接使用
                    let mut dims: Vec<usize> = vec![*size];
                    let mut current_type = element_type;
                    
                    // 处理多维数组
                    while let Type::ArrayType { element_type: inner_type, array_size: inner_size } = current_type.as_ref() {
                        if let crate::frontend::ast::ArraySize::Fixed(inner_size_val) = inner_size {
                            dims.push(*inner_size_val);
                            current_type = inner_type;
                        } else {
                            break;
                        }
                    }
                    dims
                } else {
                    // 如果array_size不是确定值，仅从AST语义信息获取维度（不做启发式）
                    let inferred_dimensions = self.infer_array_dimensions_from_ast(&ast, variable_name);
                    if inferred_dimensions.is_empty() {
                    }
                    inferred_dimensions
                };
                
                if !dimensions.is_empty() {
                    // 计算总元素个数
                    let total_elements: usize = dimensions.iter().product();
                
                    // 如果有当前函数，添加到函数中；否则添加到全局程序
                    if let Some(func) = self.context.get_current_function_mut() {
                        func.add_array_variable(
                            variable_name.clone(),
                            variable_type.clone(),
                            dimensions.clone()
                        );
                    } else {
                        // 全局数组变量，添加到程序的全局数组变量列表中
                        program.add_global_array_variable(
                            variable_name.clone(),
                            variable_type.clone(),
                            dimensions.clone()
                        );
                        // 同时也添加到全局变量列表中，因为它们是全局变量
                        // 对于全局数组，如果有初始值，需要特殊处理
                        let global_initial_value = if let Some(init_value) = initial_value {
                            // 对于数组初始化，我们需要在后续处理中生成初始化指令
                            // 这里先设置为None，后续在process_array_initialization中处理
                            None
                        } else {
                            None
                        };
                        
                        program.add_global_variable(
                            variable_name.clone(),
                            variable_type.clone(),
                            global_initial_value,
                            is_const
                        );
                    }
                    
                    // 保存数组信息到mapper中，供后续使用
                    self.mapper.map_array_info(variable_name, dimensions);
                    
                    // 为数组分配内存
                    // 使用已经计算好的总元素个数
                    let element_size = self.get_type_size(element_type);
                    let total_bytes = total_elements * element_size;
                    
                    if let Some(block) = self.context.get_current_block_mut() {
                        // 函数内数组，生成内存分配指令
                        block.add_instruction(TACInstruction::Allocate {
                            target: var_operand.clone(),
                            size: Operand::Constant(ConstantValue::Integer(total_bytes as i64)),
                        });
                        
                    } else {
                        // 全局数组，生成全局内存分配指令
                        // 注意：全局数组在编译时就确定了大小，不需要运行时分配
                    }
                }
            } else {
                // 为普通变量声明生成IR指令
                if let Some(block) = self.context.get_current_block_mut() {
                    // 如果有当前函数，在函数内生成指令
                    // 只有在没有初始值时才使用默认值
                    if initial_value.is_none() {
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
                } else {
                    // 全局变量，添加到程序的全局变量列表中
                    let initial_value_operand = if let Some(init_value) = initial_value {
                        // 如果有初始值，转换它
                        match self.convert_ast_node(init_value, program) {
                            Ok(operand) => Some(operand),
                            Err(_) => None,
                        }
                    } else {
                        None
                    };
                    
                    program.add_global_variable(
                        variable_name.clone(),
                        variable_type.clone(),
                        initial_value_operand,
                        is_const
                    );
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
                    if let Some(block) = self.context.get_current_block_mut() {
                        // 函数内变量，生成赋值指令
                        let init_result = self.convert_ast_node(init_value, program)?;
                        self.store_variable_assignment(&var_operand, init_result)?;
                    } else {
                        // 全局变量，初始值已经在add_global_variable中处理了
                        // 这里不需要额外处理
                    }
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
                    let value_result = self.convert_ast_node(expr, program)?;
                    
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
                        name: end_label.clone(),
                    });
                }
                
                // 处理标签指令，创建新块
                self.handle_label_instruction(&end_label)?;
                
                Ok(then_result)
            }
            
            Statement::ElseIf { condition, then_branch, else_branch } => {
                // ElseIf语句的处理与If语句相同
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
                        name: end_label.clone(),
                    });
                }
                
                // 处理标签指令，创建新块
                self.handle_label_instruction(&end_label)?;
                
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
                
                // 处理标签指令，创建新块
                self.handle_label_instruction(&loop_start_label)?;
                
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
                        label: loop_start_label.clone(),
                    });
                }
                
                // 处理跳转指令，建立块间关系
                self.handle_jump_instruction(&loop_start_label)?;
                
                // 添加循环结束标签
                if let Some(block) = self.context.get_current_block_mut() {
                    block.add_instruction(TACInstruction::Label {
                        name: loop_end_label.clone(),
                    });
                }
                
                // 处理标签指令，创建新块
                self.handle_label_instruction(&loop_end_label)?;
                
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
    fn convert_expression(&mut self, expr: &Expression, program: &mut TACProgram) -> Result<Operand, String> {
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
                let left_result = self.convert_ast_node(left_operand, program)?;
                let right_result = self.convert_ast_node(right_operand, program)?;
                
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
                // 对于常量的一元操作，直接计算结果
                if let AstKind::Expression(Expression::Literal(Literal::IntegerLiteral(value))) = &operand.kind {
                    if *operator == UnaryOperator::Minus {
                        // 直接返回负数常量
                        return Ok(Operand::Constant(ConstantValue::Integer(-(*value as i64))));
                    }
                }
                
                // 递归转换操作数
                let operand_result = self.convert_ast_node(operand, program)?;
                
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
                // 转换赋值表达式
                let value_result = self.convert_ast_node(value, program)?;
                
                // 检查目标是否是数组访问
                if let AstKind::Expression(Expression::ArrayAccess { array, index }) = &target.kind {
                    // 数组赋值，使用多维数组访问逻辑
                    let addr_temp_id = self.context.next_temp_id();
                    let addr_temp = Operand::Temp(addr_temp_id);
                    
                    // 收集所有索引
                    let mut indices = Vec::new();
                    let mut current_array = array;
                    
                    // 递归收集所有索引
                    loop {
                        if let AstKind::Expression(Expression::ArrayAccess { array: inner_array, index: inner_index }) = &current_array.kind {
                            let index_result = self.convert_ast_node(inner_index, program)?;
                            indices.push(index_result);
                            current_array = inner_array;
                        } else {
                            break;
                        }
                    }
                    
                    // 添加最外层的索引
                    let outer_index_result = self.convert_ast_node(index, program)?;
                    indices.push(outer_index_result);
                    
                    // 反转索引顺序
                    indices.reverse();
                    
                    // 获取数组变量
                    let array_result = self.convert_ast_node(current_array, program)?;
                    
                    if let Some(block) = self.context.get_current_block_mut() {
                        // 计算多维数组地址
                        block.add_instruction(TACInstruction::GetElementPtr {
                            target: addr_temp.clone(),
                            base: array_result,
                            indices,
                        });
                        
                        // 存储值到数组
                        block.add_instruction(TACInstruction::Store {
                            value: value_result.clone(),
                            address: addr_temp,
                        });
                    }
                } else {
                    // 普通变量赋值
                    let target_result = self.convert_ast_node(target, program)?;
                    
                    if let Some(block) = self.context.get_current_block_mut() {
                        block.add_instruction(TACInstruction::Assign {
                            target: target_result,
                            source: value_result.clone(),
                        });
                    }
                }
                
                Ok(value_result)
            }
            
            Expression::FunctionCall { function_name, arguments } => {
                // 转换所有参数
                let mut arg_operands = Vec::new();
                for arg in arguments {
                    let arg_result = self.convert_ast_node(arg, program)?;
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
                    let first_element = self.convert_ast_node(&elements[0], program)?;
                    
                    // 其他元素暂时不处理，避免生成过多临时变量
                    // 在实际实现中，这些值应该直接存储到数组中
                    
                    Ok(first_element)
                }
            }
            
            Expression::ArrayAccess { array, index } => {
                // 处理多维数组访问，如 f[x][i]
                self.convert_multidimensional_array_access(array, index, program)
            }
            
            Expression::MemberAccess { object, member_name: _ } => {
                // 成员访问，暂时返回对象本身
                self.convert_ast_node(object, program)
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
            // 检查是否为全局数组
            let is_global_array = self.context.get_current_function().is_none();
            
            if is_global_array {
                // 全局数组初始化：生成全局初始化指令
                self.process_global_array_initialization(variable_name, variable_type, elements, program)?;
            } else {
                // 函数内数组初始化：使用原有逻辑
                self.process_local_array_initialization(variable_name, variable_type, elements, program)?;
            }
            Ok(())
        }
    }
    
    /// 处理全局数组初始化
    fn process_global_array_initialization(
        &mut self,
        variable_name: &str,
        variable_type: &Type,
        elements: &[Ast],
        program: &mut TACProgram,
    ) -> Result<(), String> {
        // 获取数组维度信息
        let array_info = self.get_array_info_cloned(variable_name);
        let dimensions = if let Some((_, dims, _)) = array_info {
            dims
        } else {
            return Err(format!("无法获取数组 {} 的维度信息", variable_name));
        };
        
        // 处理初始化列表
        for (i, element) in elements.iter().enumerate() {
            // 检查元素是否是嵌套的初始化列表
            if let AstKind::Expression(Expression::InitializerList { elements: nested_elements }) = &element.kind {
                // 递归处理嵌套的初始化列表
                self.process_nested_global_array_initialization(variable_name, variable_type, nested_elements, i, &dimensions, program)?;
            } else {
                // 单个值，生成全局数组元素存储指令
                let element_result = self.convert_ast_node(element, program)?;
                
                // 计算正确的索引
                let indices = self.calculate_array_indices(i, &dimensions);
                self.store_global_array_element(variable_name, &indices, element_result)?;
            }
        }
        Ok(())
    }
    
    /// 处理函数内数组初始化
    fn process_local_array_initialization(
        &mut self,
        variable_name: &str,
        variable_type: &Type,
        elements: &[Ast],
        program: &mut TACProgram,
    ) -> Result<(), String> {
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
    
    /// 处理嵌套的全局数组初始化列表
    fn process_nested_global_array_initialization(
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
                
                self.store_global_array_element(variable_name, &indices, element_result)?;
            }
            Ok(())
        }
    }
    
    /// 存储全局数组元素
    fn store_global_array_element(&mut self, variable_name: &str, indices: &[usize], value: Operand) -> Result<(), String> {
        // 对于全局数组，我们需要在main函数开始时生成初始化指令
        // 这里我们暂时将初始化信息存储起来，在main函数开始时处理
        
        // 获取数组信息
        let array_info = self.get_array_info_cloned(variable_name);
        if let Some((_, dimensions, _)) = array_info {
            // 计算数组元素的地址偏移
            let offset = self.calculate_global_array_offset(&dimensions, indices);
            
            // 将初始化信息存储到全局初始化列表中
            // 这里我们需要修改TACProgram来支持全局初始化
            // 暂时先记录到mapper中
            self.mapper.add_global_array_init(variable_name.to_string(), offset, value);
        }
        Ok(())
    }
    
    /// 计算全局数组的偏移量
    fn calculate_global_array_offset(&self, dimensions: &[usize], indices: &[usize]) -> usize {
        if dimensions.len() != indices.len() {
            return 0;
        }
        
        let mut offset = 0;
        let mut multiplier = 1;
        
        // 从最后一个维度开始计算
        for i in (0..dimensions.len()).rev() {
            offset += indices[i] * multiplier;
            multiplier *= dimensions[i];
        }
        
        offset
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
        // 对于多维数组，如果索引数量少于维度数量，使用线性索引计算
        if indices.len() < dimensions.len() {
            // 使用线性索引计算偏移
            if indices.len() == 1 {
                let index = indices[0];
                let offset_operand = Operand::Constant(ConstantValue::Integer(index as i64));
                return Ok(offset_operand);
            } else {
                return Err(format!("不支持的索引数量: {}", indices.len()));
            }
        }
        
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
        if let Some(block) = self.context.get_current_block_mut() {
            block.add_instruction(TACInstruction::Assign {
                target: target.clone(),
                source: value.clone(),
            });
        }
        Ok(())
    }

    /// 添加返回指令，避免借用冲突
    fn add_return_instruction(&mut self, value: Option<Operand>) -> Result<(), String> {
        if let Some(block) = self.context.get_current_block_mut() {
            block.add_instruction(TACInstruction::Return { value });
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
            if let crate::frontend::ast::ArraySize::Fixed(size_val) = size {
                total_elements *= *size_val;
            } else {
                // 如果数组大小未知或为常量标识符，暂按1处理
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
    
    /// 处理多维数组访问，如 f[x][i]
    fn convert_multidimensional_array_access(&mut self, array: &Ast, index: &Ast, program: &mut TACProgram) -> Result<Operand, String> {
        // 收集所有索引，从最外层到最内层
        let mut indices = Vec::new();
        let mut current_array = array;
        
        // 递归收集所有索引
        loop {
            if let AstKind::Expression(Expression::ArrayAccess { array: inner_array, index: inner_index }) = &current_array.kind {
                // 转换当前索引
                let index_result = self.convert_ast_node(inner_index, program)?;
                indices.push(index_result);
                current_array = inner_array;
            } else {
                // 到达最底层的数组变量
                break;
            }
        }
        
        // 添加最外层的索引
        let outer_index_result = self.convert_ast_node(index, program)?;
        indices.push(outer_index_result);
        
        // 反转索引顺序，使其从最外层到最内层
        indices.reverse();
        
        // 获取数组变量
        let array_result = self.convert_ast_node(current_array, program)?;
        
        // 获取数组变量名（用于调试）
        let array_name = if let AstKind::Expression(Expression::Identifier { name }) = &current_array.kind {
            name.clone()
        } else {
            "unknown".to_string()
        };
        
        // 对于数组切片，我们不需要检查维度匹配，因为切片会改变数组的维度
        // 直接使用GetElementPtr指令处理
        
        // 创建临时变量
        let temp_operand_id = self.context.next_temp_id();
        let addr_temp_id = self.context.next_temp_id();
        let temp_operand = Operand::Temp(temp_operand_id);
        let addr_temp = Operand::Temp(addr_temp_id);
        
        if let Some(block) = self.context.get_current_block_mut() {
            // 一次性计算多维数组地址：addr_temp = getelementptr array, [index1, index2, ...]
            block.add_instruction(TACInstruction::GetElementPtr {
                target: addr_temp.clone(),
                base: array_result,
                indices,
            });
            
            // 加载数组元素值：temp_operand = load addr_temp
            block.add_instruction(TACInstruction::Load {
                target: temp_operand.clone(),
                address: addr_temp,
            });
        }
        
        Ok(temp_operand)
    }
}

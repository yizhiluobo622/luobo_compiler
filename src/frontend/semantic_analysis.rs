use crate::frontend::ast::Ast;
use crate::frontend::SemanticAnalyzer::sema::{SemanticAnalyzer, SemanticError};

/// 语义分析结果
#[derive(Debug)]
pub struct SemanticAnalysisResult {
    /// 是否成功
    pub success: bool,
    /// 错误列表
    pub errors: Vec<SemanticError>,
    /// 错误数量
    pub error_count: usize,
}

impl SemanticAnalysisResult {
    /// 创建成功的结果
    pub fn success() -> Self {
        Self {
            success: true,
            errors: Vec::new(),
            error_count: 0,
        }
    }
    
    /// 创建失败的结果
    pub fn failure(errors: Vec<SemanticError>) -> Self {
        let error_count = errors.len();
        Self {
            success: false,
            errors,
            error_count,
        }
    }
    
    /// 检查是否有错误
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
    
    /// 打印所有错误
    pub fn print_errors(&self) {
        if self.errors.is_empty() {
            println!("✅ 语义分析成功，没有发现错误");
            return;
        }
        
        println!("❌ 语义分析发现 {} 个错误:", self.error_count);
        for (i, error) in self.errors.iter().enumerate() {
            println!("  {}. {} (位置: {:?})", i + 1, error.message, error.span);
        }
    }
}

/// 语义分析器
pub struct SemanticAnalysis {
    analyzer: SemanticAnalyzer,
}

impl SemanticAnalysis {
    /// 创建新的语义分析器
    pub fn new() -> Self {
        Self {
            analyzer: SemanticAnalyzer::new(),
        }
    }
    
    /// 分析AST
    /// 
    /// # 参数
    /// * `ast` - 抽象语法树
    /// 
    /// # 返回
    /// 语义分析结果
    pub fn analyze(&mut self, ast: &Ast) -> SemanticAnalysisResult {
        // 调用内部的语义分析器进行真正的语义分析
        match self.analyzer.analyze(ast) {
            Ok(()) => SemanticAnalysisResult::success(),
            Err(errors) => SemanticAnalysisResult::failure(errors),
        }
    }
    
    /// 快速分析并打印结果
    /// 
    /// # 参数
    /// * `ast` - 抽象语法树
    /// 
    /// # 返回
    /// 是否成功
    pub fn analyze_and_print(&mut self, ast: &Ast) -> bool {
        let result = self.analyze(ast);
        result.print_errors();
        result.success
    }
    
    /// 分析AST并返回带语义信息的AST
    /// 
    /// # 参数
    /// * `ast` - 抽象语法树
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 带语义信息的AST
    /// * `Err(Vec<SemanticError>)` - 语义错误列表
    pub fn analyze_and_annotate(&mut self, mut ast: Ast) -> Result<Ast, Vec<SemanticError>> {
        // 先进行语义分析
        let analysis_result = self.analyzer.analyze(&ast);
        
        match analysis_result {
            Ok(()) => {
                // 语义分析成功，填充AST的语义信息
                // 使用analyzer内部的符号表，确保包含所有局部变量
                self.fill_semantic_info_with_analyzer(&mut ast);
                Ok(ast)
            }
            Err(errors) => Err(errors),
        }
    }
    
    /// 使用analyzer内部的符号表填充语义信息
    fn fill_semantic_info_with_analyzer(&mut self, ast: &mut Ast) {
        // 获取analyzer内部的符号表和类型系统
        let symbol_table = self.analyzer.get_symbol_table();
        let type_system = self.analyzer.get_type_system();
        
        // 递归填充所有节点的语义信息
        fill_semantic_info_recursive(ast, symbol_table, type_system);
        
        // 标记整个AST为已分析
        mark_ast_as_analyzed(ast);
    }
    
    /// 填充AST的语义信息
    fn fill_semantic_info(&mut self, ast: &mut Ast) {
        // 这里可以添加填充语义信息的逻辑
        // 例如：设置推导类型、符号信息等
        // 目前先保持简单实现
    }
    
    /// 获取错误列表
    pub fn get_errors(&self) -> &[SemanticError] {
        self.analyzer.get_errors()
    }
    
    /// 检查是否有错误
    pub fn has_errors(&self) -> bool {
        self.analyzer.has_errors()
    }
    
    /// 获取错误数量
    pub fn error_count(&self) -> usize {
        self.analyzer.get_errors().len()
    }
    
    /// 清空错误
    pub fn clear_errors(&mut self) {
        // 创建新的分析器来清空错误
        self.analyzer = SemanticAnalyzer::new();
    }
    
    /// 获取类型系统引用
    pub fn get_type_system(&self) -> &crate::frontend::SemanticAnalyzer::type_system::TypeSystem {
        self.analyzer.get_type_system()
    }
    
    /// 获取符号表引用
    pub fn get_symbol_table(&self) -> &crate::frontend::SemanticAnalyzer::symbol_table::SymbolTable {
        self.analyzer.get_symbol_table()
    }
}

/// 便捷函数：分析AST并返回结果
/// 
/// # 参数
/// * `ast` - 抽象语法树
/// 
/// # 返回
/// 语义分析结果
pub fn analyze_ast(ast: &Ast) -> SemanticAnalysisResult {
    let mut analyzer = SemanticAnalysis::new();
    analyzer.analyze(ast)
}

/// 便捷函数：分析AST并打印结果
/// 
/// # 参数
/// * `ast` - 抽象语法树
/// 
/// # 返回
/// 是否成功
pub fn analyze_ast_and_print(ast: &Ast) -> bool {
    let mut analyzer = SemanticAnalysis::new();
    analyzer.analyze_and_print(ast)
}

/// 便捷函数：分析AST并返回带语义信息的AST
/// 
/// # 参数
/// * `ast` - 抽象语法树
/// 
/// # 返回
/// * `Ok(Ast)` - 带语义信息的AST
/// * `Err(Vec<SemanticError>)` - 语义错误列表
pub fn analyze_ast_with_semantic_info(mut ast: Ast) -> Result<Ast, Vec<SemanticError>> {
    let mut analyzer = SemanticAnalysis::new();
    
    // 先进行语义分析
    let analysis_result = analyzer.analyzer.analyze(&ast);
    
    if analysis_result.is_ok() {
        // 语义分析成功，填充AST的语义信息
        // 使用analyzer内部的符号表，确保包含所有局部变量
        let symbol_table = analyzer.analyzer.get_symbol_table();
        let type_system = analyzer.analyzer.get_type_system();
        
        fill_semantic_info_recursive(&mut ast, symbol_table, type_system);
        mark_ast_as_analyzed(&mut ast);
        
        Ok(ast)
    } else {
        // 语义分析失败，返回错误
        Err(analyzer.analyzer.get_errors().to_vec())
    }
}

/// 填充AST的语义信息
fn fill_semantic_info(ast: &mut Ast, analyzer: &SemanticAnalysis) {
    // 获取符号表和类型系统
    let symbol_table = analyzer.get_symbol_table();
    let type_system = analyzer.get_type_system();
    
    // 递归填充所有节点的语义信息
    fill_semantic_info_recursive(ast, symbol_table, type_system);
    
    // 标记整个AST为已分析
    mark_ast_as_analyzed(ast);
}

/// 递归填充AST节点的语义信息
fn fill_semantic_info_recursive(ast: &mut Ast, symbol_table: &crate::frontend::SemanticAnalyzer::symbol_table::SymbolTable, type_system: &crate::frontend::SemanticAnalyzer::type_system::TypeSystem) {
    use crate::frontend::ast::{AstKind, Expression};
    
    // 根据节点类型填充语义信息
    match &ast.kind {
        AstKind::VariableDeclaration { variable_name, variable_type, .. } => {
            // 设置变量类型
            ast.semantic_info.set_deduced_type(variable_type.clone());
            // 设置符号名称
            ast.semantic_info.set_symbol_name(variable_name.clone());
        }
        AstKind::Function { function_name, return_type, .. } => {
            // 设置返回类型
            if let Some(return_type) = return_type {
                ast.semantic_info.set_deduced_type(return_type.clone());
            }
            // 设置符号名称
            ast.semantic_info.set_symbol_name(function_name.clone());
        }
        AstKind::Expression(Expression::Identifier { name }) => {
            // 从符号表获取变量类型
            if let Some(var_type) = symbol_table.get_variable_type(name) {
                ast.semantic_info.set_deduced_type(var_type);
            } else {
                ast.semantic_info.add_error(format!("未定义的变量: {}", name));
            }
            // 设置符号名称
            ast.semantic_info.set_symbol_name(name.clone());
        }
        AstKind::Expression(Expression::Literal(literal)) => {
            // 根据字面量类型设置推导类型
            use crate::frontend::ast::Literal;
            let deduced_type = match literal {
                Literal::IntegerLiteral(_) => crate::frontend::ast::Type::IntType,
                Literal::FloatLiteral(_) => crate::frontend::ast::Type::FloatType,
                Literal::StringLiteral(_) => crate::frontend::ast::Type::CharType,
                Literal::BooleanLiteral(_) => crate::frontend::ast::Type::BoolType,
            };
            ast.semantic_info.set_deduced_type(deduced_type);
        }
        AstKind::Expression(Expression::FunctionCall { function_name, .. }) => {
            // 从符号表获取函数返回类型
            if let Some(return_type) = symbol_table.get_function_return_type(function_name) {
                ast.semantic_info.set_deduced_type(return_type);
            } else {
                ast.semantic_info.add_error(format!("未定义的函数: {}", function_name));
            }
        }
        AstKind::Expression(Expression::BinaryOperation { .. }) => {
            // 二元运算的类型推导在递归处理子节点时完成
            // 这里暂时不设置类型，等子节点处理完成后再推导
        }
        AstKind::Expression(Expression::UnaryOperation { .. }) => {
            // 一元运算的类型推导在递归处理子节点时完成
            // 这里暂时不设置类型，等子节点处理完成后再推导
        }
        AstKind::Expression(Expression::Assignment { .. }) => {
            // 赋值运算的类型推导在递归处理子节点时完成
            // 这里暂时不设置类型，等子节点处理完成后再推导
        }
        _ => {
            // 对于其他类型的节点，暂时不填充语义信息
            // 未来可以扩展更复杂的类型推导
        }
    }
    
    // 递归处理子节点
    match &mut ast.kind {
        AstKind::Program { functions, global_variables } => {
            for func in functions {
                fill_semantic_info_recursive(func, symbol_table, type_system);
            }
            for var in global_variables {
                fill_semantic_info_recursive(var, symbol_table, type_system);
            }
        }
        AstKind::Function { function_body, .. } => {
            fill_semantic_info_recursive(function_body, symbol_table, type_system);
        }
        AstKind::VariableDeclaration { initial_value, .. } => {
            if let Some(init) = initial_value {
                fill_semantic_info_recursive(init, symbol_table, type_system);
            }
        }
        AstKind::Statement(stmt) => {
            use crate::frontend::ast::Statement;
            match stmt {
                Statement::Compound { statements } => {
                    for stmt in statements {
                        fill_semantic_info_recursive(stmt, symbol_table, type_system);
                    }
                }
                Statement::ExpressionStatement { expression } => {
                    fill_semantic_info_recursive(expression, symbol_table, type_system);
                }
                Statement::Return { value } => {
                    if let Some(val) = value {
                        fill_semantic_info_recursive(val, symbol_table, type_system);
                    }
                }
                Statement::If { condition, then_branch, else_branch } => {
                    fill_semantic_info_recursive(condition, symbol_table, type_system);
                    fill_semantic_info_recursive(then_branch, symbol_table, type_system);
                    if let Some(else_branch) = else_branch {
                        fill_semantic_info_recursive(else_branch, symbol_table, type_system);
                    }
                }
                Statement::While { condition, body } => {
                    fill_semantic_info_recursive(condition, symbol_table, type_system);
                    fill_semantic_info_recursive(body, symbol_table, type_system);
                }
                Statement::For { initialization, condition, update, body } => {
                    if let Some(init) = initialization {
                        fill_semantic_info_recursive(init, symbol_table, type_system);
                    }
                    if let Some(cond) = condition {
                        fill_semantic_info_recursive(cond, symbol_table, type_system);
                    }
                    if let Some(upd) = update {
                        fill_semantic_info_recursive(upd, symbol_table, type_system);
                    }
                    fill_semantic_info_recursive(body, symbol_table, type_system);
                }
                _ => {}
            }
        }
        AstKind::Expression(expr) => {
            use crate::frontend::ast::Expression;
            match expr {
                Expression::BinaryOperation { left_operand, right_operand, operator } => {
                    fill_semantic_info_recursive(left_operand.as_mut(), symbol_table, type_system);
                    fill_semantic_info_recursive(right_operand.as_mut(), symbol_table, type_system);
                    
                    // 获取左右操作数的类型
                    let left_type = left_operand.semantic_info.deduced_type.clone();
                    let right_type = right_operand.semantic_info.deduced_type.clone();
                    
                    // 根据操作符推导结果类型
                    if let (Some(ref left_type), Some(ref right_type)) = (left_type, right_type) {
                        let result_type = match operator {
                            crate::frontend::ast::BinaryOperator::Add |
                            crate::frontend::ast::BinaryOperator::Subtract |
                            crate::frontend::ast::BinaryOperator::Multiply |
                            crate::frontend::ast::BinaryOperator::Divide => {
                                // 算术运算：int + int = int, float + float = float, int + float = float
                                match (left_type, right_type) {
                                    (crate::frontend::ast::Type::IntType, crate::frontend::ast::Type::IntType) => 
                                        crate::frontend::ast::Type::IntType,
                                    (crate::frontend::ast::Type::FloatType, crate::frontend::ast::Type::FloatType) => 
                                        crate::frontend::ast::Type::FloatType,
                                    (crate::frontend::ast::Type::IntType, crate::frontend::ast::Type::FloatType) |
                                    (crate::frontend::ast::Type::FloatType, crate::frontend::ast::Type::IntType) => 
                                        crate::frontend::ast::Type::FloatType,
                                    _ => {
                                        ast.semantic_info.add_error(format!("不支持的算术运算类型：{:?} {:?} {:?}", left_type, operator, right_type));
                                        crate::frontend::ast::Type::IntType // 默认返回int
                                    }
                                }
                            }
                            crate::frontend::ast::BinaryOperator::Modulo => {
                                // 取模运算：只支持整数
                                match (left_type, right_type) {
                                    (crate::frontend::ast::Type::IntType, crate::frontend::ast::Type::IntType) => 
                                        crate::frontend::ast::Type::IntType,
                                    _ => {
                                        ast.semantic_info.add_error(format!("取模运算只支持整数类型：{:?} % {:?}", left_type, right_type));
                                        crate::frontend::ast::Type::IntType
                                    }
                                }
                            }
                            crate::frontend::ast::BinaryOperator::Equal |
                            crate::frontend::ast::BinaryOperator::NotEqual |
                            crate::frontend::ast::BinaryOperator::LessThan |
                            crate::frontend::ast::BinaryOperator::GreaterThan |
                            crate::frontend::ast::BinaryOperator::LessEqual |
                            crate::frontend::ast::BinaryOperator::GreaterEqual => {
                                // 比较运算：返回布尔类型
                                match (left_type, right_type) {
                                    (crate::frontend::ast::Type::IntType, crate::frontend::ast::Type::IntType) |
                                    (crate::frontend::ast::Type::FloatType, crate::frontend::ast::Type::FloatType) |
                                    (crate::frontend::ast::Type::IntType, crate::frontend::ast::Type::FloatType) |
                                    (crate::frontend::ast::Type::FloatType, crate::frontend::ast::Type::IntType) => 
                                        crate::frontend::ast::Type::BoolType,
                                    _ => {
                                        ast.semantic_info.add_error(format!("不支持的比较运算类型：{:?} {:?} {:?}", left_type, operator, right_type));
                                        crate::frontend::ast::Type::BoolType
                                    }
                                }
                            }
                            crate::frontend::ast::BinaryOperator::LogicalAnd |
                            crate::frontend::ast::BinaryOperator::LogicalOr => {
                                // 逻辑运算：只支持布尔类型
                                match (left_type, right_type) {
                                    (crate::frontend::ast::Type::BoolType, crate::frontend::ast::Type::BoolType) => 
                                        crate::frontend::ast::Type::BoolType,
                                    _ => {
                                        ast.semantic_info.add_error(format!("逻辑运算只支持布尔类型：{:?} {:?} {:?}", left_type, operator, right_type));
                                        crate::frontend::ast::Type::BoolType
                                    }
                                }
                            }
                            _ => {
                                ast.semantic_info.add_error(format!("不支持的二元运算符：{:?}", operator));
                                crate::frontend::ast::Type::IntType
                            }
                        };
                        ast.semantic_info.set_deduced_type(result_type);
                    } else {
                        ast.semantic_info.add_error("无法推导二元运算的类型，操作数类型未知".to_string());
                    }
                }
                Expression::UnaryOperation { operand, operator } => {
                    fill_semantic_info_recursive(operand.as_mut(), symbol_table, type_system);
                    
                    // 获取操作数类型
                    let operand_type = operand.semantic_info.deduced_type.clone();
                    
                    if let Some(operand_type) = operand_type {
                        let result_type = match operator {
                            crate::frontend::ast::UnaryOperator::LogicalNot => {
                                // 逻辑非：只支持布尔类型
                                match operand_type {
                                    crate::frontend::ast::Type::BoolType => crate::frontend::ast::Type::BoolType,
                                    _ => {
                                        ast.semantic_info.add_error(format!("逻辑非运算只支持布尔类型：!{:?}", operand_type));
                                        crate::frontend::ast::Type::BoolType
                                    }
                                }
                            }
                            crate::frontend::ast::UnaryOperator::Minus => {
                                // 负号：支持整数和浮点数
                                match operand_type {
                                    crate::frontend::ast::Type::IntType => crate::frontend::ast::Type::IntType,
                                    crate::frontend::ast::Type::FloatType => crate::frontend::ast::Type::FloatType,
                                    _ => {
                                        ast.semantic_info.add_error(format!("负号运算只支持数值类型：-{:?}", operand_type));
                                        crate::frontend::ast::Type::IntType
                                    }
                                }
                            }
                            _ => {
                                ast.semantic_info.add_error(format!("不支持的一元运算符：{:?}", operator));
                                crate::frontend::ast::Type::IntType
                            }
                        };
                        ast.semantic_info.set_deduced_type(result_type);
                    } else {
                        ast.semantic_info.add_error("无法推导一元运算的类型，操作数类型未知".to_string());
                    }
                }
                Expression::Assignment { target, value } => {
                    fill_semantic_info_recursive(target.as_mut(), symbol_table, type_system);
                    fill_semantic_info_recursive(value.as_mut(), symbol_table, type_system);
                    
                    // 获取值类型
                    let value_type = value.semantic_info.deduced_type.clone();
                    
                    if let Some(value_type) = value_type {
                        // 检查左值是否为标识符
                        if let AstKind::Expression(Expression::Identifier { name }) = &target.kind {
                            // 检查左值类型是否匹配
                            if let Some(target_type) = symbol_table.get_variable_type(name) {
                                if target_type == value_type {
                                    ast.semantic_info.set_deduced_type(value_type);
                                } else {
                                    ast.semantic_info.add_error(format!("类型不匹配：无法将 {:?} 赋值给 {:?} 类型的变量 '{}'", value_type, target_type, name));
                                }
                            } else {
                                ast.semantic_info.add_error(format!("未定义的变量：'{}'", name));
                            }
                        } else {
                            // 对于复杂的左值表达式，暂时返回右值类型
                            ast.semantic_info.set_deduced_type(value_type);
                        }
                    } else {
                        ast.semantic_info.add_error("无法推导赋值运算的类型，右值类型未知".to_string());
                    }
                }
                Expression::FunctionCall { arguments, .. } => {
                    for arg in arguments {
                        fill_semantic_info_recursive(arg, symbol_table, type_system);
                    }
                }
                Expression::ArrayAccess { array, index } => {
                    fill_semantic_info_recursive(array, symbol_table, type_system);
                    fill_semantic_info_recursive(index, symbol_table, type_system);
                }
                Expression::MemberAccess { object, .. } => {
                    fill_semantic_info_recursive(object, symbol_table, type_system);
                }
                _ => {}
            }
        }
        _ => {}
    }
}

/// 递归标记AST为已分析
fn mark_ast_as_analyzed(ast: &mut Ast) {
    // 标记当前节点为已分析
    ast.semantic_info.analyzed = true;
    
    // 递归标记所有子节点
    match &mut ast.kind {
        crate::frontend::ast::AstKind::Program { functions, global_variables } => {
            for func in functions {
                mark_ast_as_analyzed(func);
            }
            for var in global_variables {
                mark_ast_as_analyzed(var);
            }
        }
        crate::frontend::ast::AstKind::Function { function_body, .. } => {
            mark_ast_as_analyzed(function_body);
        }
        crate::frontend::ast::AstKind::VariableDeclaration { initial_value, .. } => {
            if let Some(init) = initial_value {
                mark_ast_as_analyzed(init);
            }
        }
        crate::frontend::ast::AstKind::Statement(stmt) => {
            use crate::frontend::ast::Statement;
            match stmt {
                Statement::Compound { statements } => {
                    for stmt in statements {
                        mark_ast_as_analyzed(stmt);
                    }
                }
                Statement::ExpressionStatement { expression } => {
                    mark_ast_as_analyzed(expression);
                }
                Statement::Return { value } => {
                    if let Some(val) = value {
                        mark_ast_as_analyzed(val);
                    }
                }
                Statement::If { condition, then_branch, else_branch } => {
                    mark_ast_as_analyzed(condition);
                    mark_ast_as_analyzed(then_branch);
                    if let Some(else_branch) = else_branch {
                        mark_ast_as_analyzed(else_branch);
                    }
                }
                Statement::While { condition, body } => {
                    mark_ast_as_analyzed(condition);
                    mark_ast_as_analyzed(body);
                }
                Statement::For { initialization, condition, update, body } => {
                    if let Some(init) = initialization {
                        mark_ast_as_analyzed(init);
                    }
                    if let Some(cond) = condition {
                        mark_ast_as_analyzed(cond);
                    }
                    if let Some(upd) = update {
                        mark_ast_as_analyzed(upd);
                    }
                    mark_ast_as_analyzed(body);
                }
                _ => {}
            }
        }
        crate::frontend::ast::AstKind::Expression(expr) => {
            use crate::frontend::ast::Expression;
            match expr {
                Expression::BinaryOperation { left_operand, right_operand, .. } => {
                    mark_ast_as_analyzed(left_operand);
                    mark_ast_as_analyzed(right_operand);
                }
                Expression::UnaryOperation { operand, .. } => {
                    mark_ast_as_analyzed(operand);
                }
                Expression::FunctionCall { arguments, .. } => {
                    for arg in arguments {
                        mark_ast_as_analyzed(arg);
                    }
                }
                Expression::Assignment { target, value } => {
                    mark_ast_as_analyzed(target);
                    mark_ast_as_analyzed(value);
                }
                Expression::ArrayAccess { array, index } => {
                    mark_ast_as_analyzed(array);
                    mark_ast_as_analyzed(index);
                }
                Expression::MemberAccess { object, .. } => {
                    mark_ast_as_analyzed(object);
                }
                _ => {}
            }
        }
        _ => {}
    }
}

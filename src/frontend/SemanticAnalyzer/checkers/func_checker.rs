use crate::frontend::ast::{Ast, AstKind, Type};
use crate::frontend::span::Span;
use crate::frontend::SemanticAnalyzer::symbol_table::SymbolTable;
use crate::frontend::SemanticAnalyzer::type_system::TypeSystem;
use crate::frontend::SemanticAnalyzer::sema::SemanticError;

/// 函数检查器
/// 
/// 按照clang设计理念实现：
/// 1. 函数声明检查：重复定义、参数检查
/// 2. 函数调用检查：参数匹配、返回类型
/// 3. 函数重载：支持同名不同参数的函数
/// 4. 递归检查：检查递归调用的正确性
pub struct FuncChecker;

impl FuncChecker {
    /// 创建新的函数检查器
    pub fn new() -> Self {
        Self
    }
    
    /// 收集函数声明
    pub fn collect_function_declaration(
        &self,
        func_decl: &Ast,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<SemanticError>,
    ) {
        match &func_decl.kind {
            AstKind::Function {
                function_name,
                parameters,
                return_type,
                function_body: _,
            } => {
                // 提取参数类型
                let param_types: Vec<Type> = parameters.iter()
                    .filter_map(|param| {
                        if let AstKind::VariableDeclaration { variable_type, .. } = &param.kind {
                            Some(variable_type.clone())
                        } else {
                            None
                        }
                    })
                    .collect();
                
                // 添加函数到符号表
                let return_type = return_type.as_ref().unwrap_or(&Type::VoidType);
                if let Err(msg) = symbol_table.add_function(
                    function_name, 
                    return_type.clone(), 
                    param_types, 
                    func_decl.span.clone()
                ) {
                    errors.push(SemanticError {
                        message: msg,
                        span: func_decl.span.clone(),
                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::Redefinition,
                    });
                }
            }
            _ => {
                errors.push(SemanticError {
                    message: "期望函数声明节点".to_string(),
                    span: func_decl.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::ScopeError,
                });
            }
        }
    }
    
    /// 检查函数声明
    pub fn check_function_declaration(
        &self,
        func_decl: &Ast,
        symbol_table: &mut SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {
        match &func_decl.kind {
            AstKind::Function {
                function_name,
                parameters,
                return_type,
                function_body,
            } => {
                // 检查返回类型是否有效
                if let Some(ret_type) = return_type {
                    if !type_system.is_valid_type(ret_type) {
                        errors.push(SemanticError {
                            message: format!("无效的返回类型：{:?}", ret_type),
                            span: func_decl.span.clone(),
                            error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                        });
                    }
                }
                
                // 进入函数作用域
                symbol_table.enter_scope(&format!("function_{}", function_name));
                
                // 添加函数参数到符号表
                for param in parameters {
                    if let AstKind::VariableDeclaration { variable_name, variable_type, .. } = &param.kind {
                        if !type_system.is_valid_type(variable_type) {
                            errors.push(SemanticError {
                                message: format!("无效的参数类型：{:?}", variable_type),
                                span: param.span.clone(),
                                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                            });
                        } else {
                            // 添加参数到符号表
                            if let Err(msg) = symbol_table.add_parameter(variable_name, variable_type.clone(), param.span.clone()) {
                                errors.push(SemanticError {
                                    message: msg,
                                    span: param.span.clone(),
                                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::Redefinition,
                                });
                            }
                        }
                    }
                }
                
                // 检查函数体并重建函数内的符号表
                self.check_function_body(function_body, function_name, return_type, symbol_table, type_system, errors);
                
                // 退出函数作用域，清理局部变量
                symbol_table.exit_scope();
            }
            _ => {
                errors.push(SemanticError {
                    message: "期望函数声明节点".to_string(),
                    span: func_decl.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::ScopeError,
                });
            }
        }
    }
    
    /// 检查函数体
    /// 
    /// 按照clang设计理念：检查函数体中的语句和表达式
    /// 
    /// # 参数
    /// * `function_body` - 函数体AST节点
    /// * `function_name` - 函数名称
    /// * `return_type` - 返回类型
    /// * `symbol_table` - 符号表
    /// * `type_system` - 类型系统
    /// * `errors` - 错误列表
    fn check_function_body(
        &self,
        function_body: &Ast,
        function_name: &str,
        return_type: &Option<Type>,
        symbol_table: &mut SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {
        // 检查函数体中的语句
        match &function_body.kind {
            AstKind::Statement(statement) => {
                match statement {
                    crate::frontend::ast::Statement::Compound { statements } => {
                        // 进入复合语句作用域
                        symbol_table.enter_scope(&format!("compound_{}", function_name));
                        
                        // 第一遍：处理所有变量声明
                        for stmt in statements {
                            if let AstKind::VariableDeclaration { .. } = &stmt.kind {
                                self.check_statement_in_function(stmt, return_type, symbol_table, type_system, errors);
                            }
                        }
                        
                        // 第二遍：处理所有其他语句
                        for stmt in statements {
                            if !matches!(&stmt.kind, AstKind::VariableDeclaration { .. }) {
                                self.check_statement_in_function(stmt, return_type, symbol_table, type_system, errors);
                            }
                        }
                        
                        // 退出复合语句作用域
                        symbol_table.exit_scope();
                    }
                    _ => {
                        // 单个语句
                        self.check_statement_in_function(function_body, return_type, symbol_table, type_system, errors);
                    }
                }
            }
            _ => {
                errors.push(SemanticError {
                    message: "期望函数体语句".to_string(),
                    span: function_body.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::ScopeError,
                });
            }
        }
    }
    
    /// 检查函数中的语句
    /// 
    /// 按照clang设计理念：检查语句的语义正确性
    /// 
    /// # 参数
    /// * `stmt` - 语句AST节点
    /// * `return_type` - 函数返回类型
    /// * `symbol_table` - 符号表
    /// * `type_system` - 类型系统
    /// * `errors` - 错误列表
    fn check_statement_in_function(
        &self,
        stmt: &Ast,
        return_type: &Option<Type>,
        symbol_table: &mut SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {
        match &stmt.kind {
            AstKind::VariableDeclaration { variable_name, variable_type, initial_value, is_const } => {
                // 处理变量声明
                if let Err(msg) = symbol_table.add_variable(variable_name, variable_type.clone(), stmt.span.clone(), *is_const) {
                    errors.push(SemanticError {
                        message: msg,
                        span: stmt.span.clone(),
                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::Redefinition,
                    });
                }
                
                // 使用变量检查器执行完整的声明检查（含数组初始化等）
                let var_checker = crate::frontend::SemanticAnalyzer::checkers::var_checker::VarChecker::new();
                var_checker.check_variable_declaration(stmt, symbol_table, type_system, errors);
            }
            AstKind::Statement(statement) => {
                match statement {
                    crate::frontend::ast::Statement::Return { value } => {
                        // 检查返回语句
                        if let Some(return_expr) = value {
                            self.check_return_statement(return_expr, return_type, symbol_table, type_system, errors);
                        } else {
                            // 无返回值，检查函数是否声明为void
                            if let Some(ret_type) = return_type {
                                if !matches!(ret_type, Type::VoidType) {
                                    errors.push(SemanticError {
                                        message: format!("函数声明返回类型为{:?}，但返回语句无值", ret_type),
                                        span: stmt.span.clone(),
                                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                                    });
                                }
                            }
                        }
                    }
                    crate::frontend::ast::Statement::ExpressionStatement { expression } => {
                        // 检查表达式语句
                        self.check_expression_statement(expression, symbol_table, type_system, errors);
                    }
                    crate::frontend::ast::Statement::Compound { statements } => {
                        // 进入复合语句作用域
                        symbol_table.enter_scope("compound");
                        
                        // 递归检查复合语句中的所有语句
                        for sub_stmt in statements {
                            self.check_statement_in_function(sub_stmt, return_type, symbol_table, type_system, errors);
                        }
                        
                        // 退出复合语句作用域
                        symbol_table.exit_scope();
                    }
                    crate::frontend::ast::Statement::If { condition, then_branch, else_branch } => {
                        // 检查条件表达式
                        self.check_condition_expression(condition, symbol_table, type_system, errors);
                        
                        // 检查then分支
                        self.check_statement_in_function(then_branch, return_type, symbol_table, type_system, errors);
                        
                        // 检查else分支
                        if let Some(else_branch) = else_branch {
                            self.check_statement_in_function(else_branch, return_type, symbol_table, type_system, errors);
                        }
                    }
                    crate::frontend::ast::Statement::While { condition, body } => {
                        // 检查条件表达式
                        self.check_condition_expression(condition, symbol_table, type_system, errors);
                        
                        // 检查循环体
                        self.check_statement_in_function(body, return_type, symbol_table, type_system, errors);
                    }
                    crate::frontend::ast::Statement::For { initialization, condition, update, body } => {
                        // 进入for循环作用域
                        symbol_table.enter_scope("for_loop");
                        
                        // 检查初始化语句
                        if let Some(init) = initialization {
                            self.check_statement_in_function(init, return_type, symbol_table, type_system, errors);
                        }
                        
                        // 检查条件表达式
                        if let Some(cond) = condition {
                            self.check_condition_expression(cond, symbol_table, type_system, errors);
                        }
                        
                        // 检查更新表达式
                        if let Some(upd) = update {
                            self.check_expression_statement(upd, symbol_table, type_system, errors);
                        }
                        
                        // 检查循环体
                        self.check_statement_in_function(body, return_type, symbol_table, type_system, errors);
                        
                        // 退出for循环作用域
                        symbol_table.exit_scope();
                    }
                    _ => {
                        // 其他语句类型暂时跳过
                    }
                }
            }
            _ => {
                // 非语句节点，跳过
            }
        }
    }
    
    /// 检查返回语句
    /// 
    /// 按照clang设计理念：检查返回值的类型是否与函数声明匹配
    /// 
    /// # 参数
    /// * `return_expr` - 返回表达式
    /// * `return_type` - 函数返回类型
    /// * `symbol_table` - 符号表
    /// * `type_system` - 类型系统
    /// * `errors` - 错误列表
    fn check_return_statement(
        &self,
        return_expr: &Ast,
        return_type: &Option<Type>,
        symbol_table: &SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {
        match &return_expr.kind {
            AstKind::Expression(expr) => {
                match type_system.deduce_expression_type(expr, symbol_table) {
                    Ok(expr_type) => {
                        if let Some(expected_type) = return_type {
                            if !type_system.is_type_compatible(&expr_type, expected_type) {
                                errors.push(SemanticError {
                                    message: format!("返回类型不匹配：期望{:?}，实际{:?}", expected_type, expr_type),
                                    span: return_expr.span.clone(),
                                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                                });
                            }
                        }
                    }
                    Err(msg) => {
                        errors.push(SemanticError {
                            message: format!("无法推导返回表达式类型：{}", msg),
                            span: return_expr.span.clone(),
                            error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                        });
                    }
                }
            }
            _ => {
                errors.push(SemanticError {
                    message: "期望表达式作为返回值".to_string(),
                    span: return_expr.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                });
            }
        }
    }
    
    /// 检查初始值
    /// 
    /// 按照clang设计理念：检查变量初始值的类型匹配
    /// 
    /// # 参数
    /// * `init_expr` - 初始值表达式
    /// * `var_type` - 变量类型
    /// * `symbol_table` - 符号表
    /// * `type_system` - 类型系统
    /// * `errors` - 错误列表
    fn check_initial_value(
        &self,
        init_expr: &Ast,
        var_type: &Type,
        symbol_table: &SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {
        match &init_expr.kind {
            AstKind::Expression(expr) => {
                match type_system.deduce_expression_type(expr, symbol_table) {
                    Ok(expr_type) => {
                        if !type_system.is_type_compatible(&expr_type, var_type) {
                            errors.push(SemanticError {
                                message: format!("类型不匹配：无法将{:?}类型赋值给{:?}类型变量", expr_type, var_type),
                                span: init_expr.span.clone(),
                                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                            });
                        }
                    }
                    Err(msg) => {
                        errors.push(SemanticError {
                            message: format!("无法推导初始值表达式类型：{}", msg),
                            span: init_expr.span.clone(),
                            error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                        });
                    }
                }
            }
            _ => {
                errors.push(SemanticError {
                    message: "期望表达式作为初始值".to_string(),
                    span: init_expr.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                });
            }
        }
    }
    
    /// 检查表达式语句
    /// 
    /// 按照clang设计理念：检查表达式语句的语义正确性
    /// 
    /// # 参数
    /// * `expression` - 表达式AST节点
    /// * `symbol_table` - 符号表
    /// * `type_system` - 类型系统
    /// * `errors` - 错误列表
    fn check_expression_statement(
        &self,
        expression: &Ast,
        symbol_table: &mut SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {

        match &expression.kind {
            AstKind::Expression(expr) => {
                // 使用表达式检查器进行完整的表达式检查
                let expr_checker = crate::frontend::SemanticAnalyzer::checkers::expr_checker::ExprChecker::new();
                expr_checker.check_expression(expression, symbol_table, type_system, errors);
            }
            _ => {
                errors.push(SemanticError {
                    message: "期望表达式语句".to_string(),
                    span: expression.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                });
            }
        }
    }
    
    /// 检查条件表达式
    /// 
    /// 按照clang设计理念：检查条件表达式的类型是否为布尔类型
    /// 
    /// # 参数
    /// * `condition` - 条件表达式AST节点
    /// * `symbol_table` - 符号表
    /// * `type_system` - 类型系统
    /// * `errors` - 错误列表
    fn check_condition_expression(
        &self,
        condition: &Ast,
        symbol_table: &SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {
        match &condition.kind {
            AstKind::Expression(expr) => {
                match type_system.deduce_expression_type(expr, symbol_table) {
                    Ok(expr_type) => {
                        // 允许以 IntType 作为布尔（C 风格），也兼容显式 BoolType
                        let is_boolean_like = matches!(expr_type, Type::IntType | Type::BoolType);
                        if !is_boolean_like {
                            errors.push(SemanticError {
                                message: format!("条件表达式类型不匹配：期望布尔/整型，实际{:?}", expr_type),
                                span: condition.span.clone(),
                                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                            });
                        }
                    }
                    Err(msg) => {
                        errors.push(SemanticError {
                            message: format!("无法推导条件表达式类型：{}", msg),
                            span: condition.span.clone(),
                            error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                        });
                    }
                }
            }
            _ => {
                errors.push(SemanticError {
                    message: "期望表达式作为条件".to_string(),
                    span: condition.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                });
            }
        }
    }
}

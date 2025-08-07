use crate::frontend::ast::{Ast, AstKind, Type, Expression};
use crate::frontend::span::Span;
use super::super::symbol_table::{SymbolTable, SymbolKind};
use super::super::type_system::TypeSystem;
use super::super::sema::SemanticError;

/// 变量检查器
/// 
/// 按照clang设计理念实现：
/// 1. 变量声明检查：重复定义、类型检查
/// 2. 变量使用检查：未定义变量、类型匹配
/// 3. 作用域管理：正确处理变量作用域
/// 4. 初始化检查：检查变量初始化
pub struct VarChecker;

impl VarChecker {
    /// 创建新的变量检查器
    pub fn new() -> Self {
        Self
    }
    
    /// 收集变量声明
    /// 
    /// 按照clang设计理念：先收集所有声明，建立符号表
    /// 
    /// # 参数
    /// * `var_decl` - 变量声明AST节点
    /// * `symbol_table` - 符号表
    /// * `errors` - 错误列表
    pub fn collect_variable_declaration(
        &self,
        var_decl: &Ast,
        symbol_table: &mut SymbolTable,
        errors: &mut Vec<SemanticError>,
    ) {
        match &var_decl.kind {
            AstKind::VariableDeclaration {
                variable_name,
                variable_type,
                initial_value,
            } => {
                // 添加变量到符号表
                match symbol_table.add_variable(variable_name, variable_type.clone(), var_decl.span.clone()) {
                    Ok(()) => {
                        // 收集阶段只添加符号，不进行详细检查
                        // 详细检查在第二阶段进行
                    }
                    Err(msg) => {
                        errors.push(SemanticError {
                            message: msg,
                            span: var_decl.span.clone(),
                            error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::Redefinition,
                        });
                    }
                }
            }
            _ => {
                errors.push(SemanticError {
                    message: "期望变量声明节点".to_string(),
                    span: var_decl.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::ScopeError,
                });
            }
        }
    }
    
    /// 检查变量声明
    /// 
    /// 按照clang设计理念：进行完整的语义检查
    /// 
    /// # 参数
    /// * `var_decl` - 变量声明AST节点
    /// * `symbol_table` - 符号表
    /// * `type_system` - 类型系统
    /// * `errors` - 错误列表
    pub fn check_variable_declaration(
        &self,
        var_decl: &Ast,
        symbol_table: &mut SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {
        match &var_decl.kind {
            AstKind::VariableDeclaration {
                variable_name,
                variable_type,
                initial_value,
            } => {
                // 检查类型是否有效
                if !type_system.is_valid_type(variable_type) {
                    errors.push(SemanticError {
                        message: format!("无效的类型：{:?}", variable_type),
                        span: var_decl.span.clone(),
                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                    });
                }
                
                // 检查初始值
                if let Some(init_expr) = initial_value {
                    self.check_initial_value(init_expr, variable_type, symbol_table, type_system, errors);
                }
            }
            _ => {
                errors.push(SemanticError {
                    message: "期望变量声明节点".to_string(),
                    span: var_decl.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::ScopeError,
                });
            }
        }
    }
    
    /// 检查变量使用
    /// 
    /// 按照clang设计理念：检查变量是否已定义
    /// 
    /// # 参数
    /// * `var_name` - 变量名称
    /// * `span` - 使用位置
    /// * `symbol_table` - 符号表
    /// * `errors` - 错误列表
    pub fn check_variable_use(
        &self,
        var_name: &str,
        span: &Span,
        symbol_table: &SymbolTable,
        errors: &mut Vec<SemanticError>,
    ) {
        if let Some(symbol) = symbol_table.lookup_symbol(var_name) {
            if !symbol.is_defined {
                errors.push(SemanticError {
                    message: format!("变量 '{}' 已声明但未定义", var_name),
                    span: span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::UndefinedVariable,
                });
            }
        } else {
            errors.push(SemanticError {
                message: format!("未定义的变量：'{}'", var_name),
                span: span.clone(),
                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::UndefinedVariable,
            });
        }
    }
    
    /// 检查初始值
    /// 
    /// 按照clang设计理念：使用类型系统进行完整的类型检查
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
                // 使用类型系统推导表达式类型
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
                            message: format!("无法推导表达式类型：{}", msg),
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
    
    /// 检查变量赋值
    /// 
    /// 按照clang设计理念：检查赋值操作的语义正确性
    /// 
    /// # 参数
    /// * `target_name` - 目标变量名
    /// * `value_expr` - 赋值表达式
    /// * `span` - 赋值位置
    /// * `symbol_table` - 符号表
    /// * `type_system` - 类型系统
    /// * `errors` - 错误列表
    pub fn check_variable_assignment(
        &self,
        target_name: &str,
        value_expr: &Ast,
        span: &Span,
        symbol_table: &SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {
        // 检查目标变量是否存在
        if let Some(target_symbol) = symbol_table.lookup_symbol(target_name) {
            if target_symbol.kind != SymbolKind::Variable {
                errors.push(SemanticError {
                    message: format!("'{}' 不是变量", target_name),
                    span: span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                });
                return;
            }
            
            // 检查赋值表达式类型
            match &value_expr.kind {
                AstKind::Expression(expr) => {
                    match type_system.deduce_expression_type(expr, symbol_table) {
                        Ok(value_type) => {
                            if !type_system.is_type_compatible(&value_type, &target_symbol.data_type) {
                                errors.push(SemanticError {
                                    message: format!("类型不匹配：无法将{:?}类型赋值给{:?}类型变量", 
                                                   value_type, target_symbol.data_type),
                                    span: span.clone(),
                                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                                });
                            }
                        }
                        Err(msg) => {
                            errors.push(SemanticError {
                                message: format!("无法推导赋值表达式类型：{}", msg),
                                span: span.clone(),
                                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                            });
                        }
                    }
                }
                _ => {
                    errors.push(SemanticError {
                        message: "期望表达式作为赋值值".to_string(),
                        span: span.clone(),
                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                    });
                }
            }
        } else {
            errors.push(SemanticError {
                message: format!("未定义的变量：'{}'", target_name),
                span: span.clone(),
                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::UndefinedVariable,
            });
        }
    }
}

use crate::frontend::ast::{Ast, AstKind, Statement};
use crate::frontend::span::Span;
use super::super::symbol_table::SymbolTable;
use super::super::type_system::TypeSystem;
use super::super::sema::SemanticError;
use crate::frontend::ast::Type;

/// 语句检查器
/// 
/// 按照clang设计理念实现：
/// 1. 控制流检查：break/continue在循环内
/// 2. 返回语句检查：返回类型匹配
/// 3. 条件语句检查：条件表达式类型
/// 4. 循环语句检查：循环条件类型
pub struct StmtChecker {
    /// 当前是否在循环内
    in_loop: bool,
}

impl StmtChecker {
    /// 创建新的语句检查器
    pub fn new() -> Self {
        Self {
            in_loop: false,
        }
    }
    
    /// 检查语句
    pub fn check_statement(
        &mut self,
        stmt: &Ast,
        symbol_table: &mut SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {
        match &stmt.kind {
            AstKind::Statement(statement) => {
                match statement {
                    Statement::Compound { statements } => {
                        // 进入新的作用域
                        symbol_table.enter_scope("block");
                        
                        // 检查复合语句中的所有语句
                        for stmt in statements {
                            self.check_statement(stmt, symbol_table, type_system, errors);
                        }
                        
                        // 退出作用域
                        symbol_table.exit_scope();
                    }
                    Statement::If { condition, then_branch, else_branch } => {
                        // 检查条件表达式类型
                        self.check_condition_expression(condition, symbol_table, type_system, errors);
                        
                        // 检查then分支
                        self.check_statement(then_branch, symbol_table, type_system, errors);
                        
                        // 检查else分支（如果存在）
                        if let Some(else_stmt) = else_branch {
                            self.check_statement(else_stmt, symbol_table, type_system, errors);
                        }
                    }
                    Statement::While { condition, body } => {
                        // 检查循环条件表达式类型
                        self.check_condition_expression(condition, symbol_table, type_system, errors);
                        
                        // 标记进入循环
                        let was_in_loop = self.in_loop;
                        self.in_loop = true;
                        
                        // 检查循环体
                        self.check_statement(body, symbol_table, type_system, errors);
                        
                        // 恢复循环状态
                        self.in_loop = was_in_loop;
                    }
                    Statement::For { initialization, condition, update, body } => {
                        // 检查初始化语句
                        if let Some(init) = initialization {
                            self.check_statement(init, symbol_table, type_system, errors);
                        }
                        
                        // 检查条件表达式
                        if let Some(cond) = condition {
                            self.check_condition_expression(cond, symbol_table, type_system, errors);
                        }
                        
                        // 标记进入循环
                        let was_in_loop = self.in_loop;
                        self.in_loop = true;
                        
                        // 检查循环体
                        self.check_statement(body, symbol_table, type_system, errors);
                        
                        // 检查更新语句
                        if let Some(upd) = update {
                            self.check_statement(upd, symbol_table, type_system, errors);
                        }
                        
                        // 恢复循环状态
                        self.in_loop = was_in_loop;
                    }
                    Statement::Return { value } => {
                        // 检查返回语句
                        if let Some(return_expr) = value {
                            self.check_return_expression(return_expr, symbol_table, type_system, errors);
                        }
                    }
                    Statement::Break => {
                        // 检查break语句是否在循环内
                        if !self.in_loop {
                            errors.push(SemanticError {
                                message: "break语句必须在循环内".to_string(),
                                span: stmt.span.clone(),
                                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::ControlFlowError,
                            });
                        }
                    }
                    Statement::Continue => {
                        // 检查continue语句是否在循环内
                        if !self.in_loop {
                            errors.push(SemanticError {
                                message: "continue语句必须在循环内".to_string(),
                                span: stmt.span.clone(),
                                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::ControlFlowError,
                            });
                        }
                    }
                    Statement::ExpressionStatement { expression } => {
                        // 检查表达式语句
                        self.check_expression_statement(expression, symbol_table, type_system, errors);
                    }
                    Statement::Empty => {
                        // 空语句，无需检查
                    }
                }
            }
            AstKind::VariableDeclaration { .. } => {
                // 变量声明语句，由var_checker处理
                // 这里可以添加额外的检查
            }
            _ => {
                errors.push(SemanticError {
                    message: "期望语句节点".to_string(),
                    span: stmt.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::ScopeError,
                });
            }
        }
    }
    
    /// 检查条件表达式
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
                        // 条件表达式应该是int类型（布尔值用int表示）
                        if !matches!(expr_type, Type::IntType) {
                            errors.push(SemanticError {
                                message: format!("条件表达式类型错误：期望int，实际{:?}", expr_type),
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
    
    /// 检查返回表达式
    fn check_return_expression(
        &self,
        return_expr: &Ast,
        symbol_table: &SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {
        match &return_expr.kind {
            AstKind::Expression(expr) => {
                match type_system.deduce_expression_type(expr, symbol_table) {
                    Ok(expr_type) => {
                        // 检查类型是否有效
                        if !type_system.is_valid_type(&expr_type) {
                            errors.push(SemanticError {
                                message: format!("无效的返回类型：{:?}", expr_type),
                                span: return_expr.span.clone(),
                                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                            });
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
    
    /// 检查表达式语句
    fn check_expression_statement(
        &self,
        expression: &Ast,
        symbol_table: &SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {
        match &expression.kind {
            AstKind::Expression(expr) => {
                // 检查表达式类型是否有效
                match type_system.deduce_expression_type(expr, symbol_table) {
                    Ok(_) => {
                        // 表达式类型有效，无需进一步检查
                    }
                    Err(msg) => {
                        errors.push(SemanticError {
                            message: format!("表达式语句类型错误：{}", msg),
                            span: expression.span.clone(),
                            error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                        });
                    }
                }
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
    
    /// 重置循环状态（用于新的函数检查）
    pub fn reset_loop_state(&mut self) {
        self.in_loop = false;
    }
}

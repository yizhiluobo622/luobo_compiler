use crate::frontend::ast::{Ast, AstKind, Expression};
use crate::frontend::span::Span;
use crate::frontend::SemanticAnalyzer::symbol_table::SymbolTable;
use crate::frontend::SemanticAnalyzer::type_system::TypeSystem;
use crate::frontend::SemanticAnalyzer::sema::SemanticError;

/// 表达式检查器
/// 
/// 按照clang设计理念实现：
/// 1. 类型推导：自动推导表达式类型
/// 2. 类型检查：检查操作数类型兼容性
/// 3. 函数调用检查：参数匹配、返回类型
/// 4. 常量折叠：优化常量表达式
pub struct ExprChecker;

impl ExprChecker {
    /// 创建新的表达式检查器
    pub fn new() -> Self {
        Self
    }
    
    /// 检查表达式
    pub fn check_expression(
        &self,
        expr: &Ast,
        symbol_table: &mut SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {
        match &expr.kind {
            AstKind::Expression(expression) => {
                match expression {
                    Expression::Literal(_) => {
                        // 字面量表达式，类型推导由type_system处理
                        match type_system.deduce_expression_type(expression, symbol_table) {
                            Ok(_) => {
                                // 类型推导成功，无需进一步检查
                            }
                            Err(msg) => {
                                errors.push(SemanticError {
                                    message: format!("字面量表达式类型错误：{}", msg),
                                    span: expr.span.clone(),
                                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                                });
                            }
                        }
                    }
                    Expression::Identifier { name } => {
                        // 检查标识符是否已定义
                        self.check_identifier_use(name, &expr.span, symbol_table, errors);
                    }
                    Expression::BinaryOperation { operator, left_operand, right_operand } => {
                        // 检查二元运算
                        self.check_binary_operation(operator, left_operand, right_operand, symbol_table, type_system, errors);
                    }
                    Expression::UnaryOperation { operator, operand } => {
                        // 检查一元运算
                        self.check_unary_operation(operator, operand, symbol_table, type_system, errors);
                    }
                    Expression::Assignment { target, value } => {
                        // 检查赋值运算
                        self.check_assignment_operation(target, value, symbol_table, type_system, errors);
                    }
                    Expression::FunctionCall { function_name, arguments } => {
                        // 检查函数调用
                        self.check_function_call(function_name, arguments, symbol_table, type_system, errors);
                    }
                    Expression::ArrayAccess { array: _, index: _ } => {
                        // TODO: 实现数组访问检查
                        errors.push(SemanticError {
                            message: "数组访问暂未实现".to_string(),
                            span: expr.span.clone(),
                            error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                        });
                    }
                    Expression::MemberAccess { object: _, member_name: _ } => {
                        // TODO: 实现成员访问检查
                        errors.push(SemanticError {
                            message: "成员访问暂未实现".to_string(),
                            span: expr.span.clone(),
                            error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                        });
                    }
                }
            }
            _ => {
                errors.push(SemanticError {
                    message: "期望表达式节点".to_string(),
                    span: expr.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                });
            }
        }
    }
    
    /// 检查标识符使用
    fn check_identifier_use(
        &self,
        name: &str,
        span: &Span,
        symbol_table: &SymbolTable,
        errors: &mut Vec<SemanticError>,
    ) {
        if let Some(symbol) = symbol_table.lookup_symbol(name) {
            if !symbol.is_defined {
                errors.push(SemanticError {
                    message: format!("标识符 '{}' 已声明但未定义", name),
                    span: span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::UndefinedVariable,
                });
            }
        } else {
            errors.push(SemanticError {
                message: format!("未定义的标识符：'{}'", name),
                span: span.clone(),
                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::UndefinedVariable,
            });
        }
    }
    
    /// 检查二元运算
    fn check_binary_operation(
        &self,
        operator: &crate::frontend::ast::BinaryOperator,
        left_operand: &Ast,
        right_operand: &Ast,
        symbol_table: &mut SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {
        // 递归检查左右操作数
        self.check_expression(left_operand, symbol_table, type_system, errors);
        self.check_expression(right_operand, symbol_table, type_system, errors);
        
        // 检查操作数类型兼容性
        if let AstKind::Expression(left_expr) = &left_operand.kind {
            match type_system.deduce_expression_type(left_expr, symbol_table) {
                Ok(left_type) => {
                    if let AstKind::Expression(right_expr) = &right_operand.kind {
                        match type_system.deduce_expression_type(right_expr, symbol_table) {
                            Ok(right_type) => {
                                // 检查操作数类型是否适合该运算符
                                self.check_operator_compatibility(operator, &left_type, &right_type, left_operand, errors);
                            }
                            Err(msg) => {
                                errors.push(SemanticError {
                                    message: format!("右操作数类型推导失败：{}", msg),
                                    span: right_operand.span.clone(),
                                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                                });
                            }
                        }
                    } else {
                        errors.push(SemanticError {
                            message: "右操作数必须是表达式".to_string(),
                            span: right_operand.span.clone(),
                            error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                        });
                    }
                }
                Err(msg) => {
                    errors.push(SemanticError {
                        message: format!("左操作数类型推导失败：{}", msg),
                        span: left_operand.span.clone(),
                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                    });
                }
            }
        } else {
            errors.push(SemanticError {
                message: "左操作数必须是表达式".to_string(),
                span: left_operand.span.clone(),
                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
            });
        }
    }
    
    /// 检查一元运算
    fn check_unary_operation(
        &self,
        operator: &crate::frontend::ast::UnaryOperator,
        operand: &Ast,
        symbol_table: &mut SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {
        // 递归检查操作数
        self.check_expression(operand, symbol_table, type_system, errors);
        
        // 检查操作数类型是否适合该运算符
        if let AstKind::Expression(operand_expr) = &operand.kind {
            match type_system.deduce_expression_type(operand_expr, symbol_table) {
                Ok(operand_type) => {
                    self.check_unary_operator_compatibility(operator, &operand_type, operand, errors);
                }
                Err(msg) => {
                    errors.push(SemanticError {
                        message: format!("操作数类型推导失败：{}", msg),
                        span: operand.span.clone(),
                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                    });
                }
            }
        } else {
            errors.push(SemanticError {
                message: "操作数必须是表达式".to_string(),
                span: operand.span.clone(),
                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
            });
        }
    }
    
    /// 检查赋值运算
    fn check_assignment_operation(
        &self,
        target: &Ast,
        value: &Ast,
        symbol_table: &mut SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {
        // 检查目标是否为左值
        if let AstKind::Expression(Expression::Identifier { name }) = &target.kind {
            // 检查目标变量是否存在且可写
            if let Some(symbol) = symbol_table.lookup_symbol(name) {
                if symbol.kind != crate::frontend::SemanticAnalyzer::symbol_table::SymbolKind::Variable {
                    errors.push(SemanticError {
                        message: format!("'{}' 不是可赋值的变量", name),
                        span: target.span.clone(),
                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                    });
                }
            } else {
                errors.push(SemanticError {
                    message: format!("未定义的变量：'{}'", name),
                    span: target.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::UndefinedVariable,
                });
            }
        } else {
            errors.push(SemanticError {
                message: "赋值目标必须是变量".to_string(),
                span: target.span.clone(),
                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
            });
        }
        
        // 检查值表达式
        self.check_expression(value, symbol_table, type_system, errors);
    }
    
    /// 检查函数调用
    fn check_function_call(
        &self,
        function_name: &str,
        arguments: &[Ast],
        symbol_table: &SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {
        // 检查函数是否存在
        if let Some(function_symbol) = symbol_table.lookup_symbol(function_name) {
            if function_symbol.kind != crate::frontend::SemanticAnalyzer::symbol_table::SymbolKind::Function {
                errors.push(SemanticError {
                    message: format!("'{}' 不是函数", function_name),
                    span: arguments.first().map(|arg| arg.span.clone()).unwrap_or_else(|| Span::new(0, 0, 0, 0, 0)),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::UndefinedFunction,
                });
                return;
            }
            
            // 检查参数数量和类型
            if let Some(expected_params) = &function_symbol.parameters {
                if arguments.len() != expected_params.len() {
                    errors.push(SemanticError {
                        message: format!("函数 '{}' 参数数量不匹配：期望 {} 个，实际 {} 个", 
                                       function_name, expected_params.len(), arguments.len()),
                        span: arguments.first().map(|arg| arg.span.clone()).unwrap_or_else(|| Span::new(0, 0, 0, 0, 0)),
                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::FunctionArgumentError,
                    });
                } else {
                    // 检查参数类型
                    for (i, (arg, expected_type)) in arguments.iter().zip(expected_params.iter()).enumerate() {
                        if let AstKind::Expression(expr) = &arg.kind {
                            match type_system.deduce_expression_type(expr, symbol_table) {
                                Ok(arg_type) => {
                                    if !type_system.is_type_compatible(&arg_type, expected_type) {
                                        errors.push(SemanticError {
                                            message: format!("函数 '{}' 参数 {} 类型不匹配：期望 {:?}，实际 {:?}", 
                                                           function_name, i + 1, expected_type, arg_type),
                                            span: arg.span.clone(),
                                            error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::FunctionArgumentError,
                                        });
                                    }
                                }
                                Err(msg) => {
                                    errors.push(SemanticError {
                                        message: format!("函数 '{}' 参数 {} 类型推导失败：{}", 
                                                       function_name, i + 1, msg),
                                        span: arg.span.clone(),
                                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::FunctionArgumentError,
                                    });
                                }
                            }
                        }
                    }
                }
            }
        } else {
            errors.push(SemanticError {
                message: format!("未定义的函数：'{}'", function_name),
                span: arguments.first().map(|arg| arg.span.clone()).unwrap_or_else(|| Span::new(0, 0, 0, 0, 0)),
                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::UndefinedFunction,
            });
        }
    }
    
    /// 检查二元运算符兼容性
    fn check_operator_compatibility(
        &self,
        operator: &crate::frontend::ast::BinaryOperator,
        left_type: &crate::frontend::ast::Type,
        right_type: &crate::frontend::ast::Type,
        operand: &Ast,
        errors: &mut Vec<SemanticError>,
    ) {
        // 根据运算符类型检查操作数类型
        match operator {
            crate::frontend::ast::BinaryOperator::Add | 
            crate::frontend::ast::BinaryOperator::Subtract |
            crate::frontend::ast::BinaryOperator::Multiply |
            crate::frontend::ast::BinaryOperator::Divide |
            crate::frontend::ast::BinaryOperator::Modulo => {
                // 算术运算：操作数必须是数值类型
                if !matches!(left_type, crate::frontend::ast::Type::IntType | crate::frontend::ast::Type::FloatType) {
                    errors.push(SemanticError {
                        message: format!("算术运算左操作数类型错误：期望数值类型，实际{:?}", left_type),
                        span: operand.span.clone(),
                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                    });
                }
                if !matches!(right_type, crate::frontend::ast::Type::IntType | crate::frontend::ast::Type::FloatType) {
                    errors.push(SemanticError {
                        message: format!("算术运算右操作数类型错误：期望数值类型，实际{:?}", right_type),
                        span: operand.span.clone(),
                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                    });
                }
            }
            crate::frontend::ast::BinaryOperator::Equal |
            crate::frontend::ast::BinaryOperator::NotEqual |
            crate::frontend::ast::BinaryOperator::LessThan |
            crate::frontend::ast::BinaryOperator::LessEqual |
            crate::frontend::ast::BinaryOperator::GreaterThan |
            crate::frontend::ast::BinaryOperator::GreaterEqual => {
                // 比较运算：操作数必须是数值类型
                if !matches!(left_type, crate::frontend::ast::Type::IntType | crate::frontend::ast::Type::FloatType) {
                    errors.push(SemanticError {
                        message: format!("比较运算左操作数类型错误：期望数值类型，实际{:?}", left_type),
                        span: operand.span.clone(),
                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                    });
                }
                if !matches!(right_type, crate::frontend::ast::Type::IntType | crate::frontend::ast::Type::FloatType) {
                    errors.push(SemanticError {
                        message: format!("比较运算右操作数类型错误：期望数值类型，实际{:?}", right_type),
                        span: operand.span.clone(),
                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                    });
                }
            }
            crate::frontend::ast::BinaryOperator::LogicalAnd |
            crate::frontend::ast::BinaryOperator::LogicalOr => {
                // 逻辑运算：操作数必须是int类型（布尔值）
                if !matches!(left_type, crate::frontend::ast::Type::IntType) {
                    errors.push(SemanticError {
                        message: format!("逻辑运算左操作数类型错误：期望int，实际{:?}", left_type),
                        span: operand.span.clone(),
                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                    });
                }
                if !matches!(right_type, crate::frontend::ast::Type::IntType) {
                    errors.push(SemanticError {
                        message: format!("逻辑运算右操作数类型错误：期望int，实际{:?}", right_type),
                        span: operand.span.clone(),
                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                    });
                }
            }
            _ => {
                // 其他运算符暂不支持
                errors.push(SemanticError {
                    message: format!("不支持的二元运算符：{:?}", operator),
                    span: operand.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                });
            }
        }
    }
    
    /// 检查一元运算符兼容性
    fn check_unary_operator_compatibility(
        &self,
        operator: &crate::frontend::ast::UnaryOperator,
        operand_type: &crate::frontend::ast::Type,
        operand: &Ast,
        errors: &mut Vec<SemanticError>,
    ) {
        match operator {
            crate::frontend::ast::UnaryOperator::Plus |
            crate::frontend::ast::UnaryOperator::Minus => {
                // 一元算术运算：操作数必须是数值类型
                if !matches!(operand_type, crate::frontend::ast::Type::IntType | crate::frontend::ast::Type::FloatType) {
                    errors.push(SemanticError {
                        message: format!("一元算术运算操作数类型错误：期望数值类型，实际{:?}", operand_type),
                        span: operand.span.clone(),
                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                    });
                }
            }
            crate::frontend::ast::UnaryOperator::LogicalNot => {
                // 逻辑非：操作数应该是int类型（布尔值）
                if !matches!(operand_type, crate::frontend::ast::Type::IntType) {
                    errors.push(SemanticError {
                        message: format!("逻辑非操作数类型错误：期望int，实际{:?}", operand_type),
                        span: operand.span.clone(),
                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                    });
                }
            }
            _ => {
                // 其他一元运算符暂不支持
                errors.push(SemanticError {
                    message: format!("不支持的一元运算符：{:?}", operator),
                    span: operand.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                });
            }
        }
    }
}

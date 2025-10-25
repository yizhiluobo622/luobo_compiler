use crate::frontend::ast::{Ast, AstKind, Expression, Type, BinaryOperator, Literal};
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
                        // 先进行冗余比较优化
                        let optimized_expr = self.optimize_redundant_comparisons(expr);
                        
                        // 检查优化后的二元运算
                        if let AstKind::Expression(Expression::BinaryOperation { operator, left_operand, right_operand }) = &optimized_expr.kind {
                            self.check_binary_operation(operator, left_operand, right_operand, symbol_table, type_system, errors);
                        }
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
                    Expression::ArrayAccess { array, index } => {
                        // 先递归检查子表达式
                        self.check_expression(array, symbol_table, type_system, errors);
                        self.check_expression(index, symbol_table, type_system, errors);
                        
                        // 然后进行数组访问特定的检查
                        self.check_array_access_logic(array, index, symbol_table, type_system, errors);
                    }
                    Expression::InitializerList { elements } => {
                        // 递归检查初始化列表的每个元素
                        for elem in elements {
                            self.check_expression(elem, symbol_table, type_system, errors);
                        }
                        // 具体与目标数组类型的一致性由数组初始化检查负责（var_checker）
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
                    message: format!("变量 '{}' 已声明但未定义", name),
                    span: span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::UndefinedVariable,
                });
            }
        } else {
            errors.push(SemanticError {
                message: format!("未定义的变量：'{}'", name),
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
        match &target.kind {
            AstKind::Expression(Expression::Identifier { name }) => {
                // 检查目标变量是否存在且可写
                if let Some(symbol) = symbol_table.lookup_symbol(name) {
                    if symbol.kind != crate::frontend::SemanticAnalyzer::symbol_table::SymbolKind::Variable
                        && symbol.kind != crate::frontend::SemanticAnalyzer::symbol_table::SymbolKind::Parameter
                    {
                        errors.push(SemanticError {
                            message: format!("'{}' 不是可赋值的变量", name),
                            span: target.span.clone(),
                            error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                        });
                    } else if symbol.is_const {
                        errors.push(SemanticError {
                            message: format!("不能给 const 变量 '{}' 赋值", name),
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
            }
            AstKind::Expression(Expression::ArrayAccess { array, index }) => {
                // 数组访问也是合法的左值
                // 先检查数组和索引表达式
                self.check_expression(array, symbol_table, type_system, errors);
                self.check_expression(index, symbol_table, type_system, errors);
                
                // 然后检查数组访问的特定逻辑
                self.check_array_access_logic(array, index, symbol_table, type_system, errors);
            }
            _ => {
                errors.push(SemanticError {
                    message: "赋值目标必须是变量或数组元素".to_string(),
                    span: target.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                });
            }
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
        // 先推导参数类型
        let mut argument_types = Vec::new();
        for arg in arguments {
            if let AstKind::Expression(expr) = &arg.kind {
                match type_system.deduce_expression_type(expr, symbol_table) {
                    Ok(arg_type) => argument_types.push(arg_type),
                    Err(_) => argument_types.push(Type::IntType), // 默认类型
                }
            } else {
                argument_types.push(Type::IntType); // 默认类型
            }
        }
        
        // 使用专门的函数查找方法，避免与同名变量冲突
        if let Some(function_symbol) = symbol_table.lookup_function(function_name, &argument_types) {
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
    
    /// 检查数组访问的逻辑（不包含递归检查）
    /// 
    /// 验证：
    /// 1. 被访问的对象确实是数组类型
    /// 2. 索引表达式是整数类型
    /// 3. 如果数组有已知大小，检查索引是否在范围内（常量索引）
    fn check_array_access_logic(
        &self,
        array: &Ast,
        index: &Ast,
        symbol_table: &SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {
        // 注意：调用此方法之前，子表达式已经被递归检查
        // 这里只进行数组访问特定的类型检查
        
        // 获取数组表达式的类型
        match type_system.deduce_ast_type(array, symbol_table) {
            Ok(array_type) => {
                self.check_array_access_type(array_type, index, symbol_table, type_system, errors, array);
            }
            Err(msg) => {
                errors.push(SemanticError {
                    message: format!("无法推导数组表达式类型：{}", msg),
                    span: array.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                });
            }
        }
    }
    
    /// 递归检查数组访问类型
    /// 
    /// 支持多维数组访问，递归检查每一层
    /// 
    /// # 参数
    /// * `array_type` - 当前层的数组类型
    /// * `index` - 索引表达式
    /// * `symbol_table` - 符号表
    /// * `type_system` - 类型系统
    /// * `errors` - 错误列表
    /// * `array_expr` - 数组表达式AST节点
    fn check_array_access_type(
        &self,
        array_type: crate::frontend::ast::Type,
        index: &Ast,
        symbol_table: &SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
        array_expr: &Ast,
    ) {
        match array_type {
            crate::frontend::ast::Type::ArrayType { element_type, array_size } => {
                // 验证索引类型
                match type_system.deduce_ast_type(index, symbol_table) {
                    Ok(index_type) => {
                        if !matches!(index_type, crate::frontend::ast::Type::IntType) {
                            errors.push(SemanticError {
                                message: format!("数组索引必须是整数类型，实际类型：{:?}", index_type),
                                span: index.span.clone(),
                                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                            });
                        }
                        
                        // 如果索引是常量且数组有已知大小，检查边界
                        if let crate::frontend::ast::ArraySize::Fixed(array_size) = array_size {
                            if let Ok(constant_index) = self.try_evaluate_constant_index(index) {
                                if constant_index < 0 {
                                    errors.push(SemanticError {
                                        message: format!("数组索引不能为负数：{}", constant_index),
                                        span: index.span.clone(),
                                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                                    });
                                } else if constant_index as usize >= array_size {
                                    errors.push(SemanticError {
                                        message: format!("数组索引越界：索引 {} 超出数组大小 {}", constant_index, array_size),
                                        span: index.span.clone(),
                                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                                    });
                                }
                            }
                            // 对于非常量索引，我们无法在编译时检查边界
                        }
                        
                        // 递归检查下一层（如果是嵌套数组）
                        if let crate::frontend::ast::Type::ArrayType { .. } = element_type.as_ref() {
                            // 这里不需要递归调用，因为子表达式已经被检查过了
                            // 多维数组访问会在check_expression中被递归处理
                        }
                    }
                    Err(msg) => {
                        errors.push(SemanticError {
                            message: format!("无法推导数组索引类型：{}", msg),
                            span: index.span.clone(),
                            error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                        });
                    }
                }
            }
            crate::frontend::ast::Type::PointerType { target_type: _ } => {
                // 指针也可以进行索引访问（类似C语言的指针算术）
                match type_system.deduce_ast_type(index, symbol_table) {
                    Ok(index_type) => {
                        if !matches!(index_type, crate::frontend::ast::Type::IntType) {
                            errors.push(SemanticError {
                                message: format!("指针索引必须是整数类型，实际类型：{:?}", index_type),
                                span: index.span.clone(),
                                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                            });
                        }
                    }
                    Err(msg) => {
                        errors.push(SemanticError {
                            message: format!("无法推导指针索引类型：{}", msg),
                            span: index.span.clone(),
                            error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                        });
                    }
                }
            }
            _ => {
                errors.push(SemanticError {
                    message: format!("只能对数组或指针类型进行索引访问，实际类型：{:?}", array_type),
                    span: array_expr.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                });
            }
        }
    }
    
    /// 尝试求值常量索引表达式
    /// 
    /// 这是一个简化的常量求值器，只处理整数字面量
    /// 未来可以扩展支持更复杂的常量表达式
    fn try_evaluate_constant_index(&self, index_expr: &Ast) -> Result<i32, String> {
        match &index_expr.kind {
            crate::frontend::ast::AstKind::Expression(crate::frontend::ast::Expression::Literal(
                crate::frontend::ast::Literal::IntegerLiteral(value)
            )) => Ok(*value),
            _ => Err("非常量表达式".to_string()),
        }
    }
    
    /// 优化冗余的比较操作
    /// 
    /// 检测并优化连续的 != 0 比较，如：
    /// x != 0 != 0 != .0 != 0. -> x != 0
    fn optimize_redundant_comparisons(&self, expr: &Ast) -> Ast {
        match &expr.kind {
            AstKind::Expression(Expression::BinaryOperation { 
                operator: BinaryOperator::NotEqual, 
                left_operand, 
                right_operand 
            }) => {
                // 检查右操作数是否为0
                if self.is_zero_literal(right_operand) {
                    // 检查左操作数是否也是 != 0 比较
                    if let AstKind::Expression(Expression::BinaryOperation { 
                        operator: BinaryOperator::NotEqual, 
                        left_operand: nested_left, 
                        right_operand: nested_right 
                    }) = &left_operand.kind {
                        if self.is_zero_literal(nested_right) {
                            // 递归优化嵌套的 != 0 比较
                            return self.optimize_redundant_comparisons(left_operand);
                        }
                    }
                }
                
                // 如果无法优化，返回原表达式
                expr.clone()
            }
            _ => expr.clone(),
        }
    }
    
    /// 检查表达式是否为0字面量
    fn is_zero_literal(&self, expr: &Ast) -> bool {
        match &expr.kind {
            AstKind::Expression(Expression::Literal(literal)) => {
                match literal {
                    Literal::IntegerLiteral(0) => true,
                    Literal::FloatLiteral(0.0) => true,
                    _ => false,
                }
            }
            _ => false,
        }
    }
}

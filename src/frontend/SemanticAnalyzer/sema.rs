use crate::frontend::ast::{Ast, AstKind, Statement, Expression, Type, Literal, BinaryOperator, UnaryOperator};
use crate::frontend::span::Span;
use super::type_system::TypeSystem;
use super::symbol_table::SymbolTable;
use super::checkers::{
    var_checker::VarChecker,
    func_checker::FuncChecker,
    stmt_checker::StmtChecker,
    expr_checker::ExprChecker,
};

/// 语义分析器
/// 
/// 按照clang设计理念实现：
/// 1. 模块化设计：将不同类型的语义检查分离到不同模块
/// 2. 类型系统：强大的类型推导和类型检查
/// 3. 符号表管理：作用域管理、符号查找
/// 4. 错误恢复：即使有语义错误也能继续分析
/// 5. 诊断系统：详细的错误信息和修复建议
pub struct SemanticAnalyzer {
    /// 类型系统
    type_system: TypeSystem,
    /// 符号表
    symbol_table: SymbolTable,
    /// 变量检查器
    var_checker: VarChecker,
    /// 函数检查器
    func_checker: FuncChecker,
    /// 语句检查器
    stmt_checker: StmtChecker,
    /// 表达式检查器
    expr_checker: ExprChecker,
    /// 收集的语义错误
    errors: Vec<SemanticError>,
}

/// 语义分析错误
#[derive(Debug, Clone)]
pub struct SemanticError {
    /// 错误描述信息
    pub message: String,
    /// 错误在源代码中的位置
    pub span: Span,
    /// 错误类型
    pub error_type: SemanticErrorType,
}

/// 语义错误类型
#[derive(Debug, Clone)]
pub enum SemanticErrorType {
    /// 未定义的变量
    UndefinedVariable,
    /// 未定义的函数
    UndefinedFunction,
    /// 类型不匹配
    TypeMismatch,
    /// 重复定义
    Redefinition,
    /// 作用域错误
    ScopeError,
    /// 函数参数错误
    FunctionArgumentError,
    /// 控制流错误
    ControlFlowError,
}

impl SemanticAnalyzer {
    /// 创建新的语义分析器
    pub fn new() -> Self {
        let mut analyzer = Self {
            type_system: TypeSystem::new(),
            symbol_table: SymbolTable::new(),
            var_checker: VarChecker::new(),
            func_checker: FuncChecker::new(),
            stmt_checker: StmtChecker::new(),
            expr_checker: ExprChecker::new(),
            errors: Vec::new(),
        };
        // 预注册内建函数（供语义检查识别）
        analyzer.register_builtin_functions();
        analyzer
    }

    /// 注册内建函数（如 getint/putint）
    fn register_builtin_functions(&mut self) {
        // getint: () -> int
        let _ = self.symbol_table.add_function(
            "getint",
            Type::IntType,
            vec![],
            Span::start_only(0, 0, 0, 0),
        );
        // putint: (int) -> void
        let _ = self.symbol_table.add_function(
            "putint",
            Type::VoidType,
            vec![Type::IntType],
            Span::start_only(0, 0, 0, 0),
        );
        // getch: () -> int (读取单个字符)
        let _ = self.symbol_table.add_function(
            "getch",
            Type::IntType,
            vec![],
            Span::start_only(0, 0, 0, 0),
        );
        // putch: (int) -> void (输出单个字符)
        let _ = self.symbol_table.add_function(
            "putch",
            Type::VoidType,
            vec![Type::IntType],
            Span::start_only(0, 0, 0, 0),
        );
    }
    

    
    /// 分析AST，进行语义检查
    /// 
    /// 按照clang设计理念：
    /// - 模块化检查：分别检查变量、函数、语句、表达式
    /// - 错误恢复：即使有错误也继续分析，收集所有问题
    /// - 类型推导：自动推导表达式类型
    /// - 作用域管理：正确处理变量和函数的作用域
    /// 
    /// # 参数
    /// * `ast` - 抽象语法树
    /// 
    /// # 返回
    /// * `Ok(())` - 语义分析成功
    /// * `Err(Vec<SemanticError>)` - 语义错误列表
    pub fn analyze(&mut self, ast: &Ast) -> Result<(), Vec<SemanticError>> {
        // 清空之前的错误
        self.errors.clear();
        
        // 第一遍：收集所有声明（函数和全局变量）
        self.collect_declarations(ast);
        
        // 第二遍：重建符号表并进行语义检查
        self.rebuild_symbol_table_and_check(ast);
        
        // 如果有错误，返回错误列表；否则返回成功
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }
    
    /// 收集所有声明
    /// 
    /// 按照clang设计理念：先收集所有声明，建立符号表
    fn collect_declarations(&mut self, ast: &Ast) {
        match &ast.kind {
            AstKind::Program { functions, global_variables } => {
                // 收集全局变量声明
                for var_decl in global_variables {
                    self.var_checker.collect_variable_declaration(
                        var_decl, 
                        &mut self.symbol_table, 
                        &self.type_system,
                        &mut self.errors
                    );
                }
                
                // 收集函数声明
                for func_decl in functions {
                    self.func_checker.collect_function_declaration(
                        func_decl, 
                        &mut self.symbol_table, 
                        &mut self.errors
                    );
                }
            }
            _ => {
                let error = SemanticError {
                    message: "期望程序根节点".to_string(),
                    span: ast.span.clone(),
                    error_type: SemanticErrorType::ScopeError,
                };
                self.errors.push(error);
            }
        }
    }
    
    /// 重建符号表并进行语义检查
    /// 
    /// 按照clang设计理念：在检查阶段重建完整的符号表
    fn rebuild_symbol_table_and_check(&mut self, ast: &Ast) {
        match &ast.kind {
            AstKind::Program { functions, global_variables } => {
                // 处理全局变量
                for var in global_variables {
                    self.var_checker.check_variable_declaration(var, &mut self.symbol_table, &mut self.type_system, &mut self.errors);
                }
                
                // 处理函数
                for func in functions {
                    self.func_checker.check_function_declaration(func, &mut self.symbol_table, &mut self.type_system, &mut self.errors);
                }
            }
            _ => {}
        }
    }
    
    /// 获取所有语义错误
    pub fn get_errors(&self) -> &[SemanticError] {
        &self.errors
    }
    
    /// 检查是否有语义错误
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
    
    /// 添加错误（支持错误恢复）
    pub fn add_error(&mut self, error: SemanticError) {
        self.errors.push(error);
    }
    
    /// 获取类型系统引用
    pub fn get_type_system(&self) -> &TypeSystem {
        &self.type_system
    }
    
    /// 获取符号表引用
    pub fn get_symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }
    
    /// 计算常量表达式的值
    pub fn evaluate_constant_expression(&self, expr: &Ast, constants: &std::collections::HashMap<String, i64>) -> Option<i64> {
        match &expr.kind {
            AstKind::Expression(Expression::Literal(Literal::IntegerLiteral(value))) => {
                Some(*value as i64)
            }
            AstKind::Expression(Expression::Identifier { name }) => {
                constants.get(name).copied()
            }
            AstKind::Expression(Expression::BinaryOperation { left_operand, operator, right_operand }) => {
                let left_val = self.evaluate_constant_expression(left_operand, constants)?;
                let right_val = self.evaluate_constant_expression(right_operand, constants)?;
                
                match operator {
                    BinaryOperator::Add => Some(left_val + right_val),
                    BinaryOperator::Subtract => Some(left_val - right_val),
                    BinaryOperator::Multiply => Some(left_val * right_val),
                    BinaryOperator::Divide => {
                        if right_val != 0 {
                            Some(left_val / right_val)
                        } else {
                            None
                        }
                    }
                    BinaryOperator::Modulo => {
                        if right_val != 0 {
                            Some(left_val % right_val)
                        } else {
                            None
                        }
                    }
                    _ => None, // 其他运算符不支持常量折叠
                }
            }
            AstKind::Expression(Expression::UnaryOperation { operator, operand }) => {
                let operand_val = self.evaluate_constant_expression(operand, constants)?;
                match operator {
                    UnaryOperator::Minus => Some(-operand_val),
                    UnaryOperator::Plus => Some(operand_val),
                    _ => None, // 其他一元运算符不支持常量折叠
                }
            }
            _ => None, // 其他表达式类型不支持常量折叠
        }
    }
}



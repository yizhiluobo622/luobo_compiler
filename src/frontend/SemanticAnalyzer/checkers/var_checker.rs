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
    /// * `type_system` - 类型系统（用于计算常量表达式）
    /// * `errors` - 错误列表
    pub fn collect_variable_declaration(
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
                is_const,
            } => {
                // 添加变量到符号表
                match symbol_table.add_variable(variable_name, variable_type.clone(), var_decl.span.clone(), *is_const) {
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
                is_const,
            } => {
                // 检查类型是否有效
                if !type_system.is_valid_type(variable_type) {
                    errors.push(SemanticError {
                        message: format!("无效的类型：{:?}", variable_type),
                        span: var_decl.span.clone(),
                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                    });
                }
                
                // 对数组类型进行特殊检查
                if let Type::ArrayType { element_type, array_size } = variable_type {
                    // 递归检查嵌套数组类型
                    self.check_nested_array_type(element_type, symbol_table, type_system, errors, &var_decl.span);
                    
                    // 检查数组大小
                    if let crate::frontend::ast::ArraySize::Fixed(size) = array_size {
                        if *size == 0 {
                            errors.push(SemanticError {
                                message: "数组大小不能为0".to_string(),
                                span: var_decl.span.clone(),
                                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                            });
                        }
                    }
                    
                    // 如果有初始值，需要特殊处理数组初始化
                    if let Some(init_expr) = initial_value {
                        self.check_array_initialization(init_expr, variable_type, symbol_table, type_system, errors);
                        return; // 数组初始化有特殊处理，不走普通初始值检查
                    }
                }
                
                // 处理数组大小未知的情况
                if let Type::ArrayType { element_type, array_size } = variable_type {
                    match array_size {
                        crate::frontend::ast::ArraySize::Unspecified => {
                            // 数组大小未指定，这是正常的
                        }
                        crate::frontend::ast::ArraySize::Constant(_) => {
                            // 数组大小是常量，语义分析阶段会处理
                        }
                        crate::frontend::ast::ArraySize::Fixed(_) => {
                            // 数组大小已确定
                        }
                    }
                }
                
                // 检查初始值
                if let Some(init_expr) = initial_value {
                    self.check_initial_value(init_expr, variable_type, symbol_table, type_system, errors);
                }
                // const 变量如无初始值，可选：发出警告/错误，这里先不强制
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
            if target_symbol.kind != SymbolKind::Variable && target_symbol.kind != SymbolKind::Parameter {
                errors.push(SemanticError {
                    message: format!("'{}' 不是变量", target_name),
                    span: span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                });
                return;
            }
            // 禁止给 const 变量赋值
            if target_symbol.is_const {
                errors.push(SemanticError {
                    message: format!("不能给 const 变量 '{}' 赋值", target_name),
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
    
    /// 检查数组初始化
    /// 
    /// 验证数组初始化表达式是否符合数组类型要求
    /// 
    /// # 参数
    /// * `init_expr` - 初始化表达式
    /// * `array_type` - 数组类型
    /// * `symbol_table` - 符号表
    /// * `type_system` - 类型系统
    /// * `errors` - 错误列表
    fn check_array_initialization(
        &self,
        init_expr: &Ast,
        array_type: &Type,
        symbol_table: &SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
    ) {
        // 仅处理数组类型
        let Type::ArrayType { .. } = array_type else {
            errors.push(SemanticError {
                message: "期望数组类型用于数组初始化检查".to_string(),
                span: init_expr.span.clone(),
                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
            });
            return;
        };

        // 将初始化表达式拍平成一维元素列表（按行主序）
        let mut flat_elems: Vec<&Ast> = Vec::new();
        self.flatten_initializer_elements(init_expr, &mut flat_elems);

        // 空列表 {} 允许（等价零初始化）
        if flat_elems.is_empty() {
            return;
        }

        // 计算目标数组的总元素数（若所有维度均为已知）
        if let Some(total) = self.compute_total_array_elems(array_type) {
            if flat_elems.len() > total {
                errors.push(SemanticError {
                    message: format!(
                        "初始化器中元素过多：提供了 {} 个，数组容量为 {}",
                        flat_elems.len(), total
                    ),
                    span: init_expr.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                });
            }
        }

        // 基础元素类型（最内层）
        let base_elem_type = self.get_base_element_type(array_type);

        // 校验每个拍平后的元素与基础元素类型兼容
        for elem in flat_elems {
            if let AstKind::Expression(expr) = &elem.kind {
                match type_system.deduce_expression_type(expr, symbol_table) {
                    Ok(elem_ty) => {
                        if !type_system.is_type_compatible(&elem_ty, base_elem_type) {
                            errors.push(SemanticError {
                                message: format!(
                                    "初始化元素类型不匹配：期望 {:?}，实际 {:?}",
                                    base_elem_type, elem_ty
                                ),
                                span: elem.span.clone(),
                                error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                            });
                        }
                    }
                    Err(msg) => {
                        errors.push(SemanticError {
                            message: format!("无法推导初始化元素类型：{}", msg),
                            span: elem.span.clone(),
                            error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                        });
                    }
                }
            } else {
                errors.push(SemanticError {
                    message: "初始化元素必须是表达式".to_string(),
                    span: elem.span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                });
            }
        }
    }

    /// 将初始化表达式拍平成一维元素序列
    fn flatten_initializer_elements<'a>(&self, node: &'a Ast, acc: &mut Vec<&'a Ast>) {
        match &node.kind {
            AstKind::Expression(Expression::InitializerList { elements }) => {
                for e in elements {
                    self.flatten_initializer_elements(e, acc);
                }
            }
            _ => acc.push(node),
        }
    }

    /// 计算数组的总元素数；若存在未知维度返回 None
    fn compute_total_array_elems(&self, ty: &Type) -> Option<usize> {
        match ty {
            Type::ArrayType { element_type, array_size } => {
                let inner = self.compute_total_array_elems(element_type);
                match (array_size, inner) {
                    (crate::frontend::ast::ArraySize::Fixed(n), Some(m)) => (*n).checked_mul(m),
                    (crate::frontend::ast::ArraySize::Fixed(_), None) => None,
                    (_, _) => None,
                }
            }
            _ => Some(1),
        }
    }

    /// 获取最内层基础元素类型
    fn get_base_element_type<'a>(&'a self, ty: &'a Type) -> &'a Type {
        let mut cur = ty;
        while let Type::ArrayType { element_type, .. } = cur {
            cur = element_type.as_ref();
        }
        cur
    }

    /// 递归检查嵌套数组类型
    /// 
    /// 如果类型是数组类型，则递归检查其元素类型。
    /// 
    /// # 参数
    /// * `array_type` - 当前检查的数组类型
    /// * `symbol_table` - 符号表
    /// * `type_system` - 类型系统
    /// * `errors` - 错误列表
    /// * `current_span` - 当前检查的数组声明的跨度
    fn check_nested_array_type(
        &self,
        array_type: &Box<Type>,
        symbol_table: &SymbolTable,
        type_system: &TypeSystem,
        errors: &mut Vec<SemanticError>,
        current_span: &Span,
    ) {
        if let Type::ArrayType { element_type, array_size } = array_type.as_ref() {
            // 检查元素类型是否有效
            if !type_system.is_valid_type(element_type) {
                errors.push(SemanticError {
                    message: format!("数组元素类型无效：{:?}", element_type),
                    span: current_span.clone(),
                    error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                });
            }

            // 递归检查嵌套数组类型
            if let Type::ArrayType { .. } = element_type.as_ref() {
                self.check_nested_array_type(element_type, symbol_table, type_system, errors, current_span);
            }

            // 检查数组大小
            if let crate::frontend::ast::ArraySize::Fixed(size) = array_size {
                if *size == 0 {
                    errors.push(SemanticError {
                        message: "数组大小不能为0".to_string(),
                        span: current_span.clone(),
                        error_type: crate::frontend::SemanticAnalyzer::sema::SemanticErrorType::TypeMismatch,
                    });
                }
            }
        } else {
            // 如果不是数组类型，则不进行递归检查
        }
    }
}

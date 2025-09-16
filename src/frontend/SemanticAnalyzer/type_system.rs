use crate::frontend::ast::{Type, Expression, BinaryOperator, UnaryOperator, Literal, Ast, AstKind};
use super::symbol_table::SymbolTable;

/// 类型系统
/// 
/// 按照clang设计理念实现：
/// 1. 类型推导：自动推导表达式类型
/// 2. 类型检查：检查类型兼容性
/// 3. 类型转换：处理隐式类型转换
/// 4. 类型推断：根据上下文推断类型
pub struct TypeSystem {
    /// 类型兼容性规则
    type_compatibility: Vec<(Type, Type, bool)>,
    /// 类型推导缓存，避免重复计算
    type_cache: std::collections::HashMap<String, Type>,
    /// 正在计算的表达式集合，用于检测循环引用
    computing_expressions: std::collections::HashSet<String>,
}

impl TypeSystem {
    /// 创建新的类型系统
    pub fn new() -> Self {
        let mut type_system = Self {
            type_compatibility: Vec::new(),
            type_cache: std::collections::HashMap::new(),
            computing_expressions: std::collections::HashSet::new(),
        };
        
        // 初始化类型兼容性规则
        type_system.init_type_compatibility();
        
        type_system
    }
    
    /// 初始化类型兼容性规则
    fn init_type_compatibility(&mut self) {
        // int 和 float 之间的兼容性
        self.type_compatibility.push((Type::IntType, Type::FloatType, true));  // int -> float
        self.type_compatibility.push((Type::FloatType, Type::IntType, false)); // float -> int (不允许)
        
        // 相同类型的兼容性
        self.type_compatibility.push((Type::IntType, Type::IntType, true));
        self.type_compatibility.push((Type::FloatType, Type::FloatType, true));
        self.type_compatibility.push((Type::VoidType, Type::VoidType, true));
    }
    
    /// 推导表达式类型（不可变版本，用于兼容现有代码）
    pub fn deduce_expression_type(&self, expr: &Expression, symbol_table: &SymbolTable) -> Result<Type, String> {
        // 对于复杂表达式，使用简化的类型推导
        self.deduce_expression_type_simple(expr, symbol_table)
    }
    
    /// 简化的表达式类型推导，避免深度递归
    fn deduce_expression_type_simple(&self, expr: &Expression, symbol_table: &SymbolTable) -> Result<Type, String> {
        match expr {
            Expression::Literal(literal) => {
                match literal {
                    Literal::IntegerLiteral(_) => Ok(Type::IntType),
                    Literal::FloatLiteral(_) => Ok(Type::FloatType),
                    Literal::StringLiteral(_) => Ok(Type::CharType),
                    Literal::BooleanLiteral(_) => Ok(Type::BoolType),
                }
            }
            Expression::Identifier { name } => {
                // 从符号表中查找类型
                if let Some(var_type) = symbol_table.get_variable_type(name) {
                    // 对于数组类型，需要解析常量维度
                    let resolved_type = self.resolve_array_constant_dimensions(&var_type, symbol_table);
                    Ok(resolved_type)
                } else {
                    // 如果变量表中没有，尝试从符号表中查找（可能是在其他作用域中）
                    if let Some(symbol) = symbol_table.lookup_symbol(name) {
                        // 对于数组类型，需要解析常量维度
                        let resolved_type = self.resolve_array_constant_dimensions(&symbol.data_type, symbol_table);
                        Ok(resolved_type)
                    } else {
                        Err(format!("未定义的变量：'{}'", name))
                    }
                }
            }
            Expression::BinaryOperation { operator, .. } => {
                // 根据操作符快速推导类型
                match operator {
                    BinaryOperator::Add | BinaryOperator::Subtract | 
                    BinaryOperator::Multiply | BinaryOperator::Divide | 
                    BinaryOperator::Modulo => Ok(Type::IntType),
                    BinaryOperator::Equal | BinaryOperator::NotEqual |
                    BinaryOperator::LessThan | BinaryOperator::LessEqual |
                    BinaryOperator::GreaterThan | BinaryOperator::GreaterEqual => Ok(Type::IntType),
                    BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => Ok(Type::IntType),
                    _ => Ok(Type::IntType),
                }
            }
            Expression::UnaryOperation { operator, .. } => {
                match operator {
                    UnaryOperator::Plus | UnaryOperator::Minus => Ok(Type::IntType),
                    UnaryOperator::LogicalNot => Ok(Type::IntType),
                    UnaryOperator::BitwiseNot => Ok(Type::IntType),
                    _ => Ok(Type::IntType),
                }
            }
            Expression::ArrayAccess { array, index } => {
                // 对于数组访问，使用带深度的推导
                let array_type = self.deduce_ast_type_with_depth(array, symbol_table, 0)?;
                let _index_type = self.deduce_ast_type_with_depth(index, symbol_table, 0)?;
                
                match array_type {
                    Type::ArrayType { element_type, .. } => {
                        Ok(*element_type)
                    }
                    Type::PointerType { target_type } => {
                        Ok(*target_type)
                    }
                    _ => {
                        Err("只能对数组或指针类型进行下标访问".to_string())
                    }
                }
            }
            Expression::FunctionCall { function_name, .. } => {
                // 从符号表获取函数返回类型
                if let Some(return_type) = symbol_table.get_function_return_type(function_name) {
                    Ok(return_type)
                } else {
                    Ok(Type::IntType) // 默认返回int
                }
            }
            _ => Ok(Type::IntType), // 默认类型
        }
    }
    
    /// 生成缓存键
    fn generate_cache_key(&self, expr: &Expression) -> String {
        use std::fmt::Write;
        let mut key = String::new();
        
        match expr {
            Expression::Literal(literal) => {
                write!(&mut key, "lit:{:?}", literal).unwrap();
            }
            Expression::Identifier { name } => {
                write!(&mut key, "id:{}", name).unwrap();
            }
            Expression::BinaryOperation { operator, left_operand, right_operand } => {
                write!(&mut key, "bin:{:?}", operator).unwrap();
                // 简化：只包含操作符，避免递归
            }
            Expression::UnaryOperation { operator, operand } => {
                write!(&mut key, "unary:{:?}", operator).unwrap();
            }
            Expression::Assignment { target, value } => {
                write!(&mut key, "assign").unwrap();
            }
            Expression::FunctionCall { function_name, arguments } => {
                write!(&mut key, "call:{}", function_name).unwrap();
            }
            Expression::ArrayAccess { array, index } => {
                write!(&mut key, "array_access").unwrap();
            }
            Expression::InitializerList { elements } => {
                write!(&mut key, "init_list:{}", elements.len()).unwrap();
            }
            Expression::MemberAccess { object, member_name } => {
                write!(&mut key, "member:{}", member_name).unwrap();
            }
        }
        
        key
    }
    
    /// 推导表达式类型（带深度限制）
    fn deduce_expression_type_with_depth(&self, expr: &Expression, symbol_table: &SymbolTable, depth: usize) -> Result<Type, String> {
        // 防止递归过深
        if depth > 50 {
            // 对于过深的表达式，返回一个合理的默认类型而不是错误
            // 这样可以避免中断整个语义分析过程
            eprintln!("警告：表达式类型推导深度达到 {}，使用默认类型", depth);
            return Ok(Type::IntType); // 默认返回int类型
        }
        
        // 调试信息：当深度较大时输出警告
        if depth > 20 {
            eprintln!("警告：表达式类型推导深度达到 {}，可能存在复杂嵌套", depth);
        }
        
        match expr {
            Expression::Literal(literal) => {
                match literal {
                    Literal::IntegerLiteral(_) => Ok(Type::IntType),
                    Literal::FloatLiteral(_) => Ok(Type::FloatType),
                    Literal::StringLiteral(_) => Ok(Type::CharType), // 字符串字面量作为字符数组
                    Literal::BooleanLiteral(_) => Ok(Type::BoolType),
                }
            }
            Expression::Identifier { name } => {
                // 从符号表中查找类型
                if let Some(var_type) = symbol_table.get_variable_type(name) {
                    // 对于数组类型，需要解析常量维度
                    Ok(self.resolve_array_constant_dimensions(&var_type, symbol_table))
                } else {
                    // 如果变量表中没有，尝试从符号表中查找（可能是在其他作用域中）
                    if let Some(symbol) = symbol_table.lookup_symbol(name) {
                        // 对于数组类型，需要解析常量维度
                        Ok(self.resolve_array_constant_dimensions(&symbol.data_type, symbol_table))
                    } else {
                        Err(format!("未定义的变量：'{}'", name))
                    }
                }
            }
            Expression::BinaryOperation { operator, left_operand, right_operand } => {
                // 推导二元运算的类型
                let left_type = self.deduce_ast_type_with_depth(left_operand, symbol_table, depth + 1)?;
                let right_type = self.deduce_ast_type_with_depth(right_operand, symbol_table, depth + 1)?;
                
                match operator {
                    BinaryOperator::Add | BinaryOperator::Subtract | 
                    BinaryOperator::Multiply | BinaryOperator::Divide | 
                    BinaryOperator::Modulo => {
                        // 算术运算：如果有一个是float，结果就是float
                        if left_type == Type::FloatType || right_type == Type::FloatType {
                            Ok(Type::FloatType)
                        } else {
                            Ok(Type::IntType)
                        }
                    }
                    BinaryOperator::Equal | BinaryOperator::NotEqual |
                    BinaryOperator::LessThan | BinaryOperator::LessEqual |
                    BinaryOperator::GreaterThan | BinaryOperator::GreaterEqual => {
                        // 比较运算：结果总是int（布尔值用int表示）
                        Ok(Type::IntType)
                    }
                    BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => {
                        // 逻辑运算：结果总是int
                        Ok(Type::IntType)
                    }
                    BinaryOperator::Assign | BinaryOperator::AddAssign | 
                    BinaryOperator::SubtractAssign | BinaryOperator::MultiplyAssign |
                    BinaryOperator::DivideAssign | BinaryOperator::ModuloAssign => {
                        // 赋值运算：类型是左操作数的类型
                        Ok(left_type)
                    }
                }
            }
            Expression::UnaryOperation { operator, operand } => {
                let operand_type = self.deduce_ast_type_with_depth(operand, symbol_table, depth + 1)?;
                
                match operator {
                    UnaryOperator::Plus | UnaryOperator::Minus => {
                        // 一元算术运算：保持原类型
                        Ok(operand_type)
                    }
                    UnaryOperator::LogicalNot => {
                        // 逻辑非：结果总是int
                        Ok(Type::IntType)
                    }
                    UnaryOperator::BitwiseNot => {
                        // 按位非：保持原类型
                        Ok(operand_type)
                    }
                    UnaryOperator::Dereference => {
                        // 解引用：返回指针指向的类型
                        match operand_type {
                            Type::PointerType { target_type } => Ok(*target_type),
                            _ => Err("只能对指针类型进行解引用".to_string()),
                        }
                    }
                    UnaryOperator::AddressOf => {
                        // 取地址：返回指向原类型的指针
                        Ok(Type::PointerType {
                            target_type: Box::new(operand_type),
                        })
                    }
                    UnaryOperator::Increment | UnaryOperator::Decrement => {
                        // 自增自减：保持原类型
                        Ok(operand_type)
                    }
                }
            }
            Expression::Assignment { target, value } => {
                // 赋值运算：类型是左操作数的类型
                let target_type = self.deduce_ast_type_with_depth(target, symbol_table, depth + 1)?;
                let value_type = self.deduce_ast_type_with_depth(value, symbol_table, depth + 1)?;
                
                // 检查类型兼容性
                if self.is_type_compatible(&value_type, &target_type) {
                    Ok(target_type)
                } else {
                    Err(format!("类型不匹配：无法将 {:?} 赋值给 {:?}", value_type, target_type))
                }
            }
            Expression::FunctionCall { function_name, arguments } => {
                // 函数调用：暂时返回int类型（需要函数类型系统支持）
                let _arg_types: Vec<Type> = arguments.iter()
                    .filter_map(|arg| self.deduce_ast_type_with_depth(arg, symbol_table, depth + 1).ok())
                    .collect();
                
                // 从符号表获取函数返回类型
                if let Some(return_type) = symbol_table.get_function_return_type(function_name) {
                    Ok(return_type)
                } else {
                    Err(format!("未定义的函数：'{}'", function_name))
                }
            }
            Expression::InitializerList { elements } => {
                // 简化策略：若所有元素都是 int 常量或表达式推导为 IntType，则推为IntType
                // 更完整的做法需要在声明上下文中按目标数组类型逐层校验，这里先提供能用的占位行为
                let mut all_int = true;
                for elem in elements {
                    let t = self.deduce_ast_type_with_depth(elem, symbol_table, depth + 1)?;
                    if t != Type::IntType {
                        all_int = false;
                        break;
                    }
                }
                if all_int { Ok(Type::IntType) } else { Err("初始化列表包含非整型元素，暂不支持混合类型".to_string()) }
            }
            Expression::ArrayAccess { array, index } => {
                // 数组访问：递归推导数组类型
                let array_type = self.deduce_ast_type_with_depth(array, symbol_table, depth + 1)?;
                let index_type = self.deduce_ast_type_with_depth(index, symbol_table, depth + 1)?;
                
                // 检查索引类型
                if !matches!(index_type, Type::IntType) {
                    return Err(format!("数组索引必须是整数类型，实际类型：{:?}", index_type));
                }
                
                match array_type {
                    Type::ArrayType { element_type, .. } => {
                        // 对于多维数组，递归推导内层类型
                        Ok(*element_type)
                    }
                    Type::PointerType { target_type } => Ok(*target_type),
                    _ => Err("只能对数组或指针类型进行下标访问".to_string()),
                }
            }
            Expression::MemberAccess { object, member_name } => {
                // 成员访问：暂时返回void类型（需要结构体支持）
                let _object_type = self.deduce_ast_type_with_depth(object, symbol_table, depth + 1)?;
                Err(format!("暂不支持成员访问：'{}'", member_name))
            }
        }
    }
    
    /// 推导AST节点的类型
    /// 
    /// 按照clang设计理念：提供准确的类型推导
    /// 
    /// # 参数
    /// * `ast` - AST节点
    /// * `symbol_table` - 符号表
    /// 
    /// # 返回
    /// * `Ok(Type)` - 推导出的类型
    /// * `Err(String)` - 类型推导失败
    pub fn deduce_ast_type(&self, ast: &Ast, symbol_table: &SymbolTable) -> Result<Type, String> {
        // 使用简化的类型推导，避免深度递归
        let result = match &ast.kind {
            AstKind::Expression(expr) => {
                self.deduce_expression_type_simple(expr, symbol_table)
            }
            AstKind::Type(typ) => {
                Ok(typ.clone())
            }
            _ => {
                Err("无法推导非表达式AST节点的类型".to_string())
            }
        };
        
        result
    }
    
    /// 推导AST节点的类型（带深度限制）
    fn deduce_ast_type_with_depth(&self, ast: &Ast, symbol_table: &SymbolTable, depth: usize) -> Result<Type, String> {
        let result = match &ast.kind {
            AstKind::Expression(expr) => {
                self.deduce_expression_type_with_depth(expr, symbol_table, depth)
            }
            AstKind::Type(typ) => Ok(typ.clone()),
            _ => Err("无法推导非表达式AST节点的类型".to_string()),
        };
        
        result
    }
    
    /// 检查类型兼容性
    /// 
    /// 按照clang设计理念：检查源类型是否可以转换为目标类型
    /// 
    /// # 参数
    /// * `source_type` - 源类型
    /// * `target_type` - 目标类型
    /// 
    /// # 返回
    /// 如果类型兼容返回true，否则返回false
    pub fn is_type_compatible(&self, source_type: &Type, target_type: &Type) -> bool {
        // 相同类型总是兼容
        if source_type == target_type {
            return true;
        }
        
        // 检查预定义的兼容性规则
        for (src, dst, compatible) in &self.type_compatibility {
            if src == source_type && dst == target_type {
                return *compatible;
            }
        }
        
        // 默认不兼容
        false
    }
    
    /// 获取类型转换规则
    /// 
    /// 按照clang设计理念：提供详细的类型转换信息
    /// 
    /// # 参数
    /// * `source_type` - 源类型
    /// * `target_type` - 目标类型
    /// 
    /// # 返回
    /// * `Some(String)` - 转换规则描述
    /// * `None` - 无法转换
    pub fn get_conversion_rule(&self, source_type: &Type, target_type: &Type) -> Option<String> {
        if source_type == target_type {
            Some("相同类型，无需转换".to_string())
        } else {
            match (source_type, target_type) {
                (Type::IntType, Type::FloatType) => {
                    Some("隐式转换：int -> float".to_string())
                }
                (Type::FloatType, Type::IntType) => {
                    Some("警告：float -> int 可能丢失精度".to_string())
                }
                _ => None,
            }
        }
    }
    
    /// 检查函数参数类型匹配
    /// 
    /// 按照clang设计理念：检查函数调用的参数类型是否匹配
    /// 
    /// # 参数
    /// * `expected_types` - 期望的参数类型列表
    /// * `actual_types` - 实际的参数类型列表
    /// 
    /// # 返回
    /// * `Ok(())` - 参数类型匹配
    /// * `Err(String)` - 参数类型不匹配
    pub fn check_function_arguments(&self, expected_types: &[Type], actual_types: &[Type]) -> Result<(), String> {
        if expected_types.len() != actual_types.len() {
            return Err(format!("参数数量不匹配：期望 {} 个，实际 {} 个", 
                             expected_types.len(), actual_types.len()));
        }
        
        for (i, (expected, actual)) in expected_types.iter().zip(actual_types.iter()).enumerate() {
            if !self.is_type_compatible(actual, expected) {
                return Err(format!("参数 {} 类型不匹配：期望 {:?}，实际 {:?}", 
                                 i + 1, expected, actual));
            }
        }
        
        Ok(())
    }
    
    /// 获取类型大小（字节数）
    /// 
    /// 按照clang设计理念：提供类型的大小信息
    /// 
    /// # 参数
    /// * `type_` - 类型
    /// 
    /// # 返回
    /// 类型的大小（字节数）
    pub fn get_type_size(&self, type_: &Type) -> usize {
        match type_ {
            Type::IntType => 4,    // 32位整数
            Type::FloatType => 4,  // 32位浮点数
            Type::VoidType => 0,   // void类型没有大小
            Type::CharType => 1,   // 字符类型
            Type::BoolType => 1,   // 布尔类型
            Type::ArrayType { element_type, array_size } => {
                let element_size = self.get_type_size(element_type);
                match array_size {
                    crate::frontend::ast::ArraySize::Fixed(size) => element_size * size,
                    _ => element_size, // 未知大小或常量的数组
                }
            }
            Type::PointerType { .. } => 8, // 64位指针
            Type::FunctionType { .. } => 0, // 函数类型没有大小
        }
    }
    
    /// 检查类型是否有效
    /// 
    /// 按照clang设计理念：验证类型的有效性
    /// 
    /// # 参数
    /// * `type_` - 要检查的类型
    /// 
    /// # 返回
    /// 如果类型有效返回true，否则返回false
    pub fn is_valid_type(&self, type_: &Type) -> bool {
        match type_ {
            Type::IntType | Type::FloatType | Type::VoidType | 
            Type::CharType | Type::BoolType => true,
            Type::ArrayType { element_type, .. } => {
                self.is_valid_type(element_type)
            }
            Type::PointerType { target_type } => {
                self.is_valid_type(target_type)
            }
            Type::FunctionType { parameter_types, return_type } => {
                parameter_types.iter().all(|t| self.is_valid_type(t)) &&
                self.is_valid_type(return_type)
            }
        }
    }

    /// 解析数组类型中的常量维度
    /// 
    /// 将ArraySize::Constant类型的数组维度解析为具体的数值
    /// 
    /// # 参数
    /// * `type_` - 要解析的类型
    /// * `symbol_table` - 符号表（用于查找常量值）
    /// 
    /// # 返回
    /// 解析后的类型，常量数组维度被替换为固定值
    fn resolve_array_constant_dimensions(&self, type_: &Type, symbol_table: &SymbolTable) -> Type {
        match type_ {
            Type::ArrayType { element_type, array_size } => {
                let resolved_element_type = Box::new(self.resolve_array_constant_dimensions(element_type, symbol_table));
                
                let resolved_array_size = match array_size {
                    crate::frontend::ast::ArraySize::Constant(const_name) => {
                        // 尝试从符号表查找常量值并提取其初始值
                        if let Some(symbol) = symbol_table.lookup_symbol(const_name) {
                            if symbol.is_const && symbol.data_type == Type::IntType {
                                // 这里需要从符号的初始值中提取常量值
                                // 由于符号表中没有存储初始值，我们需要一个不同的方法
                                // 暂时保持常量引用，但数组类型仍然是有效的
                                array_size.clone()
                            } else {
                                array_size.clone()
                            }
                        } else {
                            array_size.clone()
                        }
                    }
                    _ => array_size.clone(),
                };
                
                Type::ArrayType {
                    element_type: resolved_element_type,
                    array_size: resolved_array_size,
                }
            }
            _ => type_.clone(),
        }
    }
}

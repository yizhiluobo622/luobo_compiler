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
}

impl TypeSystem {
    /// 创建新的类型系统
    pub fn new() -> Self {
        let mut type_system = Self {
            type_compatibility: Vec::new(),
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
    
    /// 推导表达式类型
    /// 
    /// 按照clang设计理念：根据表达式内容自动推导类型
    /// 
    /// # 参数
    /// * `expr` - 表达式AST节点
    /// * `symbol_table` - 符号表（用于查询变量和函数类型）
    /// 
    /// # 返回
    /// * `Ok(Type)` - 推导出的类型
    /// * `Err(String)` - 类型推导失败
    pub fn deduce_expression_type(&self, expr: &Expression, symbol_table: &SymbolTable) -> Result<Type, String> {
        self.deduce_expression_type_with_depth(expr, symbol_table, 0)
    }
    
    /// 推导表达式类型（带深度限制）
    fn deduce_expression_type_with_depth(&self, expr: &Expression, symbol_table: &SymbolTable, depth: usize) -> Result<Type, String> {
        // 防止递归过深
        if depth > 100 {
            return Err("表达式嵌套过深，可能存在循环引用".to_string());
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
                // 从符号表中查找变量类型
                if let Some(var_type) = symbol_table.get_variable_type(name) {
                    Ok(var_type)
                } else {
                    Err(format!("未定义的变量：'{}'", name))
                }
            }
            Expression::BinaryOperation { operator, left_operand, right_operand } => {
                // 推导二元运算的类型
                let left_type = self.deduce_ast_type(left_operand, symbol_table, depth + 1)?;
                let right_type = self.deduce_ast_type(right_operand, symbol_table, depth + 1)?;
                
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
                let operand_type = self.deduce_ast_type(operand, symbol_table, depth + 1)?;
                
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
                let target_type = self.deduce_ast_type(target, symbol_table, depth + 1)?;
                let value_type = self.deduce_ast_type(value, symbol_table, depth + 1)?;
                
                // 检查类型兼容性
                if self.is_type_compatible(&value_type, &target_type) {
                    Ok(target_type)
                } else {
                    Err(format!("类型不兼容：无法将 {:?} 赋值给 {:?}", value_type, target_type))
                }
            }
            Expression::FunctionCall { function_name, arguments } => {
                // 从符号表中获取函数返回类型
                if let Some(return_type) = symbol_table.get_function_return_type(function_name) {
                    Ok(return_type)
                } else {
                    Err(format!("未定义的函数：'{}'", function_name))
                }
            }
            Expression::ArrayAccess { array, index } => {
                // 数组访问：返回数组元素类型
                let array_type = self.deduce_ast_type(array, symbol_table, depth + 1)?;
                let _index_type = self.deduce_ast_type(index, symbol_table, depth + 1)?;
                
                match array_type {
                    Type::ArrayType { element_type, .. } => Ok(*element_type),
                    _ => Err("只能对数组类型进行下标访问".to_string()),
                }
            }
            Expression::MemberAccess { object, member_name } => {
                // 成员访问：暂时返回void类型（需要结构体支持）
                let _object_type = self.deduce_ast_type(object, symbol_table, depth + 1)?;
                Err(format!("暂不支持成员访问：'{}'", member_name))
            }
        }
    }
    
    /// 推导AST节点的类型
    fn deduce_ast_type(&self, ast: &Ast, symbol_table: &SymbolTable, depth: usize) -> Result<Type, String> {
        match &ast.kind {
            AstKind::Expression(expr) => self.deduce_expression_type_with_depth(expr, symbol_table, depth),
            AstKind::Type(typ) => Ok(typ.clone()),
            _ => Err("无法推导非表达式AST节点的类型".to_string()),
        }
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
            return Some("相同类型，无需转换".to_string());
        }
        
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
                    Some(size) => element_size * size,
                    None => element_size, // 未知大小的数组
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
}

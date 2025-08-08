use crate::frontend::ast::{Type, Ast};
use crate::frontend::span::Span;
use std::collections::HashMap;

/// 符号类型
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    /// 变量
    Variable,
    /// 函数
    Function,
    /// 参数
    Parameter,
}

/// 符号信息
#[derive(Debug, Clone)]
pub struct Symbol {
    /// 符号名称
    pub name: String,
    /// 符号类型
    pub kind: SymbolKind,
    /// 数据类型
    pub data_type: Type,
    /// 声明位置
    pub span: Span,
    /// 是否已定义
    pub is_defined: bool,
    /// 函数参数列表（仅对函数有效）
    pub parameters: Option<Vec<Type>>,
    /// 是否为常量（仅对变量/参数有效）
    pub is_const: bool,
}

/// 作用域
#[derive(Debug)]
pub struct Scope {
    /// 作用域名称
    pub name: String,
    /// 符号表
    pub symbols: HashMap<String, Symbol>,
    /// 父作用域
    pub parent: Option<usize>,
}

/// 符号表
/// 
/// 按照clang设计理念实现：
/// 1. 作用域管理：支持嵌套作用域
/// 2. 符号查找：从内到外查找符号
/// 3. 重复定义检查：防止符号重复定义
/// 4. 生命周期管理：正确处理符号的生命周期
pub struct SymbolTable {
    /// 作用域栈
    scopes: Vec<Scope>,
    /// 当前作用域索引
    current_scope: usize,
}

impl SymbolTable {
    /// 创建新的符号表
    pub fn new() -> Self {
        let mut symbol_table = Self {
            scopes: Vec::new(),
            current_scope: 0,
        };
        
        // 创建全局作用域
        symbol_table.enter_scope("global");
        
        symbol_table
    }
    
    /// 进入新作用域
    /// 
    /// 按照clang设计理念：支持嵌套作用域
    /// 
    /// # 参数
    /// * `name` - 作用域名称
    pub fn enter_scope(&mut self, name: &str) {
        let new_scope = Scope {
            name: name.to_string(),
            symbols: HashMap::new(),
            parent: Some(self.current_scope),
        };
        
        self.scopes.push(new_scope);
        self.current_scope = self.scopes.len() - 1;
    }
    
    /// 退出当前作用域
    /// 
    /// 按照clang设计理念：正确处理作用域退出，清理符号
    pub fn exit_scope(&mut self) {
        // 不能退出全局作用域
        if self.current_scope == 0 {
            return;
        }
        
        // 清理当前作用域的符号
        if let Some(scope) = self.scopes.get_mut(self.current_scope) {
            scope.symbols.clear();
        }
        
        // 切换到父作用域
        if let Some(parent) = self.scopes[self.current_scope].parent {
            self.current_scope = parent;
        }
    }
    
    /// 添加符号
    /// 
    /// 按照clang设计理念：检查重复定义
    /// 
    /// # 参数
    /// * `symbol` - 要添加的符号
    /// 
    /// # 返回
    /// * `Ok(())` - 添加成功
    /// * `Err(String)` - 重复定义错误
    pub fn add_symbol(&mut self, symbol: Symbol) -> Result<(), String> {
        let scope = &mut self.scopes[self.current_scope];
        
        // 检查是否已存在同名符号
        if scope.symbols.contains_key(&symbol.name) {
            return Err(format!("重复定义：符号 '{}' 已在当前作用域中定义", symbol.name));
        }
        
        scope.symbols.insert(symbol.name.clone(), symbol);
        Ok(())
    }
    
    /// 查找符号
    /// 
    /// 按照clang设计理念：从内到外查找符号
    /// 
    /// # 参数
    /// * `name` - 符号名称
    /// 
    /// # 返回
    /// * `Some(Symbol)` - 找到的符号
    /// * `None` - 未找到符号
    pub fn lookup_symbol(&self, name: &str) -> Option<Symbol> {
        let mut current = self.current_scope;
        let mut visited = std::collections::HashSet::new();
        
        while current < self.scopes.len() && !visited.contains(&current) {
            visited.insert(current);
            let scope = &self.scopes[current];
            
            if let Some(symbol) = scope.symbols.get(name) {
                return Some(symbol.clone());
            }
            
            // 查找父作用域
            if let Some(parent) = scope.parent {
                if parent >= self.scopes.len() {
                    // 父作用域索引无效，退出
                    break;
                }
                current = parent;
            } else {
                break;
            }
        }
        
        None
    }
    
    /// 查找符号（仅在当前作用域）
    /// 
    /// 按照clang设计理念：支持局部查找
    /// 
    /// # 参数
    /// * `name` - 符号名称
    /// 
    /// # 返回
    /// * `Some(Symbol)` - 找到的符号
    /// * `None` - 未找到符号
    pub fn lookup_symbol_in_current_scope(&self, name: &str) -> Option<Symbol> {
        let scope = &self.scopes[self.current_scope];
        scope.symbols.get(name).cloned()
    }
    
    /// 添加变量符号
    /// 
    /// 按照clang设计理念：简化变量添加
    /// 
    /// # 参数
    /// * `name` - 变量名称
    /// * `data_type` - 变量类型
    /// * `span` - 声明位置
    /// 
    /// # 返回
    /// * `Ok(())` - 添加成功
    /// * `Err(String)` - 重复定义错误
    pub fn add_variable(&mut self, name: &str, data_type: Type, span: Span, is_const: bool) -> Result<(), String> {
        let symbol = Symbol {
            name: name.to_string(),
            kind: SymbolKind::Variable,
            data_type,
            span,
            is_defined: true,
            parameters: None,
            is_const,
        };
        
        self.add_symbol(symbol)
    }
    
    /// 添加函数符号
    /// 
    /// 按照clang设计理念：支持函数重载（未来扩展）
    /// 
    /// # 参数
    /// * `name` - 函数名称
    /// * `return_type` - 返回类型
    /// * `parameters` - 参数类型列表
    /// * `span` - 声明位置
    /// 
    /// # 返回
    /// * `Ok(())` - 添加成功
    /// * `Err(String)` - 重复定义错误
    pub fn add_function(&mut self, name: &str, return_type: Type, parameters: Vec<Type>, span: Span) -> Result<(), String> {
        let symbol = Symbol {
            name: name.to_string(),
            kind: SymbolKind::Function,
            data_type: return_type,
            span,
            is_defined: true,
            parameters: Some(parameters),
            is_const: false,
        };
        
        self.add_symbol(symbol)
    }
    
    /// 添加参数符号
    /// 
    /// 按照clang设计理念：函数参数有特殊处理
    /// 
    /// # 参数
    /// * `name` - 参数名称
    /// * `data_type` - 参数类型
    /// * `span` - 声明位置
    /// 
    /// # 返回
    /// * `Ok(())` - 添加成功
    /// * `Err(String)` - 重复定义错误
    pub fn add_parameter(&mut self, name: &str, data_type: Type, span: Span) -> Result<(), String> {
        let symbol = Symbol {
            name: name.to_string(),
            kind: SymbolKind::Parameter,
            data_type,
            span,
            is_defined: true,
            parameters: None,
            is_const: false,
        };
        
        self.add_symbol(symbol)
    }
    
    /// 检查符号是否已定义
    /// 
    /// 按照clang设计理念：区分声明和定义
    /// 
    /// # 参数
    /// * `name` - 符号名称
    /// 
    /// # 返回
    /// 如果符号已定义返回true，否则返回false
    pub fn is_symbol_defined(&self, name: &str) -> bool {
        if let Some(symbol) = self.lookup_symbol(name) {
            symbol.is_defined
        } else {
            false
        }
    }
    
    /// 获取当前作用域名称
    pub fn get_current_scope_name(&self) -> &str {
        &self.scopes[self.current_scope].name
    }
    
    /// 获取作用域深度
    pub fn get_scope_depth(&self) -> usize {
        let mut depth = 0;
        let mut current = self.current_scope;
        
        while current < self.scopes.len() {
            depth += 1;
            if let Some(parent) = self.scopes[current].parent {
                current = parent;
            } else {
                break;
            }
        }
        
        depth
    }
    
    /// 获取作用域中的所有符号
    /// 
    /// 按照clang设计理念：支持调试和诊断
    /// 
    /// # 参数
    /// * `scope_name` - 作用域名称
    /// 
    /// # 返回
    /// 作用域中的所有符号
    pub fn get_symbols_in_scope(&self, scope_name: &str) -> Vec<Symbol> {
        for scope in &self.scopes {
            if scope.name == scope_name {
                return scope.symbols.values().cloned().collect();
            }
        }
        
        Vec::new()
    }
    
    /// 检查函数签名是否匹配
    /// 
    /// 按照clang设计理念：支持函数重载检查
    /// 
    /// # 参数
    /// * `name` - 函数名称
    /// * `parameters` - 参数类型列表
    /// 
    /// # 返回
    /// * `Some(Symbol)` - 匹配的函数符号
    /// * `None` - 未找到匹配的函数
    pub fn lookup_function(&self, name: &str, parameters: &[Type]) -> Option<Symbol> {
        if let Some(symbol) = self.lookup_symbol(name) {
            if symbol.kind == SymbolKind::Function {
                if let Some(expected_params) = &symbol.parameters {
                    if expected_params.len() == parameters.len() {
                        // 简单的参数数量匹配，未来可以扩展类型匹配
                        return Some(symbol);
                    }
                }
            }
        }
        
        None
    }
    
    /// 获取当前函数的返回类型
    /// 
    /// 按照clang设计理念：支持函数上下文查询
    /// 
    /// # 参数
    /// * `function_name` - 函数名称
    /// 
    /// # 返回
    /// * `Some(Type)` - 函数返回类型
    /// * `None` - 未找到函数
    pub fn get_function_return_type(&self, function_name: &str) -> Option<Type> {
        if let Some(symbol) = self.lookup_symbol(function_name) {
            if symbol.kind == SymbolKind::Function {
                return Some(symbol.data_type.clone());
            }
        }
        None
    }
    
    /// 获取变量的类型
    /// 
    /// 按照clang设计理念：支持类型查询
    /// 
    /// # 参数
    /// * `variable_name` - 变量名称
    /// 
    /// # 返回
    /// * `Some(Type)` - 变量类型
    /// * `None` - 未找到变量
    pub fn get_variable_type(&self, variable_name: &str) -> Option<Type> {
        if let Some(symbol) = self.lookup_symbol(variable_name) {
            if symbol.kind == SymbolKind::Variable || symbol.kind == SymbolKind::Parameter {
                return Some(symbol.data_type.clone());
            }
        }
        None
    }
    
    /// 打印符号表（调试用）
    /// 
    /// 按照clang设计理念：提供详细的调试信息
    pub fn print_symbol_table(&self) {
        println!("=== 符号表 ===");
        for (i, scope) in self.scopes.iter().enumerate() {
            println!("作用域 {}: {}", i, scope.name);
            for (name, symbol) in &scope.symbols {
                println!("  {}: {:?} ({:?})", name, symbol.data_type, symbol.kind);
            }
        }
        println!("当前作用域: {}", self.get_current_scope_name());
        println!("作用域深度: {}", self.get_scope_depth());
    }
    
    /// 调试方法：检查变量是否存在
    pub fn debug_check_variable(&self, name: &str) -> bool {
        if let Some(symbol) = self.lookup_symbol(name) {
            println!("✅ 找到变量 '{}': {:?}", name, symbol.data_type);
            true
        } else {
            println!("❌ 未找到变量 '{}'", name);
            self.print_symbol_table();
            false
        }
    }
}

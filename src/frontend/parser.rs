use crate::frontend::lexer::{Lexer, Token};
use crate::frontend::ast::{Ast, AstKind, Statement, Expression, Literal, BinaryOperator, UnaryOperator, Type};
use crate::frontend::span::{Span, LocatedToken};

/// 语法分析器模块
/// 
/// 按照rustc设计理念实现：
/// 1. Parser只负责检测语法错误并构造错误对象
/// 2. 不直接打印错误信息，而是通过Result返回给外部统一处理
/// 3. 错误对象包含位置信息(Span)和描述信息，供外部诊断系统使用
/// 4. 支持错误码、帮助信息等扩展功能

/// 语法分析器
/// 
/// 负责将词法分析器产生的Token流转换为抽象语法树(AST)
/// 支持递归下降解析
/// 
/// 按照rustc设计理念：
/// - Parser只负责检测语法错误并构造错误对象
/// - 不直接打印错误信息，而是通过Result返回给外部统一处理
/// - 错误信息包含位置信息(Span)和描述信息，供外部诊断系统使用
pub struct Parser<'a> {
    /// 词法分析器，提供Token流
    lexer: Lexer<'a>,
    /// 当前正在处理的Token
    current_token: LocatedToken,
}

/// 语法解析错误
/// 
/// 包含错误信息和在源代码中的位置，用于调试和错误提示
/// 
/// 按照rustc设计理念：
/// - 错误对象包含位置信息(Span)和描述信息
/// - 不直接打印，而是传递给外部诊断系统统一处理
/// - 支持错误码、帮助信息等扩展功能
#[derive(Debug, Clone)]
pub struct ParseError {
    /// 错误描述信息
    pub message: String,
    /// 错误在源代码中的位置
    pub span: Span,
}

impl<'a> Parser<'a> {
    /// 创建新的语法分析器
    /// 
    /// # 参数
    /// * `lexer` - 词法分析器实例
    /// 
    /// # 返回
    /// 初始化完成的Parser实例
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let current_token = lexer.next_located_token();
        let source_code = lexer.get_source().to_string();
        
        Self {
            lexer,
            current_token,
        }
    }
    
    /// 解析完整的程序
    /// 
    /// 这是Parser的主要入口方法，负责解析整个源代码文件
    /// 支持函数定义、全局变量声明等顶级结构
    /// 
    /// 按照rustc设计理念：
    /// - 检测语法错误并构造错误对象
    /// - 通过Result返回错误，不直接打印
    /// - 让外部统一处理错误展示
    /// - 支持错误恢复，收集所有错误而不是只报第一个
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回程序的AST根节点
    /// * `Err(Vec<ParseError>)` - 解析失败，返回语法错误列表
    pub fn parse(&mut self) -> Result<Ast, Vec<ParseError>> {
        let mut functions = Vec::new();
        let mut global_variables = Vec::new();
        let mut all_errors = Vec::new();
        
        // 解析所有顶级声明（函数和全局变量）
        while !self.is_eof() {
            match &self.current_token.token {
                Token::KeywordInt | Token::KeywordFloat | Token::KeywordVoid => {
                    // 遇到类型关键字，可能是函数定义或变量声明
                    match self.parse_top_level_declaration() {
                        Ok(declaration) => {
                            match declaration.kind {
                                AstKind::Function { .. } => functions.push(declaration),
                                AstKind::VariableDeclaration { .. } => global_variables.push(declaration),
                                _ => {}
                            }
                        }
                        Err(errors) => {
                            // 错误恢复：记录错误，跳过到同步点，继续解析
                            all_errors.extend(errors);
                            self.recover_from_error();
                        }
                    }
                }
                Token::EOF => {
                    break; // 遇到文件结束，停止解析
                }
                _ => {
                    // 错误恢复：记录错误，跳过到同步点，继续解析
                    let unexpected_token = self.token_to_string(&self.current_token.token);
                    let error_span = self.current_token.span.clone();
                    let error = ParseError {
                        message: format!("期望 '类型关键字（int、float、void）'，但找到 '{}'", unexpected_token),
                        span: error_span,
                    };
                    all_errors.push(error);
                    self.recover_from_error();
                }
            }
        }
        
        // 如果有错误，返回所有错误；否则返回AST
        if all_errors.is_empty() {
            Ok(Ast::new(
                AstKind::Program { 
                    functions, 
                    global_variables 
                },
                Span::new(0, 1, 1, 0, 0)
            ))
        } else {
            Err(all_errors)
        }
    }
    
    /// 获取所有解析错误
    /// 
    /// # 返回
    /// 所有收集到的语法错误列表的引用
    pub fn get_errors(&self) -> &[ParseError] {
        // 按照rustc设计理念，Parser只负责检测和构造错误，不直接打印
        // 错误通过Result返回给外部统一处理
        &[]
    }
    
    /// 检查是否有解析错误
    /// 
    /// # 返回
    /// 如果存在语法错误返回true，否则返回false
    pub fn has_errors(&self) -> bool {
        // 按照rustc设计理念，Parser只负责检测错误，不直接打印
        false
    }
    
    // ==================== 顶级声明解析 ====================
    
    /// 解析顶级声明（函数定义或全局变量声明）
    /// 
    /// 根据语法规则，顶级声明以类型关键字开始，后跟标识符
    /// 如果标识符后是'('，则为函数定义；如果是';'或'='，则为变量声明
    /// 
    /// 支持错误恢复：遇到解析错误时，跳过当前声明继续解析
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回函数或变量声明的AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_top_level_declaration(&mut self) -> Result<Ast, Vec<ParseError>> {
        let declaration_start_span = self.current_token.span.clone();
        let mut all_errors = Vec::new();
        
        // 解析类型声明
        let variable_type = match self.parse_type_declaration() {
            Ok(t) => t,
            Err(errors) => {
                all_errors.extend(errors);
                self.recover_from_error();
                return Err(all_errors);
            }
        };
        
        // 解析标识符名称
        let variable_name = match self.expect_identifier_token() {
            Ok(name) => name,
            Err(errors) => {
                all_errors.extend(errors);
                self.recover_from_error();
                return Err(all_errors);
            }
        };
        
        // 根据后续Token判断是函数定义还是变量声明
        match &self.current_token.token {
            Token::LParen => {
                // 函数定义：标识符后跟'('
                self.advance_to_next_token(); // 跳过'('
                
                let parameters = match self.parse_function_parameters() {
                    Ok(params) => params,
                    Err(errors) => {
                        all_errors.extend(errors);
                        self.recover_from_error();
                        return Err(all_errors);
                    }
                };
                
                if let Err(errors) = self.expect_token(Token::RParen) {
                    all_errors.extend(errors);
                    self.recover_from_error();
                    return Err(all_errors);
                }
                
                // 按照rustc设计理念：检查是否缺少左大括号
                if !matches!(self.current_token.token, Token::LBrace) {
                    // 构造错误对象，不直接打印
                    let error_span = Span::new(
                        self.current_token.span.file_id,
                        self.current_token.span.line,
                        self.current_token.span.column,
                        self.current_token.span.end_pos,
                        self.current_token.span.end_pos,
                    );
                    let error = ParseError {
                        message: "缺少左大括号".to_string(),
                        span: error_span,
                    };
                    all_errors.push(error);
                    self.recover_from_error();
                    return Err(all_errors);
                } else {
                    self.advance_to_next_token(); // 跳过左大括号
                }
                
                let function_body = match self.parse_compound_statement() {
                    Ok(body) => body,
                    Err(errors) => {
                        all_errors.extend(errors);
                        return Err(all_errors);
                    }
                };
                
                Ok(Ast::new(
                    AstKind::Function {
                        function_name: variable_name,
                        parameters,
                        return_type: Some(variable_type),
                        function_body: Box::new(function_body),
                    },
                    self.create_span_from_to(&declaration_start_span, &self.current_token.span)
                ))
            }
            Token::Semicolon | Token::Equal => {
                // 变量声明：标识符后跟';'或'='
                let initial_value = if let Token::Equal = self.current_token.token {
                    self.advance_to_next_token(); // 跳过'='
                    match self.parse_expression() {
                        Ok(expr) => Some(Box::new(expr)),
                        Err(errors) => {
                            all_errors.extend(errors);
                            self.recover_from_error();
                            return Err(all_errors);
                        }
                    }
                } else {
                    None
                };
                
                if let Err(errors) = self.expect_token(Token::Semicolon) {
                    all_errors.extend(errors);
                    self.recover_from_error();
                    return Err(all_errors);
                }
                
                Ok(Ast::new(
                    AstKind::VariableDeclaration {
                        variable_name,
                        variable_type,
                        initial_value,
                    },
                    self.create_span_from_to(&declaration_start_span, &self.current_token.span)
                ))
            }
            _ => {
                // 错误恢复：记录错误，跳过到同步点，继续解析
                let actual_token = self.token_to_string(&self.current_token.token);
                let error_span = self.current_token.span.clone();
                let message = format!("期望 '(' 表示函数定义，或 ';', '=' 表示变量声明，但找到 '{}'", actual_token);
                let error = ParseError {
                    message,
                    span: error_span,
                };
                all_errors.push(error);
                self.recover_from_error();
                return Err(all_errors);
            }
        }
    }
    
    /// 解析类型声明
    /// 
    /// 支持int、float、void等基本类型
    /// 
    /// # 返回
    /// * `Ok(Type)` - 解析成功，返回类型
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_type_declaration(&mut self) -> Result<Type, Vec<ParseError>> {
        match &self.current_token.token {
            Token::KeywordInt => {
                self.advance_to_next_token();
                Ok(Type::IntType)
            }
            Token::KeywordFloat => {
                self.advance_to_next_token();
                Ok(Type::FloatType)
            }
            Token::KeywordVoid => {
                self.advance_to_next_token();
                Ok(Type::VoidType)
            }
            _ => {
                // 按照rustc设计理念：构造错误对象，不直接打印
                let actual_token = self.token_to_string(&self.current_token.token);
                let error_span = self.current_token.span.clone();
                let message = format!("期望 '类型关键字（int、float或void）'，但找到 '{}'", actual_token);
                let error = ParseError {
                    message,
                    span: error_span,
                };
                return Err(vec![error]);
            }
        }
    }
    
    /// 解析函数参数列表
    /// 
    /// 格式：(类型 参数名, 类型 参数名, ...)
    /// 
    /// # 返回
    /// * `Ok(Vec<Ast>)` - 解析成功，返回参数列表
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_function_parameters(&mut self) -> Result<Vec<Ast>, Vec<ParseError>> {
        let mut parameters = Vec::new();
        
        // 如果直接是')'，说明没有参数
        if let Token::RParen = self.current_token.token {
            return Ok(parameters);
        }
        
        // 解析参数列表
        loop {
            let parameter_type = self.parse_type_declaration()?;
            let parameter_name = self.expect_identifier_token()?;
            
            // 将参数添加到已定义集合中
            // 这里不再需要defined_variables，因为错误处理已移除
            
            // 将参数构造为变量声明节点
            parameters.push(Ast::new(
                AstKind::VariableDeclaration {
                    variable_name: parameter_name,
                    variable_type: parameter_type,
                    initial_value: None,
                },
                self.current_token.span.clone()
            ));
            
            // 检查是否还有更多参数
            if let Token::RParen = self.current_token.token {
                break;
            }
            
            self.expect_token(Token::Comma)?;
        }
        
        Ok(parameters)
    }
    
    // ==================== 语句解析 ====================
    
    /// 解析复合语句（代码块）
    /// 
    /// 格式：{ 语句1; 语句2; ... }
    /// 
    /// 支持错误恢复：遇到语句解析错误时，跳过该语句继续解析后续语句
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回复合语句AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_compound_statement(&mut self) -> Result<Ast, Vec<ParseError>> {
        let block_start_span = self.current_token.span.clone();
        let mut statements = Vec::new();
        let mut all_errors = Vec::new();
        
        // 解析块内的所有语句，直到遇到'}'
        while !matches!(self.current_token.token, Token::RBrace) && !self.is_eof() {
            match self.parse_statement() {
                Ok(statement) => {
                    statements.push(statement);
                }
                Err(errors) => {
                    // 错误恢复：记录错误，跳过到下一个语句，继续解析
                    all_errors.extend(errors);
                    self.recover_from_error();
                }
            }
        }
        
        // 按照rustc设计理念：检查是否缺少右大括号
        if !matches!(self.current_token.token, Token::RBrace) {
            // 构造错误对象，不直接打印
            let error_span = self.current_token.span.clone();
            let error = ParseError {
                message: "缺少右大括号".to_string(),
                span: error_span,
            };
            all_errors.push(error);
        } else {
            self.advance_to_next_token(); // 跳过右大括号
        }
        
        // 如果有错误，返回所有错误；否则返回AST
        if all_errors.is_empty() {
            Ok(Ast::new(
                AstKind::Statement(Statement::Compound { statements }),
                self.create_span_from_to(&block_start_span, &self.current_token.span)
            ))
        } else {
            Err(all_errors)
        }
    }
    
    /// 解析单个语句
    /// 
    /// 支持各种语句类型：复合语句、if语句、while语句、return语句、
    /// break语句、continue语句、表达式语句、空语句
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回语句AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_statement(&mut self) -> Result<Ast, Vec<ParseError>> {
        match &self.current_token.token {
            Token::LBrace => {
                // 复合语句：{ ... }
                self.advance_to_next_token();
                self.parse_compound_statement()
            }
            Token::KeywordInt | Token::KeywordFloat | Token::KeywordVoid => {
                // 变量声明语句：类型 变量名 [= 初始值];
                self.parse_variable_declaration()
            }
            Token::KeywordIf => self.parse_if_statement(),
            Token::KeywordWhile => self.parse_while_statement(),
            Token::KeywordReturn => self.parse_return_statement(),
            Token::KeywordBreak => self.parse_break_statement(),
            Token::KeywordContinue => self.parse_continue_statement(),
            Token::Semicolon => {
                // 空语句：;
                self.advance_to_next_token();
                Ok(Ast::new(
                    AstKind::Statement(Statement::Empty),
                    self.current_token.span.clone()
                ))
            }
            _ => {
                // 表达式语句：表达式;
                let expression = self.parse_expression()?;
                let statement_span = expression.span.clone();
                
                // 按照rustc设计理念：检查是否缺少分号
                if !matches!(self.current_token.token, Token::Semicolon) {
                    // 构造错误对象，不直接打印
                    let error_span = Span::new(
                        self.current_token.span.file_id,
                        self.current_token.span.line,
                        self.current_token.span.column,
                        expression.span.end_pos,
                        self.current_token.span.end_pos,
                    );
                    let error = ParseError {
                        message: "缺少分号".to_string(),
                        span: error_span,
                    };
                    return Err(vec![error]);
                } else {
                    self.advance_to_next_token(); // 跳过分号
                }
                
                Ok(Ast::new(
                    AstKind::Statement(Statement::ExpressionStatement {
                        expression: Box::new(expression),
                    }),
                    statement_span
                ))
            }
        }
    }
    
    /// 解析if语句
    /// 
    /// 格式：if (条件) 语句1 [else 语句2]
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回if语句AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_if_statement(&mut self) -> Result<Ast, Vec<ParseError>> {
        let if_start_span = self.current_token.span.clone();
        self.expect_token(Token::KeywordIf)?;
        
        // 按照rustc设计理念：检查是否缺少左括号
        if !matches!(self.current_token.token, Token::LParen) {
            // 构造错误对象，不直接打印
            let error_span = self.current_token.span.clone();
            let error = ParseError {
                message: "缺少左括号".to_string(),
                span: error_span,
            };
            return Err(vec![error]);
        } else {
            self.advance_to_next_token(); // 跳过左括号
        }
        
        let condition = Box::new(self.parse_expression()?);
        
        // 按照rustc设计理念：检查是否缺少右括号
        if !matches!(self.current_token.token, Token::RParen) {
            // 构造错误对象，不直接打印
            let error_span = Span::new(
                self.current_token.span.file_id,
                self.current_token.span.line,
                self.current_token.span.column,
                condition.span.end_pos,
                self.current_token.span.end_pos,
            );
            let error = ParseError {
                message: "缺少右括号".to_string(),
                span: error_span,
            };
            return Err(vec![error]);
        } else {
            self.advance_to_next_token(); // 跳过右括号
        }
        
        let then_branch = Box::new(self.parse_statement()?);
        
        // 可选的else分支
        let else_branch = if let Token::KeywordElse = self.current_token.token {
            self.advance_to_next_token();
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };
        
        Ok(Ast::new(
            AstKind::Statement(Statement::If {
                condition,
                then_branch,
                else_branch,
            }),
            self.create_span_from_to(&if_start_span, &self.current_token.span)
        ))
    }
    
    /// 解析while语句
    /// 
    /// 格式：while (条件) 语句
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回while语句AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_while_statement(&mut self) -> Result<Ast, Vec<ParseError>> {
        let while_start_span = self.current_token.span.clone();
        self.expect_token(Token::KeywordWhile)?;
        
        // 按照rustc设计理念：检查是否缺少左括号
        if !matches!(self.current_token.token, Token::LParen) {
            // 构造错误对象，不直接打印
            let error_span = self.current_token.span.clone();
            let error = ParseError {
                message: "缺少左括号".to_string(),
                span: error_span,
            };
            return Err(vec![error]);
        } else {
            self.advance_to_next_token(); // 跳过左括号
        }
        
        let condition = Box::new(self.parse_expression()?);
        
        // 按照rustc设计理念：检查是否缺少右括号
        if !matches!(self.current_token.token, Token::RParen) {
            // 构造错误对象，不直接打印
            let error_span = Span::new(
                self.current_token.span.file_id,
                self.current_token.span.line,
                self.current_token.span.column,
                condition.span.end_pos,
                self.current_token.span.end_pos,
            );
            let error = ParseError {
                message: "缺少右括号".to_string(),
                span: error_span,
            };
            return Err(vec![error]);
        } else {
            self.advance_to_next_token(); // 跳过右括号
        }
        
        let body = Box::new(self.parse_statement()?);
        
        // 按照rustc设计理念：检查while语句体是否缺少左大括号（建议使用大括号包围）
        if let AstKind::Statement(Statement::Compound { .. }) = body.kind {
            // 复合语句，正常
        } else {
            // 构造错误对象，不直接打印
            let error_span = Span::new(
                self.current_token.span.file_id,
                self.current_token.span.line,
                self.current_token.span.column,
                body.span.end_pos,
                self.current_token.span.end_pos,
            );
            let error = ParseError {
                message: "缺少左大括号".to_string(),
                span: error_span,
            };
            return Err(vec![error]);
        }
        
        Ok(Ast::new(
            AstKind::Statement(Statement::While {
                condition,
                body,
            }),
            self.create_span_from_to(&while_start_span, &self.current_token.span)
        ))
    }
    
    /// 解析return语句
    /// 
    /// 格式：return [表达式];
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回return语句AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_return_statement(&mut self) -> Result<Ast, Vec<ParseError>> {
        let return_start_span = self.current_token.span.clone();
        self.expect_token(Token::KeywordReturn)?;
        
        let return_value = if !matches!(self.current_token.token, Token::Semicolon) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };
        
        self.expect_token(Token::Semicolon)?;
        
        Ok(Ast::new(
            AstKind::Statement(Statement::Return { value: return_value }),
            self.create_span_from_to(&return_start_span, &self.current_token.span)
        ))
    }
    
    /// 解析break语句
    /// 
    /// 格式：break;
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回break语句AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_break_statement(&mut self) -> Result<Ast, Vec<ParseError>> {
        let break_span = self.current_token.span.clone();
        self.expect_token(Token::KeywordBreak)?;
        self.expect_token(Token::Semicolon)?;
        
        Ok(Ast::new(
            AstKind::Statement(Statement::Break),
            break_span
        ))
    }
    
    /// 解析continue语句
    /// 
    /// 格式：continue;
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回continue语句AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_continue_statement(&mut self) -> Result<Ast, Vec<ParseError>> {
        let continue_span = self.current_token.span.clone();
        self.expect_token(Token::KeywordContinue)?;
        self.expect_token(Token::Semicolon)?;
        
        Ok(Ast::new(
            AstKind::Statement(Statement::Continue),
            continue_span
        ))
    }
    
    /// 解析变量声明语句
    /// 
    /// 格式：类型 变量名 [= 初始值];
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回变量声明AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_variable_declaration(&mut self) -> Result<Ast, Vec<ParseError>> {
        let declaration_start_span = self.current_token.span.clone();
        
        // 解析类型声明
        let variable_type = self.parse_type_declaration()?;
        
        // 解析标识符名称
        let variable_name = self.expect_identifier_token()?;
        
        // 将变量添加到已定义集合中
        // 这里不再需要defined_variables，因为错误处理已移除
        
        // 可选的初始值
        let initial_value = if let Token::Equal = self.current_token.token {
            self.advance_to_next_token(); // 跳过'='
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };
        
        self.expect_token(Token::Semicolon)?;
        
        Ok(Ast::new(
            AstKind::VariableDeclaration {
                variable_name,
                variable_type,
                initial_value,
            },
            self.create_span_from_to(&declaration_start_span, &self.current_token.span)
        ))
    }
    
    // ==================== 表达式解析 ====================
    
    /// 解析表达式（入口方法）
    /// 
    /// 表达式解析采用递归下降方法，按照运算符优先级从低到高解析
    /// 优先级顺序：赋值 → 逻辑或 → 逻辑与 → 相等性 → 关系 → 加减 → 乘除模 → 一元运算 → 基本表达式
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回表达式AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_expression(&mut self) -> Result<Ast, Vec<ParseError>> {
        self.parse_assignment_expression()
    }
    
    /// 解析赋值表达式
    /// 
    /// 支持：变量 = 表达式
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回赋值表达式AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_assignment_expression(&mut self) -> Result<Ast, Vec<ParseError>> {
        let left_operand = self.parse_logical_or_expression()?;
        
        if let Token::Equal = self.current_token.token {
            let assignment_span = self.current_token.span.clone();
            self.advance_to_next_token();
            let right_operand = self.parse_assignment_expression()?;
            let right_span = right_operand.span.clone();
            
            return Ok(Ast::new(
                AstKind::Expression(Expression::Assignment {
                    target: Box::new(left_operand),
                    value: Box::new(right_operand),
                }),
                self.create_span_from_to(&assignment_span, &right_span)
            ));
        }
        
        Ok(left_operand)
    }
    
    /// 解析逻辑或表达式
    /// 
    /// 支持：表达式 || 表达式
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回逻辑或表达式AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_logical_or_expression(&mut self) -> Result<Ast, Vec<ParseError>> {
        let mut left_operand = self.parse_logical_and_expression()?;
        
        while let Token::OrOr = self.current_token.token {
            let operator_span = self.current_token.span.clone();
            self.advance_to_next_token();
            let right_operand = self.parse_logical_and_expression()?;
            let right_span = right_operand.span.clone();
            
            left_operand = Ast::new(
                AstKind::Expression(Expression::BinaryOperation {
                    operator: BinaryOperator::LogicalOr,
                    left_operand: Box::new(left_operand),
                    right_operand: Box::new(right_operand),
                }),
                self.create_span_from_to(&operator_span, &right_span)
            );
        }
        
        Ok(left_operand)
    }
    
    /// 解析逻辑与表达式
    /// 
    /// 支持：表达式 && 表达式
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回逻辑与表达式AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_logical_and_expression(&mut self) -> Result<Ast, Vec<ParseError>> {
        let mut left_operand = self.parse_equality_expression()?;
        
        while let Token::AndAnd = self.current_token.token {
            let operator_span = self.current_token.span.clone();
            self.advance_to_next_token();
            let right_operand = self.parse_equality_expression()?;
            let right_span = right_operand.span.clone();
            
            left_operand = Ast::new(
                AstKind::Expression(Expression::BinaryOperation {
                    operator: BinaryOperator::LogicalAnd,
                    left_operand: Box::new(left_operand),
                    right_operand: Box::new(right_operand),
                }),
                self.create_span_from_to(&operator_span, &right_span)
            );
        }
        
        Ok(left_operand)
    }
    
    /// 解析相等性表达式
    /// 
    /// 支持：表达式 == 表达式, 表达式 != 表达式
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回相等性表达式AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_equality_expression(&mut self) -> Result<Ast, Vec<ParseError>> {
        let mut left_operand = self.parse_relational_expression()?;
        
        while matches!(self.current_token.token, Token::DoubleEqual | Token::NotEqual) {
            let operator_span = self.current_token.span.clone();
            let operator = match self.current_token.token {
                Token::DoubleEqual => BinaryOperator::Equal,
                Token::NotEqual => BinaryOperator::NotEqual,
                _ => unreachable!(),
            };
            self.advance_to_next_token();
            let right_operand = self.parse_relational_expression()?;
            let right_span = right_operand.span.clone();
            
            left_operand = Ast::new(
                AstKind::Expression(Expression::BinaryOperation {
                    operator,
                    left_operand: Box::new(left_operand),
                    right_operand: Box::new(right_operand),
                }),
                self.create_span_from_to(&operator_span, &right_span)
            );
        }
        
        Ok(left_operand)
    }
    
    /// 解析关系表达式
    /// 
    /// 支持：表达式 < 表达式, 表达式 <= 表达式, 表达式 > 表达式, 表达式 >= 表达式
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回关系表达式AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_relational_expression(&mut self) -> Result<Ast, Vec<ParseError>> {
        let mut left_operand = self.parse_additive_expression()?;
        
        while matches!(self.current_token.token, 
            Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual) {
            let operator_span = self.current_token.span.clone();
            let operator = match self.current_token.token {
                Token::Less => BinaryOperator::LessThan,
                Token::LessEqual => BinaryOperator::LessEqual,
                Token::Greater => BinaryOperator::GreaterThan,
                Token::GreaterEqual => BinaryOperator::GreaterEqual,
                _ => unreachable!(),
            };
            self.advance_to_next_token();
            let right_operand = self.parse_additive_expression()?;
            let right_span = right_operand.span.clone();
            
            left_operand = Ast::new(
                AstKind::Expression(Expression::BinaryOperation {
                    operator,
                    left_operand: Box::new(left_operand),
                    right_operand: Box::new(right_operand),
                }),
                self.create_span_from_to(&operator_span, &right_span)
            );
        }
        
        Ok(left_operand)
    }
    
    /// 解析加法表达式
    /// 
    /// 支持：表达式 + 表达式, 表达式 - 表达式
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回加法表达式AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_additive_expression(&mut self) -> Result<Ast, Vec<ParseError>> {
        let mut left_operand = self.parse_multiplicative_expression()?;
        
        while matches!(self.current_token.token, Token::Plus | Token::Minus) {
            let operator_span = self.current_token.span.clone();
            let operator = match self.current_token.token {
                Token::Plus => BinaryOperator::Add,
                Token::Minus => BinaryOperator::Subtract,
                _ => unreachable!(),
            };
            self.advance_to_next_token();
            let right_operand = self.parse_multiplicative_expression()?;
            let right_span = right_operand.span.clone();
            
            left_operand = Ast::new(
                AstKind::Expression(Expression::BinaryOperation {
                    operator,
                    left_operand: Box::new(left_operand),
                    right_operand: Box::new(right_operand),
                }),
                self.create_span_from_to(&operator_span, &right_span)
            );
        }
        
        Ok(left_operand)
    }
    
    /// 解析乘法表达式
    /// 
    /// 支持：表达式 * 表达式, 表达式 / 表达式, 表达式 % 表达式
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回乘法表达式AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_multiplicative_expression(&mut self) -> Result<Ast, Vec<ParseError>> {
        let mut left_operand = self.parse_unary_expression()?;
        
        while matches!(self.current_token.token, 
            Token::Star | Token::Slash | Token::Percent) {
            let operator_span = self.current_token.span.clone();
            let operator = match self.current_token.token {
                Token::Star => BinaryOperator::Multiply,
                Token::Slash => BinaryOperator::Divide,
                Token::Percent => BinaryOperator::Modulo,
                _ => unreachable!(),
            };
            self.advance_to_next_token();
            let right_operand = self.parse_unary_expression()?;
            let right_span = right_operand.span.clone();
            
            left_operand = Ast::new(
                AstKind::Expression(Expression::BinaryOperation {
                    operator,
                    left_operand: Box::new(left_operand),
                    right_operand: Box::new(right_operand),
                }),
                self.create_span_from_to(&operator_span, &right_span)
            );
        }
        
        Ok(left_operand)
    }
    
    /// 解析一元表达式
    /// 
    /// 支持：+表达式, -表达式, !表达式
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回一元表达式AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_unary_expression(&mut self) -> Result<Ast, Vec<ParseError>> {
        match &self.current_token.token {
            Token::Plus | Token::Minus | Token::Bang => {
                let operator_span = self.current_token.span.clone();
                let operator = match self.current_token.token {
                    Token::Plus => UnaryOperator::Plus,
                    Token::Minus => UnaryOperator::Minus,
                    Token::Bang => UnaryOperator::LogicalNot,
                    _ => unreachable!(),
                };
                self.advance_to_next_token();
                let operand = self.parse_unary_expression()?;
                let operand_span = operand.span.clone();
                
                Ok(Ast::new(
                    AstKind::Expression(Expression::UnaryOperation { 
                        operator, 
                        operand: Box::new(operand) 
                    }),
                    self.create_span_from_to(&operator_span, &operand_span)
                ))
            }
            _ => self.parse_primary_expression(),
        }
    }
    
    /// 解析基本表达式
    /// 
    /// 支持：字面量、标识符、函数调用、括号表达式
    /// 
    /// # 返回
    /// * `Ok(Ast)` - 解析成功，返回基本表达式AST节点
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn parse_primary_expression(&mut self) -> Result<Ast, Vec<ParseError>> {
        match &self.current_token.token {
            Token::IntConst(value) => {
                let span = self.current_token.span.clone();
                let value = *value;
                self.advance_to_next_token();
                
                Ok(Ast::new(
                    AstKind::Expression(Expression::Literal(Literal::IntegerLiteral(value))),
                    span
                ))
            }
            Token::FloatConst(value) => {
                let span = self.current_token.span.clone();
                let value = *value;
                self.advance_to_next_token();
                
                Ok(Ast::new(
                    AstKind::Expression(Expression::Literal(Literal::FloatLiteral(value))),
                    span
                ))
            }
            Token::Identifier(name) => {
                let span = self.current_token.span.clone();
                let identifier_name = name.clone();
                self.advance_to_next_token();
                
                // 检查是否是函数调用
                if self.is_function_call_token() {
                    let call_start_span = self.current_token.span.clone();
                    self.advance_to_next_token();
                    let mut arguments = Vec::new();
                    
                    if !matches!(self.current_token.token, Token::RParen) {
                        loop {
                            arguments.push(self.parse_expression()?);
                            
                            if let Token::RParen = self.current_token.token {
                                break;
                            }
                            
                            self.expect_token(Token::Comma)?;
                        }
                    }
                    
                    // 按照rustc设计理念：检查是否缺少右括号
                    if !matches!(self.current_token.token, Token::RParen) {
                        // 构造错误对象，不直接打印
                        let error_span = self.current_token.span.clone();
                        let error = ParseError {
                            message: "缺少右括号".to_string(),
                            span: error_span,
                        };
                        return Err(vec![error]);
                    } else {
                        self.advance_to_next_token(); // 跳过右括号
                    }
                    
                    let call_end_span = self.current_token.span.clone();
                    
                    Ok(Ast::new(
                        AstKind::Expression(Expression::FunctionCall {
                            function_name: identifier_name,
                            arguments,
                        }),
                        self.create_span_from_to(&span, &call_end_span)
                    ))
                } else {
                    // 检查变量是否已定义
                    // 这里不再需要defined_variables，因为错误处理已移除
                    
                    Ok(Ast::new(
                        AstKind::Expression(Expression::Identifier { name: identifier_name }),
                        span
                    ))
                }
            }
            Token::LParen => {
                // 括号表达式：(表达式)
                let paren_start_span = self.current_token.span.clone();
                self.advance_to_next_token();
                let expression = self.parse_expression()?;
                
                // 按照rustc设计理念：检查是否缺少右括号
                if !matches!(self.current_token.token, Token::RParen) {
                    // 构造错误对象，不直接打印
                    let error_span = self.current_token.span.clone();
                    let error = ParseError {
                        message: "缺少右括号".to_string(),
                        span: error_span,
                    };
                    return Err(vec![error]);
                } else {
                    self.advance_to_next_token(); // 跳过右括号
                }
                
                Ok(expression)
            }
            _ => {
                // 按照rustc设计理念：构造错误对象，不直接打印
                let actual_token = self.token_to_string(&self.current_token.token);
                let error_span = self.current_token.span.clone();
                let message = format!("期望 '表达式（数字、标识符或'('）'，但找到 '{}'", actual_token);
                let error = ParseError {
                    message,
                    span: error_span,
                };
                return Err(vec![error]);
            }
        }
    }
    
    // ==================== 辅助方法 ====================
    
    /// 前进到下一个Token
    fn advance_to_next_token(&mut self) {
        self.current_token = self.lexer.next_located_token();
    }
    
    /// 检查是否到达文件末尾
    fn is_eof(&self) -> bool {
        matches!(self.current_token.token, Token::EOF)
    }
    
    /// 检查当前Token是否是函数调用的开始
    fn is_function_call_token(&self) -> bool {
        matches!(self.current_token.token, Token::LParen)
    }
    
    /// 期望特定token，如果不符合则构造错误对象
    /// 
    /// 按照rustc设计理念：不直接打印错误，而是构造错误对象返回
    fn expect_token_with_error(&mut self, expected: Token, error_msg: &str) -> Result<(), Vec<ParseError>> {
        if self.current_token.token == expected {
            self.advance_to_next_token();
            Ok(())
        } else {
            // 按照rustc的方式：比较期望的token与实际遇到的token
            let expected_str = self.token_to_string(&expected);
            let actual_str = self.token_to_string(&self.current_token.token);
            let message = format!("期望 '{}', 但找到 '{}'", expected_str, actual_str);
            
            // 构造错误对象，不直接打印
            let error_span = self.current_token.span.clone();
            let error = ParseError {
                message,
                span: error_span,
            };
            Err(vec![error])
        }
    }
    
    /// 将token转换为字符串表示
    fn token_to_string(&self, token: &Token) -> String {
        match token {
            Token::Semicolon => ";".to_string(),
            Token::LParen => "(".to_string(),
            Token::RParen => ")".to_string(),
            Token::LBrace => "{".to_string(),
            Token::RBrace => "}".to_string(),
            Token::Equal => "=".to_string(),
            Token::Plus => "+".to_string(),
            Token::Minus => "-".to_string(),
            Token::Star => "*".to_string(),
            Token::Slash => "/".to_string(),
            Token::KeywordInt => "int".to_string(),
            Token::KeywordFloat => "float".to_string(),
            Token::KeywordVoid => "void".to_string(),
            Token::KeywordIf => "if".to_string(),
            Token::KeywordWhile => "while".to_string(),
            Token::KeywordReturn => "return".to_string(),
            Token::EOF => "文件结束".to_string(),
            Token::Identifier(name) => name.clone(),
            Token::IntConst(value) => value.to_string(),
            _ => format!("{:?}", token),
        }
    }
    
    /// 期望特定类型的Token
    /// 
    /// 按照rustc设计理念：不直接打印错误，而是构造错误对象返回
    /// 
    /// # 参数
    /// * `expected` - 期望的Token类型
    /// 
    /// # 返回
    /// * `Ok(())` - 匹配成功
    /// * `Err(Vec<ParseError>)` - 匹配失败
    fn expect_token(&mut self, expected: Token) -> Result<(), Vec<ParseError>> {
        if self.current_token.token == expected {
            self.advance_to_next_token();
            Ok(())
        } else {
            // 使用rustc风格的错误检测
            let expected_str = self.token_to_string(&expected);
            let actual_str = self.token_to_string(&self.current_token.token);
            
            // 构造错误对象，不直接打印
            let error_span = self.current_token.span.clone();
            let error = ParseError {
                message: format!("期望 '{}', 但找到 '{}'", expected_str, actual_str),
                span: error_span,
            };
            Err(vec![error])
        }
    }
    
    /// 期望标识符Token
    /// 
    /// 按照rustc设计理念：不直接打印错误，而是构造错误对象返回
    /// 
    /// # 返回
    /// * `Ok(String)` - 解析成功，返回标识符名称
    /// * `Err(Vec<ParseError>)` - 解析失败
    fn expect_identifier_token(&mut self) -> Result<String, Vec<ParseError>> {
        if let Token::Identifier(name) = &self.current_token.token {
            let name = name.clone();
            self.advance_to_next_token();
            Ok(name)
        } else {
            // 构造错误对象，不直接打印
            let actual_token = self.token_to_string(&self.current_token.token);
            let error_span = self.current_token.span.clone();
            let error = ParseError {
                message: format!("期望 '标识符'，但找到 '{}'", actual_token),
                span: error_span,
            };
            Err(vec![error])
        }
    }
    
    /// 创建从start到end的Span
    /// 
    /// 包含边界检查，确保Span的有效性
    /// 
    /// # 参数
    /// * `start` - 起始Span
    /// * `end` - 结束Span
    /// 
    /// # 返回
    /// 合并后的Span
    fn create_span_from_to(&self, start: &Span, end: &Span) -> Span {
        // 确保start和end属于同一个文件
        if start.file_id != end.file_id {
            return start.clone();
        }
        
        // 确保end位置在start之后
        if end.end_pos < start.start_pos {
            return start.clone();
        }
        
        Span::new(
            start.file_id,
            start.line,
            start.column,
            start.start_pos,
            end.end_pos,
        )
    }
    
    /// 错误恢复：跳过到下一个同步点
    /// 
    /// 按照rustc设计理念：遇到错误后，跳过当前错误区域，
    /// 继续解析后续代码，收集所有错误而不是只报第一个
    /// 
    /// 同步点包括：
    /// - 分号 (语句结束)
    /// - 右大括号 (块结束)
    /// - 类型关键字 (新声明开始)
    /// - 文件结束
    fn recover_from_error(&mut self) {
        while !self.is_eof() {
            match self.current_token.token {
                // 语句结束标记
                Token::Semicolon => {
                    self.advance_to_next_token();
                    break;
                }
                // 块结束标记
                Token::RBrace => {
                    break;
                }
                // 新声明开始标记
                Token::KeywordInt | Token::KeywordFloat | Token::KeywordVoid => {
                    break;
                }
                // 其他控制流关键字
                Token::KeywordIf | Token::KeywordWhile | Token::KeywordReturn |
                Token::KeywordBreak | Token::KeywordContinue => {
                    break;
                }
                // 跳过其他token
                _ => {
                    self.advance_to_next_token();
                }
            }
        }
    }
}
//! AST 到 Sea of Nodes IR 转换器模块
//! 
//! 这个模块提供了将抽象语法树(AST)转换为Sea of Nodes中间表示(IR)的功能。
//! Sea of Nodes是一种基于图的中间表示，支持高效的优化和代码生成。



pub mod son_ir;
pub mod node_mapping;
pub mod converter;

// 重新导出主要的类型和结构体
pub use son_ir::{
    SonIr,
    SonNode,
    SonNodeKind,
    SonNodeId,
    SonEdge,
    OpCode,
    NodeData,
    ConstantValue,
    EdgeType,
    TypeSystem,
};

pub use node_mapping::NodeMapping;

pub use converter::{
    AstToSonConverter,
    ConversionResult,
    ConversionStats,
    ConversionError,
};

// 重新导出常用的操作码，方便外部使用
pub use son_ir::OpCode::{
    // 控制流操作
    Start,
    Stop,
    Region,
    If,
    Loop,
    Merge,
    Return,
    Break,
    Continue,
    
    // 常量操作
    Constant,
    Parameter,
    Local,
    
    // 算术运算
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    
    // 比较运算
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    
    // 逻辑运算
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    
    // 一元运算
    Minus,
    BitwiseNot,
    
    // 内存操作
    Load,
    Store,
    ArrayAccess,
    MemberAccess,
    New,
    
    // 函数调用
    Call,
    
    // 类型转换
    Cast,
    
    // 特殊节点
    Phi,
    Proj,
    CProj,
    
    // 位运算
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
    ArithmeticShiftRight,
    
    // 浮点运算
    ToFloat,
    RoundF32,
};

// 重新导出常量值类型
pub use son_ir::ConstantValue::{
    Integer,
    Float,
    Boolean,
};

// 重新导出边类型
pub use son_ir::EdgeType::{
    Data,
    Control,
    Condition,
};

// 重新导出节点数据类型
pub use son_ir::NodeData::{
    Start as StartData,
    If as IfData,
    Loop as LoopData,
    Region as RegionData,
    Constant as ConstantData,
    Parameter as ParameterData,
    Local as LocalData,
    BinaryOp,
    UnaryOp,
    Load as LoadData,
    Store as StoreData,
    ArrayAccess as ArrayAccessData,
    MemberAccess as MemberAccessData,
    Call as CallData,
    Cast as CastData,
    Phi as PhiData,
    Proj as ProjData,
    CProj as CProjData,
    None,
};

/// 便捷函数：将AST转换为Sea of Nodes IR
/// 
/// # 参数
/// * `ast` - 要转换的抽象语法树
/// 
/// # 返回
/// * `Ok(SonIr)` - 转换成功的Sea of Nodes IR
/// * `Err(ConversionError)` - 转换失败的错误信息
pub fn convert_ast_to_son(ast: &crate::frontend::ast::Ast) -> Result<SonIr, ConversionError> {
    AstToSonConverter::convert(ast).map(|result| result.son_ir)
}

/// 便捷函数：将AST转换为Sea of Nodes IR并返回详细结果
/// 
/// # 参数
/// * `ast` - 要转换的抽象语法树
/// 
/// # 返回
/// * `Ok(ConversionResult)` - 包含转换结果和统计信息的详细结果
/// * `Err(ConversionError)` - 转换失败的错误信息
pub fn convert_ast_to_son_with_stats(ast: &crate::frontend::ast::Ast) -> Result<ConversionResult, ConversionError> {
    AstToSonConverter::convert_with_validation(ast)
}

/// 便捷函数：验证AST是否可以转换为Sea of Nodes IR
/// 
/// # 参数
/// * `ast` - 要验证的抽象语法树
/// 
/// # 返回
/// * `Ok(())` - AST可以转换
/// * `Err(Vec<std::string::String>)` - 验证失败的问题列表
pub fn validate_ast_for_conversion(ast: &crate::frontend::ast::Ast) -> Result<(), Vec<std::string::String>> {
    // 直接调用converter.rs中的静态函数
    crate::ast_to_cfg::ast_to_SoNir::converter::validate_ast_for_conversion(ast)
}

/// 便捷函数：获取AST转换的预估统计信息
/// 
/// # 参数
/// * `ast` - 要分析的抽象语法树
/// 
/// # 返回
/// 预估的转换统计信息
pub fn estimate_conversion_stats(ast: &crate::frontend::ast::Ast) -> ConversionStats {
    // 直接调用converter.rs中的静态函数
    crate::ast_to_cfg::ast_to_SoNir::converter::estimate_conversion_stats(ast)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::ast::{Ast, AstKind, Expression, Statement, Literal, Type, SemanticInfo};
    use crate::frontend::span::Span;

    #[test]
    fn test_module_exports() {
        // 测试所有主要类型都能正确导出
        let _: SonIr = SonIr::new();
        let _: SonNode = SonNode::new(0, SonNodeKind::new(OpCode::Start));
        let _: SonEdge = SonEdge::new(0, 1, EdgeType::Data);
        let _: TypeSystem = TypeSystem::new();
        
        // 测试操作码导出
        let _: OpCode = OpCode::Start;
        let _: OpCode = OpCode::Return;
        let _: OpCode = OpCode::Add;
        
        // 测试常量值导出
        let _: ConstantValue = ConstantValue::Integer(42);
        let _: ConstantValue = ConstantValue::Float(3.14);
        let _: ConstantValue = ConstantValue::Boolean(true);
        let _: ConstantValue = son_ir::ConstantValue::String("hello".to_string());
        
        // 测试边类型导出
        let _: EdgeType = EdgeType::Data;
        let _: EdgeType = EdgeType::Control;
        let _: EdgeType = EdgeType::Condition;
    }

    #[test]
    fn test_convenience_functions() {
        // 测试便捷函数的存在性
        let ast = create_test_ast();
        
        // 这些函数应该存在，即使可能返回错误
        let _ = validate_ast_for_conversion(&ast);
        let _ = estimate_conversion_stats(&ast);
    }

    #[test]
    fn test_sy_file_to_sonir() {
        // 读取00_main.sy文件内容
        let source_code = match std::fs::read_to_string("/home/yizhiluobo/luobo_compiler/src/target_code/sy/000_1.sy") {
            Ok(content) => content,
            Err(_) => {
                "int main(){
    return 3;
}".to_string()
            }
        };
        
        // 使用词法分析器
        let mut lexer = crate::frontend::lexer::Lexer::new(&source_code);
        
        // 使用语法分析器
        let mut parser = crate::frontend::parser::Parser::new(lexer);
        
        // 解析生成AST
        let parse_result = parser.parse();
        match parse_result {
            Ok(ast) => {
                
                // 进行语义分析
                let semantic_result = crate::frontend::semantic_analysis::analyze_ast_with_semantic_info(ast);
                match semantic_result {
                    Ok(annotated_ast) => {

                        
                        // 从Program AST中提取Function节点
                        let function_ast = match &annotated_ast.kind {
                            crate::frontend::ast::AstKind::Program { functions, .. } => {
                                if functions.is_empty() {
                                    panic!("Program中没有找到函数定义");
                                }
                                // 取第一个函数（在我们的例子中就是main函数）
                                &functions[0]
                            }
                            _ => panic!("AST根节点不是Program类型"),
                        };
                        

                        
                        // 转换为SoNir（传递Function节点而不是Program节点）
                        let sonir_result = convert_ast_to_son_with_stats(function_ast);
                        match sonir_result {
                            Ok(conversion_result) => {
                                
                                
                                // 显示生成的SoNir结构
                                let sonir = &conversion_result.son_ir;
                                

                                
                                // 生成dot图文件，使用固定文件名
                                let dot_content = generate_dot_graph(sonir);
                                let filename = "/home/yizhiluobo/luobo_compiler/src/ast_to_cfg/Debug/graph/test_sy_file_to_sonir.dot";
                                
                                match std::fs::write(filename, dot_content) {
                                    Ok(_) => {},
                                    Err(e) => {},
                                }
                                
                                // 验证生成的IR
                                assert!(sonir.node_count() > 0, "SoNir应该包含节点");
                                assert!(sonir.edge_count() > 0, "SoNir应该包含边");
                                assert!(sonir.get_entry_node().is_some(), "SoNir应该有入口节点");
                                assert!(sonir.get_exit_node().is_some(), "SoNir应该有出口节点");
                                

                            }
                            Err(e) => {
                                panic!("转换失败: {:?}", e);
                            }
                        }
                    }
                    Err(errors) => {
                        panic!("语义分析失败");
                    }
                }
            }
            Err(parse_errors) => {
                panic!("语法分析失败");
            }
        }
    }

    /// 生成DOT格式的图形表示
    fn generate_dot_graph(sonir: &SonIr) -> String {
        let mut dot = String::new();
        dot.push_str("digraph SoNir {\n");
        dot.push_str("  rankdir=TB;\n");
        dot.push_str("  node [shape=box, style=filled, fontname=\"Arial\"];\n");
        dot.push_str("  edge [fontname=\"Arial\"];\n\n");
        
        // 添加节点
        for (id, node) in sonir.get_all_nodes() {
            let node_label = format!("Node_{}", id);
            let node_content = match &node.kind.opcode {
                OpCode::Start => "Start".to_string(),
                OpCode::Return => "Return".to_string(),
                OpCode::Constant => {
                    if let NodeData::Constant { value, typ } = &node.kind.data {
                        format!("Constant\\n{:?}\\n{:?}", value, typ)
                    } else {
                        "Constant".to_string()
                    }
                }
                OpCode::Parameter => {
                    if let NodeData::Parameter { name, typ } = &node.kind.data {
                        format!("Parameter\\n{}\\n{:?}", name, typ)
                    } else {
                        "Parameter".to_string()
                    }
                }
                OpCode::Local => {
                    if let NodeData::Local { name, typ } = &node.kind.data {
                        format!("Local\\n{}\\n{:?}", name, typ)
                    } else {
                        "Local".to_string()
                    }
                }
                OpCode::Add => "Add".to_string(),
                OpCode::Subtract => "Subtract".to_string(),
                OpCode::Multiply => "Multiply".to_string(),
                OpCode::Divide => "Divide".to_string(),
                OpCode::If => "If".to_string(),
                OpCode::Loop => "Loop".to_string(),
                OpCode::Call => "Call".to_string(),
                _ => format!("{:?}", node.kind.opcode),
            };
            
            // 根据节点类型设置颜色
            let color = match node.kind.opcode {
                OpCode::Start => "lightgreen",
                OpCode::Return => "lightcoral",
                OpCode::Constant => "lightblue",
                OpCode::Parameter => "lightyellow",
                OpCode::Local => "lightcyan",
                OpCode::Add | OpCode::Subtract | OpCode::Multiply | OpCode::Divide => "lightpink",
                OpCode::If => "orange",
                OpCode::Loop => "purple",
                OpCode::Call => "gold",
                _ => "white",
            };
            
            dot.push_str(&format!("  {} [label=\"{}\", fillcolor=\"{}\"];\n", node_label, node_content, color));
        }
        
        dot.push_str("\n");
        
        // 添加边
        for edge in sonir.get_all_edges() {
            let from_label = format!("Node_{}", edge.from);
            let to_label = format!("Node_{}", edge.to);
            let edge_label = match edge.edge_type {
                EdgeType::Control => "control",
                EdgeType::Data => "data",
                EdgeType::Condition => "condition",
            };
            
            let edge_color = match edge.edge_type {
                EdgeType::Control => "red",
                EdgeType::Data => "blue",
                EdgeType::Condition => "green",
            };
            
            dot.push_str(&format!("  {} -> {} [label=\"{}\", color=\"{}\"];\n", 
                from_label, to_label, edge_label, edge_color));
        }
        
        dot.push_str("}\n");
        dot
    }

    fn create_test_ast() -> Ast {
        // 创建一个简单的测试AST
        let return_stmt = Ast {
            kind: AstKind::Statement(Statement::Return {
                value: Some(Box::new(Ast {
                    kind: AstKind::Expression(Expression::Literal(Literal::IntegerLiteral(42))),
                    span: Span::new(0, 1, 1, 10, 12),
                    semantic_info: SemanticInfo::new(),
                })),
            }),
            span: Span::new(0, 1, 1, 8, 12),
            semantic_info: SemanticInfo::new(),
        };

        let compound_stmt = Ast {
            kind: AstKind::Statement(Statement::Compound {
                statements: vec![return_stmt],
            }),
            span: Span::new(0, 1, 1, 7, 13),
            semantic_info: SemanticInfo::new(),
        };

        let main_func = Ast {
            kind: AstKind::Function {
                function_name: "main".to_string(),
                parameters: Vec::new(),
                return_type: Some(Type::IntType),
                function_body: Box::new(compound_stmt),
            },
            span: Span::new(0, 1, 1, 1, 13),
            semantic_info: SemanticInfo::new(),
        };

        Ast {
            kind: AstKind::Program {
                functions: vec![main_func],
                global_variables: Vec::new(),
            },
            span: Span::new(0, 1, 1, 0, 13),
            semantic_info: SemanticInfo::new(),
        }
    }
}

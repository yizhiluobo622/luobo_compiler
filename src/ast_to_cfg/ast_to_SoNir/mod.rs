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

/// 生成SoN IR的DOT图文件
/// 
/// # 参数
/// * `son_ir` - 要生成图的Sea of Nodes IR
/// * `output_path` - 输出文件路径
/// 
/// # 返回
/// * `Ok(())` - 成功生成图文件
/// * `Err(std::io::Error)` - 生成失败的错误信息
pub fn generate_son_ir_dot(son_ir: &SonIr, output_path: &str) -> std::io::Result<()> {
    use std::fs::File;
    use std::io::Write;
    
    let mut file = File::create(output_path)?;
    
    // 写入DOT文件头部
    writeln!(file, "digraph SoNir {{")?;
    writeln!(file, "  rankdir=TB;")?;
    writeln!(file, "  node [shape=box, style=filled, fontname=\"Arial\"];")?;
    writeln!(file, "  edge [fontname=\"Arial\"];")?;
    writeln!(file, "")?;
    
    // 生成节点
    for (node_id, node) in son_ir.get_all_nodes() {
        let node_label = match &node.kind {
            SonNodeKind { opcode: OpCode::Constant, data: NodeData::Constant { value, .. } } => {
                format!("Constant\\n{:?}", value)
            }
            SonNodeKind { opcode: OpCode::Local, data: NodeData::Local { name, typ, .. } } => {
                format!("Local\\n{}\\n{:?}", name, typ)
            }
            SonNodeKind { opcode: OpCode::Parameter, data: NodeData::Parameter { name, typ, .. } } => {
                format!("Parameter\\n{}\\n{:?}", name, typ)
            }
            SonNodeKind { opcode: OpCode::Add, data: NodeData::BinaryOp { left, right } } => {
                format!("Add\\nL: Node_{}\\nR: Node_{}", 
                    left.map(|id| id.to_string()).unwrap_or_else(|| "None".to_string()),
                    right.map(|id| id.to_string()).unwrap_or_else(|| "None".to_string()))
            }
            SonNodeKind { opcode: OpCode::Subtract, data: NodeData::BinaryOp { left, right } } => {
                format!("Subtract\\nL: Node_{}\\nR: Node_{}", 
                    left.map(|id| id.to_string()).unwrap_or_else(|| "None".to_string()),
                    right.map(|id| id.to_string()).unwrap_or_else(|| "None".to_string()))
            }
            SonNodeKind { opcode: OpCode::Multiply, data: NodeData::BinaryOp { left, right } } => {
                format!("Multiply\\nL: Node_{}\\nR: Node_{}", 
                    left.map(|id| id.to_string()).unwrap_or_else(|| "None".to_string()),
                    right.map(|id| id.to_string()).unwrap_or_else(|| "None".to_string()))
            }
            SonNodeKind { opcode: OpCode::Divide, data: NodeData::BinaryOp { left, right } } => {
                format!("Divide\\nL: Node_{}\\nR: Node_{}", 
                    left.map(|id| id.to_string()).unwrap_or_else(|| "None".to_string()),
                    right.map(|id| id.to_string()).unwrap_or_else(|| "None".to_string()))
            }
            SonNodeKind { opcode: OpCode::Store, data: NodeData::Store { name, value, .. } } => {
                format!("Store\\n{}\\nValue: Node_{}", 
                    name,
                    value.map(|id| id.to_string()).unwrap_or_else(|| "None".to_string()))
            }
            SonNodeKind { opcode: OpCode::Load, data: NodeData::Load { name, .. } } => {
                format!("Load\\n{}", name)
            }
            SonNodeKind { opcode: OpCode::Return, data: NodeData::None } => "Return".to_string(),
            SonNodeKind { opcode: OpCode::Start, data: NodeData::None } => "Start".to_string(),
            _ => format!("{:?}", node.kind.opcode),
        };
        
        let fillcolor = match node.kind.opcode {
            OpCode::Start => "lightgreen",
            OpCode::Return => "lightcoral",
            OpCode::Constant => "lightblue",
            OpCode::Local | OpCode::Parameter => "lightcyan",
            OpCode::Add | OpCode::Subtract | OpCode::Multiply | OpCode::Divide => "lightpink",
            OpCode::Store | OpCode::Load => "white",
            _ => "lightgray",
        };
        
        writeln!(file, "  Node_{} [label=\"{}\", fillcolor=\"{}\"];", node_id, node_label, fillcolor)?;
    }
    
    writeln!(file, "")?;
    
    // 生成边
    for edge in son_ir.get_all_edges() {
        let edge_label = match edge.edge_type {
            EdgeType::Data => "data",
            EdgeType::Control => "control",
        };
        
        let edge_color = match edge.edge_type {
            EdgeType::Data => "blue",
            EdgeType::Control => "red",
        };
        
        writeln!(file, "  Node_{} -> Node_{} [label=\"{}\", color=\"{}\"];", edge.from, edge.to, edge_label, edge_color)?;
    }
    
    writeln!(file, "}}")?;
    Ok(())
}

//! AST 到 Sea of Nodes IR 转换器模块
//! 
//! 这个模块负责将带语义信息的 AST 转换为 Sea of Nodes IR，
//! 支持控制流和数据流的统一表示。

pub mod converter;
pub mod son_ir;
pub mod node_mapping;

pub use converter::AstToSonConverter;
pub use son_ir::{SonIr, SonNode, SonNodeId, SonEdge, SonNodeKind};
pub use node_mapping::NodeMapping;

/// 转换结果
#[derive(Debug)]
pub struct ConversionResult {
    /// 是否成功
    pub success: bool,
    /// 生成的 Sea of Nodes IR
    pub son_ir: Option<SonIr>,
    /// 转换过程中的错误
    pub errors: Vec<String>,
}

impl ConversionResult {
    /// 创建成功的结果
    pub fn success(son_ir: SonIr) -> Self {
        Self {
            success: true,
            son_ir: Some(son_ir),
            errors: Vec::new(),
        }
    }
    
    /// 创建失败的结果
    pub fn failure(errors: Vec<String>) -> Self {
        Self {
            success: false,
            son_ir: None,
            errors,
        }
    }
    
    /// 检查是否有错误
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
    
    /// 打印所有错误
    pub fn print_errors(&self) {
        if self.errors.is_empty() {
            println!("✅ AST 转换成功");
            return;
        }
        
        println!("❌ AST 转换发现 {} 个错误:", self.errors.len());
        for (i, error) in self.errors.iter().enumerate() {
            println!("  {}. {}", i + 1, error);
        }
    }
}

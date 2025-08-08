use super::types::HighLevelType;

#[derive(Debug, Clone)]
pub enum HighLevelValue {
    Variable(String),
    Literal(HighLevelType, String),
    Temporary(usize),
    Constant(HighLevelType, String),
}

impl HighLevelValue {
    pub fn name(&self) -> &str {
        match self {
            HighLevelValue::Variable(name) => name,
            HighLevelValue::Literal(_, value) => value,
            HighLevelValue::Temporary(id) => {
                // 这里需要实现一个静态字符串池来避免生命周期问题
                // 简化实现，实际应该使用更安全的方式
                "temp"
            }
            HighLevelValue::Constant(_, value) => value,
        }
    }
    
    pub fn type_(&self) -> &HighLevelType {
        match self {
            HighLevelValue::Variable(_) => {
                // 这里需要从符号表获取类型信息
                // 简化实现
                &HighLevelType::Int(super::types::IntType::I32)
            }
            HighLevelValue::Literal(typ, _) => typ,
            HighLevelValue::Temporary(_) => {
                // 临时变量类型需要从上下文推断
                &HighLevelType::Int(super::types::IntType::I32)
            }
            HighLevelValue::Constant(typ, _) => typ,
        }
    }
    
    pub fn is_constant(&self) -> bool {
        matches!(self, HighLevelValue::Literal(_, _) | HighLevelValue::Constant(_, _))
    }
}

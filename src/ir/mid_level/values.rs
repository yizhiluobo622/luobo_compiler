use super::types::MidLevelType;

#[derive(Debug, Clone)]
pub enum MidLevelValue {
    Register(String),
    Immediate(String),
    Memory(String),
    Constant(MidLevelType, String),
}

impl MidLevelValue {
    pub fn name(&self) -> &str {
        match self {
            MidLevelValue::Register(name) => name,
            MidLevelValue::Immediate(value) => value,
            MidLevelValue::Memory(addr) => addr,
            MidLevelValue::Constant(_, value) => value,
        }
    }
    
    pub fn type_(&self) -> MidLevelType {
        match self {
            MidLevelValue::Register(_) => {
                // 寄存器类型需要从上下文推断
                MidLevelType::I32
            }
            MidLevelValue::Immediate(_) => {
                // 立即数类型需要从上下文推断
                MidLevelType::I32
            }
            MidLevelValue::Memory(_) => {
                // 内存地址类型
                MidLevelType::Pointer { target_type: Box::new(MidLevelType::I32) }
            }
            MidLevelValue::Constant(typ, _) => typ.clone(),
        }
    }
    
    pub fn is_constant(&self) -> bool {
        matches!(self, MidLevelValue::Immediate(_) | MidLevelValue::Constant(_, _))
    }
}

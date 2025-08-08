#[derive(Debug, Clone)]
pub enum LowLevelType {
    // 机器类型
    I8, I16, I32, I64,
    F32, F64,
    Bool,
    Void,
    
    // 寄存器类型
    Register { reg_name: String, size: usize },
    
    // 内存类型
    Memory { addr: String, size: usize },
    
    // 立即数类型
    Immediate { value: String, size: usize },
}

#[derive(Debug, Clone)]
pub enum CmpPredicate {
    Eq, Ne, Lt, Le, Gt, Ge,
}

impl LowLevelType {
    pub fn is_primitive(&self) -> bool {
        matches!(self, LowLevelType::I8 | LowLevelType::I16 | LowLevelType::I32 | LowLevelType::I64 | 
                        LowLevelType::F32 | LowLevelType::F64 | LowLevelType::Bool | LowLevelType::Void)
    }
    
    pub fn size(&self) -> usize {
        match self {
            LowLevelType::I8 => 1,
            LowLevelType::I16 => 2,
            LowLevelType::I32 => 4,
            LowLevelType::I64 => 8,
            LowLevelType::F32 => 4,
            LowLevelType::F64 => 8,
            LowLevelType::Bool => 1,
            LowLevelType::Void => 0,
            LowLevelType::Register { size, .. } => *size,
            LowLevelType::Memory { size, .. } => *size,
            LowLevelType::Immediate { size, .. } => *size,
        }
    }
}

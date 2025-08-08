#[derive(Debug, Clone)]
pub enum MidLevelType {
    // 基本类型（简化版）
    I8, I16, I32, I64,
    F32, F64,
    Bool,
    Void,
    
    // 指针类型
    Pointer { target_type: Box<MidLevelType> },
    
    // 向量类型
    Vector { element_type: Box<MidLevelType>, size: usize },
    
    // 数组类型
    Array { element_type: Box<MidLevelType>, size: usize },
}

#[derive(Debug, Clone)]
pub enum CmpPredicate {
    Eq, Ne, Lt, Le, Gt, Ge,
}

impl MidLevelType {
    pub fn is_primitive(&self) -> bool {
        matches!(self, MidLevelType::I8 | MidLevelType::I16 | MidLevelType::I32 | MidLevelType::I64 | 
                        MidLevelType::F32 | MidLevelType::F64 | MidLevelType::Bool | MidLevelType::Void)
    }
    
    pub fn size(&self) -> usize {
        match self {
            MidLevelType::I8 => 1,
            MidLevelType::I16 => 2,
            MidLevelType::I32 => 4,
            MidLevelType::I64 => 8,
            MidLevelType::F32 => 4,
            MidLevelType::F64 => 8,
            MidLevelType::Bool => 1,
            MidLevelType::Void => 0,
            MidLevelType::Pointer { .. } => 8, // 假设64位系统
            MidLevelType::Vector { element_type, size } => {
                element_type.size() * size
            }
            MidLevelType::Array { element_type, size } => {
                element_type.size() * size
            }
        }
    }
}

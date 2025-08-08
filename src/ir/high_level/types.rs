#[derive(Debug, Clone)]
pub enum HighLevelType {
    // 基本类型
    Int(IntType),
    Float(FloatType),
    Bool,
    Void,
    Char,
    
    // 复合类型
    Array { element_type: Box<HighLevelType>, size: Option<usize> },
    Pointer { target_type: Box<HighLevelType> },
    Struct { fields: Vec<(String, HighLevelType)> },
    Function { params: Vec<HighLevelType>, return_type: Box<HighLevelType> },
    
    // 高级类型
    Vector { element_type: Box<HighLevelType>, size: usize },
    Tuple { elements: Vec<HighLevelType> },
    Union { variants: Vec<(String, HighLevelType)> },
}

#[derive(Debug, Clone)]
pub enum IntType {
    I8, I16, I32, I64, I128,
    U8, U16, U32, U64, U128,
}

#[derive(Debug, Clone)]
pub enum FloatType {
    F32, F64, F128,
}

#[derive(Debug, Clone)]
pub enum CmpPredicate {
    Eq, Ne, Lt, Le, Gt, Ge,
}

impl HighLevelType {
    pub fn is_primitive(&self) -> bool {
        matches!(self, HighLevelType::Int(_) | HighLevelType::Float(_) | HighLevelType::Bool | HighLevelType::Void | HighLevelType::Char)
    }
    
    pub fn size(&self) -> usize {
        match self {
            HighLevelType::Int(IntType::I8) => 1,
            HighLevelType::Int(IntType::I16) => 2,
            HighLevelType::Int(IntType::I32) => 4,
            HighLevelType::Int(IntType::I64) => 8,
            HighLevelType::Int(IntType::I128) => 16,
            HighLevelType::Int(IntType::U8) => 1,
            HighLevelType::Int(IntType::U16) => 2,
            HighLevelType::Int(IntType::U32) => 4,
            HighLevelType::Int(IntType::U64) => 8,
            HighLevelType::Int(IntType::U128) => 16,
            HighLevelType::Float(FloatType::F32) => 4,
            HighLevelType::Float(FloatType::F64) => 8,
            HighLevelType::Float(FloatType::F128) => 16,
            HighLevelType::Bool => 1,
            HighLevelType::Void => 0,
            HighLevelType::Char => 1,
            HighLevelType::Array { element_type, size } => {
                let element_size = element_type.size();
                size.unwrap_or(0) * element_size
            }
            HighLevelType::Pointer { .. } => 8, // 假设64位系统
            HighLevelType::Struct { fields } => {
                fields.iter().map(|(_, field_type)| field_type.size()).sum()
            }
            HighLevelType::Function { .. } => 8, // 函数指针
            HighLevelType::Vector { element_type, size } => {
                element_type.size() * size
            }
            HighLevelType::Tuple { elements } => {
                elements.iter().map(|t| t.size()).sum()
            }
            HighLevelType::Union { variants } => {
                variants.iter().map(|(_, variant_type)| variant_type.size()).max().unwrap_or(0)
            }
        }
    }
}

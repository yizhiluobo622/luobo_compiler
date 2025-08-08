use super::types::LowLevelType;

#[derive(Debug, Clone)]
pub enum LowLevelValue {
    Register(String),
    Memory(String),
    Immediate(String),
    Label(String),
}

impl LowLevelValue {
    pub fn name(&self) -> &str {
        match self {
            LowLevelValue::Register(name) => name,
            LowLevelValue::Memory(addr) => addr,
            LowLevelValue::Immediate(value) => value,
            LowLevelValue::Label(label) => label,
        }
    }
    
    pub fn type_(&self) -> LowLevelType {
        match self {
            LowLevelValue::Register(name) => {
                // 根据寄存器名称推断类型
                if name.starts_with("rax") || name.starts_with("rbx") || name.starts_with("rcx") || name.starts_with("rdx") {
                    LowLevelType::I64
                } else if name.starts_with("eax") || name.starts_with("ebx") || name.starts_with("ecx") || name.starts_with("edx") {
                    LowLevelType::I32
                } else if name.starts_with("ax") || name.starts_with("bx") || name.starts_with("cx") || name.starts_with("dx") {
                    LowLevelType::I16
                } else if name.starts_with("al") || name.starts_with("bl") || name.starts_with("cl") || name.starts_with("dl") {
                    LowLevelType::I8
                } else {
                    LowLevelType::I32 // 默认类型
                }
            }
            LowLevelValue::Memory(_) => {
                LowLevelType::Memory { addr: "".to_string(), size: 8 }
            }
            LowLevelValue::Immediate(value) => {
                // 根据立即数值推断类型
                if value.contains('.') {
                    LowLevelType::F64
                } else {
                    LowLevelType::I32
                }
            }
            LowLevelValue::Label(_) => {
                LowLevelType::Void
            }
        }
    }
    
    pub fn is_constant(&self) -> bool {
        matches!(self, LowLevelValue::Immediate(_))
    }
}

use super::types::{LowLevelType, CmpPredicate};
use super::values::LowLevelValue;

#[derive(Debug, Clone)]
pub enum LowLevelOperation {
    // 指令操作
    Mov { source: LowLevelValue, destination: LowLevelValue },
    Add { left: LowLevelValue, right: LowLevelValue, result: LowLevelValue },
    Sub { left: LowLevelValue, right: LowLevelValue, result: LowLevelValue },
    Mul { left: LowLevelValue, right: LowLevelValue, result: LowLevelValue },
    Div { left: LowLevelValue, right: LowLevelValue, result: LowLevelValue },
    Mod { left: LowLevelValue, right: LowLevelValue, result: LowLevelValue },
    
    // 比较和跳转
    Cmp { left: LowLevelValue, right: LowLevelValue, predicate: CmpPredicate },
    Jmp { target: LowLevelValue },
    Je { target: LowLevelValue },
    Jne { target: LowLevelValue },
    Jl { target: LowLevelValue },
    Jle { target: LowLevelValue },
    Jg { target: LowLevelValue },
    Jge { target: LowLevelValue },
    
    // 逻辑运算
    And { left: LowLevelValue, right: LowLevelValue, result: LowLevelValue },
    Or { left: LowLevelValue, right: LowLevelValue, result: LowLevelValue },
    Not { operand: LowLevelValue, result: LowLevelValue },
    
    // 位运算
    BitAnd { left: LowLevelValue, right: LowLevelValue, result: LowLevelValue },
    BitOr { left: LowLevelValue, right: LowLevelValue, result: LowLevelValue },
    BitXor { left: LowLevelValue, right: LowLevelValue, result: LowLevelValue },
    BitNot { operand: LowLevelValue, result: LowLevelValue },
    Shl { left: LowLevelValue, right: LowLevelValue, result: LowLevelValue },
    Shr { left: LowLevelValue, right: LowLevelValue, result: LowLevelValue },
    
    // 内存操作
    Load { address: LowLevelValue, result: LowLevelValue },
    Store { value: LowLevelValue, address: LowLevelValue },
    
    // 函数调用
    Call { function: LowLevelValue },
    Ret,
    
    // 栈操作
    Push { value: LowLevelValue },
    Pop { result: LowLevelValue },
    
    // 标签
    Label { name: String },
    
    // 特殊指令
    Nop,
    Hlt,
}

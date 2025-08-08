use super::types::{MidLevelType, CmpPredicate};
use super::values::MidLevelValue;

#[derive(Debug, Clone)]
pub enum MidLevelOperation {
    // 基本块操作
    BasicBlock { name: String, operations: Vec<MidLevelOperation> },
    Branch { target: String },
    CondBranch { condition: MidLevelValue, true_target: String, false_target: String },
    
    // 算术运算
    Add { left: MidLevelValue, right: MidLevelValue, result: MidLevelValue },
    Sub { left: MidLevelValue, right: MidLevelValue, result: MidLevelValue },
    Mul { left: MidLevelValue, right: MidLevelValue, result: MidLevelValue },
    Div { left: MidLevelValue, right: MidLevelValue, result: MidLevelValue },
    Mod { left: MidLevelValue, right: MidLevelValue, result: MidLevelValue },
    
    // 比较运算
    Cmp { left: MidLevelValue, right: MidLevelValue, predicate: CmpPredicate, result: MidLevelValue },
    
    // 逻辑运算
    And { left: MidLevelValue, right: MidLevelValue, result: MidLevelValue },
    Or { left: MidLevelValue, right: MidLevelValue, result: MidLevelValue },
    Not { operand: MidLevelValue, result: MidLevelValue },
    
    // 位运算
    BitAnd { left: MidLevelValue, right: MidLevelValue, result: MidLevelValue },
    BitOr { left: MidLevelValue, right: MidLevelValue, result: MidLevelValue },
    BitXor { left: MidLevelValue, right: MidLevelValue, result: MidLevelValue },
    BitNot { operand: MidLevelValue, result: MidLevelValue },
    Shl { left: MidLevelValue, right: MidLevelValue, result: MidLevelValue },
    Shr { left: MidLevelValue, right: MidLevelValue, result: MidLevelValue },
    
    // 内存操作
    Load { pointer: MidLevelValue, result: MidLevelValue },
    Store { value: MidLevelValue, pointer: MidLevelValue },
    
    // 函数调用
    Call { function: String, arguments: Vec<MidLevelValue>, result: Option<MidLevelValue> },
    Return { value: Option<MidLevelValue> },
    
    // 向量操作
    VectorAdd { left: MidLevelValue, right: MidLevelValue, result: MidLevelValue },
    VectorSub { left: MidLevelValue, right: MidLevelValue, result: MidLevelValue },
    VectorMul { left: MidLevelValue, right: MidLevelValue, result: MidLevelValue },
    VectorDiv { left: MidLevelValue, right: MidLevelValue, result: MidLevelValue },
    VectorLoad { pointer: MidLevelValue, result: MidLevelValue },
    VectorStore { value: MidLevelValue, pointer: MidLevelValue },
    VectorExtract { vector: MidLevelValue, index: usize, result: MidLevelValue },
    VectorInsert { vector: MidLevelValue, value: MidLevelValue, index: usize, result: MidLevelValue },
    
    // 特殊操作
    Phi { inputs: Vec<(MidLevelValue, String)>, result: MidLevelValue },
    Select { condition: MidLevelValue, true_value: MidLevelValue, false_value: MidLevelValue, result: MidLevelValue },
    
    // 类型转换
    Cast { value: MidLevelValue, target_type: MidLevelType, result: MidLevelValue },
    
    // 并行操作
    ParallelFor { start: MidLevelValue, end: MidLevelValue, step: MidLevelValue, body: Vec<MidLevelOperation> },
    Barrier,
    AtomicAdd { pointer: MidLevelValue, value: MidLevelValue, result: MidLevelValue },
    AtomicSub { pointer: MidLevelValue, value: MidLevelValue, result: MidLevelValue },
}

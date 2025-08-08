use super::types::{HighLevelType, CmpPredicate};
use super::values::HighLevelValue;

#[derive(Debug, Clone)]
pub enum HighLevelOperation {
    // 函数操作
    Function { name: String, params: Vec<Parameter>, body: Vec<Box<HighLevelOperation>>, return_type: HighLevelType },
    Call { function: String, arguments: Vec<HighLevelValue>, result: HighLevelValue },
    Return { value: Option<HighLevelValue> },
    
    // 控制流操作
    If { condition: HighLevelValue, then_branch: Vec<Box<HighLevelOperation>>, else_branch: Option<Vec<Box<HighLevelOperation>>> },
    While { condition: HighLevelValue, body: Vec<Box<HighLevelOperation>> },
    For { init: Option<Box<HighLevelOperation>>, condition: Option<HighLevelValue>, update: Option<Box<HighLevelOperation>>, body: Vec<Box<HighLevelOperation>> },
    Break,
    Continue,
    
    // 内存操作
    Alloca { var_type: HighLevelType, result: HighLevelValue },
    Load { pointer: HighLevelValue, result: HighLevelValue },
    Store { value: HighLevelValue, pointer: HighLevelValue },
    
    // 算术运算
    Add { left: HighLevelValue, right: HighLevelValue, result: HighLevelValue },
    Sub { left: HighLevelValue, right: HighLevelValue, result: HighLevelValue },
    Mul { left: HighLevelValue, right: HighLevelValue, result: HighLevelValue },
    Div { left: HighLevelValue, right: HighLevelValue, result: HighLevelValue },
    
    // 比较运算
    Cmp { left: HighLevelValue, right: HighLevelValue, predicate: CmpPredicate, result: HighLevelValue },
    
    // 逻辑运算
    And { left: HighLevelValue, right: HighLevelValue, result: HighLevelValue },
    Or { left: HighLevelValue, right: HighLevelValue, result: HighLevelValue },
    Not { operand: HighLevelValue, result: HighLevelValue },
    
    // 位运算
    BitAnd { left: HighLevelValue, right: HighLevelValue, result: HighLevelValue },
    BitOr { left: HighLevelValue, right: HighLevelValue, result: HighLevelValue },
    BitXor { left: HighLevelValue, right: HighLevelValue, result: HighLevelValue },
    BitNot { operand: HighLevelValue, result: HighLevelValue },
    Shl { left: HighLevelValue, right: HighLevelValue, result: HighLevelValue },
    Shr { left: HighLevelValue, right: HighLevelValue, result: HighLevelValue },
    
    // 复合类型操作
    ArrayAccess { array: HighLevelValue, index: HighLevelValue, result: HighLevelValue },
    StructAccess { struct_ptr: HighLevelValue, field: String, result: HighLevelValue },
    VectorExtract { vector: HighLevelValue, index: usize, result: HighLevelValue },
    VectorInsert { vector: HighLevelValue, value: HighLevelValue, index: usize, result: HighLevelValue },
    
    // 类型转换
    Cast { value: HighLevelValue, target_type: HighLevelType, result: HighLevelValue },
    
    // 特殊操作
    Phi { inputs: Vec<(HighLevelValue, String)>, result: HighLevelValue }, // SSA合并
    Select { condition: HighLevelValue, true_value: HighLevelValue, false_value: HighLevelValue, result: HighLevelValue },
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub param_type: HighLevelType,
}

impl Parameter {
    pub fn new(name: String, param_type: HighLevelType) -> Self {
        Self { name, param_type }
    }
    
    pub fn name(&self) -> &str {
        &self.name
    }
    
    pub fn type_(&self) -> &HighLevelType {
        &self.param_type
    }
}

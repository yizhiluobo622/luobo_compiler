pub mod high_level;
pub mod mid_level;
pub mod low_level;

// 重新导出主要类型
pub use high_level::{HighLevelIR, HighLevelType, HighLevelOperation, HighLevelValue};
pub use mid_level::{MidLevelIR, MidLevelType, MidLevelOperation, MidLevelValue};
pub use low_level::{LowLevelIR, LowLevelType, LowLevelOperation, LowLevelValue};

// 统一IR接口
pub trait IRModule {
    fn functions(&self) -> Vec<&dyn Function>;
    fn add_function(&mut self, function: Box<dyn Function>);
    fn get_type_system(&self) -> &dyn TypeSystem;
    fn get_operation_set(&self) -> &dyn OperationSet;
}

pub trait Function {
    fn name(&self) -> &str;
    fn parameters(&self) -> Vec<&dyn Parameter>;
    fn body(&self) -> Vec<&dyn Operation>;
    fn return_type(&self) -> &dyn Type;
}

pub trait Operation {
    fn name(&self) -> &str;
    fn operands(&self) -> Vec<&dyn Value>;
    fn results(&self) -> Vec<&dyn Value>;
    fn verify(&self) -> Result<(), String>;
}

pub trait TypeSystem {
    fn get_type(&self, name: &str) -> Option<&dyn Type>;
    fn add_type(&mut self, name: String, typ: Box<dyn Type>);
    fn is_compatible(&self, t1: &dyn Type, t2: &dyn Type) -> bool;
}

pub trait OperationSet {
    fn get_operation(&self, name: &str) -> Option<&dyn Operation>;
    fn add_operation(&mut self, name: String, op: Box<dyn Operation>);
    fn list_operations(&self) -> Vec<&str>;
}

pub trait Type {
    fn name(&self) -> &str;
    fn size(&self) -> usize;
    fn is_primitive(&self) -> bool;
}

pub trait Value {
    fn name(&self) -> &str;
    fn type_(&self) -> &dyn Type;
    fn is_constant(&self) -> bool;
}

pub trait Parameter {
    fn name(&self) -> &str;
    fn type_(&self) -> &dyn Type;
}

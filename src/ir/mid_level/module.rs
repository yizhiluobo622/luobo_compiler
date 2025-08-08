use super::operations::MidLevelOperation;
use super::types::MidLevelType;
use super::values::MidLevelValue;

#[derive(Debug, Clone)]
pub struct MidLevelIR {
    pub functions: Vec<MidLevelFunction>,
    pub global_variables: Vec<MidLevelVariable>,
}

#[derive(Debug, Clone)]
pub struct MidLevelFunction {
    pub name: String,
    pub parameters: Vec<MidLevelParameter>,
    pub basic_blocks: Vec<MidLevelBasicBlock>,
    pub return_type: MidLevelType,
}

#[derive(Debug, Clone)]
pub struct MidLevelParameter {
    pub name: String,
    pub param_type: MidLevelType,
}

#[derive(Debug, Clone)]
pub struct MidLevelBasicBlock {
    pub name: String,
    pub operations: Vec<MidLevelOperation>,
}

#[derive(Debug, Clone)]
pub struct MidLevelVariable {
    pub name: String,
    pub var_type: MidLevelType,
    pub initial_value: Option<MidLevelValue>,
}

impl MidLevelIR {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            global_variables: Vec::new(),
        }
    }
    
    pub fn add_function(&mut self, function: MidLevelFunction) {
        self.functions.push(function);
    }
    
    pub fn add_global_variable(&mut self, variable: MidLevelVariable) {
        self.global_variables.push(variable);
    }
    
    pub fn get_function(&self, name: &str) -> Option<&MidLevelFunction> {
        self.functions.iter().find(|f| f.name == name)
    }
    
    pub fn get_global_variable(&self, name: &str) -> Option<&MidLevelVariable> {
        self.global_variables.iter().find(|v| v.name == name)
    }
}

impl MidLevelFunction {
    pub fn new(name: String, parameters: Vec<MidLevelParameter>, basic_blocks: Vec<MidLevelBasicBlock>, return_type: MidLevelType) -> Self {
        Self {
            name,
            parameters,
            basic_blocks,
            return_type,
        }
    }
}

impl MidLevelParameter {
    pub fn new(name: String, param_type: MidLevelType) -> Self {
        Self { name, param_type }
    }
}

impl MidLevelBasicBlock {
    pub fn new(name: String, operations: Vec<MidLevelOperation>) -> Self {
        Self { name, operations }
    }
}

impl MidLevelVariable {
    pub fn new(name: String, var_type: MidLevelType, initial_value: Option<MidLevelValue>) -> Self {
        Self {
            name,
            var_type,
            initial_value,
        }
    }
}

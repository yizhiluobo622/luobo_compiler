use super::operations::LowLevelOperation;
use super::types::LowLevelType;
use super::values::LowLevelValue;

#[derive(Debug, Clone)]
pub struct LowLevelIR {
    pub functions: Vec<LowLevelFunction>,
    pub global_variables: Vec<LowLevelVariable>,
}

#[derive(Debug, Clone)]
pub struct LowLevelFunction {
    pub name: String,
    pub parameters: Vec<LowLevelParameter>,
    pub instructions: Vec<LowLevelOperation>,
    pub return_type: LowLevelType,
}

#[derive(Debug, Clone)]
pub struct LowLevelParameter {
    pub name: String,
    pub param_type: LowLevelType,
}

#[derive(Debug, Clone)]
pub struct LowLevelVariable {
    pub name: String,
    pub var_type: LowLevelType,
    pub initial_value: Option<LowLevelValue>,
}

impl LowLevelIR {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            global_variables: Vec::new(),
        }
    }
    
    pub fn add_function(&mut self, function: LowLevelFunction) {
        self.functions.push(function);
    }
    
    pub fn add_global_variable(&mut self, variable: LowLevelVariable) {
        self.global_variables.push(variable);
    }
    
    pub fn get_function(&self, name: &str) -> Option<&LowLevelFunction> {
        self.functions.iter().find(|f| f.name == name)
    }
    
    pub fn get_global_variable(&self, name: &str) -> Option<&LowLevelVariable> {
        self.global_variables.iter().find(|v| v.name == name)
    }
}

impl LowLevelFunction {
    pub fn new(name: String, parameters: Vec<LowLevelParameter>, instructions: Vec<LowLevelOperation>, return_type: LowLevelType) -> Self {
        Self {
            name,
            parameters,
            instructions,
            return_type,
        }
    }
}

impl LowLevelParameter {
    pub fn new(name: String, param_type: LowLevelType) -> Self {
        Self { name, param_type }
    }
}

impl LowLevelVariable {
    pub fn new(name: String, var_type: LowLevelType, initial_value: Option<LowLevelValue>) -> Self {
        Self {
            name,
            var_type,
            initial_value,
        }
    }
}

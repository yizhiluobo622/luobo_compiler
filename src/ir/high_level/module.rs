use super::operations::{HighLevelOperation, Parameter};
use super::types::HighLevelType;
use super::values::HighLevelValue;

#[derive(Debug, Clone)]
pub struct HighLevelIR {
    pub functions: Vec<HighLevelFunction>,
    pub global_variables: Vec<HighLevelVariable>,
}

#[derive(Debug, Clone)]
pub struct HighLevelFunction {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub body: Vec<HighLevelOperation>,
    pub return_type: HighLevelType,
}

#[derive(Debug, Clone)]
pub struct HighLevelVariable {
    pub name: String,
    pub var_type: HighLevelType,
    pub initial_value: Option<HighLevelValue>,
}

impl HighLevelIR {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            global_variables: Vec::new(),
        }
    }
    
    pub fn add_function(&mut self, function: HighLevelFunction) {
        self.functions.push(function);
    }
    
    pub fn add_global_variable(&mut self, variable: HighLevelVariable) {
        self.global_variables.push(variable);
    }
    
    pub fn get_function(&self, name: &str) -> Option<&HighLevelFunction> {
        self.functions.iter().find(|f| f.name == name)
    }
    
    pub fn get_global_variable(&self, name: &str) -> Option<&HighLevelVariable> {
        self.global_variables.iter().find(|v| v.name == name)
    }
}

impl HighLevelFunction {
    pub fn new(name: String, parameters: Vec<Parameter>, body: Vec<HighLevelOperation>, return_type: HighLevelType) -> Self {
        Self {
            name,
            parameters,
            body,
            return_type,
        }
    }
}

impl HighLevelVariable {
    pub fn new(name: String, var_type: HighLevelType, initial_value: Option<HighLevelValue>) -> Self {
        Self {
            name,
            var_type,
            initial_value,
        }
    }
}

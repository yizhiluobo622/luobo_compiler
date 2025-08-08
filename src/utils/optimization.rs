use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct OptimizationContext {
    pub variables: HashMap<String, VariableInfo>,
    pub constants: HashMap<String, String>,
    pub reachable_blocks: HashSet<String>,
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub name: String,
    pub type_: String,
    pub is_constant: bool,
    pub value: Option<String>,
    pub uses: Vec<String>,
    pub definitions: Vec<String>,
}

impl OptimizationContext {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            constants: HashMap::new(),
            reachable_blocks: HashSet::new(),
        }
    }
    
    pub fn add_variable(&mut self, name: String, type_: String) {
        self.variables.insert(name.clone(), VariableInfo {
            name,
            type_,
            is_constant: false,
            value: None,
            uses: Vec::new(),
            definitions: Vec::new(),
        });
    }
    
    pub fn mark_constant(&mut self, name: &str, value: String) {
        if let Some(var_info) = self.variables.get_mut(name) {
            var_info.is_constant = true;
            var_info.value = Some(value.clone());
        }
        self.constants.insert(name.to_string(), value);
    }
    
    pub fn add_use(&mut self, var_name: &str, location: String) {
        if let Some(var_info) = self.variables.get_mut(var_name) {
            var_info.uses.push(location);
        }
    }
    
    pub fn add_definition(&mut self, var_name: &str, location: String) {
        if let Some(var_info) = self.variables.get_mut(var_name) {
            var_info.definitions.push(location);
        }
    }
    
    pub fn is_dead_code(&self, var_name: &str) -> bool {
        if let Some(var_info) = self.variables.get(var_name) {
            var_info.uses.is_empty() && !var_info.is_constant
        } else {
            false
        }
    }
    
    pub fn get_constant_value(&self, var_name: &str) -> Option<&String> {
        self.constants.get(var_name)
    }
}

pub struct DeadCodeEliminator {
    pub context: OptimizationContext,
}

impl DeadCodeEliminator {
    pub fn new() -> Self {
        Self {
            context: OptimizationContext::new(),
        }
    }
    
    pub fn eliminate_dead_code(&mut self, instructions: &[String]) -> Vec<String> {
        let mut optimized_instructions = Vec::new();
        
        for instruction in instructions {
            if self.is_useful_instruction(instruction) {
                optimized_instructions.push(instruction.clone());
            }
        }
        
        optimized_instructions
    }
    
    fn is_useful_instruction(&self, instruction: &str) -> bool {
        // 简化实现：检查指令是否有副作用或有用输出
        instruction.contains("store") || 
        instruction.contains("call") || 
        instruction.contains("return") ||
        instruction.contains("branch")
    }
}

pub struct ConstantFolder {
    pub context: OptimizationContext,
}

impl ConstantFolder {
    pub fn new() -> Self {
        Self {
            context: OptimizationContext::new(),
        }
    }
    
    pub fn fold_constants(&mut self, instructions: &[String]) -> Vec<String> {
        let mut folded_instructions = Vec::new();
        
        for instruction in instructions {
            if let Some(folded) = self.try_fold_instruction(instruction) {
                folded_instructions.push(folded);
            } else {
                folded_instructions.push(instruction.clone());
            }
        }
        
        folded_instructions
    }
    
    fn try_fold_instruction(&self, instruction: &str) -> Option<String> {
        // 简化实现：尝试折叠常量表达式
        if instruction.contains("add") && instruction.contains("0") {
            // 简化加法优化
            Some(instruction.replace("add", "mov").replace(" + 0", ""))
        } else if instruction.contains("mul") && instruction.contains("1") {
            // 简化乘法优化
            Some(instruction.replace("mul", "mov").replace(" * 1", ""))
        } else {
            None
        }
    }
}

pub struct CopyPropagator {
    pub context: OptimizationContext,
}

impl CopyPropagator {
    pub fn new() -> Self {
        Self {
            context: OptimizationContext::new(),
        }
    }
    
    pub fn propagate_copies(&mut self, instructions: &[String]) -> Vec<String> {
        let mut propagated_instructions = Vec::new();
        
        for instruction in instructions {
            if let Some(propagated) = self.try_propagate_copy(instruction) {
                propagated_instructions.push(propagated);
            } else {
                propagated_instructions.push(instruction.clone());
            }
        }
        
        propagated_instructions
    }
    
    fn try_propagate_copy(&self, instruction: &str) -> Option<String> {
        // 简化实现：尝试传播复制操作
        if instruction.contains("mov") {
            // 这里应该实现复制传播逻辑
            Some(instruction.to_string())
        } else {
            None
        }
    }
}

use std::collections::{HashMap, HashSet};
use crate::utils::cfg::{ControlFlowGraph, BasicBlock};

#[derive(Debug, Clone)]
pub struct SSAConverter {
    pub variable_counter: HashMap<String, usize>,
    pub variable_stack: HashMap<String, Vec<String>>,
}

impl SSAConverter {
    pub fn new() -> Self {
        Self {
            variable_counter: HashMap::new(),
            variable_stack: HashMap::new(),
        }
    }
    
    pub fn convert_to_ssa(&mut self, cfg: &ControlFlowGraph) -> ControlFlowGraph {
        let mut ssa_cfg = cfg.clone();
        
        // 1. 计算支配者
        let dominators = cfg.find_dominators();
        
        // 2. 计算支配边界
        let dominance_frontiers = self.compute_dominance_frontiers(cfg, &dominators);
        
        // 3. 插入Phi节点
        self.insert_phi_nodes(&mut ssa_cfg, &dominance_frontiers);
        
        // 4. 重命名变量
        self.rename_variables(&mut ssa_cfg);
        
        ssa_cfg
    }
    
    fn compute_dominance_frontiers(&self, cfg: &ControlFlowGraph, dominators: &HashMap<String, HashSet<String>>) -> HashMap<String, HashSet<String>> {
        let mut frontiers = HashMap::new();
        
        for block_name in cfg.blocks.keys() {
            frontiers.insert(block_name.clone(), HashSet::new());
        }
        
        for block_name in cfg.blocks.keys() {
            if let Some(block) = cfg.blocks.get(block_name) {
                if block.predecessors.len() > 1 {
                    for pred in &block.predecessors {
                        let mut runner = pred.clone();
                        while runner != *block_name {
                            if let Some(frontier) = frontiers.get_mut(&runner) {
                                frontier.insert(block_name.clone());
                            }
                            if let Some(pred_dom) = dominators.get(&runner) {
                                if let Some(block_dom) = dominators.get(block_name) {
                                    if pred_dom.contains(block_name) {
                                        break;
                                    }
                                }
                            }
                            // 找到下一个支配者
                            if let Some(block) = cfg.blocks.get(&runner) {
                                if let Some(first_pred) = block.predecessors.first() {
                                    runner = first_pred.clone();
                                } else {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }
                    }
                }
            }
        }
        
        frontiers
    }
    
    fn insert_phi_nodes(&mut self, cfg: &mut ControlFlowGraph, dominance_frontiers: &HashMap<String, HashSet<String>>) {
        // 简化实现：为每个变量在每个支配边界插入Phi节点
        for (block_name, frontiers) in dominance_frontiers {
            for frontier in frontiers {
                // 这里应该为每个变量插入Phi节点
                // 简化实现，实际需要更复杂的逻辑
                println!("Inserting Phi node at {} for block {}", frontier, block_name);
            }
        }
    }
    
    fn rename_variables(&mut self, cfg: &mut ControlFlowGraph) {
        // 简化实现：重命名变量以确保SSA形式
        for block in cfg.blocks.values_mut() {
            for instruction in &mut block.instructions {
                // 这里应该重命名指令中的变量
                // 简化实现，实际需要更复杂的逻辑
                println!("Renaming variables in instruction: {}", instruction);
            }
        }
    }
    
    pub fn get_new_variable_name(&mut self, base_name: &str) -> String {
        let counter = self.variable_counter.entry(base_name.to_string()).or_insert(0);
        *counter += 1;
        format!("{}_{}", base_name, counter)
    }
    
    pub fn push_variable(&mut self, var_name: &str, ssa_name: &str) {
        self.variable_stack.entry(var_name.to_string())
            .or_insert_with(Vec::new)
            .push(ssa_name.to_string());
    }
    
    pub fn pop_variable(&mut self, var_name: &str) -> Option<String> {
        self.variable_stack.get_mut(var_name)?.pop()
    }
    
    pub fn get_current_variable(&self, var_name: &str) -> Option<&String> {
        self.variable_stack.get(var_name)?.last()
    }
}

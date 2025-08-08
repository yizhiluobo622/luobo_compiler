use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub name: String,
    pub instructions: Vec<String>,
    pub predecessors: Vec<String>,
    pub successors: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    pub blocks: HashMap<String, BasicBlock>,
    pub entry_block: String,
    pub exit_blocks: HashSet<String>,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        Self {
            blocks: HashMap::new(),
            entry_block: String::new(),
            exit_blocks: HashSet::new(),
        }
    }
    
    pub fn add_block(&mut self, block: BasicBlock) {
        let name = block.name.clone();
        self.blocks.insert(name, block);
    }
    
    pub fn add_edge(&mut self, from: &str, to: &str) {
        if let Some(from_block) = self.blocks.get_mut(from) {
            from_block.successors.push(to.to_string());
        }
        if let Some(to_block) = self.blocks.get_mut(to) {
            to_block.predecessors.push(from.to_string());
        }
    }
    
    pub fn get_block(&self, name: &str) -> Option<&BasicBlock> {
        self.blocks.get(name)
    }
    
    pub fn get_successors(&self, block_name: &str) -> Vec<&BasicBlock> {
        if let Some(block) = self.blocks.get(block_name) {
            block.successors.iter()
                .filter_map(|name| self.blocks.get(name))
                .collect()
        } else {
            Vec::new()
        }
    }
    
    pub fn get_predecessors(&self, block_name: &str) -> Vec<&BasicBlock> {
        if let Some(block) = self.blocks.get(block_name) {
            block.predecessors.iter()
                .filter_map(|name| self.blocks.get(name))
                .collect()
        } else {
            Vec::new()
        }
    }
    
    pub fn find_dominators(&self) -> HashMap<String, HashSet<String>> {
        let mut dominators = HashMap::new();
        let block_names: Vec<String> = self.blocks.keys().cloned().collect();
        
        // 初始化支配者集合
        for name in &block_names {
            let mut dom_set = HashSet::new();
            if name == &self.entry_block {
                dom_set.insert(name.clone());
            } else {
                dom_set.extend(block_names.iter().cloned());
            }
            dominators.insert(name.clone(), dom_set);
        }
        
        // 迭代计算支配者
        let mut changed = true;
        while changed {
            changed = false;
            for name in &block_names {
                if name == &self.entry_block {
                    continue;
                }
                
                let mut new_dom = HashSet::new();
                new_dom.insert(name.clone());
                
                if let Some(block) = self.blocks.get(name) {
                    if let Some(first_pred) = block.predecessors.first() {
                        if let Some(first_pred_dom) = dominators.get(first_pred) {
                            new_dom.extend(first_pred_dom.iter().cloned());
                        }
                        
                        for pred in &block.predecessors[1..] {
                            if let Some(pred_dom) = dominators.get(pred) {
                                new_dom = new_dom.intersection(pred_dom).cloned().collect();
                            }
                        }
                    }
                }
                
                if let Some(current_dom) = dominators.get(name) {
                    if current_dom != &new_dom {
                        dominators.insert(name.clone(), new_dom);
                        changed = true;
                    }
                }
            }
        }
        
        dominators
    }
    
    pub fn find_loops(&self) -> Vec<Loop> {
        let mut loops = Vec::new();
        let mut visited = HashSet::new();
        
        for block_name in self.blocks.keys() {
            if !visited.contains(block_name) {
                if let Some(loop_info) = self.find_loop_at(block_name, &mut visited) {
                    loops.push(loop_info);
                }
            }
        }
        
        loops
    }
    
    fn find_loop_at(&self, start: &str, visited: &mut HashSet<String>) -> Option<Loop> {
        let mut stack = vec![start.to_string()];
        let mut loop_blocks = HashSet::new();
        
        while let Some(current) = stack.pop() {
            if visited.contains(&current) {
                continue;
            }
            visited.insert(current.clone());
            loop_blocks.insert(current.clone());
            
            if let Some(block) = self.blocks.get(&current) {
                for successor in &block.successors {
                    if !visited.contains(successor) {
                        stack.push(successor.clone());
                    } else if successor == start {
                        // 找到循环
                        return Some(Loop {
                            header: start.to_string(),
                            body: loop_blocks,
                        });
                    }
                }
            }
        }
        
        None
    }
}

#[derive(Debug, Clone)]
pub struct Loop {
    pub header: String,
    pub body: HashSet<String>,
}

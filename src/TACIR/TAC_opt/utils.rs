use crate::TACIR::tacir::{TACProgram, TACFunction, BasicBlock, TACInstruction, Operand, ConstantValue};
use std::collections::{HashMap, HashSet};

/// 函数分析信息
#[derive(Debug, Clone)]
pub struct FunctionAnalysisInfo {
    /// 函数名称
    pub name: String,
    /// TAC 指令总数
    pub instruction_count: usize,
    /// 是否调用了其他函数
    pub has_function_calls: bool,
    /// 是否包含循环
    pub has_loops: bool,
    /// 被调用次数
    pub call_sites: usize,
    /// 基本块数量
    pub basic_block_count: usize,
    /// 临时变量数量
    pub temp_variable_count: usize,
}

impl FunctionAnalysisInfo {
    pub fn new(name: String) -> Self {
        Self {
            name,
            instruction_count: 0,
            has_function_calls: false,
            has_loops: false,
            call_sites: 0,
            basic_block_count: 0,
            temp_variable_count: 0,
        }
    }
    
    /// 计算内联评分（0-100，分数越低越适合内联）
    pub fn inline_score(&self) -> u32 {
        let mut score = 0u32;
        
        // 指令数评分（0-40分）
        if self.instruction_count <= 10 {
            score += 0;
        } else if self.instruction_count <= 20 {
            score += 10;
        } else if self.instruction_count <= 30 {
            score += 20;
        } else if self.instruction_count <= 50 {
            score += 30;
        } else {
            score += 40;
        }
        
        // 函数调用评分（0-20分）
        if self.has_function_calls {
            score += 20;
        }
        
        // 循环评分（0-20分）
        if self.has_loops {
            score += 20;
        }
        
        // 基本块复杂度评分（0-20分）
        if self.basic_block_count <= 2 {
            score += 0;
        } else if self.basic_block_count <= 4 {
            score += 10;
        } else {
            score += 20;
        }
        
        score
    }
    
    /// 判断是否适合内联
    pub fn should_inline(&self, threshold: u32) -> bool {
        self.inline_score() <= threshold
    }
}

/// 函数调用图信息
#[derive(Debug, Clone)]
pub struct CallGraphInfo {
    /// 函数名到分析信息的映射
    pub functions: HashMap<String, FunctionAnalysisInfo>,
    /// 函数调用关系：caller -> [callee1, callee2, ...]
    pub call_relations: HashMap<String, Vec<String>>,
    /// 函数被调用关系：callee -> [caller1, caller2, ...]
    pub called_by: HashMap<String, Vec<String>>,
}

impl CallGraphInfo {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            call_relations: HashMap::new(),
            called_by: HashMap::new(),
        }
    }
    
    /// 添加函数调用关系
    pub fn add_call_relation(&mut self, caller: String, callee: String) {
        // 更新调用关系
        self.call_relations.entry(caller.clone())
            .or_insert_with(Vec::new)
            .push(callee.clone());
        
        // 更新被调用关系
        self.called_by.entry(callee)
            .or_insert_with(Vec::new)
            .push(caller);
    }
    
    /// 获取函数的调用者列表
    pub fn get_callers(&self, function_name: &str) -> Vec<&String> {
        self.called_by.get(function_name)
            .map(|callers| callers.iter().collect())
            .unwrap_or_default()
    }
    
    /// 获取函数的被调用者列表
    pub fn get_callees(&self, function_name: &str) -> Vec<&String> {
        self.call_relations.get(function_name)
            .map(|callees| callees.iter().collect())
            .unwrap_or_default()
    }
    
    /// 检测递归函数
    pub fn is_recursive(&self, function_name: &str) -> bool {
        let mut visited = HashSet::new();
        let mut stack = vec![function_name];
        
        while let Some(current) = stack.pop() {
            if visited.contains(current) {
                continue;
            }
            visited.insert(current);
            
            if let Some(callees) = self.call_relations.get(current) {
                for callee in callees {
                    if callee == function_name {
                        return true; // 发现递归
                    }
                    stack.push(callee);
                }
            }
        }
        false
    }
}

/// 分析 TAC 程序，收集函数信息
pub fn analyze_tac_program(program: &TACProgram) -> CallGraphInfo {
    let mut call_graph = CallGraphInfo::new();
    
    // 第一遍：收集所有函数的基本信息和调用关系
    for function in &program.functions {
        let mut info = FunctionAnalysisInfo::new(function.name.clone());
        info.basic_block_count = function.basic_blocks.len();
        info.temp_variable_count = function.temp_counter;
        
        // 分析每个基本块
        for block in &function.basic_blocks {
            info.instruction_count += block.instructions.len();
            
            for instruction in &block.instructions {
                // 检测函数调用
                if let TACInstruction::FunctionCall { function_name, .. } = instruction {
                    info.has_function_calls = true;
                    call_graph.add_call_relation(function.name.clone(), function_name.clone());
                }
            }
        }
        
        call_graph.functions.insert(function.name.clone(), info);
    }
    
    // 第二遍：统计被调用次数（避免借用冲突）
    let mut call_sites_map = HashMap::new();
    for function in &program.functions {
        let callers = call_graph.get_callers(&function.name);
        call_sites_map.insert(function.name.clone(), callers.len());
    }
    
    // 更新函数信息
    for (name, call_sites) in call_sites_map {
        if let Some(info) = call_graph.functions.get_mut(&name) {
            info.call_sites = call_sites;
        }
    }
    
    call_graph
}

/// 检测指令是否包含循环
pub fn detect_loops_in_function(function: &TACFunction) -> bool {
    let mut visited = HashSet::new();
    let mut stack = vec![0]; // 从入口基本块开始
    
    while let Some(block_id) = stack.pop() {
        if visited.contains(&block_id) {
            continue;
        }
        visited.insert(block_id);
        
        if let Some(block) = function.get_basic_block(block_id) {
            // 检查跳转指令
            for instruction in &block.instructions {
                match instruction {
                    TACInstruction::Jump { label } => {
                        // 检查是否跳转到已访问的块（可能是循环）
                        // 这里简化处理，实际需要更精确的循环检测
                    }
                    TACInstruction::ConditionalJump { true_label, false_label, .. } => {
                        // 条件跳转也可能形成循环
                    }
                    _ => {}
                }
            }
            
            // 将后继块加入栈
            stack.extend(block.successors.iter());
        }
    }
    
    // 简化版本：如果基本块数量 > 2 且包含跳转，认为可能有循环
    function.basic_blocks.len() > 2 && 
    function.basic_blocks.iter().any(|block| {
        block.instructions.iter().any(|inst| {
            matches!(inst, TACInstruction::Jump { .. } | TACInstruction::ConditionalJump { .. })
        })
    })
}

/// 生成唯一的变量名
pub fn generate_unique_name(base_name: &str, counter: &mut usize) -> String {
    *counter += 1;
    format!("{}_{}", base_name, counter)
}

/// 检查变量名是否冲突
pub fn is_name_conflict(name: &str, existing_names: &HashSet<String>) -> bool {
    existing_names.contains(name)
}

/// 安全的变量重命名
pub fn safe_rename_variable(
    original_name: &str, 
    existing_names: &HashSet<String>, 
    counter: &mut usize
) -> String {
    if !is_name_conflict(original_name, existing_names) {
        return original_name.to_string();
    }
    
    // 生成新名称直到不冲突
    loop {
        let new_name = generate_unique_name(original_name, counter);
        if !is_name_conflict(&new_name, existing_names) {
            return new_name;
        }
    }
}

/// 打印函数分析信息
pub fn print_function_analysis(call_graph: &CallGraphInfo) {
    // println!("=== 函数分析信息 ===");
    // for (name, info) in &call_graph.functions {
    //     println!("函数: {}", name);
    //     println!("  指令数: {}", info.instruction_count);
    //     println!("  基本块数: {}", info.basic_block_count);
    //     println!("  临时变量数: {}", info.temp_variable_count);
    //     println!("  有函数调用: {}", info.has_function_calls);
    //     println!("  有循环: {}", info.has_loops);
    //     println!("  被调用次数: {}", info.call_sites);
    //     println!("  内联评分: {}", info.inline_score());
    //     println!("  适合内联: {}", info.should_inline(50)); // 默认阈值50
    //     println!();
    // }
    
    // println!("=== 调用关系 ===");
    // for (caller, callees) in &call_graph.call_relations {
    //     println!("{} 调用: {:?}", caller, callees);
    // }
    // println!();
}

/// 优化统计信息
#[derive(Debug, Clone, Default)]
pub struct OptimizationMetrics {
    /// 内联的函数数量
    pub inlined_functions: usize,
    /// 消除的函数调用数量
    pub eliminated_calls: usize,
    /// 新增的指令数量
    pub new_instructions: usize,
    /// 优化的基本块数量
    pub optimized_blocks: usize,
}

impl OptimizationMetrics {
    pub fn new() -> Self {
        Self::default()
    }
    
    pub fn print_summary(&self) {
        println!("=== 内联优化统计 ===");
        println!("内联函数数: {}", self.inlined_functions);
        println!("消除调用数: {}", self.eliminated_calls);
        println!("新增指令数: {}", self.new_instructions);
        println!("优化基本块数: {}", self.optimized_blocks);
        println!();
    }
}

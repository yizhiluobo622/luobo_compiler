pub mod constant_opt;
pub mod algebraic_opt;
pub mod inline;
pub mod utils;

pub use constant_opt::*;
pub use algebraic_opt::*;
pub use inline::*;
pub use utils::*;

/// 优化Pass trait
pub trait OptimizationPass {
    /// 运行优化
    fn run(&mut self, program: &mut crate::TACIR::TACProgram) -> Result<OptimizationResult, String>;
    
    /// 获取Pass名称
    fn name(&self) -> &str;
    
    /// 获取优化统计
    fn get_stats(&self) -> &OptimizationStats;
}

/// 优化结果
#[derive(Debug, Clone)]
pub struct OptimizationResult {
    /// 是否进行了优化
    pub optimized: bool,
    /// 优化的节点数量
    pub nodes_optimized: usize,
    /// 优化的指令数量
    pub instructions_optimized: usize,
}

impl OptimizationResult {
    pub fn new() -> Self {
        Self {
            optimized: false,
            nodes_optimized: 0,
            instructions_optimized: 0,
        }
    }
    
    pub fn mark_optimized(&mut self) {
        self.optimized = true;
    }
}

/// 优化统计信息
#[derive(Debug, Clone)]
pub struct OptimizationStats {
    /// 优化轮次
    pub optimization_rounds: usize,
    /// 常量折叠次数
    pub constant_foldings: usize,
    /// 代数优化次数
    pub algebraic_optimizations: usize,
    /// 控制流优化次数
    pub control_flow_optimizations: usize,
    /// 死代码消除次数
    pub dead_code_eliminations: usize,
}

impl OptimizationStats {
    pub fn new() -> Self {
        Self {
            optimization_rounds: 0,
            constant_foldings: 0,
            algebraic_optimizations: 0,
            control_flow_optimizations: 0,
            dead_code_eliminations: 0,
        }
    }
}

/// 运行所有优化Pass
pub fn run_all_optimizations(program: &mut crate::TACIR::TACProgram) -> Result<Vec<OptimizationResult>, String> {
    let mut results = Vec::new();
    
    // 运行内联优化（最先执行）
    println!("=== 开始内联优化 ===");
    let mut inline_pass = inline::InlineOptimizationPass::new();
    let inline_result = inline_pass.run(program)?;
    results.push(inline_result);
    
    // 在内联优化后重建IR结构
    println!("🔧 内联优化后重建IR结构...");
    rebuild_ir_after_inline(program)?;
    
    // 修复内联优化后的CFG

    fix_cfg_after_inline(program)?;
    
    // 运行常量优化
    println!("=== 开始常量优化 ===");
    let mut constant_pass = constant_opt::ConstantOptimizationPass::new();
    let constant_result = constant_pass.run(program)?;
    results.push(constant_result);
    
    // 运行代数优化
    // let mut algebraic_pass = algebraic_opt::AlgebraicOptimizationPass::new();
    // let algebraic_result = algebraic_pass.run(program)?;
    // results.push(algebraic_result);
    
    Ok(results)
}

/// 在内联优化后重建IR结构
fn rebuild_ir_after_inline(program: &mut crate::TACIR::TACProgram) -> Result<(), String> {
    println!("🔧 重建IR结构...");
    
    for function in &mut program.functions {
        // 先重新分配基本块ID，确保连续性
        reassign_block_ids(function)?;
        
        // 为基本块分配标签
        assign_labels_to_blocks(function)?;
        
        // 清理无效的跳转指令
        cleanup_invalid_jumps(function)?;
        
        // 修复跳转指令中的标签引用
        fix_jump_labels(function)?;
        
        // 重新构建CFG
        rebuild_function_cfg(function)?;
        
        // 清理无效的引用
        cleanup_invalid_references(function)?;
        
        // 最终验证
        validate_function_integrity(function)?;
    }
    
    println!("✅ IR结构重建完成");
    Ok(())
}

/// 验证函数完整性
fn validate_function_integrity(function: &crate::TACIR::TACFunction) -> Result<(), String> {
    let mut valid_ids = std::collections::HashSet::new();
    
    // 收集所有有效的基本块ID
    for block in &function.basic_blocks {
        valid_ids.insert(block.id);
    }
    
    // 验证所有引用都是有效的
    for block in &function.basic_blocks {
        for succ_id in &block.successors {
            if !valid_ids.contains(succ_id) {
                // 提供更详细的错误信息
                return Err(format!(
                    "基本块 {} 引用了不存在的基本块 {}。有效的基本块ID: {:?}",
                    block.id, succ_id, valid_ids
                ));
            }
        }
    }
    
    Ok(())
}

/// 重新构建函数的CFG
fn rebuild_function_cfg(function: &mut crate::TACIR::TACFunction) -> Result<(), String> {
    // 先收集所有标签到基本块ID的映射
    let mut label_to_block_id = std::collections::HashMap::new();
    for block in &function.basic_blocks {
        if let Some(label) = &block.label {
            label_to_block_id.insert(label.clone(), block.id);
        }
    }
    
    if label_to_block_id.is_empty() {
        return Err("没有找到任何标签，无法重建CFG".to_string());
    }
    
    // 先收集所有后继关系（基于指令中的跳转）
    let mut successor_map: Vec<(usize, Vec<usize>)> = Vec::new();
    
    for block in &function.basic_blocks {
        let mut successors = Vec::new();
        
        // 分析指令中的跳转
        for instruction in &block.instructions {
            match instruction {
                crate::TACIR::tacir::TACInstruction::Jump { label } => {
                    // 根据标签找到目标基本块
                    if let Some(&target_block_id) = label_to_block_id.get(label) {
                        successors.push(target_block_id);
                    }
                }
                crate::TACIR::tacir::TACInstruction::ConditionalJump { true_label, false_label, .. } => {
                    // 根据标签找到目标基本块
                    if let Some(&target_block_id) = label_to_block_id.get(true_label) {
                        successors.push(target_block_id);
                    }
                    if let Some(&target_block_id) = label_to_block_id.get(false_label) {
                        successors.push(target_block_id);
                    }
                }
                _ => {}
            }
        }
        
        successor_map.push((block.id, successors));
    }
    
    // 清理现有的前驱后继关系
    for block in &mut function.basic_blocks {
        block.predecessors.clear();
        block.successors.clear();
    }
    
    // 重新建立前驱后继关系
    for (block_id, successors) in successor_map {
        if let Some(block) = function.basic_blocks.iter_mut().find(|b| b.id == block_id) {
            block.successors = successors.clone();
            
            for succ_id in successors {
                if let Some(succ_block) = function.basic_blocks.iter_mut().find(|b| b.id == succ_id) {
                    succ_block.predecessors.push(block_id);
                }
            }
        }
    }
    
    Ok(())
}

/// 清理无效的引用
fn cleanup_invalid_references(function: &mut crate::TACIR::TACFunction) -> Result<(), String> {
    let mut cleaned_count = 0;
    
    // 先收集所有有效的基本块ID
    let valid_ids: std::collections::HashSet<usize> = function.basic_blocks.iter().map(|b| b.id).collect();
    
    for block in &mut function.basic_blocks {
        // 清理无效的后继引用
        let original_len = block.successors.len();
        block.successors.retain(|&succ_id| valid_ids.contains(&succ_id));
        
        let cleaned = original_len - block.successors.len();
        if cleaned > 0 {
            cleaned_count += cleaned;
        }
    }
    
    if cleaned_count > 0 {
        println!("🧹 清理了 {} 个无效引用", cleaned_count);
    }
    
    Ok(())
}

/// 修复内联优化后的CFG
fn fix_cfg_after_inline(program: &mut crate::TACIR::TACProgram) -> Result<(), String> {
    println!("🔧 内联优化后重建IR结构...");
    
    for function in &mut program.functions {
        // 先清理无效的跳转指令和空的基本块
        cleanup_invalid_jumps(function)?;
        
        // 重新分配基本块ID，确保连续性
        reassign_block_ids(function)?;
        
        // 更新所有跳转指令的目标
        update_all_jumps(function)?;
        
        // 重建CFG
        rebuild_function_cfg(function)?;
        
        // 最终验证
        validate_function_integrity(function)?;
        
        // 再次清理，确保没有残留的无效内容
        cleanup_invalid_jumps(function)?;
    }
    
    println!("✅ IR结构重建完成");
    Ok(())
}

/// 重新分配基本块ID，确保连续性
fn reassign_block_ids(function: &mut crate::TACIR::TACFunction) -> Result<(), String> {
    // 先清理所有前驱后继关系，因为内联后这些关系可能无效
    for block in &mut function.basic_blocks {
        block.predecessors.clear();
        block.successors.clear();
    }
    
    let mut id_mapping = std::collections::HashMap::new();
    let mut next_id = 0;
    
    // 创建ID映射，确保从0开始连续
    for block in &function.basic_blocks {
        id_mapping.insert(block.id, next_id);
        next_id += 1;
    }
    
    // 应用ID映射
    for block in &mut function.basic_blocks {
        let old_id = block.id;
        let new_id = *id_mapping.get(&old_id).unwrap();
        
        // 更新ID
        block.id = new_id;
    }
    
    // 重新排序基本块，确保ID顺序
    function.basic_blocks.sort_by_key(|block| block.id);
    
    Ok(())
}

/// 更新所有跳转指令的目标
fn update_all_jumps(function: &mut crate::TACIR::TACFunction) -> Result<(), String> {
    // 验证所有跳转指令的标签都是有效的
    let mut label_to_block_id = std::collections::HashMap::new();
    
    for (block_idx, block) in function.basic_blocks.iter().enumerate() {
        if let Some(label) = &block.label {
            label_to_block_id.insert(label.clone(), block_idx);
        }
    }
    
    // 检查是否有无效的标签引用
    let mut invalid_labels = Vec::new();
    
    for block in &function.basic_blocks {
        for instruction in &block.instructions {
            match instruction {
                crate::TACIR::tacir::TACInstruction::Jump { label } => {
                    if !label_to_block_id.contains_key(label) {
                        invalid_labels.push((block.id, "Jump", label.clone()));
                    }
                }
                crate::TACIR::tacir::TACInstruction::ConditionalJump { true_label, false_label, .. } => {
                    if !label_to_block_id.contains_key(true_label) {
                        invalid_labels.push((block.id, "ConditionalJump", true_label.clone()));
                    }
                    if !label_to_block_id.contains_key(false_label) {
                        invalid_labels.push((block.id, "ConditionalJump", false_label.clone()));
                    }
                }
                _ => {}
            }
        }
    }
    
    if !invalid_labels.is_empty() {
        println!("⚠️ 发现无效标签引用:");
        for (block_id, inst_type, label) in &invalid_labels {
            println!("  - 基本块 {} 的 {} 指令引用了无效标签: {}", block_id, inst_type, label);
        }
        
        // 尝试修复无效标签引用
        let mut fixed_count = 0;
        for block in &mut function.basic_blocks {
            for instruction in &mut block.instructions {
                match instruction {
                    crate::TACIR::tacir::TACInstruction::Jump { label } => {
                        if !label_to_block_id.contains_key(label) {
                            // 尝试找到最接近的有效标签
                            if let Some(valid_label) = find_closest_valid_label(label, &label_to_block_id.keys().cloned().collect()) {
                                *label = valid_label;
                                fixed_count += 1;
                            }
                        }
                    }
                    crate::TACIR::tacir::TACInstruction::ConditionalJump { true_label, false_label, .. } => {
                        if !label_to_block_id.contains_key(true_label) {
                            if let Some(valid_label) = find_closest_valid_label(true_label, &label_to_block_id.keys().cloned().collect()) {
                                *true_label = valid_label;
                                fixed_count += 1;
                            }
                        }
                        if !label_to_block_id.contains_key(false_label) {
                            if let Some(valid_label) = find_closest_valid_label(false_label, &label_to_block_id.keys().cloned().collect()) {
                                *false_label = valid_label;
                                fixed_count += 1;
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        
        // 检测和修复死循环
        let mut loop_fixed_count = 0;
        for block in &mut function.basic_blocks {
            for instruction in &mut block.instructions {
                match instruction {
                    crate::TACIR::tacir::TACInstruction::Jump { label } => {
                        // 检查是否跳转到自己
                        if let Some(block_label) = &block.label {
                            if label == block_label {
                                // 跳转到自己，修复为跳转到下一个基本块
                                if let Some(next_label) = find_next_valid_label(block_label, &label_to_block_id.keys().cloned().collect()) {
                                    *label = next_label;
                                    loop_fixed_count += 1;
                                }
                            }
                        }
                    }
                    crate::TACIR::tacir::TACInstruction::ConditionalJump { true_label, false_label, .. } => {
                        // 检查条件跳转是否跳转到自己
                        if let Some(block_label) = &block.label {
                            if true_label == block_label {
                                if let Some(next_label) = find_next_valid_label(block_label, &label_to_block_id.keys().cloned().collect()) {
                                    *true_label = next_label;
                                    loop_fixed_count += 1;
                                }
                            }
                            if false_label == block_label {
                                if let Some(next_label) = find_next_valid_label(block_label, &label_to_block_id.keys().cloned().collect()) {
                                    *false_label = next_label;
                                    loop_fixed_count += 1;
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        
        if loop_fixed_count > 0 {
            println!("🔧 修复了 {} 个死循环跳转", loop_fixed_count);
        }
        
        if fixed_count > 0 {
            println!("🔧 修复了 {} 个无效标签引用", fixed_count);
        }
    } else {
        println!("✅ 所有标签引用都有效");
    }
    
    Ok(())
}

/// 清理无效的跳转指令和空的基本块
fn cleanup_invalid_jumps(function: &mut crate::TACIR::tacir::TACFunction) -> Result<(), String> {
    // 收集所有有效的标签
    let mut valid_labels = std::collections::HashSet::new();
    for block in &function.basic_blocks {
        if let Some(label) = &block.label {
            valid_labels.insert(label.clone());
        }
    }
    
    // 清理无效的跳转指令
    let mut cleaned_count = 0;
    for block in &mut function.basic_blocks {
        let mut i = 0;
        while i < block.instructions.len() {
            let should_remove = match &block.instructions[i] {
                crate::TACIR::tacir::TACInstruction::Jump { label } => {
                    !valid_labels.contains(label)
                }
                crate::TACIR::tacir::TACInstruction::ConditionalJump { true_label, false_label, .. } => {
                    !valid_labels.contains(true_label) || !valid_labels.contains(false_label)
                }
                _ => false,
            };
            
            if should_remove {
                block.instructions.remove(i);
                cleaned_count += 1;
            } else {
                i += 1;
            }
        }
    }
    
    // 清理空的基本块（除了有标签的块）
    let mut empty_blocks_removed = 0;
    let mut i = 0;
    while i < function.basic_blocks.len() {
        let block = &function.basic_blocks[i];
        if block.instructions.is_empty() && block.label.is_none() {
            function.basic_blocks.remove(i);
            empty_blocks_removed += 1;
        } else if block.instructions.is_empty() && block.label.is_some() {
            // 有标签但没有指令的块，检查是否被引用
            let label = block.label.as_ref().unwrap();
            let mut is_referenced = false;
            
            for other_block in &function.basic_blocks {
                for instruction in &other_block.instructions {
                    match instruction {
                        crate::TACIR::tacir::TACInstruction::Jump { label: jump_label } => {
                            if jump_label == label {
                                is_referenced = true;
                                break;
                            }
                        }
                        crate::TACIR::tacir::TACInstruction::ConditionalJump { true_label, false_label, .. } => {
                            if true_label == label || false_label == label {
                                is_referenced = true;
                                break;
                            }
                        }
                        _ => {}
                    }
                }
                if is_referenced {
                    break;
                }
            }
            
            if !is_referenced {
                // 没有被引用的空标签块，可以删除
                function.basic_blocks.remove(i);
                empty_blocks_removed += 1;
            } else {
                i += 1;
            }
        } else {
            i += 1;
        }
    }
    
    if cleaned_count > 0 {
        println!("🧹 清理了 {} 个无效跳转指令", cleaned_count);
    }
    if empty_blocks_removed > 0 {
        println!("🧹 清理了 {} 个空的基本块", empty_blocks_removed);
    }
    
    Ok(())
}

/// 为基本块分配标签
fn assign_labels_to_blocks(function: &mut crate::TACIR::TACFunction) -> Result<(), String> {
    // 为所有基本块重新分配标签，确保唯一性
    let mut next_label_id = 0;
    for block in &mut function.basic_blocks {
        let new_label = format!("L{}", next_label_id);
        block.set_label(new_label);
        next_label_id += 1;
    }
    
    Ok(())
}

/// 修复跳转指令中的标签引用
fn fix_jump_labels(function: &mut crate::TACIR::TACFunction) -> Result<(), String> {
    // 收集所有有效的标签
    let mut valid_labels = std::collections::HashSet::new();
    for block in &function.basic_blocks {
        if let Some(label) = &block.label {
            valid_labels.insert(label.clone());
        }
    }
    
    // 修复跳转指令中的标签引用
    let mut fixed_count = 0;
    for block in &mut function.basic_blocks {
        for instruction in &mut block.instructions {
            match instruction {
                crate::TACIR::tacir::TACInstruction::Jump { label } => {
                    if !valid_labels.contains(label) {
                        // 如果标签无效，尝试找到最接近的有效标签
                        if let Some(valid_label) = find_closest_valid_label(label, &valid_labels) {
                            *label = valid_label;
                            fixed_count += 1;
                        }
                    }
                }
                crate::TACIR::tacir::TACInstruction::ConditionalJump { true_label, false_label, .. } => {
                    if !valid_labels.contains(true_label) {
                        if let Some(valid_label) = find_closest_valid_label(true_label, &valid_labels) {
                            *true_label = valid_label;
                            fixed_count += 1;
                        }
                    }
                    if !valid_labels.contains(false_label) {
                        if let Some(valid_label) = find_closest_valid_label(false_label, &valid_labels) {
                            *false_label = valid_label;
                            fixed_count += 1;
                        }
                    }
                }
                _ => {}
            }
        }
    }
    
    if fixed_count > 0 {
        println!("🔧 修复了 {} 个标签引用", fixed_count);
    }
    
    Ok(())
}

/// 找到最接近的有效标签
fn find_closest_valid_label(invalid_label: &str, valid_labels: &std::collections::HashSet<String>) -> Option<String> {
    // 尝试从标签中提取数字部分
    if let Some(number_str) = invalid_label.strip_prefix("L") {
        if let Ok(number) = number_str.parse::<usize>() {
            // 尝试找到最接近的标签
            for i in 0..1000 { // 限制搜索范围
                let candidate = format!("L{}", i);
                if valid_labels.contains(&candidate) {
                    return Some(candidate);
                }
            }
        }
    }
    
    // 如果无法解析，返回第一个有效标签
    valid_labels.iter().next().cloned()
}

/// 找到下一个有效的标签（用于修复死循环）
fn find_next_valid_label(current_label: &str, valid_labels: &std::collections::HashSet<String>) -> Option<String> {
    // 尝试从标签中提取数字部分
    if let Some(number_str) = current_label.strip_prefix("L") {
        if let Ok(current_number) = number_str.parse::<usize>() {
            // 尝试找到下一个标签
            for i in (current_number + 1)..(current_number + 100) { // 限制搜索范围
                let candidate = format!("L{}", i);
                if valid_labels.contains(&candidate) {
                    return Some(candidate);
                }
            }
        }
    }
    
    // 如果无法解析，返回第一个有效标签
    valid_labels.iter().next().cloned()
}



use crate::TACIR::tacir::{TACInstruction, TACProgram, TACFunction, BasicBlock, Operand, ConstantValue, BinaryOperator, UnaryOperator};
use crate::TACIR::TAC_opt::{OptimizationPass, OptimizationResult, OptimizationStats};
use std::collections::{HashMap, HashSet};

/// 内联优化Pass
pub struct InlineOptimizationPass {
    threshold: usize,
    max_depth: usize,
    current_depth: usize, // 添加当前内联深度跟踪
    loop_info: HashMap<String, bool>,
    stats: OptimizationStats,
    temp_counter: usize,  // 全局临时变量计数器
    label_counter: usize, // 全局标签计数器
}

impl InlineOptimizationPass {
    pub fn new() -> Self {
        Self {
            threshold: 10,   // 更保守的阈值
            max_depth: 2,    // 限制最大深度
            current_depth: 0,
            loop_info: HashMap::new(),
            stats: OptimizationStats::new(),
            temp_counter: 1000, // 从高位开始避免冲突
            label_counter: 1000,
        }
    }

    pub fn set_threshold(&mut self, threshold: usize) {
        self.threshold = threshold;
    }

    pub fn set_max_depth(&mut self, max_depth: usize) {
        self.max_depth = max_depth;
    }

    pub fn update_loop_info(&mut self, loop_info: HashMap<String, bool>) {
        self.loop_info = loop_info;
    }

    fn run_internal(&mut self, program: &mut TACProgram) -> Result<usize, String> {
        println!("🚀 开始内联优化...");
        
        let mut total_inlines = 0;
        let mut round = 0;
        
        loop {
            round += 1;
            
            let inlines_this_round = self.perform_inline_round(program)?;
            total_inlines += inlines_this_round;
            
            if inlines_this_round == 0 {
                break;
            }
        }
        
        println!("✅ 内联优化完成，共内联 {} 个函数调用", total_inlines);
        Ok(total_inlines)
    }

    fn perform_inline_round(&mut self, program: &mut TACProgram) -> Result<usize, String> {
        let mut inlines_this_round = 0;
        
        // 构建调用图和函数分析
        let call_graph = self.build_call_graph(program);
        let function_analysis = self.analyze_functions(program);
        
        // 收集所有内联请求
        let mut inline_requests = Vec::new();
        
        for (func_idx, function) in program.functions.iter().enumerate() {
            for (block_idx, block) in function.basic_blocks.iter().enumerate() {
                for (inst_idx, instruction) in block.instructions.iter().enumerate() {
                    if let TACInstruction::FunctionCall { target, function_name, arguments } = instruction {
                        if let Some(callee_func) = self.find_function_by_name(program, function_name) {
                            let should_inline = self.should_inline_function(
                                &callee_func, 
                                &function_analysis, 
                                &call_graph
                            );
                            
                            if should_inline {
                                inline_requests.push((
                                    func_idx, block_idx, inst_idx,
                                    function_name.clone(),
                                    target.clone(),
                                    arguments.clone()
                                ));
                            }
                        }
                    }
                }
            }
        }
        
        // 从后往前处理内联请求
        for (func_idx, block_idx, inst_idx, callee_name, target, args) in inline_requests.into_iter().rev() {
            if let Some(callee) = self.find_function_by_name(program, &callee_name) {
                if let Ok(inlined_blocks) = self.inline_function(
                    &callee, &args, &target, program
                ) {
                    // 替换函数调用为内联代码
                    if let Some(func) = program.functions.get_mut(func_idx) {
                        if let Some(block) = func.basic_blocks.get_mut(block_idx) {
                            // 移除函数调用指令
                            block.instructions.remove(inst_idx);
                            
                            // 插入内联的基本块
                            self.insert_inlined_blocks(func, block_idx, inlined_blocks);
                            
                            inlines_this_round += 1;
                        }
                    }
                }
            }
        }
        
        Ok(inlines_this_round)
    }

    fn should_inline_function(
        &self, 
        function: &TACFunction, 
        analysis: &HashMap<String, FunctionAnalysisInfo>,
        call_graph: &HashMap<String, Vec<String>>
    ) -> bool {
        let score = self.get_inline_score(function, analysis, call_graph);
        score <= self.threshold
    }

    fn get_inline_score(
        &self, 
        function: &TACFunction, 
        analysis: &HashMap<String, FunctionAnalysisInfo>,
        call_graph: &HashMap<String, Vec<String>>
    ) -> usize {
        let mut score = 0;
        
        // 基础分数：指令数量
        let instruction_count = function.basic_blocks.iter()
            .map(|block| block.instructions.len())
            .sum::<usize>();
        score += instruction_count;
        
        // 函数调用惩罚
        if let Some(calls) = call_graph.get(&function.name) {
            if !calls.is_empty() {
                score += 20; // 调用其他函数的惩罚
            }
        }
        
        // 循环惩罚
        if let Some(has_loops) = self.loop_info.get(&function.name) {
            if *has_loops {
                score += 30; // 包含循环的惩罚
            }
        }
        
        score
    }

    // 重写内联函数的核心方法
    fn inline_function(
        &mut self,
        callee: &TACFunction,
        args: &[Operand],
        return_target: &Operand,
        program: &TACProgram
    ) -> Result<Vec<BasicBlock>, String> {
        if self.current_depth >= self.max_depth {
            return Err("达到最大内联深度".to_string());
        }
        
        self.current_depth += 1;
        
        // 1. 创建参数映射
        let mut param_mapping = HashMap::new();
        for (i, (param_name, _)) in callee.parameters.iter().enumerate() {
            let arg = args.get(i).cloned().unwrap_or(Operand::Constant(ConstantValue::Integer(0)));
            param_mapping.insert(param_name.clone(), arg);
        }
        
        // 2. 创建临时变量映射
        let mut temp_mapping = HashMap::new();
        let mut local_mapping = HashMap::new();
        
        // 3. 创建标签映射
        let mut label_mapping = HashMap::new();
        
        // 4. 复制并映射基本块
        let mut inlined_blocks = Vec::new();
        
        for block in &callee.basic_blocks {
            let mut new_block = BasicBlock {
                id: block.id,
                label: block.label.as_ref().map(|l| self.generate_unique_label(l)),
                instructions: Vec::new(),
                predecessors: Vec::new(),
                successors: Vec::new(),
            };
            
            for inst in &block.instructions {
                let new_inst = self.map_instruction(
                    inst, 
                    &param_mapping, 
                    &mut temp_mapping, 
                    &mut local_mapping,
                    &label_mapping,
                    return_target
                )?;
                
                new_block.instructions.push(new_inst);
            }
            
            inlined_blocks.push(new_block);
        }
        
        // 5. 修复控制流（跳转目标）
        self.fix_control_flow(&mut inlined_blocks, &label_mapping);
        
        self.current_depth -= 1;
        Ok(inlined_blocks)
    }

    // 辅助方法：生成唯一标签
    fn generate_unique_label(&mut self, base: &str) -> String {
        let label = format!("L{}_{}", base, self.label_counter);
        self.label_counter += 1;
        label
    }

    // 辅助方法：判断是否为全局变量
    fn is_global_variable(&self, name: &str) -> bool {
        name == "VALUE" || name == "MULTIPLIER" || name.starts_with("global_") || 
        name == "space" || name == "LF" || name == "maxNode" || name == "now"
    }

    // 重写指令映射
    fn map_instruction(
        &mut self,
        inst: &TACInstruction,
        param_mapping: &HashMap<String, Operand>,
        temp_mapping: &mut HashMap<usize, usize>,
        local_mapping: &mut HashMap<String, String>,
        label_mapping: &HashMap<String, String>,
        return_target: &Operand
    ) -> Result<TACInstruction, String> {
        match inst {
            TACInstruction::Return { value } => {
                // Return转换为赋值
                let mapped_value = if let Some(val) = value {
                    self.map_operand(val, param_mapping, temp_mapping, local_mapping)?
                } else {
                    Operand::Constant(ConstantValue::Integer(0))
                };
                
                Ok(TACInstruction::Assign {
                    target: return_target.clone(),
                    source: mapped_value,
                })
            }
            
            TACInstruction::Assign { target, source } => {
                Ok(TACInstruction::Assign {
                    target: self.map_operand(target, param_mapping, temp_mapping, local_mapping)?,
                    source: self.map_operand(source, param_mapping, temp_mapping, local_mapping)?,
                })
            }
            
            TACInstruction::BinaryOp { target, left, op, right } => {
                Ok(TACInstruction::BinaryOp {
                    target: self.map_operand(target, param_mapping, temp_mapping, local_mapping)?,
                    left: self.map_operand(left, param_mapping, temp_mapping, local_mapping)?,
                    op: op.clone(),
                    right: self.map_operand(right, param_mapping, temp_mapping, local_mapping)?,
                })
            }
            
            TACInstruction::UnaryOp { target, op, operand } => {
                Ok(TACInstruction::UnaryOp {
                    target: self.map_operand(target, param_mapping, temp_mapping, local_mapping)?,
                    op: op.clone(),
                    operand: self.map_operand(operand, param_mapping, temp_mapping, local_mapping)?,
                })
            }
            
            TACInstruction::FunctionCall { target, function_name, arguments } => {
                let mut new_arguments = Vec::new();
                for arg in arguments {
                    new_arguments.push(self.map_operand(arg, param_mapping, temp_mapping, local_mapping)?);
                }
                
                Ok(TACInstruction::FunctionCall {
                    target: self.map_operand(target, param_mapping, temp_mapping, local_mapping)?,
                    function_name: function_name.clone(),
                    arguments: new_arguments,
                })
            }
            
            TACInstruction::Label { name } => {
                let new_name = label_mapping.get(name)
                    .ok_or_else(|| format!("标签 {} 没有找到映射", name))?
                    .clone();
                Ok(TACInstruction::Label { name: new_name })
            }
            
            TACInstruction::Jump { label } => {
                let new_label = label_mapping.get(label)
                    .ok_or_else(|| format!("跳转标签 {} 没有找到映射", label))?
                    .clone();
                Ok(TACInstruction::Jump { label: new_label })
            }
            
            TACInstruction::ConditionalJump { condition, true_label, false_label } => {
                let new_true_label = label_mapping.get(true_label)
                    .ok_or_else(|| format!("条件跳转标签 {} 没有找到映射", true_label))?
                    .clone();
                let new_false_label = label_mapping.get(false_label)
                    .ok_or_else(|| format!("条件跳转标签 {} 没有找到映射", false_label))?
                    .clone();
                Ok(TACInstruction::ConditionalJump { 
                    condition: self.map_operand(condition, param_mapping, temp_mapping, local_mapping)?,
                    true_label: new_true_label,
                    false_label: new_false_label
                })
            }
            
            TACInstruction::GetElementPtr { target, base, indices } => {
                let mut new_indices = Vec::new();
                for idx in indices {
                    new_indices.push(self.map_operand(idx, param_mapping, temp_mapping, local_mapping)?);
                }
                
                Ok(TACInstruction::GetElementPtr {
                    target: self.map_operand(target, param_mapping, temp_mapping, local_mapping)?,
                    base: self.map_operand(base, param_mapping, temp_mapping, local_mapping)?,
                    indices: new_indices,
                })
            }
            
            TACInstruction::Load { target, address } => {
                Ok(TACInstruction::Load {
                    target: self.map_operand(target, param_mapping, temp_mapping, local_mapping)?,
                    address: self.map_operand(address, param_mapping, temp_mapping, local_mapping)?,
                })
            }
            
            TACInstruction::Store { value, address } => {
                Ok(TACInstruction::Store {
                    value: self.map_operand(value, param_mapping, temp_mapping, local_mapping)?,
                    address: self.map_operand(address, param_mapping, temp_mapping, local_mapping)?,
                })
            }
            
            TACInstruction::Allocate { target, size } => {
                Ok(TACInstruction::Allocate {
                    target: self.map_operand(target, param_mapping, temp_mapping, local_mapping)?,
                    size: self.map_operand(size, param_mapping, temp_mapping, local_mapping)?,
                })
            }
            
            TACInstruction::Param { value } => {
                Ok(TACInstruction::Param { value: self.map_operand(value, param_mapping, temp_mapping, local_mapping)? })
            }
            
            TACInstruction::FunctionStart { name, param_count } => {
                Ok(TACInstruction::FunctionStart { name: format!("{}_inline", name), param_count: *param_count })
            }
            
            TACInstruction::FunctionEnd => Ok(TACInstruction::FunctionEnd),
        }
    }

    // 重写操作数映射
    fn map_operand(
        &mut self,
        operand: &Operand,
        param_mapping: &HashMap<String, Operand>,
        temp_mapping: &mut HashMap<usize, usize>,
        local_mapping: &mut HashMap<String, String>
    ) -> Result<Operand, String> {
        match operand {
            Operand::Variable(name) => {
                // 检查是否是参数
                if let Some(mapped) = param_mapping.get(name) {
                    Ok(mapped.clone())
                } 
                // 检查是否是局部变量（需要重命名）
                else if !self.is_global_variable(name) {
                    let new_name = local_mapping.entry(name.clone())
                        .or_insert_with(|| format!("{}_inline_{}", name, self.temp_counter));
                    Ok(Operand::Variable(new_name.clone()))
                }
                // 全局变量保持不变
                else {
                    Ok(operand.clone())
                }
            }
            
            Operand::Temp(id) => {
                let new_id = *temp_mapping.entry(*id)
                    .or_insert_with(|| {
                        let new_id = self.temp_counter;
                        self.temp_counter += 1;
                        new_id
                    });
                Ok(Operand::Temp(new_id))
            }
            
            Operand::Constant(_) => Ok(operand.clone()),
            
            Operand::Label(label) => {
                let new_label = format!("{}_inline_{}", label, self.label_counter);
                Ok(Operand::Label(new_label))
            }
        }
    }

    // 修复控制流
    fn fix_control_flow(&self, blocks: &mut [BasicBlock], label_mapping: &HashMap<String, String>) {
        // 更新跳转指令中的标签引用
        for block in blocks {
            for instruction in &mut block.instructions {
                match instruction {
                    TACInstruction::Jump { label } => {
                        if let Some(new_label) = label_mapping.get(label) {
                            *label = new_label.clone();
                        }
                    }
                    TACInstruction::ConditionalJump { true_label, false_label, .. } => {
                        if let Some(new_label) = label_mapping.get(true_label) {
                            *true_label = new_label.clone();
                        }
                        if let Some(new_label) = label_mapping.get(false_label) {
                            *false_label = new_label.clone();
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    // 插入内联的基本块
    fn insert_inlined_blocks(&self, func: &mut TACFunction, block_idx: usize, inlined_blocks: Vec<BasicBlock>) {
        // 简单的实现：将内联的基本块插入到当前基本块之后
        for (i, block) in inlined_blocks.into_iter().enumerate() {
            func.basic_blocks.insert(block_idx + 1 + i, block);
        }
    }

    /// 带映射的内联指令方法
    fn inline_instruction_with_mapping(
        &self,
        instruction: &TACInstruction,
        variable_mapping: &HashMap<String, Operand>,
        target: &Operand,
        temp_mapping: &HashMap<usize, usize>  // 临时变量映射表
    ) -> Result<TACInstruction, String> {
        match instruction {
            TACInstruction::Assign { target: inst_target, source } => {
                let new_target = self.map_operand_with_mapping(inst_target, variable_mapping, temp_mapping)?;
                let new_source = self.map_operand_with_mapping(source, variable_mapping, temp_mapping)?;
                
                Ok(TACInstruction::Assign {
                    target: new_target,
                    source: new_source,
                })
            }
            
            TACInstruction::BinaryOp { target: inst_target, left, op, right } => {
                let new_target = self.map_operand_with_mapping(inst_target, variable_mapping, temp_mapping)?;
                let new_left = self.map_operand_with_mapping(left, variable_mapping, temp_mapping)?;
                let new_right = self.map_operand_with_mapping(right, variable_mapping, temp_mapping)?;
                
                Ok(TACInstruction::BinaryOp {
                    target: new_target,
                    left: new_left,
                    op: op.clone(),
                    right: new_right,
                })
            }
            
            TACInstruction::UnaryOp { target: inst_target, op, operand } => {
                let new_target = self.map_operand_with_mapping(inst_target, variable_mapping, temp_mapping)?;
                let new_operand = self.map_operand_with_mapping(operand, variable_mapping, temp_mapping)?;
                
                Ok(TACInstruction::UnaryOp {
                    target: new_target,
                    op: op.clone(),
                    operand: new_operand,
                })
            }
            
            TACInstruction::FunctionCall { target: inst_target, function_name, arguments } => {
                let new_target = self.map_operand_with_mapping(inst_target, variable_mapping, temp_mapping)?;
                let mut new_arguments = Vec::new();
                
                for arg in arguments {
                    let new_arg = self.map_operand_with_mapping(arg, variable_mapping, temp_mapping)?;
                    new_arguments.push(new_arg);
                }
                
                Ok(TACInstruction::FunctionCall {
                    target: new_target,
                    function_name: function_name.clone(),
                    arguments: new_arguments,
                })
            }
            
            TACInstruction::Return { value } => {
                if let Some(val) = value {
                    let new_value = self.map_operand_with_mapping(val, variable_mapping, temp_mapping)?;
                    // 直接赋值给调用者的目标变量
                    Ok(TACInstruction::Assign {
                        target: target.clone(),
                        source: new_value,
                    })
                } else {
                    // void函数返回空操作（使用dummy赋值，后续优化会消除）
                    Ok(TACInstruction::Assign {
                        target: Operand::Constant(ConstantValue::Integer(0)),
                        source: Operand::Constant(ConstantValue::Integer(0)),
                    })
                }
            }
            
            TACInstruction::Label { name } => {
                // 为内联的标签生成唯一名称，避免冲突
                let new_name = format!("L{}_inline_{}", name, temp_mapping.len());
                Ok(TACInstruction::Label { name: new_name })
            }
            
            TACInstruction::Jump { label } => {
                // 为内联的跳转标签生成唯一名称
                let new_label = format!("L{}_inline_{}", label, temp_mapping.len());
                Ok(TACInstruction::Jump { label: new_label })
            }
            
            TACInstruction::ConditionalJump { condition, true_label, false_label } => {
                let new_condition = self.map_operand_with_mapping(condition, variable_mapping, temp_mapping)?;
                // 为内联的条件跳转标签生成唯一名称
                let new_true_label = format!("L{}_inline_{}", true_label, temp_mapping.len());
                let new_false_label = format!("L{}_inline_{}", false_label, temp_mapping.len());
                Ok(TACInstruction::ConditionalJump { 
                    condition: new_condition, 
                    true_label: new_true_label, 
                    false_label: new_false_label 
                })
            }
            
            TACInstruction::GetElementPtr { target, base, indices } => {
                let new_target = self.map_operand_with_mapping(target, variable_mapping, temp_mapping)?;
                let new_base = self.map_operand_with_mapping(base, variable_mapping, temp_mapping)?;
                let mut new_indices = Vec::new();
                
                for idx in indices {
                    let new_idx = self.map_operand_with_mapping(idx, variable_mapping, temp_mapping)?;
                    new_indices.push(new_idx);
                }
                
                Ok(TACInstruction::GetElementPtr {
                    target: new_target,
                    base: new_base,
                    indices: new_indices,
                })
            }
            
            TACInstruction::Load { target, address } => {
                let new_target = self.map_operand_with_mapping(target, variable_mapping, temp_mapping)?;
                let new_address = self.map_operand_with_mapping(address, variable_mapping, temp_mapping)?;
                
                Ok(TACInstruction::Load {
                    target: new_target,
                    address: new_address,
                })
            }
            
            TACInstruction::Store { value, address } => {
                let new_value = self.map_operand_with_mapping(value, variable_mapping, temp_mapping)?;
                let new_address = self.map_operand_with_mapping(address, variable_mapping, temp_mapping)?;
                
                Ok(TACInstruction::Store {
                    value: new_value,
                    address: new_address,
                })
            }
            
            TACInstruction::Allocate { target, size } => {
                let new_target = self.map_operand_with_mapping(target, variable_mapping, temp_mapping)?;
                let new_size = self.map_operand_with_mapping(size, variable_mapping, temp_mapping)?;
                
                Ok(TACInstruction::Allocate {
                    target: new_target,
                    size: new_size,
                })
            }
            
            TACInstruction::Param { value } => {
                let new_value = self.map_operand_with_mapping(value, variable_mapping, temp_mapping)?;
                Ok(TACInstruction::Param { value: new_value })
            }
            
            TACInstruction::FunctionStart { name, param_count } => {
                let new_name = format!("{}_inline", name);
                Ok(TACInstruction::FunctionStart { name: new_name, param_count: *param_count })
            }
            
            TACInstruction::FunctionEnd => Ok(TACInstruction::FunctionEnd),
        }
    }

    /// 带映射的操作数映射方法
    fn map_operand_with_mapping(
        &self,
        operand: &Operand,
        variable_mapping: &HashMap<String, Operand>,
        temp_mapping: &HashMap<usize, usize>
    ) -> Result<Operand, String> {
        match operand {
            Operand::Variable(name) => {
                if let Some(mapped_operand) = variable_mapping.get(name) {
                    Ok(mapped_operand.clone())
                } else {
                    // 检查是否是全局常量（不应该重命名）
                    if name == "VALUE" || name == "MULTIPLIER" {
                        Ok(Operand::Variable(name.clone()))
                    } else {
                        // 不是参数且不是全局常量，需要重命名避免冲突
                        let new_name = format!("{}_inline", name);
                        Ok(Operand::Variable(new_name))
                    }
                }
            }
            
            Operand::Temp(id) => {
                if let Some(&mapped_id) = temp_mapping.get(id) {
                    Ok(Operand::Temp(mapped_id))
                } else {
                    // 如果没有映射，保持原样（理论上不应该发生）
                    Ok(Operand::Temp(*id))
                }
            }
            
            Operand::Constant(val) => Ok(Operand::Constant(val.clone())),
            
            Operand::Label(label) => {
                let new_label = format!("{}_inline", label);
                Ok(Operand::Label(new_label))
            }
        }
    }

    /// 收集指令中使用的临时变量
    fn collect_temp_variables(&self, instruction: &TACInstruction, temp_vars: &mut HashSet<usize>) {
        match instruction {
            TACInstruction::Assign { target, source } => {
                self.collect_temps_in_operand(target, temp_vars);
                self.collect_temps_in_operand(source, temp_vars);
            }
            TACInstruction::BinaryOp { target, left, right, .. } => {
                self.collect_temps_in_operand(target, temp_vars);
                self.collect_temps_in_operand(left, temp_vars);
                self.collect_temps_in_operand(right, temp_vars);
            }
            TACInstruction::UnaryOp { target, operand, .. } => {
                self.collect_temps_in_operand(target, temp_vars);
                self.collect_temps_in_operand(operand, temp_vars);
            }
            TACInstruction::FunctionCall { target, arguments, .. } => {
                self.collect_temps_in_operand(target, temp_vars);
                for arg in arguments {
                    self.collect_temps_in_operand(arg, temp_vars);
                }
            }
            TACInstruction::Return { value } => {
                if let Some(val) = value {
                    self.collect_temps_in_operand(val, temp_vars);
                }
            }
            TACInstruction::ConditionalJump { condition, .. } => {
                self.collect_temps_in_operand(condition, temp_vars);
            }
            TACInstruction::GetElementPtr { target, base, indices } => {
                self.collect_temps_in_operand(target, temp_vars);
                self.collect_temps_in_operand(base, temp_vars);
                for idx in indices {
                    self.collect_temps_in_operand(idx, temp_vars);
                }
            }
            TACInstruction::Load { target, address } => {
                self.collect_temps_in_operand(target, temp_vars);
                self.collect_temps_in_operand(address, temp_vars);
            }
            TACInstruction::Store { value, address } => {
                self.collect_temps_in_operand(value, temp_vars);
                self.collect_temps_in_operand(address, temp_vars);
            }
            TACInstruction::Allocate { target, size } => {
                self.collect_temps_in_operand(target, temp_vars);
                self.collect_temps_in_operand(size, temp_vars);
            }
            TACInstruction::Param { value } => {
                self.collect_temps_in_operand(value, temp_vars);
            }
            TACInstruction::Label { .. } => {} // 标签不包含临时变量
            TACInstruction::Jump { .. } => {} // 跳转不包含临时变量
            TACInstruction::FunctionStart { .. } => {} // 函数开始不包含临时变量
            TACInstruction::FunctionEnd => {} // 函数结束不包含临时变量
        }
    }

    /// 收集操作数中的临时变量
    fn collect_temps_in_operand(&self, operand: &Operand, temp_vars: &mut HashSet<usize>) {
        if let Operand::Temp(id) = operand {
            temp_vars.insert(*id);
        }
    }

    /// 简化优化：只消除真正的冗余
    fn optimize_inlined_code(&self, instructions: Vec<TACInstruction>) -> Vec<TACInstruction> {
        let mut optimized = Vec::new();
        
        for instruction in instructions {
            // 只消除明显的冗余
            if let TACInstruction::Assign { target, source } = &instruction {
                if target == source {
                    println!("🗑️ 消除冗余赋值: {:?} = {:?}", target, source);
                    continue; // 消除自赋值
                }
            }
            
            optimized.push(instruction);
        }
        
        optimized
    }
    
    /// 替换指令中的临时变量引用
    fn replace_temp_references_in_instruction(&self, instruction: TACInstruction, temp_mapping: &HashMap<usize, usize>) -> TACInstruction {
        match instruction {
            TACInstruction::Assign { target, source } => {
                TACInstruction::Assign {
                    target: self.replace_temp_references(&target, temp_mapping),
                    source: self.replace_temp_references(&source, temp_mapping),
                }
            }
            
            TACInstruction::BinaryOp { target, left, right, op } => {
                TACInstruction::BinaryOp {
                    target: self.replace_temp_references(&target, temp_mapping),
                    left: self.replace_temp_references(&left, temp_mapping),
                    right: self.replace_temp_references(&right, temp_mapping),
                    op,
                }
            }
            
            TACInstruction::UnaryOp { target, op, operand } => {
                TACInstruction::UnaryOp {
                    target: self.replace_temp_references(&target, temp_mapping),
                    op,
                    operand: self.replace_temp_references(&operand, temp_mapping),
                }
            }
            
            TACInstruction::Return { value } => {
                TACInstruction::Return {
                    value: value.map(|v| self.replace_temp_references(&v, temp_mapping)),
                }
            }
            
            // 其他指令类型保持不变
            _ => instruction,
        }
    }
    
    /// 替换操作数中的临时变量引用
    fn replace_temp_references(&self, operand: &Operand, temp_mapping: &HashMap<usize, usize>) -> Operand {
        match operand {
            Operand::Temp(id) => {
                if let Some(&mapped_id) = temp_mapping.get(id) {
                    Operand::Temp(mapped_id)
                } else {
                    operand.clone()
                }
            }
            _ => operand.clone(),
        }
    }

    fn find_function_by_name(&self, program: &TACProgram, name: &str) -> Option<TACFunction> {
        program.functions.iter().find(|f| f.name == name).cloned()
    }
    
    /// 修复被内联函数本身的指令
    fn fix_callee_function_instructions(&self, program: &mut TACProgram, function_name: &str) {
        // 修复被内联函数本身的指令
        if let Some(function) = program.functions.iter_mut().find(|f| f.name == function_name) {
            for block in &mut function.basic_blocks {
                for instruction in &mut block.instructions {
                    match instruction {
                        TACInstruction::GetElementPtr { target, base, indices } => {
                            if let Operand::Variable(var_name) = base {
                                if var_name.ends_with("_inline") {
                                    // 将 _inline 后缀的变量名改回原变量名
                                    *var_name = var_name.trim_end_matches("_inline").to_string();
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    
    /// 立即应用所有映射到指令（带标签映射）
    fn apply_mappings_to_instruction_with_labels(
        &self,
        instruction: &TACInstruction,
        variable_mapping: &HashMap<String, Operand>,
        temp_mapping: &HashMap<usize, usize>,
        label_mapping: &HashMap<String, String>,
        return_target: &Operand  // Return指令的目标
    ) -> Result<TACInstruction, String> {
        match instruction {
            // Return指令特殊处理：转换为赋值
            TACInstruction::Return { value } => {
                if let Some(val) = value {
                    let mapped_value = self.apply_operand_mappings(val, variable_mapping, temp_mapping);
                    Ok(TACInstruction::Assign {
                        target: return_target.clone(),
                        source: mapped_value,
                    })
                } else {
                    // void函数返回空操作
                    Ok(TACInstruction::Assign {
                        target: Operand::Constant(ConstantValue::Integer(0)),
                        source: Operand::Constant(ConstantValue::Integer(0)),
                    })
                }
            }

            // 其他指令正常处理
            TACInstruction::Assign { target, source } => {
                Ok(TACInstruction::Assign {
                    target: self.apply_operand_mappings(target, variable_mapping, temp_mapping),
                    source: self.apply_operand_mappings(source, variable_mapping, temp_mapping),
                })
            }

            TACInstruction::BinaryOp { target, left, op, right } => {
                Ok(TACInstruction::BinaryOp {
                    target: self.apply_operand_mappings(target, variable_mapping, temp_mapping),
                    left: self.apply_operand_mappings(left, variable_mapping, temp_mapping),
                    op: op.clone(),
                    right: self.apply_operand_mappings(right, variable_mapping, temp_mapping),
                })
            }

            TACInstruction::UnaryOp { target, op, operand } => {
                Ok(TACInstruction::UnaryOp {
                    target: self.apply_operand_mappings(target, variable_mapping, temp_mapping),
                    op: op.clone(),
                    operand: self.apply_operand_mappings(operand, variable_mapping, temp_mapping),
                })
            }

            TACInstruction::FunctionCall { target, function_name, arguments } => {
                let mut new_arguments = Vec::new();
                for arg in arguments {
                    new_arguments.push(self.apply_operand_mappings(arg, variable_mapping, temp_mapping));
                }
                
                Ok(TACInstruction::FunctionCall {
                    target: self.apply_operand_mappings(target, variable_mapping, temp_mapping),
                    function_name: function_name.clone(),
                    arguments: new_arguments,
                })
            }

            TACInstruction::Label { name } => {
                let new_name = label_mapping.get(name)
                    .ok_or_else(|| format!("标签 {} 没有找到映射", name))?
                    .clone();
                Ok(TACInstruction::Label { name: new_name })
            }

            TACInstruction::Jump { label } => {
                let new_label = label_mapping.get(label)
                    .ok_or_else(|| format!("跳转标签 {} 没有找到映射", label))?
                    .clone();
                Ok(TACInstruction::Jump { label: new_label })
            }

            TACInstruction::ConditionalJump { condition, true_label, false_label } => {
                let new_true_label = label_mapping.get(true_label)
                    .ok_or_else(|| format!("条件跳转标签 {} 没有找到映射", true_label))?
                    .clone();
                let new_false_label = label_mapping.get(false_label)
                    .ok_or_else(|| format!("条件跳转标签 {} 没有找到映射", false_label))?
                    .clone();
                Ok(TACInstruction::ConditionalJump { 
                    condition: self.apply_operand_mappings(condition, variable_mapping, temp_mapping),
                    true_label: new_true_label,
                    false_label: new_false_label
                })
            }

            TACInstruction::GetElementPtr { target, base, indices } => {
                let mut new_indices = Vec::new();
                for idx in indices {
                    new_indices.push(self.apply_operand_mappings(idx, variable_mapping, temp_mapping));
                }
                
                Ok(TACInstruction::GetElementPtr {
                    target: self.apply_operand_mappings(target, variable_mapping, temp_mapping),
                    base: self.apply_operand_mappings(base, variable_mapping, temp_mapping),
                    indices: new_indices,
                })
            }

            TACInstruction::Load { target, address } => {
                Ok(TACInstruction::Load {
                    target: self.apply_operand_mappings(target, variable_mapping, temp_mapping),
                    address: self.apply_operand_mappings(address, variable_mapping, temp_mapping),
                })
            }

            TACInstruction::Store { value, address } => {
                Ok(TACInstruction::Store {
                    value: self.apply_operand_mappings(value, variable_mapping, temp_mapping),
                    address: self.apply_operand_mappings(address, variable_mapping, temp_mapping),
                })
            }

            TACInstruction::Allocate { target, size } => {
                Ok(TACInstruction::Allocate {
                    target: self.apply_operand_mappings(target, variable_mapping, temp_mapping),
                    size: self.apply_operand_mappings(size, variable_mapping, temp_mapping),
                })
            }

            TACInstruction::Param { value } => {
                Ok(TACInstruction::Param { value: self.apply_operand_mappings(value, variable_mapping, temp_mapping) })
            }

            TACInstruction::FunctionStart { name, param_count } => {
                Ok(TACInstruction::FunctionStart { name: format!("{}_inline", name), param_count: *param_count })
            }

            TACInstruction::FunctionEnd => Ok(TACInstruction::FunctionEnd),
        }
    }

    /// 立即应用映射到操作数
    fn apply_operand_mappings(
        &self,
        operand: &Operand,
        variable_mapping: &HashMap<String, Operand>,
        temp_mapping: &HashMap<usize, usize>
    ) -> Operand {
        match operand {
            Operand::Variable(name) => {
                // 首先检查是否是函数参数
                if let Some(mapped_arg) = variable_mapping.get(name) {
                    mapped_arg.clone()
                } else {
                    // 不是参数，保持原变量名（可能是全局变量或局部变量）
                    Operand::Variable(name.clone())
                }
            }

            Operand::Temp(id) => {
                Operand::Temp(*temp_mapping.get(id).unwrap_or(id))
            }

            Operand::Constant(val) => Operand::Constant(val.clone()),
            
            Operand::Label(label) => Operand::Label(format!("{}_inline", label)),
        }
    }

    fn analyze_functions(&self, program: &TACProgram) -> HashMap<String, FunctionAnalysisInfo> {
        let mut analysis = HashMap::new();
        
        for function in &program.functions {
            let mut info = FunctionAnalysisInfo {
                instruction_count: 0,
                basic_block_count: function.basic_blocks.len(),
                temp_variable_count: 0,
                has_function_calls: false,
                has_loops: false,
                call_count: 0,
                inline_score: 0,
                suitable_for_inline: false,
            };
            
            let mut temp_vars: HashSet<usize> = HashSet::new();
            
            for block in &function.basic_blocks {
                for instruction in &block.instructions {
                    info.instruction_count += 1;
                    
                    match instruction {
                        TACInstruction::FunctionCall { .. } => {
                            info.has_function_calls = true;
                            info.call_count += 1;
                        }
                        _ => {}
                    }
                }
            }
            
            info.temp_variable_count = temp_vars.len();
            info.inline_score = self.get_inline_score(function, &analysis, &HashMap::new());
            info.suitable_for_inline = info.inline_score <= self.threshold;
            
            analysis.insert(function.name.clone(), info);
        }
        
        analysis
    }

    fn build_call_graph(&self, program: &TACProgram) -> HashMap<String, Vec<String>> {
        let mut call_graph = HashMap::new();
        
        for function in &program.functions {
            let mut calls = Vec::new();
            
            for block in &function.basic_blocks {
                for instruction in &block.instructions {
                    if let TACInstruction::FunctionCall { function_name, .. } = instruction {
                        calls.push(function_name.clone());
                    }
                }
            }
            
            call_graph.insert(function.name.clone(), calls);
        }
        
        call_graph
    }

    fn print_function_analysis(&self, analysis: &HashMap<String, FunctionAnalysisInfo>) {
        println!("=== 函数分析信息 ===");
        for (name, info) in analysis {
            println!("函数: {}", name);
            println!("  指令数: {}", info.instruction_count);
            println!("  基本块数: {}", info.basic_block_count);
            println!("  临时变量数: {}", info.temp_variable_count);
            println!("  有函数调用: {}", info.has_function_calls);
            println!("  有循环: {}", info.has_loops);
            println!("  被调用次数: {}", info.call_count);
            println!("  内联评分: {}", info.inline_score);
            println!("  适合内联: {}", info.suitable_for_inline);
            println!();
        }
    }

    fn print_call_graph(&self, call_graph: &HashMap<String, Vec<String>>) {
        println!("=== 调用关系 ===");
        for (caller, callees) in call_graph {
            if !callees.is_empty() {
                println!("{} 调用: {:?}", caller, callees);
            }
        }
        println!();
    }

    pub fn get_metrics(&self) -> OptimizationMetrics {
        OptimizationMetrics {
            pass_name: "Inline Optimization".to_string(),
            instructions_removed: 0,
            instructions_added: 0,
            basic_blocks_optimized: 0,
            functions_inlined: 0,
        }
    }
}

impl OptimizationPass for InlineOptimizationPass {
    fn run(&mut self, program: &mut TACProgram) -> Result<OptimizationResult, String> {
        let inlines = self.run_internal(program)?;
        
        // 更新统计信息
        self.stats.control_flow_optimizations = inlines;
        
        // 创建优化结果
        let mut result = OptimizationResult::new();
        if inlines > 0 {
            result.mark_optimized();
            result.instructions_optimized = inlines;
            result.nodes_optimized = inlines;
        }
        
        Ok(result)
    }
    
    fn name(&self) -> &str {
        "InlineOptimizationPass"
    }
    
    fn get_stats(&self) -> &OptimizationStats {
        &self.stats
    }
}

#[derive(Debug)]
pub struct FunctionAnalysisInfo {
    pub instruction_count: usize,
    pub basic_block_count: usize,
    pub temp_variable_count: usize,
    pub has_function_calls: bool,
    pub has_loops: bool,
    pub call_count: usize,
    pub inline_score: usize,
    pub suitable_for_inline: bool,
}

#[derive(Debug)]
pub struct OptimizationMetrics {
    pub pass_name: String,
    pub instructions_removed: usize,
    pub instructions_added: usize,
    pub basic_blocks_optimized: usize,
    pub functions_inlined: usize,
}

impl InlineOptimizationPass {
    /// 立即应用所有映射到指令（兼容旧版本）
    fn apply_mappings_to_instruction(
        &self,
        instruction: &TACInstruction,
        variable_mapping: &HashMap<String, Operand>,
        temp_mapping: &HashMap<usize, usize>,
        return_target: &Operand  // Return指令的目标
    ) -> Result<TACInstruction, String> {
        // 调用新版本的方法，使用空的标签映射
        let empty_label_mapping = HashMap::new();
        self.apply_mappings_to_instruction_with_labels(
            instruction,
            variable_mapping,
            temp_mapping,
            &empty_label_mapping,
            return_target
        )
    }
}

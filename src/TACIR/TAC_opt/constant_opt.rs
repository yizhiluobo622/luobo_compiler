use crate::TACIR::tacir::{TACProgram, TACFunction, BasicBlock, TACInstruction, Operand, ConstantValue, BinaryOperator, UnaryOperator};
use super::{OptimizationPass, OptimizationResult, OptimizationStats};
use std::collections::{HashMap, HashSet};

/// 常量优化Pass
/// 基于Worklist的智能常量传播算法
pub struct ConstantOptimizationPass {
    stats: OptimizationStats,
    /// 变量到常量值的映射
    variable_constants: HashMap<String, ConstantValue>,
    /// 临时变量到常量值的映射
    temp_constants: HashMap<usize, ConstantValue>,
    /// 工作队列：需要重新分析的基本块ID
    worklist: Vec<usize>,
    /// 已处理的基本块ID集合（避免重复处理）
    processed_blocks: HashSet<usize>,
}

impl ConstantOptimizationPass {
    pub fn new() -> Self {
        Self {
            stats: OptimizationStats::new(),
            variable_constants: HashMap::new(),
            temp_constants: HashMap::new(),
            worklist: Vec::new(),
            processed_blocks: HashSet::new(),
        }
    }
    
    /// 运行基于Worklist的常量传播优化
    pub fn run(&mut self, program: &mut TACProgram) -> Result<OptimizationResult, String> {
        let mut result = OptimizationResult::new();
        let mut total_optimizations = 0;
        let mut round = 0;
        const MAX_ROUNDS: usize = 20; // 防止无限循环
        
        // 遍历所有函数
        for function in &mut program.functions {
            // 初始化Worklist：包含所有基本块
            self.worklist = function.basic_blocks.iter().map(|b| b.id).collect();
            self.processed_blocks.clear();
            
            // 清空常量映射表
            self.variable_constants.clear();
            self.temp_constants.clear();
            
            // 主循环：基于Worklist的常量传播
            while !self.worklist.is_empty() && round < MAX_ROUNDS {
                round += 1;
                
                // 取出一个基本块ID
                let block_id = self.worklist.pop().unwrap();
                
                // 如果已经处理过，跳过
                if self.processed_blocks.contains(&block_id) {
                    continue;
                }
                
                // 标记为已处理
                self.processed_blocks.insert(block_id);
                
                // 获取基本块
                if let Some(block) = function.get_basic_block_mut(block_id) {
                    // 记录优化前的常量数量
                    let old_constant_count = self.variable_constants.len() + self.temp_constants.len();
                    
                    // 分析和优化该基本块
                    let block_optimizations = self.analyze_and_optimize_block(block)?;
                    total_optimizations += block_optimizations;
                    
                    // 检查是否有新的常量信息
                    let new_constant_count = self.variable_constants.len() + self.temp_constants.len();
                    let constants_changed = new_constant_count > old_constant_count || block_optimizations > 0;
                    
                    // 如果常量状态发生变化，将后继基本块加入Worklist
                    if constants_changed {
                        let successors: Vec<usize> = block.successors.clone();
                        for succ_id in successors {
                            if !self.worklist.contains(&succ_id) && !self.processed_blocks.contains(&succ_id) {
                                self.worklist.push(succ_id);
                            }
                        }
                    }
                }
            }
        }
        
        // 更新统计信息
        self.stats.constant_foldings = total_optimizations;
        self.stats.optimization_rounds = round;
        
        // 标记优化结果
        if total_optimizations > 0 {
            result.mark_optimized();
            result.instructions_optimized = total_optimizations;
        }
        
        Ok(result)
    }
    
        /// 分析和优化基本块
    fn analyze_and_optimize_block(&mut self, block: &mut BasicBlock) -> Result<usize, String> {
        let mut optimizations = 0;
        let mut i = 0;
        
        while i < block.instructions.len() {
            let instruction = block.instructions[i].clone();
            
            // 尝试常量折叠
            if let Some(folded_instruction) = self.try_constant_folding(&instruction)? {
                block.instructions[i] = folded_instruction;
                optimizations += 1;
            }
            // 尝试消除临时变量
            else if let Some(optimized_instruction) = self.try_temp_elimination(&instruction)? {
                block.instructions[i] = optimized_instruction;
                optimizations += 1;
            }
            
            // 更新常量映射
            self.update_constants_from_instruction(&block.instructions[i]);
            
            i += 1;
        }
        
        // 死代码消除（需要单独处理，因为会改变指令数量）
        let dead_code_eliminations = self.eliminate_dead_code(block)?;
        optimizations += dead_code_eliminations;
        
        Ok(optimizations)
    }
    
    /// 执行死代码消除
    fn eliminate_dead_code(&self, block: &mut BasicBlock) -> Result<usize, String> {
        let mut eliminations = 0;
        let mut i = 0;
        
        while i < block.instructions.len() {
            if let Some(dead_index) = self.try_dead_code_elimination(&block.instructions, i)? {
                // 删除死代码指令
                block.instructions.remove(dead_index);
                eliminations += 1;
                // 不需要增加i，因为后面的指令会前移
            } else {
                i += 1;
            }
        }
        
        Ok(eliminations)
    }
    
    /// 从指令更新常量映射
    fn update_constants_from_instruction(&mut self, instruction: &TACInstruction) {
        match instruction {
            TACInstruction::Assign { target, source } => {
                // 如果右值是常量，更新映射
                if let Operand::Constant(value) = source {
                    match target {
                        Operand::Variable(name) => {
                            self.variable_constants.insert(name.clone(), value.clone());
                        }
                        Operand::Temp(id) => {
                            self.temp_constants.insert(*id, value.clone());
                        }
                        _ => {}
                    }
                }
                // 如果右值可解析为常量，更新映射
                else if let Some(constant_value) = self.resolve_operand_to_constant(source).unwrap_or(None) {
                    match target {
                        Operand::Variable(name) => {
                            self.variable_constants.insert(name.clone(), constant_value);
                        }
                        Operand::Temp(id) => {
                            self.temp_constants.insert(*id, constant_value);
                        }
                        _ => {}
                    }
                } else {
                    // 不可解析为常量：杀掉原有的常量映射，避免陈旧常量污染
                    match target {
                        Operand::Variable(name) => {
                            self.variable_constants.remove(name);
                        }
                        Operand::Temp(id) => {
                            self.temp_constants.remove(id);
                        }
                        _ => {}
                    }
                }
            }
            // 这些指令写入目标，但当前无法确定为常量：统一执行 kill
            TACInstruction::BinaryOp { target, .. }
            | TACInstruction::UnaryOp { target, .. }
            | TACInstruction::FunctionCall { target, .. }
            | TACInstruction::GetElementPtr { target, .. }
            | TACInstruction::Load { target, .. }
            | TACInstruction::Allocate { target, .. } => {
                match target {
                    Operand::Variable(name) => {
                        self.variable_constants.remove(name);
                    }
                    Operand::Temp(id) => {
                        self.temp_constants.remove(id);
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }
    
    /// 尝试常量折叠
    fn try_constant_folding(&self, instruction: &TACInstruction) -> Result<Option<TACInstruction>, String> {
        match instruction {
            TACInstruction::BinaryOp { target, left, op, right } => {
                // 检查左右操作数是否都是常量，或者可以解析为常量
                let left_val = self.resolve_operand_to_constant(left)?;
                let right_val = self.resolve_operand_to_constant(right)?;
                
                if let (Some(left_const), Some(right_const)) = (left_val, right_val) {
                    // 执行常量折叠
                    let result_value = self.compute_binary_operation(&left_const, op, &right_const)?;
                    
                    // 创建新的赋值指令
                    let folded_instruction = TACInstruction::Assign {
                        target: target.clone(),
                        source: Operand::Constant(result_value),
                    };
                    
                    Ok(Some(folded_instruction))
                } else {
                    // 至少有一个不是常量：如果这是条件相关的比较运算，避免错误地把控制流条件变成常量
                    // 不做替换，保持原指令
                    Ok(None)
                }
            }
            
            TACInstruction::UnaryOp { target, op, operand } => {
                // 检查操作数是否是常量，或者可以解析为常量
                let operand_val = self.resolve_operand_to_constant(operand)?;
                
                if let Some(operand_const) = operand_val {
                    // 执行常量折叠
                    let result_value = self.compute_unary_operation(op, &operand_const)?;
                    
                    // 创建新的赋值指令
                    let folded_instruction = TACInstruction::Assign {
                        target: target.clone(),
                        source: Operand::Constant(result_value),
                    };
                    
                    Ok(Some(folded_instruction))
                } else {
                    // 不是常量：保持原指令
                    Ok(None)
                }
            }
            
            _ => Ok(None),
        }
    }
    
    /// 尝试消除临时变量（当临时变量只被使用一次时）
    fn try_temp_elimination(&self, instruction: &TACInstruction) -> Result<Option<TACInstruction>, String> {
        match instruction {
            TACInstruction::Assign { target, source } => {
                // 如果目标是变量，源是临时变量，且临时变量是已知常量
                if let Operand::Variable(_) = target {
                    if let Operand::Temp(temp_id) = source {
                        if let Some(constant_value) = self.temp_constants.get(temp_id) {
                            // 直接替换为常量赋值
                            let optimized_instruction = TACInstruction::Assign {
                                target: target.clone(),
                                source: Operand::Constant(constant_value.clone()),
                            };
                            return Ok(Some(optimized_instruction));
                        }
                    }
                }
                Ok(None)
            }
            _ => Ok(None),
        }
    }
    
    /// 尝试死代码消除（删除冗余赋值）
    fn try_dead_code_elimination(&self, instructions: &[TACInstruction], current_index: usize) -> Result<Option<usize>, String> {
        let current_instruction = &instructions[current_index];
        
        match current_instruction {
            TACInstruction::Assign { target, .. } => {
                // 检查后续指令是否覆盖了这个目标
                for i in (current_index + 1)..instructions.len() {
                    let next_instruction = &instructions[i];
                    match next_instruction {
                        TACInstruction::Assign { target: next_target, .. } => {
                            if target == next_target {
                                // 找到覆盖，但需要检查中间是否有使用
                                if !self.is_variable_used_between(instructions, current_index + 1, i, target)? {
                                    // 只有在中间没有被使用的情况下才删除
                                    return Ok(Some(current_index));
                                }
                                // 如果中间被使用了，不能删除
                                break;
                            }
                        }
                        TACInstruction::Return { .. } => {
                            // 到达return，不再有覆盖
                            break;
                        }
                        _ => {}
                    }
                }
                Ok(None)
            }
            _ => Ok(None),
        }
    }
    
    /// 检查变量在两个位置之间是否被使用
    fn is_variable_used_between(&self, instructions: &[TACInstruction], start: usize, end: usize, target: &Operand) -> Result<bool, String> {
        for i in start..end {
            let instruction = &instructions[i];
            if self.instruction_uses_operand(instruction, target)? {
                return Ok(true);
            }
        }
        Ok(false)
    }
    
    /// 检查指令是否使用了指定的操作数
    fn instruction_uses_operand(&self, instruction: &TACInstruction, operand: &Operand) -> Result<bool, String> {
        match instruction {
            TACInstruction::Assign { source, .. } => {
                Ok(source == operand)
            }
            TACInstruction::BinaryOp { left, right, .. } => {
                Ok(left == operand || right == operand)
            }
            TACInstruction::UnaryOp { operand: op_operand, .. } => {
                Ok(op_operand == operand)
            }
            TACInstruction::Store { value, address } => {
                Ok(value == operand || address == operand)
            }
            TACInstruction::Load { address, .. } => {
                Ok(address == operand)
            }
            TACInstruction::Allocate { size, .. } => {
                Ok(size == operand)
            }
            TACInstruction::GetElementPtr { base, indices, .. } => {
                Ok(base == operand || indices.iter().any(|idx| idx == operand))
            }
            TACInstruction::FunctionCall { arguments, .. } => {
                Ok(arguments.iter().any(|arg| arg == operand))
            }
            TACInstruction::Return { value } => {
                if let Some(ret_value) = value {
                    Ok(ret_value == operand)
                } else {
                    Ok(false)
                }
            }
            TACInstruction::ConditionalJump { condition, .. } => {
                Ok(condition == operand)
            }
            TACInstruction::Jump { .. } => Ok(false),
            TACInstruction::Label { .. } => Ok(false),
            TACInstruction::Param { value } => {
                Ok(value == operand)
            }
            TACInstruction::FunctionStart { .. } => Ok(false),
            TACInstruction::FunctionEnd => Ok(false),
        }
    }
    
    /// 尝试将操作数解析为常量值
    fn resolve_operand_to_constant(&self, operand: &Operand) -> Result<Option<ConstantValue>, String> {
        match operand {
            Operand::Constant(value) => Ok(Some(value.clone())),
            Operand::Temp(id) => {
                // 查找临时变量的常量值
                Ok(self.temp_constants.get(id).cloned())
            }
            Operand::Variable(name) => {
                // 查找变量的常量值
                Ok(self.variable_constants.get(name).cloned())
            }
            Operand::Label(_) => Ok(None),
        }
    }
    
    /// 计算二元运算的常量值
    fn compute_binary_operation(&self, left: &ConstantValue, op: &BinaryOperator, right: &ConstantValue) -> Result<ConstantValue, String> {
        match (left, op, right) {
            (ConstantValue::Integer(a), BinaryOperator::Add, ConstantValue::Integer(b)) => {
                Ok(ConstantValue::Integer(a + b))
            }
            (ConstantValue::Integer(a), BinaryOperator::Subtract, ConstantValue::Integer(b)) => {
                Ok(ConstantValue::Integer(a - b))
            }
            (ConstantValue::Integer(a), BinaryOperator::Multiply, ConstantValue::Integer(b)) => {
                Ok(ConstantValue::Integer(a * b))
            }
            (ConstantValue::Integer(a), BinaryOperator::Divide, ConstantValue::Integer(b)) => {
                if *b == 0 {
                    return Err("除零错误".to_string());
                }
                Ok(ConstantValue::Integer(a / b))
            }
            (ConstantValue::Integer(a), BinaryOperator::Modulo, ConstantValue::Integer(b)) => {
                if *b == 0 {
                    return Err("模零错误".to_string());
                }
                Ok(ConstantValue::Integer(a % b))
            }
            // 布尔值和整数的算术运算
            (ConstantValue::Boolean(a), BinaryOperator::Add, ConstantValue::Integer(b)) => {
                Ok(ConstantValue::Integer(if *a { 1 } else { 0 } + b))
            }
            (ConstantValue::Integer(a), BinaryOperator::Add, ConstantValue::Boolean(b)) => {
                Ok(ConstantValue::Integer(a + if *b { 1 } else { 0 }))
            }
            (ConstantValue::Boolean(a), BinaryOperator::Subtract, ConstantValue::Integer(b)) => {
                Ok(ConstantValue::Integer(if *a { 1 } else { 0 } - b))
            }
            (ConstantValue::Integer(a), BinaryOperator::Subtract, ConstantValue::Boolean(b)) => {
                Ok(ConstantValue::Integer(a - if *b { 1 } else { 0 }))
            }
            (ConstantValue::Boolean(a), BinaryOperator::Multiply, ConstantValue::Integer(b)) => {
                Ok(ConstantValue::Integer(if *a { 1 } else { 0 } * b))
            }
            (ConstantValue::Integer(a), BinaryOperator::Multiply, ConstantValue::Boolean(b)) => {
                Ok(ConstantValue::Integer(a * if *b { 1 } else { 0 }))
            }
            (ConstantValue::Boolean(a), BinaryOperator::Divide, ConstantValue::Integer(b)) => {
                if *b == 0 {
                    return Err("除零错误".to_string());
                }
                Ok(ConstantValue::Integer(if *a { 1 } else { 0 } / b))
            }
            (ConstantValue::Integer(a), BinaryOperator::Divide, ConstantValue::Boolean(b)) => {
                let b_val = if *b { 1 } else { 0 };
                if b_val == 0 {
                    return Err("除零错误".to_string());
                }
                Ok(ConstantValue::Integer(a / b_val))
            }
            (ConstantValue::Boolean(a), BinaryOperator::Modulo, ConstantValue::Integer(b)) => {
                if *b == 0 {
                    return Err("模零错误".to_string());
                }
                Ok(ConstantValue::Integer(if *a { 1 } else { 0 } % b))
            }
            (ConstantValue::Integer(a), BinaryOperator::Modulo, ConstantValue::Boolean(b)) => {
                let b_val = if *b { 1 } else { 0 };
                if b_val == 0 {
                    return Err("模零错误".to_string());
                }
                Ok(ConstantValue::Integer(a % b_val))
            }
            (ConstantValue::Integer(a), BinaryOperator::Equal, ConstantValue::Integer(b)) => {
                Ok(ConstantValue::Boolean(a == b))
            }
            (ConstantValue::Integer(a), BinaryOperator::NotEqual, ConstantValue::Integer(b)) => {
                Ok(ConstantValue::Boolean(a != b))
            }
            (ConstantValue::Integer(a), BinaryOperator::Less, ConstantValue::Integer(b)) => {
                Ok(ConstantValue::Boolean(a < b))
            }
            (ConstantValue::Integer(a), BinaryOperator::LessEqual, ConstantValue::Integer(b)) => {
                Ok(ConstantValue::Boolean(a <= b))
            }
            (ConstantValue::Integer(a), BinaryOperator::Greater, ConstantValue::Integer(b)) => {
                Ok(ConstantValue::Boolean(a > b))
            }
            (ConstantValue::Integer(a), BinaryOperator::GreaterEqual, ConstantValue::Integer(b)) => {
                Ok(ConstantValue::Boolean(a >= b))
            }
            (ConstantValue::Boolean(a), BinaryOperator::And, ConstantValue::Boolean(b)) => {
                Ok(ConstantValue::Boolean(*a && *b))
            }
            (ConstantValue::Boolean(a), BinaryOperator::Or, ConstantValue::Boolean(b)) => {
                Ok(ConstantValue::Boolean(*a || *b))
            }
            // 整数和布尔值的逻辑运算
            (ConstantValue::Integer(a), BinaryOperator::And, ConstantValue::Boolean(b)) => {
                Ok(ConstantValue::Boolean(*a != 0 && *b))
            }
            (ConstantValue::Boolean(a), BinaryOperator::And, ConstantValue::Integer(b)) => {
                Ok(ConstantValue::Boolean(*a && *b != 0))
            }
            (ConstantValue::Integer(a), BinaryOperator::Or, ConstantValue::Boolean(b)) => {
                Ok(ConstantValue::Boolean(*a != 0 || *b))
            }
            (ConstantValue::Boolean(a), BinaryOperator::Or, ConstantValue::Integer(b)) => {
                Ok(ConstantValue::Boolean(*a || *b != 0))
            }
            // 整数和整数的逻辑运算
            (ConstantValue::Integer(a), BinaryOperator::And, ConstantValue::Integer(b)) => {
                Ok(ConstantValue::Boolean(*a != 0 && *b != 0))
            }
            (ConstantValue::Integer(a), BinaryOperator::Or, ConstantValue::Integer(b)) => {
                Ok(ConstantValue::Boolean(*a != 0 || *b != 0))
            }
            _ => Err("不支持的二元运算类型组合".to_string()),
        }
    }
    
    /// 计算一元运算的常量值
    fn compute_unary_operation(&self, op: &UnaryOperator, operand: &ConstantValue) -> Result<ConstantValue, String> {
        match (op, operand) {
            (UnaryOperator::Minus, ConstantValue::Integer(a)) => {
                Ok(ConstantValue::Integer(-a))
            }
            (UnaryOperator::Not, ConstantValue::Boolean(a)) => {
                Ok(ConstantValue::Boolean(!a))
            }
            (UnaryOperator::Not, ConstantValue::Integer(a)) => {
                Ok(ConstantValue::Boolean(*a == 0))
            }
            (UnaryOperator::BitwiseNot, ConstantValue::Integer(a)) => {
                Ok(ConstantValue::Integer(!a))
            }
            _ => Err("不支持的一元运算类型组合".to_string()),
        }
    }
    

}

impl OptimizationPass for ConstantOptimizationPass {
    fn run(&mut self, program: &mut crate::TACIR::TACProgram) -> Result<OptimizationResult, String> {
        self.run(program)
    }
    
    fn name(&self) -> &str {
        "ConstantOptimizationPass"
    }
    
    fn get_stats(&self) -> &OptimizationStats {
        &self.stats
    }
}

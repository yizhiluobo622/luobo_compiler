use crate::TACIR::tacir::{TACProgram, TACFunction, BasicBlock, TACInstruction, Operand, ConstantValue, BinaryOperator, UnaryOperator};
use super::{OptimizationPass, OptimizationResult, OptimizationStats};
use std::collections::{HashMap, HashSet, VecDeque, BTreeMap};

/// 常量优化错误类型
#[derive(Debug)]
pub enum ConstantOptimizationError {
    DivisionByZero,
    ModuloByZero,
    UnsupportedOperation(String),
    DataFlowAnalysisFailed(String),
    InvalidOperand(String),
}

impl std::fmt::Display for ConstantOptimizationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::DivisionByZero => write!(f, "除零错误"),
            Self::ModuloByZero => write!(f, "模零错误"),
            Self::UnsupportedOperation(op) => write!(f, "不支持的运算: {}", op),
            Self::DataFlowAnalysisFailed(msg) => write!(f, "数据流分析失败: {}", msg),
            Self::InvalidOperand(msg) => write!(f, "无效操作数: {}", msg),
        }
    }
}

impl std::error::Error for ConstantOptimizationError {}

/// 常量状态 - 使用HashMap保持兼容性
#[derive(Debug, Clone, PartialEq, Default)]
struct ConstantState {
    variable_constants: HashMap<String, ConstantValue>,
    temp_constants: HashMap<usize, ConstantValue>,
}

impl ConstantState {
    fn new() -> Self {
        Self {
            variable_constants: HashMap::new(),
            temp_constants: HashMap::new(),
        }
    }
    
    /// 计算两个状态的交集
    fn intersect(&self, other: &Self) -> Self {
        let mut result = Self::new();
        
        // 变量常量的交集
        for (name, value) in &self.variable_constants {
            if let Some(other_value) = other.variable_constants.get(name) {
                if value == other_value {
                    result.variable_constants.insert(name.clone(), value.clone());
                }
            }
        }
        
        // 临时变量常量的交集
        for (id, value) in &self.temp_constants {
            if let Some(other_value) = other.temp_constants.get(id) {
                if value == other_value {
                    result.temp_constants.insert(*id, value.clone());
                }
            }
        }
        
        result
    }
    
    /// 增量更新状态，减少克隆开销
    fn update_incrementally(&mut self, other: &Self) {
        for (key, value) in &other.variable_constants {
            if self.variable_constants.get(key) != Some(value) {
                self.variable_constants.insert(key.clone(), value.clone());
            }
        }
        
        for (id, value) in &other.temp_constants {
            if self.temp_constants.get(id) != Some(value) {
                self.temp_constants.insert(*id, value.clone());
            }
        }
    }
    
    /// 检查状态是否为空
    fn is_empty(&self) -> bool {
        self.variable_constants.is_empty() && self.temp_constants.is_empty()
    }
}

/// 数据流分析框架 - 使用工作列表算法提高性能
struct DataFlowAnalysis {
    // 每个基本块的入口和出口状态
    in_states: HashMap<usize, ConstantState>,
    out_states: HashMap<usize, ConstantState>,
    // 前驱和后继关系
    predecessors: HashMap<usize, Vec<usize>>,
    successors: HashMap<usize, Vec<usize>>,
}

impl DataFlowAnalysis {
    fn new() -> Self {
        Self {
            in_states: HashMap::new(),
            out_states: HashMap::new(),
            predecessors: HashMap::new(),
            successors: HashMap::new(),
        }
    }
    
    /// 构建控制流图
    fn build_cfg(&mut self, function: &TACFunction) {
        self.predecessors.clear();
        self.successors.clear();
        
        // 初始化前驱后继关系
        for block in &function.basic_blocks {
            self.predecessors.insert(block.id, Vec::new());
            self.successors.insert(block.id, Vec::new());
        }
        
        // 建立前驱后继关系
        for block in &function.basic_blocks {
            for succ_id in &block.successors {
                if let Some(preds) = self.predecessors.get_mut(succ_id) {
                    preds.push(block.id);
                }
                if let Some(succs) = self.successors.get_mut(&block.id) {
                    succs.push(*succ_id);
                }
            }
        }
    }
    
    /// 重建IR结构，确保在优化后的一致性
    fn rebuild_ir_structure(&mut self, function: &mut TACFunction) -> Result<(), String> {
        
        // 1. 验证所有基本块ID的有效性
        self.validate_basic_block_ids(function)?;
        
        // 2. 重新构建CFG
        self.build_cfg(function);
        
        // 3. 清理无效的引用
        self.cleanup_invalid_references(function)?;
        
        // 4. 重新分配指令ID（如果需要）
        self.reassign_instruction_ids(function)?;
        
        Ok(())
    }
    
    /// 验证基本块ID的有效性
    fn validate_basic_block_ids(&self, function: &TACFunction) -> Result<(), String> {
        let mut valid_ids = HashSet::new();
        
        // 收集所有有效的基本块ID
        for block in &function.basic_blocks {
            valid_ids.insert(block.id);
        }
        
        // 验证所有引用都是有效的
        for block in &function.basic_blocks {
            for succ_id in &block.successors {
                if !valid_ids.contains(succ_id) {
                    return Err(format!("无效的基本块引用: {} -> {}", block.id, succ_id));
                }
            }
        }
        
        Ok(())
    }
    
    /// 清理无效的引用
    fn cleanup_invalid_references(&mut self, function: &mut TACFunction) -> Result<(), String> {
        let mut cleaned_count = 0;
        
        // 先收集所有有效的基本块ID
        let valid_ids: HashSet<usize> = function.basic_blocks.iter().map(|b| b.id).collect();
        
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
        }
        
        Ok(())
    }
    
    /// 重新分配指令ID
    fn reassign_instruction_ids(&mut self, function: &mut TACFunction) -> Result<(), String> {
        let mut next_id = 0;
        
        for block in &mut function.basic_blocks {
            for instruction in &mut block.instructions {
                // 为每条指令分配新的ID（如果需要）
                // 这里可以根据具体需求实现
                next_id += 1;
            }
        }
        
        Ok(())
    }
    
    /// 使用工作列表算法分析函数的数据流
    fn analyze_with_worklist(&mut self, function: &TACFunction, global_constants: &HashMap<String, ConstantValue>) -> Result<(), String> {
        // 初始化前驱后继关系
        self.build_cfg(function);
        
        // 初始化入口基本块的状态
        if let Some(entry_block) = function.basic_blocks.first() {
            let mut entry_state = ConstantState::new();
            entry_state.variable_constants = global_constants.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
            self.in_states.insert(entry_block.id, entry_state);
        }
        
        // 使用工作列表算法，只处理变化的基本块
        let mut worklist: VecDeque<usize> = VecDeque::new();
        
        // 初始化工作列表：从入口基本块开始
        if let Some(entry_block) = function.basic_blocks.first() {
            worklist.push_back(entry_block.id);
        }
        
        let mut iterations = 0;
        const MAX_ITERATIONS: usize = 50; // 增加最大迭代次数
        
        while let Some(block_id) = worklist.pop_front() {
            if iterations >= MAX_ITERATIONS {
                break;
            }
            iterations += 1;
            
            // 计算入口状态（前驱基本块出口状态的交集）
            let old_in_state = self.in_states.get(&block_id).cloned().unwrap_or_default();
            let new_in_state = self.compute_in_state(block_id, global_constants);
            
            let mut state_changed = false;
            
            if new_in_state != old_in_state {
                self.in_states.insert(block_id, new_in_state.clone());
                state_changed = true;
            }
            
            // 基于入口状态计算出口状态
            let old_out_state = self.out_states.get(&block_id).cloned().unwrap_or_default();
            
            // 安全获取基本块，避免unwrap崩溃
            let basic_block = match function.get_basic_block(block_id) {
                Some(block) => block,
                None => {
                    continue;
                }
            };
            
            let new_out_state = self.transfer_function(basic_block, &new_in_state);
            
            if new_out_state != old_out_state {
                self.out_states.insert(block_id, new_out_state);
                state_changed = true;
            }
            
            // 如果状态变化，将后继基本块加入工作列表
            if state_changed {
                if let Some(succs) = self.successors.get(&block_id) {
                    for succ_id in succs {
                        if !worklist.contains(succ_id) {
                            worklist.push_back(*succ_id);
                        }
                    }
                }
            }
        }
        
        if iterations >= MAX_ITERATIONS {
        }
        

        Ok(())
    }
    
    /// 计算基本块入口状态
    fn compute_in_state(&self, block_id: usize, global_constants: &HashMap<String, ConstantValue>) -> ConstantState {
        let mut in_state = ConstantState::new();
        
        // 获取前驱基本块
        if let Some(preds) = self.predecessors.get(&block_id) {
            if preds.is_empty() {
                // 入口基本块，使用全局常量
                in_state.variable_constants = global_constants.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
            } else {
                // 计算前驱基本块出口状态的交集
                let mut has_predecessor = false;
                for pred_id in preds {
                    if let Some(pred_out_state) = self.out_states.get(pred_id) {
                        if !has_predecessor {
                            in_state = pred_out_state.clone();
                            has_predecessor = true;
                        } else {
                            in_state = in_state.intersect(pred_out_state);
                        }
                    }
                }
                
                // 如果没有前驱状态，使用全局常量
                if !has_predecessor {
                    in_state.variable_constants = global_constants.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                }
            }
        }
        
        in_state
    }
    
    /// 传输函数：基于入口状态计算出口状态
    fn transfer_function(&self, block: &BasicBlock, in_state: &ConstantState) -> ConstantState {
        let mut out_state = in_state.clone();
        
        for instruction in &block.instructions {
            match instruction {
                TACInstruction::Assign { target, source } => {
                    // 使用in_state来解析源操作数（不是out_state！）
                    if let Some(constant_value) = self.resolve_operand_to_constant(source, in_state) {
                        self.update_target_in_state(&mut out_state, target, constant_value);
                    } else {
                        self.clear_target_in_state(&mut out_state, target);
                    }
                }
                TACInstruction::BinaryOp { target, left, op, right } => {
                    // 使用in_state来解析操作数
                    if let (Some(left_const), Some(right_const)) = (
                        self.resolve_operand_to_constant(left, in_state),
                        self.resolve_operand_to_constant(right, in_state)
                    ) {
                        if let Ok(result_const) = self.compute_binary_operation(&left_const, op, &right_const) {
                            self.update_target_in_state(&mut out_state, target, result_const);
                            continue;
                        }
                    }
                    // 清除目标变量的常量状态
                    self.clear_target_in_state(&mut out_state, target);
                }
                TACInstruction::UnaryOp { target, op, operand } => {
                    // 支持一元运算的常量折叠
                    if let Some(operand_const) = self.resolve_operand_to_constant(operand, in_state) {
                        if let Ok(result_const) = self.compute_unary_operation(op, &operand_const) {
                            self.update_target_in_state(&mut out_state, target, result_const);
                            continue;
                        }
                    }
                    self.clear_target_in_state(&mut out_state, target);
                }
                _ => {
                    // 其他指令清除目标变量的常量状态
                    if let Some(target) = self.get_instruction_target(instruction) {
                        self.clear_target_in_state(&mut out_state, &target);
                    }
                }
            }
        }
        
        out_state
    }
    
    /// 更新状态中的目标变量
    fn update_target_in_state(&self, state: &mut ConstantState, target: &Operand, value: ConstantValue) {
        match target {
            Operand::Variable(name) => {
                state.variable_constants.insert(name.clone(), value);
            }
            Operand::Temp(id) => {
                state.temp_constants.insert(*id, value);
            }
            _ => {}
        }
    }
    
    /// 清除状态中的目标变量
    fn clear_target_in_state(&self, state: &mut ConstantState, target: &Operand) {
        match target {
            Operand::Variable(name) => {
                state.variable_constants.remove(name);
            }
            Operand::Temp(id) => {
                state.temp_constants.remove(id);
            }
            _ => {}
        }
    }
    
    /// 解析操作数为常量值
    fn resolve_operand_to_constant(&self, operand: &Operand, state: &ConstantState) -> Option<ConstantValue> {
        match operand {
            Operand::Constant(value) => Some(value.clone()),
            Operand::Temp(id) => state.temp_constants.get(id).cloned(),
            Operand::Variable(name) => state.variable_constants.get(name).cloned(),
            _ => None,
        }
    }
    
    /// 获取指令的目标操作数
    fn get_instruction_target(&self, instruction: &TACInstruction) -> Option<Operand> {
        match instruction {
            TACInstruction::Assign { target, .. } => Some(target.clone()),
            TACInstruction::BinaryOp { target, .. } => Some(target.clone()),
            TACInstruction::UnaryOp { target, .. } => Some(target.clone()),
            TACInstruction::FunctionCall { target, .. } => Some(target.clone()),
            TACInstruction::GetElementPtr { target, .. } => Some(target.clone()),
            TACInstruction::Load { target, .. } => Some(target.clone()),
            TACInstruction::Allocate { target, .. } => Some(target.clone()),
            _ => None,
        }
    }
    
    /// 计算二元运算的常量值
    fn compute_binary_operation(&self, left: &ConstantValue, op: &BinaryOperator, right: &ConstantValue) -> Result<ConstantValue, ConstantOptimizationError> {
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
                    return Err(ConstantOptimizationError::DivisionByZero);
                }
                Ok(ConstantValue::Integer(a / b))
            }
            (ConstantValue::Integer(a), BinaryOperator::Modulo, ConstantValue::Integer(b)) => {
                if *b == 0 {
                    return Err(ConstantOptimizationError::ModuloByZero);
                }
                Ok(ConstantValue::Integer(a % b))
            }
            _ => Err(ConstantOptimizationError::UnsupportedOperation(
                format!("不支持的二元运算: {:?} {:?} {:?}", left, op, right)
            )),
        }
    }
    
    /// 计算一元运算的常量值
    fn compute_unary_operation(&self, op: &UnaryOperator, operand: &ConstantValue) -> Result<ConstantValue, ConstantOptimizationError> {
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
            _ => Err(ConstantOptimizationError::UnsupportedOperation(
                format!("不支持的一元运算: {:?} {:?}", op, operand)
            )),
        }
    }
}

/// 常量优化Pass
/// 基于正确数据流分析的智能常量传播和折叠算法
pub struct ConstantOptimizationPass {
    stats: OptimizationStats,
    /// 全局常量映射
    global_constants: HashMap<String, ConstantValue>,
    /// 当前基本块的常量状态
    variable_constants: HashMap<String, ConstantValue>,
    temp_constants: HashMap<usize, ConstantValue>,
    /// 常量折叠统计
    constant_foldings: usize,
    /// 常量传播统计
    constant_propagations: usize,
}

impl ConstantOptimizationPass {
    pub fn new() -> Self {
        Self {
            stats: OptimizationStats::new(),
            global_constants: HashMap::new(),
            variable_constants: HashMap::new(),
            temp_constants: HashMap::new(),
            constant_foldings: 0,
            constant_propagations: 0,
        }
    }
    
    /// 运行常量优化 - 支持多轮优化处理链式依赖
    pub fn run(&mut self, program: &mut TACProgram) -> Result<OptimizationResult, String> {
        
        let mut result = OptimizationResult::new();
        let mut total_optimizations = 0;
        let mut round = 0;
        const MAX_ROUNDS: usize = 5; // 支持多轮优化
        
        // 1. 初始化全局常量
        self.initialize_global_constants(program);
        
        // 2. 多轮优化，处理链式依赖
        while round < MAX_ROUNDS {
            let mut round_optimizations = 0;
            round += 1;
            

            
            // 对每个函数进行优化
            for function in &mut program.functions {
                // 执行数据流分析
                let mut dataflow = DataFlowAnalysis::new();
                dataflow.analyze_with_worklist(function, &self.global_constants)?;
                
                // 基于数据流分析结果优化每个基本块
                for block in &mut function.basic_blocks {
                    // 获取基本块的入口常量状态
                    let in_state = dataflow.in_states.get(&block.id)
                        .cloned()
                        .unwrap_or_else(|| {
                            let mut state = ConstantState::new();
                            state.variable_constants = self.global_constants.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                            state
                        });
                    
                    // 优化基本块（使用入口状态）
                    let optimizations = self.optimize_block_with_state(block, &in_state)?;
                    round_optimizations += optimizations;
                }
            }
            
            total_optimizations += round_optimizations;

            
            // 如果没有新的优化，说明已经收敛
            if round_optimizations == 0 {
                break;
            }
        }
        
        // 显示优化结果
        
        // 更新统计信息
        self.stats.constant_foldings = self.constant_foldings;
        self.stats.optimization_rounds = round;
        
        if total_optimizations > 0 {
            result.mark_optimized();
            result.instructions_optimized = total_optimizations;
        }
        
        Ok(result)
    }
    
    /// 初始化全局常量
    fn initialize_global_constants(&mut self, program: &TACProgram) {
        for (var_name, var_type, initial_value, is_const) in &program.global_variables {
            // 只有真正的常量变量才被当作常量
            if *is_const {
                if let Some(operand) = initial_value {
                    if let Operand::Constant(constant_value) = operand {
                        self.global_constants.insert(var_name.clone(), constant_value.clone());
                    }
                }
            }
        }
    }
    
    /// 使用指定状态优化基本块
    fn optimize_block_with_state(&mut self, block: &mut BasicBlock, in_state: &ConstantState) -> Result<usize, String> {
        let mut optimizations = 0;
        let mut i = 0;
        
        while i < block.instructions.len() {
            let instruction = block.instructions[i].clone();
            
            // 使用入口状态来尝试优化（不是当前状态！）
            if let Some(folded) = self.try_constant_folding_with_state(&instruction, &in_state.variable_constants, &in_state.temp_constants)? {
                let folded_clone = folded.clone();
                block.instructions[i] = folded;
                optimizations += 1;
                self.constant_foldings += 1;

            }
            else if let Some(propagated) = self.try_constant_propagation_with_state(&instruction, &in_state.variable_constants, &in_state.temp_constants)? {
                let propagated_clone = propagated.clone();
                block.instructions[i] = propagated;
                optimizations += 1;
                self.constant_propagations += 1;
            }
            
            // 更新当前状态（用于后续指令）
            self.update_constants_from_instruction(&block.instructions[i]);
            i += 1;
        }
        
        // 死代码消除
        optimizations += self.eliminate_dead_code(block)?;
        
        Ok(optimizations)
    }
    
    /// 使用指定状态进行常量折叠 - 支持更多类型
    fn try_constant_folding_with_state(
        &self, 
        instruction: &TACInstruction, 
        variable_constants: &HashMap<String, ConstantValue>,
        temp_constants: &HashMap<usize, ConstantValue>
    ) -> Result<Option<TACInstruction>, String> {
        match instruction {
            TACInstruction::BinaryOp { target, left, op, right } => {
                let left_const = self.resolve_operand_to_constant_with_state(left, variable_constants, temp_constants)?;
                let right_const = self.resolve_operand_to_constant_with_state(right, variable_constants, temp_constants)?;
                
                if let (Some(left_val), Some(right_val)) = (left_const, right_const) {
                    let result = self.compute_binary_operation(&left_val, op, &right_val)?;
                    Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: Operand::Constant(result),
                    }))
                } else {
                    Ok(None)
                }
            }
            TACInstruction::UnaryOp { target, op, operand } => {
                // 支持一元运算的常量折叠
                if let Some(operand_const) = self.resolve_operand_to_constant_with_state(operand, variable_constants, temp_constants)? {
                    let result = self.compute_unary_operation(op, &operand_const)?;
                    Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: Operand::Constant(result),
                    }))
                } else {
                    Ok(None)
                }
            }
            _ => Ok(None),
        }
    }
    
    /// 使用指定状态进行常量传播
    fn try_constant_propagation_with_state(
        &self, 
        instruction: &TACInstruction, 
        variable_constants: &HashMap<String, ConstantValue>,
        temp_constants: &HashMap<usize, ConstantValue>
    ) -> Result<Option<TACInstruction>, String> {
        match instruction {
            TACInstruction::Assign { target, source } => {
                // 如果源操作数是常量，直接传播
                if let Operand::Constant(const_val) = source {
                    return Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: Operand::Constant(const_val.clone()),
                    }));
                }
                
                // 如果源操作数是已知的常量变量或临时变量
                if let Some(const_val) = self.resolve_operand_to_constant_with_state(source, variable_constants, temp_constants)? {
                    return Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: Operand::Constant(const_val),
                    }));
                }
                
                Ok(None)
            }
            _ => Ok(None),
        }
    }
    
    /// 使用指定状态解析操作数
    fn resolve_operand_to_constant_with_state(
        &self, 
        operand: &Operand, 
        variable_constants: &HashMap<String, ConstantValue>,
        temp_constants: &HashMap<usize, ConstantValue>
    ) -> Result<Option<ConstantValue>, String> {
        match operand {
            Operand::Constant(value) => Ok(Some(value.clone())),
            Operand::Temp(id) => Ok(temp_constants.get(id).cloned()),
            Operand::Variable(name) => Ok(variable_constants.get(name).cloned()),
            _ => Ok(None),
        }
    }
    
    /// 解析操作数为常量值（使用当前状态）
    fn resolve_operand_to_constant(&self, operand: &Operand) -> Result<Option<ConstantValue>, String> {
        match operand {
            Operand::Constant(value) => Ok(Some(value.clone())),
            Operand::Temp(id) => Ok(self.temp_constants.get(id).cloned()),
            Operand::Variable(name) => Ok(self.variable_constants.get(name).cloned()),
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
    
    /// 从指令更新常量映射
    fn update_constants_from_instruction(&mut self, instruction: &TACInstruction) {
        match instruction {
            TACInstruction::Assign { target, source } => {
                if let Operand::Constant(value) = source {
                    self.update_target_constant(target, value.clone());
                } else if let Some(constant_value) = self.resolve_operand_to_constant(source).unwrap_or(None) {
                    self.update_target_constant(target, constant_value);
                } else {
                    self.kill_target_constant(target);
                }
            }
            TACInstruction::BinaryOp { target, left, op, right } => {
                if let (Some(left_const), Some(right_const)) = (
                    self.resolve_operand_to_constant(left).unwrap_or(None),
                    self.resolve_operand_to_constant(right).unwrap_or(None)
                ) {
                    if let Ok(result_const) = self.compute_binary_operation(&left_const, op, &right_const) {
                        self.update_target_constant(target, result_const);
                        return;
                    }
                }
                self.kill_target_constant(target);
            }
            _ => {
                if let Some(target) = self.get_instruction_target(instruction) {
                    self.kill_target_constant(&target);
                }
            }
        }
    }
    
    /// 更新目标操作数的常量值
    fn update_target_constant(&mut self, target: &Operand, constant_value: ConstantValue) {
        match target {
            Operand::Variable(name) => {
                self.variable_constants.insert(name.clone(), constant_value);
            }
            Operand::Temp(id) => {
                self.temp_constants.insert(*id, constant_value);
            }
            _ => {}
        }
    }
    
    /// 清除目标操作数的常量值
    fn kill_target_constant(&mut self, target: &Operand) {
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
    
    /// 获取指令的目标操作数
    fn get_instruction_target(&self, instruction: &TACInstruction) -> Option<Operand> {
        match instruction {
            TACInstruction::Assign { target, .. } => Some(target.clone()),
            TACInstruction::BinaryOp { target, .. } => Some(target.clone()),
            TACInstruction::UnaryOp { target, .. } => Some(target.clone()),
            TACInstruction::FunctionCall { target, .. } => Some(target.clone()),
            TACInstruction::GetElementPtr { target, .. } => Some(target.clone()),
            TACInstruction::Load { target, .. } => Some(target.clone()),
            TACInstruction::Allocate { target, .. } => Some(target.clone()),
            _ => None,
        }
    }
    
    /// 死代码消除
    fn eliminate_dead_code(&self, block: &mut BasicBlock) -> Result<usize, String> {
        let mut eliminations = 0;
        let mut i = 0;
        
        while i < block.instructions.len() {
            let instruction = &block.instructions[i];
            
            // 检查是否是死代码（赋值给临时变量但后续没有被使用）
            if self.is_dead_assignment(instruction, block, i) {

                block.instructions.remove(i);
                eliminations += 1;
            } else {
                i += 1;
            }
        }
        
        Ok(eliminations)
    }
    
    /// 检查是否是死赋值
    fn is_dead_assignment(&self, instruction: &TACInstruction, block: &BasicBlock, current_index: usize) -> bool {
        match instruction {
            TACInstruction::Assign { target, .. } => {
                // 只检查临时变量的死赋值
                if let Operand::Temp(target_id) = target {
                    // 检查后续指令是否使用这个临时变量
                    for j in (current_index + 1)..block.instructions.len() {
                        let next_instruction = &block.instructions[j];
                        if self.instruction_uses_temp(next_instruction, *target_id) {
                            return false; // 被使用了，不是死代码
                        }
                    }
                    return true; // 没有被使用，是死代码
                }
            }
            _ => {}
        }
        false
    }
    
    /// 检查指令是否使用指定的临时变量
    fn instruction_uses_temp(&self, instruction: &TACInstruction, temp_id: usize) -> bool {
        match instruction {
            TACInstruction::Assign { source, .. } => {
                if let Operand::Temp(id) = source {
                    return *id == temp_id;
                }
            }
            TACInstruction::BinaryOp { left, right, .. } => {
                if let Operand::Temp(id) = left {
                    if *id == temp_id {
                        return true;
                    }
                }
                if let Operand::Temp(id) = right {
                    if *id == temp_id {
                        return true;
                    }
                }
            }
            TACInstruction::UnaryOp { operand, .. } => {
                if let Operand::Temp(id) = operand {
                    return *id == temp_id;
                }
            }
            TACInstruction::FunctionCall { arguments, .. } => {
                for arg in arguments {
                    if let Operand::Temp(id) = arg {
                        if *id == temp_id {
                            return true;
                        }
                    }
                }
            }
            TACInstruction::Return { value } => {
                if let Some(val) = value {
                    if let Operand::Temp(id) = val {
                        return *id == temp_id;
                    }
                }
            }
            _ => {}
        }
        false
    }
}

impl OptimizationPass for ConstantOptimizationPass {
    fn run(&mut self, program: &mut crate::TACIR::TACProgram) -> Result<OptimizationResult, String> {
        // 重置统计和状态
        self.stats = OptimizationStats::new();
        self.global_constants.clear();
        self.variable_constants.clear();
        self.temp_constants.clear();
        self.constant_foldings = 0;
        self.constant_propagations = 0;
        
        // 调用优化逻辑
        self.run_optimization(program)
    }
    
    fn name(&self) -> &str {
        "ConstantOptimizationPass"
    }
    
    fn get_stats(&self) -> &OptimizationStats {
        &self.stats
    }
}

impl ConstantOptimizationPass {
    /// 将原有的run逻辑移到新方法中，避免递归调用
    fn run_optimization(&mut self, program: &mut TACProgram) -> Result<OptimizationResult, String> {
        
        let mut result = OptimizationResult::new();
        let mut total_optimizations = 0;
        let mut round = 0;
        const MAX_ROUNDS: usize = 5; // 支持多轮优化
        
        // 1. 初始化全局常量
        self.initialize_global_constants(program);
        
        // 2. 多轮优化，处理链式依赖
        while round < MAX_ROUNDS {
            let mut round_optimizations = 0;
            round += 1;
            

            
            // 对每个函数进行优化
            for function in &mut program.functions {
                // 执行数据流分析
                let mut dataflow = DataFlowAnalysis::new();
                
                // 在数据流分析前重建IR结构
                dataflow.rebuild_ir_structure(function)?;
                
                dataflow.analyze_with_worklist(function, &self.global_constants)?;
                
                // 基于数据流分析结果优化每个基本块
                for block in &mut function.basic_blocks {
                    // 获取基本块的入口常量状态
                    let in_state = dataflow.in_states.get(&block.id)
                        .cloned()
                        .unwrap_or_else(|| {
                            let mut state = ConstantState::new();
                            state.variable_constants = self.global_constants.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                            state
                        });
                    
                    // 优化基本块（使用入口状态）
                    let optimizations = self.optimize_block_with_state(block, &in_state)?;
                    round_optimizations += optimizations;
                }
            }
            
            total_optimizations += round_optimizations;

            
            // 如果没有新的优化，说明已经收敛
            if round_optimizations == 0 {
                break;
            }
        }
        
        // 显示优化结果
        
        // 更新统计信息
        self.stats.constant_foldings = self.constant_foldings;
        self.stats.optimization_rounds = round;
        
        if total_optimizations > 0 {
            result.mark_optimized();
            result.instructions_optimized = total_optimizations;
        }
        
        Ok(result)
    }

    fn run_constant_propagation(&mut self, function: &mut TACFunction) -> Result<usize, String> {
        let mut optimized_count = 0;
        let mut round = 0;
        
        loop {
            round += 1;
            let mut changed = false;
            
            // 数据流分析
            let mut constant_values = HashMap::new();
            
            // 初始化常量值
            for block in &function.basic_blocks {
                for instruction in &block.instructions {
                    if let TACInstruction::Assign { target, source } = instruction {
                        if let Operand::Constant(constant) = source {
                            if let Operand::Temp(temp_id) = target {
                                constant_values.insert(*temp_id, constant.clone());
                            }
                        }
                    }
                }
            }
            
            // 传播常量值
            for block in &mut function.basic_blocks {
                for instruction in &mut block.instructions {
                    match instruction {
                        TACInstruction::Assign { target, source } => {
                            if let Operand::Temp(temp_id) = source {
                                if let Some(constant) = constant_values.get(temp_id) {
                                    *source = Operand::Constant(constant.clone());
                                    changed = true;
                                    optimized_count += 1;
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            
            if !changed {
                break;
            }
        }
        
        if optimized_count > 0 {
        }
        
        Ok(optimized_count)
    }
}

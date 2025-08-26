use crate::TACIR::tacir::{TACProgram, TACFunction, BasicBlock, TACInstruction, Operand, ConstantValue, BinaryOperator, UnaryOperator};
use super::{OptimizationPass, OptimizationResult, OptimizationStats};
use std::collections::HashMap;

/// 代数优化错误类型
#[derive(Debug)]
pub enum AlgebraicOptimizationError {
    UnsupportedOperation(String),
    InvalidOperand(String),
    OptimizationFailed(String),
}

impl std::fmt::Display for AlgebraicOptimizationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            AlgebraicOptimizationError::UnsupportedOperation(op) => write!(f, "不支持的操作: {}", op),
            AlgebraicOptimizationError::InvalidOperand(op) => write!(f, "无效的操作数: {}", op),
            AlgebraicOptimizationError::OptimizationFailed(msg) => write!(f, "优化失败: {}", msg),
        }
    }
}

impl std::error::Error for AlgebraicOptimizationError {}

/// 代数优化Pass - 参考LLVM InstCombine的设计
/// 实现各种代数恒等式和简化规则，包括分配律、结合律等
pub struct AlgebraicOptimizationPass {
    stats: OptimizationStats,
    /// 代数优化统计
    algebraic_optimizations: usize,
    /// 强度削减统计
    strength_reductions: usize,
    /// 恒等式消除统计
    identity_eliminations: usize,
    /// 分配律优化统计
    distributive_optimizations: usize,
    /// 因式分解优化统计
    factorization_optimizations: usize,
    /// 临时变量映射，用于跟踪指令间的依赖关系
    temp_to_instruction: HashMap<String, TACInstruction>,
}

impl AlgebraicOptimizationPass {
    pub fn new() -> Self {
        Self {
            stats: OptimizationStats::new(),
            algebraic_optimizations: 0,
            strength_reductions: 0,
            identity_eliminations: 0,
            distributive_optimizations: 0,
            factorization_optimizations: 0,
            temp_to_instruction: HashMap::new(),
        }
    }
    
    /// 运行代数优化
    pub fn run(&mut self, program: &mut TACProgram) -> Result<OptimizationResult, String> {
        println!("🧮 开始代数优化...");
        
        let mut result = OptimizationResult::new();
        let mut total_optimizations = 0;
        let mut round = 0;
        const MAX_ROUNDS: usize = 3; // 代数优化通常不需要太多轮次
        
        // 多轮优化，处理嵌套的代数表达式
        while round < MAX_ROUNDS {
            let mut round_optimizations = 0;
            round += 1;
            
            println!("📊 代数优化第 {} 轮...", round);
            
            // 对每个函数进行优化
            for function in &mut program.functions {
                for block in &mut function.basic_blocks {
                    let optimizations = self.optimize_block(block)?;
                    round_optimizations += optimizations;
                }
            }
            
            total_optimizations += round_optimizations;
            
            // 如果本轮没有优化，提前结束
            if round_optimizations == 0 {
                println!("✅ 代数优化第 {} 轮无变化，提前结束", round);
                break;
            }
            
            println!("📈 代数优化第 {} 轮完成，优化了 {} 条指令", round, round_optimizations);
        }
        
        // 更新统计信息
        self.stats.optimization_rounds = round;
        self.stats.algebraic_optimizations = self.algebraic_optimizations;
        
        if total_optimizations > 0 {
            result.mark_optimized();
            result.instructions_optimized = total_optimizations;
            println!("✅ 代数优化完成！总共优化了 {} 条指令", total_optimizations);
            println!("📊 代数优化统计: 代数简化 {}, 强度削减 {}, 恒等式消除 {}", 
                    self.algebraic_optimizations, self.strength_reductions, self.identity_eliminations);
        } else {
            println!("ℹ️ 代数优化未发现可优化的指令");
        }
        
        Ok(result)
    }
    
    /// 优化基本块
    fn optimize_block(&mut self, block: &mut BasicBlock) -> Result<usize, String> {
        let mut optimizations = 0;
        let mut i = 0;
        
        while i < block.instructions.len() {
            let instruction = &block.instructions[i].clone();
            
            if let Some(optimized_instruction) = self.try_algebraic_optimization(instruction)? {
                block.instructions[i] = optimized_instruction;
                optimizations += 1;
                self.algebraic_optimizations += 1;
            }
            
            i += 1;
        }
        
        Ok(optimizations)
    }
    
    /// 尝试代数优化
    fn try_algebraic_optimization(&mut self, instruction: &TACInstruction) -> Result<Option<TACInstruction>, String> {
        match instruction {
            TACInstruction::BinaryOp { target, left, op, right } => {
                // 尝试各种代数优化规则
                if let Some(optimized) = self.try_identity_rules(target, left, op, right)? {
                    self.identity_eliminations += 1;
                    return Ok(Some(optimized));
                }
                
                if let Some(optimized) = self.try_strength_reduction(target, left, op, right)? {
                    self.strength_reductions += 1;
                    return Ok(Some(optimized));
                }
                
                if let Some(optimized) = self.try_algebraic_simplification(target, left, op, right)? {
                    return Ok(Some(optimized));
                }
            }
            TACInstruction::UnaryOp { target, op, operand } => {
                if let Some(optimized) = self.try_unary_optimization(target, op, operand)? {
                    return Ok(Some(optimized));
                }
            }
            _ => {}
        }
        
        Ok(None)
    }
    
    /// 恒等式规则优化
    /// 例如: x + 0 = x, x * 1 = x, x - 0 = x, x / 1 = x
    fn try_identity_rules(&self, target: &Operand, left: &Operand, op: &BinaryOperator, right: &Operand) -> Result<Option<TACInstruction>, String> {
        match op {
            BinaryOperator::Add => {
                // x + 0 = x
                if self.is_zero(right) {
                    return Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: left.clone(),
                    }));
                }
                // 0 + x = x
                if self.is_zero(left) {
                    return Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: right.clone(),
                    }));
                }
            }
            BinaryOperator::Subtract => {
                // x - 0 = x
                if self.is_zero(right) {
                    return Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: left.clone(),
                    }));
                }
                // x - x = 0 (如果操作数相同)
                if self.operands_equal(left, right) {
                    return Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: Operand::Constant(ConstantValue::Integer(0)),
                    }));
                }
            }
            BinaryOperator::Multiply => {
                // x * 1 = x
                if self.is_one(right) {
                    return Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: left.clone(),
                    }));
                }
                // 1 * x = x
                if self.is_one(left) {
                    return Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: right.clone(),
                    }));
                }
                // x * 0 = 0
                if self.is_zero(right) || self.is_zero(left) {
                    return Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: Operand::Constant(ConstantValue::Integer(0)),
                    }));
                }
            }
            BinaryOperator::Divide => {
                // x / 1 = x
                if self.is_one(right) {
                    return Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: left.clone(),
                    }));
                }
                // x / x = 1 (如果操作数相同且非零)
                if self.operands_equal(left, right) && !self.is_zero(left) {
                    return Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: Operand::Constant(ConstantValue::Integer(1)),
                    }));
                }
            }
            BinaryOperator::And => {
                // x && true = x
                if self.is_true(right) {
                    return Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: left.clone(),
                    }));
                }
                // true && x = x
                if self.is_true(left) {
                    return Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: right.clone(),
                    }));
                }
                // x && false = false
                if self.is_false(right) || self.is_false(left) {
                    return Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: Operand::Constant(ConstantValue::Boolean(false)),
                    }));
                }
            }
            BinaryOperator::Or => {
                // x || false = x
                if self.is_false(right) {
                    return Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: left.clone(),
                    }));
                }
                // false || x = x
                if self.is_false(left) {
                    return Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: right.clone(),
                    }));
                }
                // x || true = true
                if self.is_true(right) || self.is_true(left) {
                    return Ok(Some(TACInstruction::Assign {
                        target: target.clone(),
                        source: Operand::Constant(ConstantValue::Boolean(true)),
                    }));
                }
            }
            _ => {}
        }
        
        Ok(None)
    }
    
    /// 强度削减优化 - 参考LLVM InstCombine的实现
    /// 例如: x * 2 = x + x, x * 4 = x << 2, x + x = x << 1
    fn try_strength_reduction(&self, target: &Operand, left: &Operand, op: &BinaryOperator, right: &Operand) -> Result<Option<TACInstruction>, String> {
        match op {
            BinaryOperator::Add => {
                // LLVM规则: X + X → X << 1 (左移比加法更高效)
                if self.operands_equal(left, right) {
                    // 这里简化为 X * 2，实际可以生成左移指令
                    return Ok(Some(TACInstruction::BinaryOp {
                        target: target.clone(),
                        left: left.clone(),
                        op: BinaryOperator::Multiply,
                        right: Operand::Constant(ConstantValue::Integer(2)),
                    }));
                }
            }
            BinaryOperator::Multiply => {
                // x * 2 = x + x (加法通常比乘法快)
                if let Operand::Constant(ConstantValue::Integer(2)) = right {
                    return Ok(Some(TACInstruction::BinaryOp {
                        target: target.clone(),
                        left: left.clone(),
                        op: BinaryOperator::Add,
                        right: left.clone(),
                    }));
                }
                // 2 * x = x + x
                if let Operand::Constant(ConstantValue::Integer(2)) = left {
                    return Ok(Some(TACInstruction::BinaryOp {
                        target: target.clone(),
                        left: right.clone(),
                        op: BinaryOperator::Add,
                        right: right.clone(),
                    }));
                }
                
                // LLVM规则: 乘以2的幂次转换为左移
                if let Some(power) = self.get_power_of_two(right) {
                    if power <= 31 && power > 1 { // power=1已经在上面处理
                        return self.generate_power_of_two_multiplication(target, left, power);
                    }
                }
                if let Some(power) = self.get_power_of_two(left) {
                    if power <= 31 && power > 1 {
                        return self.generate_power_of_two_multiplication(target, right, power);
                    }
                }
            }
            BinaryOperator::Divide => {
                // x / 2 可以优化为右移（对于正数）
                if let Some(power) = self.get_power_of_two(right) {
                    if power <= 31 && power > 0 {
                        // 简化处理：保持除法，实际编译器会生成算术右移
                        // 因为需要正确处理负数的符号扩展
                    }
                }
            }
            _ => {}
        }
        
        Ok(None)
    }
    
    /// 代数简化 - 参考LLVM的分配律和结合律优化
    /// 例如: (x + y) - x = y, (x * y) / x = y, A & (B | C) → (A&B) | (A&C)
    fn try_algebraic_simplification(&self, target: &Operand, left: &Operand, op: &BinaryOperator, right: &Operand) -> Result<Option<TACInstruction>, String> {
        match op {
            BinaryOperator::Add | BinaryOperator::Multiply => {
                // LLVM规则1: 交换律 - 常量操作数移到右侧便于后续优化
                if self.is_constant(left) && !self.is_constant(right) {
                    return Ok(Some(TACInstruction::BinaryOp {
                        target: target.clone(),
                        left: right.clone(),
                        op: op.clone(),
                        right: left.clone(),
                    }));
                }
            }
            BinaryOperator::Subtract => {
                // LLVM规则: (A + B) - A = B, (A + B) - B = A
                if let Some(optimized) = self.try_subtract_simplification(target, left, right)? {
                    return Ok(Some(optimized));
                }
            }
            BinaryOperator::And => {
                // LLVM分配律: A & (B | C) → (A&B) | (A&C)
                if let Some(optimized) = self.try_distributive_and(target, left, right)? {
                    return Ok(Some(optimized));
                }
            }
            BinaryOperator::Or => {
                // LLVM分配律: A | (B & C) → (A|B) & (A|C)
                if let Some(optimized) = self.try_distributive_or(target, left, right)? {
                    return Ok(Some(optimized));
                }
            }
            _ => {}
        }
        
        Ok(None)
    }
    
    /// 一元操作优化
    fn try_unary_optimization(&self, target: &Operand, op: &UnaryOperator, operand: &Operand) -> Result<Option<TACInstruction>, String> {
        match op {
            UnaryOperator::Minus => {
                // -(-x) = x
                // 这需要更复杂的分析来检测嵌套的负号
            }
            UnaryOperator::Not => {
                // !(!x) = x
                // 同样需要更复杂的分析
            }
            _ => {}
        }
        
        Ok(None)
    }
    
    /// 生成2的幂次乘法的优化代码 - 参考LLVM的位移优化
    fn generate_power_of_two_multiplication(&self, target: &Operand, operand: &Operand, power: u32) -> Result<Option<TACInstruction>, String> {
        match power {
            1 => {
                // x * 2 = x + x (已在上层处理)
                Ok(Some(TACInstruction::BinaryOp {
                    target: target.clone(),
                    left: operand.clone(),
                    op: BinaryOperator::Add,
                    right: operand.clone(),
                }))
            }
            2 => {
                // x * 4 = x << 2，这里用连续加法模拟
                // 实际编译器后端会生成左移指令
                // 暂时保持原乘法，等待后端优化
                Ok(None)
            }
            3..=5 => {
                // x * 8, x * 16, x * 32 等
                // 对于小的幂次，可以考虑生成位移指令
                // 这里简化处理，保持原乘法
                Ok(None)
            }
            _ => {
                // 对于更大的幂次，保持原乘法
                // 避免生成过于复杂的指令序列
                Ok(None)
            }
        }
    }
    
    /// 检查操作数是否为零
    fn is_zero(&self, operand: &Operand) -> bool {
        match operand {
            Operand::Constant(ConstantValue::Integer(0)) => true,
            Operand::Constant(ConstantValue::Float(f)) => *f == 0.0,
            _ => false,
        }
    }
    
    /// 检查操作数是否为一
    fn is_one(&self, operand: &Operand) -> bool {
        match operand {
            Operand::Constant(ConstantValue::Integer(1)) => true,
            Operand::Constant(ConstantValue::Float(f)) => *f == 1.0,
            _ => false,
        }
    }
    
    /// 检查操作数是否为true
    fn is_true(&self, operand: &Operand) -> bool {
        match operand {
            Operand::Constant(ConstantValue::Boolean(true)) => true,
            Operand::Constant(ConstantValue::Integer(i)) => *i != 0,
            _ => false,
        }
    }
    
    /// 检查操作数是否为false
    fn is_false(&self, operand: &Operand) -> bool {
        match operand {
            Operand::Constant(ConstantValue::Boolean(false)) => true,
            Operand::Constant(ConstantValue::Integer(0)) => true,
            _ => false,
        }
    }
    
    /// 检查操作数是否为常量
    fn is_constant(&self, operand: &Operand) -> bool {
        matches!(operand, Operand::Constant(_))
    }
    
    /// 检查两个操作数是否相等
    fn operands_equal(&self, left: &Operand, right: &Operand) -> bool {
        left == right
    }
    
    /// 获取2的幂次（如果是的话）
    fn get_power_of_two(&self, operand: &Operand) -> Option<u32> {
        if let Operand::Constant(ConstantValue::Integer(n)) = operand {
            if *n > 0 && (*n & (*n - 1)) == 0 {
                // 是2的幂
                return Some((*n as u32).trailing_zeros());
            }
        }
        None
    }

    /// 减法简化 - 参考LLVM的减法优化规则
    /// 实现 (A + B) - A = B, (A + B) - B = A 等规则
    fn try_subtract_simplification(&self, _target: &Operand, _left: &Operand, _right: &Operand) -> Result<Option<TACInstruction>, String> {
        // 暂时简化实现，因为需要复杂的指令依赖分析
        // 实际实现需要在优化过程中维护temp_to_instruction映射
        // 这里返回None表示暂不进行此类优化
        Ok(None)
    }

    /// AND分配律优化 - 参考LLVM的分配律展开
    /// 实现 A & (B | C) → (A&B) | (A&C)
    fn try_distributive_and(&self, _target: &Operand, _left: &Operand, _right: &Operand) -> Result<Option<TACInstruction>, String> {
        // 暂时简化实现，因为需要复杂的多指令生成和依赖分析
        // 实际实现需要:
        // 1. 检测右操作数是否为OR操作的结果
        // 2. 生成多条指令序列: temp1 = A & B, temp2 = A & C, target = temp1 | temp2
        // 3. 维护指令间的依赖关系
        // 这里返回None表示暂不进行此类优化
        Ok(None)
    }

    /// OR分配律优化 - 参考LLVM的分配律展开
    /// 实现 A | (B & C) → (A|B) & (A|C)
    fn try_distributive_or(&self, _target: &Operand, _left: &Operand, _right: &Operand) -> Result<Option<TACInstruction>, String> {
        // 暂时简化实现，因为需要复杂的多指令生成和依赖分析
        // 实际实现需要:
        // 1. 检测右操作数是否为AND操作的结果
        // 2. 生成多条指令序列: temp1 = A | B, temp2 = A | C, target = temp1 & temp2
        // 3. 维护指令间的依赖关系
        // 这里返回None表示暂不进行此类优化
        Ok(None)
    }
}

impl OptimizationPass for AlgebraicOptimizationPass {
    fn run(&mut self, program: &mut crate::TACIR::TACProgram) -> Result<OptimizationResult, String> {
        self.run(program)
    }
    
    fn name(&self) -> &str {
        "AlgebraicOptimization"
    }
    
    fn get_stats(&self) -> &OptimizationStats {
        &self.stats
    }
}
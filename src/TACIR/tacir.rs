use std::collections::HashMap;
use std::fmt;
use crate::frontend::ast::Type;

/// 三地址码IR系统
/// 负责基础优化：常量优化、代数优化、控制流优化

/// 操作数类型
#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    /// 临时变量 (t1, t2, ...)
    Temp(usize),
    /// 常量值
    Constant(ConstantValue),
    /// 变量名
    Variable(String),
    /// 标签 (用于跳转)
    Label(String),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Temp(id) => write!(f, "t{}", id),
            Operand::Constant(value) => write!(f, "{}", value),
            Operand::Variable(name) => write!(f, "{}", name),
            Operand::Label(name) => write!(f, "{}", name),
        }
    }
}

/// 常量值
#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
}

impl fmt::Display for ConstantValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConstantValue::Integer(value) => write!(f, "{}", value),
            ConstantValue::Float(value) => write!(f, "{}", value),
            ConstantValue::Boolean(value) => write!(f, "{}", value),
            ConstantValue::String(value) => write!(f, "\"{}\"", value),
        }
    }
}

/// 三地址码指令
#[derive(Debug, Clone)]
pub enum TACInstruction {
    /// 赋值: x = y
    Assign {
        target: Operand,
        source: Operand,
    },
    
    /// 二元运算: x = y op z
    BinaryOp {
        target: Operand,
        left: Operand,
        op: BinaryOperator,
        right: Operand,
    },
    
    /// 一元运算: x = op y
    UnaryOp {
        target: Operand,
        op: UnaryOperator,
        operand: Operand,
    },
    
    /// 内存存储: store value to address
    Store {
        value: Operand,
        address: Operand,
    },
    
    /// 内存加载: x = load address
    Load {
        target: Operand,
        address: Operand,
    },
    
    /// 内存分配: x = allocate size
    Allocate {
        target: Operand,
        size: Operand,
    },
    
    /// 获取数组元素指针: x = getelementptr base, indices...
    GetElementPtr {
        target: Operand,
        base: Operand,
        indices: Vec<Operand>,
    },
    
    /// 跳转: goto label
    Jump {
        label: String,
    },
    
    /// 条件跳转: if x goto label
    ConditionalJump {
        condition: Operand,
        true_label: String,
        false_label: String,
    },
    
    /// 标签: label:
    Label {
        name: String,
    },
    
    /// 函数调用: x = call func(args...)
    FunctionCall {
        target: Operand,
        function_name: String,
        arguments: Vec<Operand>,
    },
    
    /// 返回: return x
    Return {
        value: Option<Operand>,
    },
    
    /// 参数传递: param x
    Param {
        value: Operand,
    },
    
    /// 函数定义开始: function func
    FunctionStart {
        name: String,
        param_count: usize,
    },
    
    /// 函数定义结束: end function
    FunctionEnd,
}

impl fmt::Display for TACInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TACInstruction::Assign { target, source } => {
                write!(f, "{} = {}", target, source)
            }
            
            TACInstruction::BinaryOp { target, left, op, right } => {
                write!(f, "{} = {} {} {}", target, left, op, right)
            }
            
            TACInstruction::UnaryOp { target, op, operand } => {
                write!(f, "{} = {} {}", target, op, operand)
            }
            
            TACInstruction::Store { value, address } => {
                write!(f, "store {} to {}", value, address)
            }
            
            TACInstruction::Load { target, address } => {
                write!(f, "{} = load {}", target, address)
            }
            
            TACInstruction::Allocate { target, size } => {
                write!(f, "{} = allocate {}", target, size)
            }
            
            TACInstruction::GetElementPtr { target, base, indices } => {
                let indices_str = indices.iter()
                    .map(|idx| idx.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{} = getelementptr {}, [{}]", target, base, indices_str)
            }
            
            TACInstruction::Jump { label } => {
                write!(f, "goto {}", label)
            }
            
            TACInstruction::ConditionalJump { condition, true_label, false_label } => {
                write!(f, "if {} goto {} else goto {}", condition, true_label, false_label)
            }
            
            TACInstruction::Label { name } => {
                write!(f, "{}:", name)
            }
            
            TACInstruction::FunctionCall { target, function_name, arguments } => {
                let args_str = arguments.iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{} = call {}({})", target, function_name, args_str)
            }
            
            TACInstruction::Return { value } => {
                match value {
                    Some(val) => write!(f, "return {}", val),
                    None => write!(f, "return"),
                }
            }
            
            TACInstruction::Param { value } => {
                write!(f, "param {}", value)
            }
            
            TACInstruction::FunctionStart { name, param_count } => {
                write!(f, "function {} ({} params)", name, param_count)
            }
            
            TACInstruction::FunctionEnd => {
                write!(f, "end function")
            }
        }
    }
}

/// 二元运算符
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,        // +
    Subtract,   // -
    Multiply,   // *
    Divide,     // /
    Modulo,     // %
    Equal,      // ==
    NotEqual,   // !=
    Less,       // <
    LessEqual,  // <=
    Greater,    // >
    GreaterEqual, // >=
    And,        // &&
    Or,         // ||
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Subtract => write!(f, "-"),
            BinaryOperator::Multiply => write!(f, "*"),
            BinaryOperator::Divide => write!(f, "/"),
            BinaryOperator::Modulo => write!(f, "%"),
            BinaryOperator::Equal => write!(f, "=="),
            BinaryOperator::NotEqual => write!(f, "!="),
            BinaryOperator::Less => write!(f, "<"),
            BinaryOperator::LessEqual => write!(f, "<="),
            BinaryOperator::Greater => write!(f, ">"),
            BinaryOperator::GreaterEqual => write!(f, ">="),
            BinaryOperator::And => write!(f, "&&"),
            BinaryOperator::Or => write!(f, "||"),
        }
    }
}

/// 一元运算符
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Minus,      // -
    Not,        // !
    BitwiseNot, // ~
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::Minus => write!(f, "-"),
            UnaryOperator::Not => write!(f, "!"),
            UnaryOperator::BitwiseNot => write!(f, "~"),
        }
    }
}

/// 基本块
#[derive(Debug, Clone)]
pub struct BasicBlock {
    /// 基本块ID
    pub id: usize,
    /// 基本块标签
    pub label: Option<String>,
    /// 指令列表
    pub instructions: Vec<TACInstruction>,
    /// 前驱基本块
    pub predecessors: Vec<usize>,
    /// 后继基本块
    pub successors: Vec<usize>,
}

impl BasicBlock {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            label: None,
            instructions: Vec::new(),
            predecessors: Vec::new(),
            successors: Vec::new(),
        }
    }
    
    /// 添加指令
    pub fn add_instruction(&mut self, instruction: TACInstruction) {
        self.instructions.push(instruction);
    }
    
    /// 设置标签
    pub fn set_label(&mut self, label: String) {
        self.label = Some(label);
    }
    
    /// 添加前驱
    pub fn add_predecessor(&mut self, pred_id: usize) {
        if !self.predecessors.contains(&pred_id) {
            self.predecessors.push(pred_id);
        }
    }
    
    /// 添加后继
    pub fn add_successor(&mut self, succ_id: usize) {
        if !self.successors.contains(&succ_id) {
            self.successors.push(succ_id);
        }
    }
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // 打印标签
        if let Some(label) = &self.label {
            writeln!(f, "{}:", label)?;
        } else {
            writeln!(f, "基本块 {}:", self.id)?;
        }
        
        // 打印指令
        for (i, instruction) in self.instructions.iter().enumerate() {
            writeln!(f, "  {}: {}", i, instruction)?;
        }
        
        // 打印控制流信息
        if !self.predecessors.is_empty() || !self.successors.is_empty() {
            write!(f, "  前驱: {:?}, 后继: {:?}", self.predecessors, self.successors)?;
        }
        
        Ok(())
    }
}

/// 函数
#[derive(Debug, Clone)]
pub struct TACFunction {
    /// 函数名
    pub name: String,
    /// 返回类型
    pub return_type: Type,
    /// 参数列表
    pub parameters: Vec<(String, Type)>,
    /// AST中的参数数量（用于调试）
    pub ast_parameter_count: usize,
    /// 基本块列表
    pub basic_blocks: Vec<BasicBlock>,
    /// 临时变量计数器
    pub temp_counter: usize,
    /// 标签计数器
    pub label_counter: usize,
    /// 数组变量信息：变量名 -> (类型, 维度列表, 总大小)
    pub array_variables: HashMap<String, (Type, Vec<usize>, usize)>,
}

impl TACFunction {
    pub fn new(name: String, return_type: Type) -> Self {
        Self {
            name,
            return_type,
            parameters: Vec::new(),
            ast_parameter_count: 0,
            basic_blocks: Vec::new(),
            temp_counter: 0,
            label_counter: 0,
            array_variables: HashMap::new(),
        }
    }
    
    /// 生成新的临时变量
    pub fn new_temp(&mut self) -> Operand {
        let temp_id = self.temp_counter;
        self.temp_counter += 1;
        Operand::Temp(temp_id)
    }
    
    /// 创建新标签
    pub fn new_label(&mut self) -> String {
        let label_id = self.label_counter;
        self.label_counter += 1;
        format!("L{}", label_id)
    }
    
    /// 添加数组变量
    pub fn add_array_variable(&mut self, name: String, array_type: Type, dimensions: Vec<usize>) {
        let total_size = dimensions.iter().product();
        self.array_variables.insert(name, (array_type, dimensions, total_size));
    }
    
    /// 获取数组信息
    pub fn get_array_info(&self, name: &str) -> Option<&(Type, Vec<usize>, usize)> {
        self.array_variables.get(name)
    }
    
    /// 添加基本块
    pub fn add_basic_block(&mut self, block: BasicBlock) -> usize {
        let block_id = block.id;
        self.basic_blocks.push(block);
        block_id
    }
    
    /// 获取基本块
    pub fn get_basic_block(&self, id: usize) -> Option<&BasicBlock> {
        self.basic_blocks.iter().find(|block| block.id == id)
    }
    
    /// 获取基本块（可变引用）
    pub fn get_basic_block_mut(&mut self, id: usize) -> Option<&mut BasicBlock> {
        self.basic_blocks.iter_mut().find(|block| block.id == id)
    }
    
    /// 生成下一个基本块ID
    pub fn next_block_id(&mut self) -> usize {
        let next_id = if self.basic_blocks.is_empty() {
            0
        } else {
            self.basic_blocks.iter().map(|block| block.id).max().unwrap() + 1
        };
        next_id
    }
}

impl fmt::Display for TACFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Function: {}", self.name)?;
        writeln!(f, "Return Type: {:?}", self.return_type)?;
        writeln!(f, "Parameters:")?;
        for (name, param_type) in &self.parameters {
            writeln!(f, "  {}: {:?}", name, param_type)?;
        }
        writeln!(f, "Basic Blocks:")?;
        for (i, block) in self.basic_blocks.iter().enumerate() {
            writeln!(f, "  Basic Block {}:", i)?;
            writeln!(f, "{}", block)?;
        }
        Ok(())
    }
}

/// 三地址码IR程序
#[derive(Debug, Clone)]
pub struct TACProgram {
    /// 全局变量
    pub global_variables: Vec<(String, Type, Option<Operand>, bool)>, // 添加 is_const 字段
    /// 全局数组变量
    pub global_array_variables: Vec<(String, Type, Vec<usize>)>,
    /// 函数列表
    pub functions: Vec<TACFunction>,
    /// 主函数ID
    pub main_function_id: Option<usize>,
}

impl TACProgram {
    pub fn new() -> Self {
        Self {
            global_variables: Vec::new(),
            global_array_variables: Vec::new(),
            functions: Vec::new(),
            main_function_id: None,
        }
    }
    
    /// 添加函数
    pub fn add_function(&mut self, function: TACFunction) -> usize {
        let function_id = self.functions.len();
        
        // 如果是main函数，记录其ID
        if function.name == "main" {
            self.main_function_id = Some(function_id);
        }
        
        self.functions.push(function);
        function_id
    }
    
    /// 获取主函数
    pub fn get_main_function(&self) -> Option<&TACFunction> {
        self.main_function_id.map(|id| &self.functions[id])
    }
    
    /// 获取主函数（可变引用）
    pub fn get_main_function_mut(&mut self) -> Option<&mut TACFunction> {
        self.main_function_id.map(|id| &mut self.functions[id])
    }
    
    /// 添加全局变量
    pub fn add_global_variable(&mut self, name: String, var_type: Type, initial_value: Option<Operand>, is_const: bool) {
        self.global_variables.push((name, var_type, initial_value, is_const));
    }
    
    /// 添加全局数组变量
    pub fn add_global_array_variable(&mut self, name: String, array_type: Type, dimensions: Vec<usize>) {
        self.global_array_variables.push((name, array_type, dimensions));
    }
}

impl fmt::Display for TACProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "TAC Program")?;
        writeln!(f, "Global Variables:")?;
        for (name, var_type, initial_value, is_const) in &self.global_variables {
            let initial_value_str = match initial_value {
                Some(val) => val.to_string(),
                None => "None".to_string(),
            };
            writeln!(f, "  {}: {:?} = {}", name, var_type, initial_value_str)?;
        }
        writeln!(f, "Global Array Variables:")?;
        for (name, array_type, dimensions) in &self.global_array_variables {
            writeln!(f, "  {}: {:?} 维度: {:?}", name, array_type, dimensions)?;
        }
        writeln!(f, "Functions:")?;
        for (i, func) in self.functions.iter().enumerate() {
            writeln!(f, "  Function {}:", i)?;
            writeln!(f, "{}", func)?;
        }
        writeln!(f, "Main Function ID: {:?}", self.main_function_id)?;
        Ok(())
    }
}

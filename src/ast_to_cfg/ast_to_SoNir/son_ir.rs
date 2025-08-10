use std::collections::{HashMap, HashSet};
use crate::frontend::ast::Type;
use crate::frontend::span::Span;

/// Sea of Nodes IR 节点 ID
pub type SonNodeId = usize;

/// 常量值
#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
}

/// 操作类型枚举
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum OpCode {
    // 控制流操作
    Start,
    Stop,
    Region,
    If,
    Loop,
    Merge,
    Return,
    Break,
    Continue,
    
    // 常量操作
    Constant,
    Parameter,
    Local,
    
    // 算术运算
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    
    // 比较运算
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    
    // 逻辑运算
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    
    // 一元运算
    Minus,
    BitwiseNot,
    
    // 内存操作
    Load,
    Store,
    ArrayAccess,
    MemberAccess,
    New,
    
    // 函数调用
    Call,
    
    // 类型转换
    Cast,
    
    // 特殊节点
    Phi,
    Proj,
    CProj,
    
    // 位运算
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
    ArithmeticShiftRight,
    
    // 浮点运算
    ToFloat,
    RoundF32,
}

/// Sea of Nodes IR 节点类型
#[derive(Debug, Clone)]
pub struct SonNodeKind {
    /// 操作码
    pub opcode: OpCode,
    /// 操作数（具体数据）
    pub data: NodeData,
}

/// 节点数据
#[derive(Debug, Clone)]
pub enum NodeData {
    // 控制流节点
    Start {
        args: Vec<Type>,
    },
    If {
        condition: Option<SonNodeId>,
    },
    Loop {
        entry: Option<SonNodeId>,
        back: Option<SonNodeId>,
    },
    Region {
        inputs: Vec<Option<SonNodeId>>,
    },
    
    // 常量节点
    Constant {
        value: ConstantValue,
        typ: Type,
    },
    Parameter {
        name: String,
        typ: Type,
    },
    Local {
        name: String,
        typ: Type,
    },
    
    // 运算节点
    BinaryOp {
        left: Option<SonNodeId>,
        right: Option<SonNodeId>,
    },
    UnaryOp {
        operand: Option<SonNodeId>,
    },
    
    // 内存操作
    Load {
        name: String,
        alias: u32,
        declared_type: Type,
        mem: Option<SonNodeId>,
        ptr: Option<SonNodeId>,
        offset: Option<SonNodeId>,
    },
    Store {
        name: String,
        alias: u32,
        declared_type: Type,
        mem: Option<SonNodeId>,
        ptr: Option<SonNodeId>,
        offset: Option<SonNodeId>,
        value: Option<SonNodeId>,
        init: bool,
    },
    ArrayAccess {
        array: Option<SonNodeId>,
        index: Option<SonNodeId>,
    },
    MemberAccess {
        object: Option<SonNodeId>,
        field: String,
    },
    
    // 函数调用
    Call {
        function_name: String,
        return_type: Type,
        arguments: Vec<SonNodeId>,
    },
    
    // 类型转换
    Cast {
        from_type: Type,
        to_type: Type,
        value: Option<SonNodeId>,
    },
    
    // 特殊节点
    Phi {
        typ: Type,
        inputs: Vec<Option<SonNodeId>>,
    },
    Proj {
        index: usize,
        label: String,
    },
    CProj {
        index: usize,
        label: String,
    },
    
    // 空数据
    None,
}

impl SonNodeKind {
    /// 创建新的节点类型
    pub fn new(opcode: OpCode) -> Self {
        Self {
            opcode,
            data: NodeData::None,
        }
    }
    
    /// 创建带数据的节点类型
    pub fn with_data(opcode: OpCode, data: NodeData) -> Self {
        Self { opcode, data }
    }
    
    /// 检查是否为控制流节点
    pub fn is_control_flow(&self) -> bool {
        matches!(self.opcode, 
            OpCode::Start | OpCode::Stop | OpCode::Region | 
            OpCode::If | OpCode::Loop | OpCode::Merge |
            OpCode::Return | OpCode::Break | OpCode::Continue
        )
    }
    
    /// 检查是否为数据流节点
    pub fn is_data_flow(&self) -> bool {
        !self.is_control_flow()
    }
    
    /// 获取节点标签
    pub fn label(&self) -> String {
        match &self.opcode {
            OpCode::Start => "Start".to_string(),
            OpCode::Stop => "Stop".to_string(),
            OpCode::Region => "Region".to_string(),
            OpCode::If => "If".to_string(),
            OpCode::Loop => "Loop".to_string(),
            OpCode::Merge => "Merge".to_string(),
            OpCode::Constant => "Constant".to_string(),
            OpCode::Parameter => "Parameter".to_string(),
            OpCode::Local => "Local".to_string(),
            OpCode::Add => "+".to_string(),
            OpCode::Subtract => "-".to_string(),
            OpCode::Multiply => "*".to_string(),
            OpCode::Divide => "/".to_string(),
            OpCode::Modulo => "%".to_string(),
            OpCode::Equal => "==".to_string(),
            OpCode::NotEqual => "!=".to_string(),
            OpCode::LessThan => "<".to_string(),
            OpCode::LessEqual => "<=".to_string(),
            OpCode::GreaterThan => ">".to_string(),
            OpCode::GreaterEqual => ">=".to_string(),
            OpCode::LogicalAnd => "&&".to_string(),
            OpCode::LogicalOr => "||".to_string(),
            OpCode::LogicalNot => "!".to_string(),
            OpCode::Minus => "-".to_string(),
            OpCode::BitwiseNot => "~".to_string(),
            OpCode::Load => "Load".to_string(),
            OpCode::Store => "Store".to_string(),
            OpCode::ArrayAccess => "[]".to_string(),
            OpCode::MemberAccess => ".".to_string(),
            OpCode::Call => "Call".to_string(),
            OpCode::Cast => "Cast".to_string(),
            OpCode::Phi => "Phi".to_string(),
            OpCode::Proj => "Proj".to_string(),
            OpCode::CProj => "CProj".to_string(),
            OpCode::BitwiseAnd => "&".to_string(),
            OpCode::BitwiseOr => "|".to_string(),
            OpCode::BitwiseXor => "^".to_string(),
            OpCode::ShiftLeft => "<<".to_string(),
            OpCode::ShiftRight => ">>".to_string(),
            OpCode::ArithmeticShiftRight => ">>>".to_string(),
            OpCode::ToFloat => "ToFloat".to_string(),
            OpCode::RoundF32 => "RoundF32".to_string(),
            OpCode::New => "New".to_string(),
            OpCode::Return => "Return".to_string(),
            OpCode::Break => "Break".to_string(),
            OpCode::Continue => "Continue".to_string(),
        }
    }
}

/// Sea of Nodes IR 节点
#[derive(Debug, Clone)]
pub struct SonNode {
    /// 节点 ID
    pub id: SonNodeId,
    /// 节点类型
    pub kind: SonNodeKind,
    /// 输入边（数据依赖）
    pub inputs: Vec<SonNodeId>,
    /// 输出边（数据依赖）
    pub outputs: Vec<SonNodeId>,
    /// 控制流输入
    pub control_inputs: Vec<SonNodeId>,
    /// 控制流输出
    pub control_outputs: Vec<SonNodeId>,
    /// 源位置信息
    pub span: Option<Span>,
    /// 节点属性
    pub attributes: HashMap<String, String>,
    /// 节点类型（用于类型检查）
    pub node_type: Option<Type>,
    /// 是否被标记为重要节点
    pub is_keep: bool,
    /// 是否被标记为死亡节点
    pub is_dead: bool,
}

impl SonNode {
    /// 创建新的节点
    pub fn new(id: SonNodeId, kind: SonNodeKind) -> Self {
        Self {
            id,
            kind,
            inputs: Vec::new(),
            outputs: Vec::new(),
            control_inputs: Vec::new(),
            control_outputs: Vec::new(),
            span: None,
            attributes: HashMap::new(),
            node_type: None,
            is_keep: false,
            is_dead: false,
        }
    }
    
    /// 添加数据输入
    pub fn add_input(&mut self, input_id: SonNodeId) {
        if !self.inputs.contains(&input_id) {
            self.inputs.push(input_id);
        }
    }
    
    /// 移除数据输入
    pub fn remove_input(&mut self, input_id: SonNodeId) {
        self.inputs.retain(|&id| id != input_id);
    }
    
    /// 添加数据输出
    pub fn add_output(&mut self, output_id: SonNodeId) {
        if !self.outputs.contains(&output_id) {
            self.outputs.push(output_id);
        }
    }
    
    /// 移除数据输出
    pub fn remove_output(&mut self, output_id: SonNodeId) {
        self.outputs.retain(|&id| id != output_id);
    }
    
    /// 添加控制流输入
    pub fn add_control_input(&mut self, input_id: SonNodeId) {
        if !self.control_inputs.contains(&input_id) {
            self.control_inputs.push(input_id);
        }
    }
    
    /// 移除控制流输入
    pub fn remove_control_input(&mut self, input_id: SonNodeId) {
        self.control_inputs.retain(|&id| id != input_id);
    }
    
    /// 添加控制流输出
    pub fn add_control_output(&mut self, output_id: SonNodeId) {
        if !self.control_outputs.contains(&output_id) {
            self.control_outputs.push(output_id);
        }
    }
    
    /// 移除控制流输出
    pub fn remove_control_output(&mut self, output_id: SonNodeId) {
        self.control_outputs.retain(|&id| id != output_id);
    }
    
    /// 设置源位置
    pub fn set_span(&mut self, span: Span) {
        self.span = Some(span);
    }
    
    /// 添加属性
    pub fn add_attribute(&mut self, key: String, value: String) {
        self.attributes.insert(key, value);
    }
    
    /// 检查是否为控制流节点
    pub fn is_control_flow(&self) -> bool {
        self.kind.is_control_flow()
    }
    
    /// 检查是否为数据流节点
    pub fn is_data_flow(&self) -> bool {
        self.kind.is_data_flow()
    }
    
    /// 设置节点类型
    pub fn set_type(&mut self, typ: Type) {
        self.node_type = Some(typ);
    }
    
    /// 获取节点类型
    pub fn get_type(&self) -> Option<&Type> {
        self.node_type.as_ref()
    }
    
    /// 标记为重要节点
    pub fn mark_keep(&mut self) {
        self.is_keep = true;
    }
    
    /// 取消重要标记
    pub fn unmark_keep(&mut self) {
        self.is_keep = false;
    }
    
    /// 标记为死亡节点
    pub fn mark_dead(&mut self) {
        self.is_dead = true;
    }
    
    /// 取消死亡标记
    pub fn unmark_dead(&mut self) {
        self.is_dead = false;
    }
    
    /// 检查是否为死亡节点
    pub fn is_dead(&self) -> bool {
        self.is_dead
    }
    
    /// 检查是否为重要节点
    pub fn is_keep(&self) -> bool {
        self.is_keep
    }
}

/// 边类型
#[derive(Debug, Clone, PartialEq)]
pub enum EdgeType {
    /// 数据依赖边
    Data,
    /// 控制流边
    Control,
    /// 条件边（用于分支）
    Condition,
}

/// Sea of Nodes IR 边
#[derive(Debug, Clone)]
pub struct SonEdge {
    /// 源节点 ID
    pub from: SonNodeId,
    /// 目标节点 ID
    pub to: SonNodeId,
    /// 边类型
    pub edge_type: EdgeType,
    /// 边标签（可选）
    pub label: Option<String>,
}

impl SonEdge {
    /// 创建新的边
    pub fn new(from: SonNodeId, to: SonNodeId, edge_type: EdgeType) -> Self {
        Self {
            from,
            to,
            edge_type,
            label: None,
        }
    }
    
    /// 创建带标签的边
    pub fn with_label(mut self, label: String) -> Self {
        self.label = Some(label);
        self
    }
}

/// Sea of Nodes IR 图
#[derive(Debug, Clone)]
pub struct SonIr {
    /// 节点集合
    nodes: HashMap<SonNodeId, SonNode>,
    /// 边集合
    edges: Vec<SonEdge>,
    /// 下一个可用节点 ID
    next_node_id: SonNodeId,
    /// 入口节点 ID
    entry_node: Option<SonNodeId>,
    /// 出口节点 ID
    exit_node: Option<SonNodeId>,
    /// 类型系统
    type_system: TypeSystem,
}

/// 类型系统
#[derive(Debug, Clone)]
pub struct TypeSystem {
    /// 类型映射表
    types: HashMap<String, Type>,
    /// 类型别名
    aliases: HashMap<String, String>,
}

impl TypeSystem {
    /// 创建新的类型系统
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
            aliases: HashMap::new(),
        }
    }
    
    /// 注册类型
    pub fn register_type(&mut self, name: String, typ: Type) {
        self.types.insert(name, typ);
    }
    
    /// 查找类型
    pub fn find_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
    }
    
    /// 添加类型别名
    pub fn add_alias(&mut self, alias: String, target: String) {
        self.aliases.insert(alias, target);
    }
    
    /// 解析类型别名
    pub fn resolve_alias(&self, name: &str) -> Option<&Type> {
        if let Some(alias) = self.aliases.get(name) {
            self.types.get(alias)
        } else {
            self.types.get(name)
        }
    }
}

impl SonIr {
    /// 创建新的 IR 图
    pub fn new() -> Self {
        Self {
            nodes: HashMap::new(),
            edges: Vec::new(),
            next_node_id: 0,
            entry_node: None,
            exit_node: None,
            type_system: TypeSystem::new(),
        }
    }
    
    /// 添加节点到图中
    pub fn add_node(&mut self, mut node: SonNode) -> SonNodeId {
        let id = self.next_node_id;
        node.id = id;
        self.nodes.insert(id, node);
        self.next_node_id += 1;
        id
    }
    
    /// 获取节点引用
    pub fn get_node(&self, id: SonNodeId) -> Option<&SonNode> {
        self.nodes.get(&id)
    }
    
    /// 获取节点可变引用
    pub fn get_node_mut(&mut self, id: SonNodeId) -> Option<&mut SonNode> {
        self.nodes.get_mut(&id)
    }
    
    /// 添加边到图中
    pub fn add_edge(&mut self, edge: SonEdge) {
        // 检查边是否已存在，避免重复
        if self.edges.iter().any(|e| 
            e.from == edge.from && e.to == edge.to && e.edge_type == edge.edge_type
        ) {
            return; // 边已存在，不重复添加
        }
        
        // 更新节点的输入输出列表
        if let Some(from_node) = self.nodes.get_mut(&edge.from) {
            match edge.edge_type {
                EdgeType::Data => from_node.add_output(edge.to),
                EdgeType::Control => from_node.add_control_output(edge.to),
                EdgeType::Condition => from_node.add_control_output(edge.to),
            }
        }
        
        if let Some(to_node) = self.nodes.get_mut(&edge.to) {
            match edge.edge_type {
                EdgeType::Data => to_node.add_input(edge.from),
                EdgeType::Control => to_node.add_control_input(edge.from),
                EdgeType::Condition => to_node.add_control_input(edge.from),
            }
        }
        
        self.edges.push(edge);
    }
    
    /// 移除边
    pub fn remove_edge(&mut self, edge: &SonEdge) {
        // 从节点的输入输出列表中移除
        if let Some(from_node) = self.nodes.get_mut(&edge.from) {
            match edge.edge_type {
                EdgeType::Data => from_node.remove_output(edge.to),
                EdgeType::Control => from_node.remove_control_output(edge.to),
                EdgeType::Condition => from_node.remove_control_output(edge.to),
            }
        }
        
        if let Some(to_node) = self.nodes.get_mut(&edge.to) {
            match edge.edge_type {
                EdgeType::Data => to_node.remove_input(edge.from),
                EdgeType::Control => to_node.remove_control_input(edge.from),
                EdgeType::Condition => to_node.remove_control_input(edge.from),
            }
        }
        
        // 从边列表中移除
        self.edges.retain(|e| !(e.from == edge.from && e.to == edge.to && e.edge_type == edge.edge_type));
    }
    
    /// 设置入口节点
    pub fn set_entry_node(&mut self, id: SonNodeId) {
        self.entry_node = Some(id);
    }
    
    /// 设置出口节点
    pub fn set_exit_node(&mut self, id: SonNodeId) {
        self.exit_node = Some(id);
    }
    
    /// 获取入口节点
    pub fn get_entry_node(&self) -> Option<SonNodeId> {
        self.entry_node
    }
    
    /// 获取出口节点
    pub fn get_exit_node(&self) -> Option<SonNodeId> {
        self.exit_node
    }
    
    /// 获取所有节点
    pub fn get_all_nodes(&self) -> &HashMap<SonNodeId, SonNode> {
        &self.nodes
    }
    
    /// 获取所有边
    pub fn get_all_edges(&self) -> &[SonEdge] {
        &self.edges
    }
    
    /// 获取节点数量
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }
    
    /// 获取边数量
    pub fn edge_count(&self) -> usize {
        self.edges.len()
    }
    
    /// 检查是否为空
    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }
    
    /// 清空图
    pub fn clear(&mut self) {
        self.nodes.clear();
        self.edges.clear();
        self.next_node_id = 0;
        self.entry_node = None;
        self.exit_node = None;
        self.type_system = TypeSystem::new();
    }
    
    /// 获取类型系统
    pub fn get_type_system(&self) -> &TypeSystem {
        &self.type_system
    }
    
    /// 获取类型系统可变引用
    pub fn get_type_system_mut(&mut self) -> &mut TypeSystem {
        &mut self.type_system
    }
    
    /// 打印图结构
    pub fn print_graph(&self) {
        // 调试信息已移除
    }
    
    /// 验证图的一致性
    pub fn validate(&self) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();
        
        // 检查所有边引用的节点是否存在
        for edge in &self.edges {
            if !self.nodes.contains_key(&edge.from) {
                errors.push(format!("边引用不存在的源节点: {}", edge.from));
            }
            if !self.nodes.contains_key(&edge.to) {
                errors.push(format!("边引用不存在的目标节点: {}", edge.to));
            }
        }
        
        // 检查节点的输入输出一致性
        for (id, node) in &self.nodes {
            for &input_id in &node.inputs {
                if let Some(input_node) = self.nodes.get(&input_id) {
                    if !input_node.outputs.contains(id) {
                        errors.push(format!("节点 {} 的输入 {} 没有对应的输出边", id, input_id));
                    }
                }
            }
            
            for &output_id in &node.outputs {
                if let Some(output_node) = self.nodes.get(&output_id) {
                    if !output_node.inputs.contains(id) {
                        errors.push(format!("节点 {} 的输出 {} 没有对应的输入边", id, output_id));
                    }
                }
            }
        }
        
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}

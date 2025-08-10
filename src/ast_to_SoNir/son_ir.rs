use std::collections::{HashMap, HashSet};
use crate::frontend::ast::{Type, Span};

/// Sea of Nodes IR 节点 ID
pub type SonNodeId = usize;

/// Sea of Nodes IR 节点类型
#[derive(Debug, Clone, PartialEq)]
pub enum SonNodeKind {
    // 控制流节点
    Start,
    Stop,
    Region,
    If,
    Loop,
    Merge,
    
    // 数据流节点
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
    
    // 函数调用
    Call {
        function_name: String,
        return_type: Type,
    },
    
    // 类型转换
    Cast {
        from_type: Type,
        to_type: Type,
    },
    
    // 特殊节点
    Phi {
        typ: Type,
    },
    Return,
    Break,
    Continue,
}

/// 常量值
#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue {
    Integer(i32),
    Float(f32),
    Boolean(bool),
    String(String),
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
}

impl SonNode {
    /// 创建新节点
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
        }
    }
    
    /// 添加数据输入
    pub fn add_input(&mut self, input_id: SonNodeId) {
        if !self.inputs.contains(&input_id) {
            self.inputs.push(input_id);
        }
    }
    
    /// 添加数据输出
    pub fn add_output(&mut self, output_id: SonNodeId) {
        if !self.outputs.contains(&output_id) {
            self.outputs.push(output_id);
        }
    }
    
    /// 添加控制流输入
    pub fn add_control_input(&mut self, input_id: SonNodeId) {
        if !self.control_inputs.contains(&input_id) {
            self.control_inputs.push(input_id);
        }
    }
    
    /// 添加控制流输出
    pub fn add_control_output(&mut self, output_id: SonNodeId) {
        if !self.control_outputs.contains(&output_id) {
            self.control_outputs.push(output_id);
        }
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
        matches!(self.kind, 
            SonNodeKind::Start | 
            SonNodeKind::Stop | 
            SonNodeKind::Region | 
            SonNodeKind::If | 
            SonNodeKind::Loop | 
            SonNodeKind::Merge
        )
    }
    
    /// 检查是否为数据流节点
    pub fn is_data_flow(&self) -> bool {
        !self.is_control_flow()
    }
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

impl SonEdge {
    /// 创建新边
    pub fn new(from: SonNodeId, to: SonNodeId, edge_type: EdgeType) -> Self {
        Self {
            from,
            to,
            edge_type,
            label: None,
        }
    }
    
    /// 设置边标签
    pub fn with_label(mut self, label: String) -> Self {
        self.label = Some(label);
        self
    }
}

/// Sea of Nodes IR 图
#[derive(Debug)]
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
        }
    }
    
    /// 添加节点
    pub fn add_node(&mut self, mut node: SonNode) -> SonNodeId {
        let id = self.next_node_id;
        node.id = id;
        self.next_node_id += 1;
        self.nodes.insert(id, node);
        id
    }
    
    /// 获取节点
    pub fn get_node(&self, id: SonNodeId) -> Option<&SonNode> {
        self.nodes.get(&id)
    }
    
    /// 获取可变节点引用
    pub fn get_node_mut(&mut self, id: SonNodeId) -> Option<&mut SonNode> {
        self.nodes.get_mut(&id)
    }
    
    /// 添加边
    pub fn add_edge(&mut self, edge: SonEdge) {
        // 更新节点的连接信息
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
    
    /// 检查图是否为空
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
    }
    
    /// 打印图结构（用于调试）
    pub fn print_graph(&self) {
        println!("=== Sea of Nodes IR Graph ===");
        println!("节点数量: {}", self.node_count());
        println!("边数量: {}", self.edge_count());
        
        if let Some(entry) = self.entry_node {
            println!("入口节点: {}", entry);
        }
        if let Some(exit) = self.exit_node {
            println!("出口节点: {}", exit);
        }
        
        println!("\n节点列表:");
        for (id, node) in &self.nodes {
            println!("  {}: {:?}", id, node.kind);
            if !node.inputs.is_empty() {
                println!("    数据输入: {:?}", node.inputs);
            }
            if !node.control_inputs.is_empty() {
                println!("    控制输入: {:?}", node.control_inputs);
            }
            if !node.outputs.is_empty() {
                println!("    数据输出: {:?}", node.outputs);
            }
            if !node.control_outputs.is_empty() {
                println!("    控制输出: {:?}", node.control_outputs);
            }
        }
        
        println!("\n边列表:");
        for edge in &self.edges {
            let label = edge.label.as_deref().unwrap_or("");
            println!("  {} -> {} ({:?}) {}", 
                edge.from, edge.to, edge.edge_type, label);
        }
    }
}

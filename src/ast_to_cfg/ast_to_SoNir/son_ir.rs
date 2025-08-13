use std::collections::{HashMap, HashSet};
use crate::frontend::ast::Type;
use crate::frontend::span::Span;
use std::fmt;

/// Sea of Nodes IR 节点 ID
pub type SonNodeId = usize;

/// 常量值
#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    DeadControl,  // 死控制流（~Ctrl）
}

/// 操作类型枚举
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BoolOp {
    EQ,    // ==
    LT,    // <
    LE,    // <=
    GT,    // >
    GE,    // >=
    NE,    // !=
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpCode {
    // 控制流操作
    Start,
    Stop,           // 程序终止节点
    Region,         // 控制流合并点
    If,             // 条件分支节点
    Loop,
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
    
    // 布尔运算节点
    Bool,
    
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
    Phi,         // 值合并节点
    Proj,        // 投影节点
    CProj,       // 控制投影节点
    
    // 作用域管理
    Scope,       // 作用域节点
    
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
        outputs: Vec<Type>,  // 动态多值输出：[ctrl, arg, ...]
        ctrl_type: Type,     // 控制类型
        arg_type: Type,      // 参数类型
    },
    If {
        condition: Option<SonNodeId>,  // 条件表达式节点
        true_branch: Option<SonNodeId>, // true分支控制流
        false_branch: Option<SonNodeId>, // false分支控制流
    },
    Region {
        inputs: Vec<Option<SonNodeId>>, // 控制流输入列表
    },
    Stop {
        return_nodes: Vec<SonNodeId>,   // 所有return节点
    },
    Loop {
        entry: Option<SonNodeId>,       // 循环入口节点
        back: Option<SonNodeId>,        // 循环回边节点
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
    
    // 布尔运算节点
    Bool {
        left: Option<SonNodeId>,
        right: Option<SonNodeId>,
        op: BoolOp,
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
        label: String,
        typ: Type,
        inputs: Vec<Option<SonNodeId>>, // 数据输入：第一个是Region节点，其余是各分支的数据值
        region: Option<SonNodeId>,       // 对应的Region节点（控制流输入）
    },
    Proj {
        index: usize,
        label: String,
    },
    CProj {
        index: usize,
        label: String,
    },
    
    // 作用域节点
    Scope {
        symbol_tables: Vec<HashMap<String, usize>>, // 符号表栈
        scope_level: usize,                         // 当前作用域级别
        parent_scope: Option<SonNodeId>,            // 父作用域节点
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
            OpCode::If | OpCode::Loop |
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
            OpCode::Bool => "Bool".to_string(),
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
            OpCode::Scope => "Scope".to_string(),
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
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EdgeType {
    /// 数据依赖边（空心箭头）
    Data,
    /// 控制依赖边（实心箭头）
    Control,
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
    /// 类型格结构：用于类型推断和优化
    type_lattice: TypeLattice,
}

/// 类型格结构：实现原文要求的格理论
#[derive(Debug, Clone)]
pub struct TypeLattice {
    /// 类型层次关系
    type_hierarchy: HashMap<String, Vec<String>>,
    /// 类型约束
    type_constraints: HashMap<String, TypeConstraint>,
}

/// 类型约束：表示类型可能的值域
#[derive(Debug, Clone)]
pub struct TypeConstraint {
    /// 类型名称
    pub type_name: String,
    /// 可能的值域（格的下界）
    pub lower_bound: Option<ConstantValue>,
    /// 可能的值域（格的上界）
    pub upper_bound: Option<ConstantValue>,
    /// 是否为常量
    pub is_constant: bool,
    /// 常量值（如果已知）
    pub constant_value: Option<ConstantValue>,
}

impl TypeLattice {
    /// 创建新的类型格
    pub fn new() -> Self {
        Self {
            type_hierarchy: HashMap::new(),
            type_constraints: HashMap::new(),
        }
    }
    
    /// 添加类型层次关系
    pub fn add_type_hierarchy(&mut self, parent: String, child: String) {
        self.type_hierarchy.entry(parent)
            .or_insert_with(Vec::new)
            .push(child);
    }
    
    /// 获取类型的约束
    pub fn get_type_constraint(&self, type_name: &str) -> Option<&TypeConstraint> {
        self.type_constraints.get(type_name)
    }
    
    /// 设置类型约束
    pub fn set_type_constraint(&mut self, constraint: TypeConstraint) {
        let type_name = constraint.type_name.clone();
        self.type_constraints.insert(type_name, constraint);
    }
    
    /// 格操作：meet（下确界）
    pub fn meet(&self, type1: &str, type2: &str) -> Option<String> {
        // 找到两个类型的公共父类型
        if let (Some(parents1), Some(parents2)) = (
            self.type_hierarchy.get(type1),
            self.type_hierarchy.get(type2)
        ) {
            for parent in parents1 {
                if parents2.contains(parent) {
                    return Some(parent.clone());
                }
            }
        }
        None
    }
    
    /// 格操作：join（上确界）
    pub fn join(&self, type1: &str, type2: &str) -> Option<String> {
        // 找到两个类型的公共子类型
        if let (Some(children1), Some(children2)) = (
            self.type_hierarchy.get(type1),
            self.type_hierarchy.get(type2)
        ) {
            for child in children1 {
                if children2.contains(child) {
                    return Some(child.clone());
                }
            }
        }
        None
    }
    
    /// 类型推断：基于格结构推断节点类型
    pub fn infer_node_type(&self, input_types: &[&Type]) -> Option<Type> {
        if input_types.is_empty() {
            return None;
        }
        
        // 使用meet操作找到最具体的类型
        let mut inferred_type = input_types[0];
        for input_type in input_types.iter().skip(1) {
            if let Some(common_type) = self.meet(&inferred_type.to_string(), &input_type.to_string()) {
                // 这里需要将字符串类型名转换回Type类型
                // 简化实现，实际应该查找类型映射
                inferred_type = input_type; // 临时简化
            }
        }
        
        Some(inferred_type.clone())
    }
}

impl TypeConstraint {
    /// 创建新的类型约束
    pub fn new(type_name: String) -> Self {
        Self {
            type_name,
            lower_bound: None,
            upper_bound: None,
            is_constant: false,
            constant_value: None,
        }
    }
    
    /// 设置为常量
    pub fn set_constant(&mut self, value: ConstantValue) {
        self.is_constant = true;
        self.constant_value = Some(value.clone());
        self.lower_bound = Some(value.clone());
        self.upper_bound = Some(value);
    }
    
    /// 设置值域范围
    pub fn set_range(&mut self, lower: ConstantValue, upper: ConstantValue) {
        self.lower_bound = Some(lower);
        self.upper_bound = Some(upper);
        self.is_constant = false;
        self.constant_value = None;
    }
    
    /// 检查是否为常量
    pub fn is_constant(&self) -> bool {
        self.is_constant
    }
    
    /// 获取常量值
    pub fn get_constant_value(&self) -> Option<&ConstantValue> {
        self.constant_value.as_ref()
    }
}

impl TypeSystem {
    /// 创建新的类型系统
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
            aliases: HashMap::new(),
            type_lattice: TypeLattice::new(),
        }
    }
    
    /// 注册类型
    pub fn register_type(&mut self, name: String, typ: Type) {
        let type_name = name.clone();
        self.types.insert(name, typ);
        
        // 为基本类型创建类型约束
        let mut constraint = TypeConstraint::new(type_name.clone());
        match &self.types[&type_name] {
            Type::IntType => {
                constraint.set_range(
                    ConstantValue::Integer(i64::MIN),
                    ConstantValue::Integer(i64::MAX)
                );
            }
            Type::FloatType => {
                constraint.set_range(
                    ConstantValue::Float(f64::NEG_INFINITY),
                    ConstantValue::Float(f64::INFINITY)
                );
            }
            Type::BoolType => {
                constraint.set_range(
                    ConstantValue::Boolean(false),
                    ConstantValue::Boolean(true)
                );
            }
            Type::CharType => {
                constraint.set_range(
                    ConstantValue::Integer(0),
                    ConstantValue::Integer(255)
                );
            }
            _ => {}
        }
        self.type_lattice.set_type_constraint(constraint);
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
    
    /// 获取类型格
    pub fn get_type_lattice(&self) -> &TypeLattice {
        &self.type_lattice
    }
    
    /// 获取类型格的可变引用
    pub fn get_type_lattice_mut(&mut self) -> &mut TypeLattice {
        &mut self.type_lattice
    }
    
    /// 检查节点是否为常量（原文要求的核心功能）
    pub fn is_node_constant(&self, node_id: SonNodeId, son_ir: &SonIr) -> bool {
        if let Some(node) = son_ir.get_node(node_id) {
            if let Some(node_type) = &node.node_type {
                if let Some(constraint) = self.type_lattice.get_type_constraint(&node_type.to_string()) {
                    return constraint.is_constant();
                }
            }
        }
        false
    }
    
    /// 获取节点的常量值（原文要求的核心功能）
    pub fn get_node_constant_value(&self, node_id: SonNodeId, son_ir: &SonIr) -> Option<&ConstantValue> {
        if let Some(node) = son_ir.get_node(node_id) {
            if let Some(node_type) = &node.node_type {
                if let Some(constraint) = self.type_lattice.get_type_constraint(&node_type.to_string()) {
                    return constraint.get_constant_value();
                }
            }
        }
        None
    }
    
    /// 设置节点为常量（原文要求的核心功能）
    pub fn set_node_as_constant(&mut self, node_id: SonNodeId, value: ConstantValue, son_ir: &mut SonIr) {
        if let Some(node) = son_ir.get_node_mut(node_id) {
            if let Some(node_type) = &node.node_type {
                let type_name = node_type.to_string();
                if let Some(constraint) = self.type_lattice.get_type_constraint(&type_name) {
                    let mut new_constraint = constraint.clone();
                    new_constraint.set_constant(value);
                    self.type_lattice.set_type_constraint(new_constraint);
                }
            }
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
            }
        }
        
        if let Some(to_node) = self.nodes.get_mut(&edge.to) {
            match edge.edge_type {
                EdgeType::Data => to_node.add_input(edge.from),
                EdgeType::Control => to_node.add_control_input(edge.from),
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
            }
        }
        
        if let Some(to_node) = self.nodes.get_mut(&edge.to) {
            match edge.edge_type {
                EdgeType::Data => to_node.remove_input(edge.from),
                EdgeType::Control => to_node.remove_control_input(edge.from),
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
    
    /// 验证图的完整性
    pub fn validate(&self) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();
        
        // 检查所有节点是否都有有效的ID
        for (id, node) in &self.nodes {
            if *id != node.id {
                errors.push(format!("Node ID mismatch: stored ID {}, node ID {}", id, node.id));
            }
        }
        
        // 检查边是否连接到有效的节点
        for edge in &self.edges {
            if !self.nodes.contains_key(&edge.from) {
                errors.push(format!("Edge from non-existent node: {}", edge.from));
            }
            if !self.nodes.contains_key(&edge.to) {
                errors.push(format!("Edge to non-existent node: {}", edge.to));
            }
        }
        
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// 替换节点：用新节点替换指定ID的节点，保持所有连接关系
    pub fn replace_node(&mut self, old_id: SonNodeId, new_node: SonNode) -> Result<(), String> {
        // 检查旧节点是否存在
        let old_node = self.nodes.get(&old_id)
            .ok_or_else(|| format!("Node {} does not exist", old_id))?;
        
        // 保存旧节点的所有连接信息
        let old_inputs = old_node.inputs.clone();
        let old_outputs = old_node.outputs.clone();
        let old_control_inputs = old_node.control_inputs.clone();
        let old_control_outputs = old_node.control_outputs.clone();
        
        // 创建新节点，使用旧节点的ID
        let mut new_node_with_id = new_node;
        new_node_with_id.id = old_id;
        
        // 复制旧节点的连接信息到新节点
        new_node_with_id.inputs = old_inputs;
        new_node_with_id.outputs = old_outputs;
        new_node_with_id.control_inputs = old_control_inputs;
        new_node_with_id.control_outputs = old_control_outputs;
        
        // 替换节点
        self.nodes.insert(old_id, new_node_with_id);
        
        Ok(())
    }

    /// 移除节点：移除指定ID的节点，并重新连接其输入输出
    pub fn remove_node(&mut self, node_id: SonNodeId) -> Result<(), String> {
        let node = self.nodes.get(&node_id)
            .ok_or_else(|| format!("Node {} does not exist", node_id))?;
        
        // 获取节点的所有连接
        let inputs = node.inputs.clone();
        let outputs = node.outputs.clone();
        let control_inputs = node.control_inputs.clone();
        let control_outputs = node.control_outputs.clone();
        
        // 移除所有相关的边
        let edges_to_remove: Vec<_> = self.edges.iter()
            .filter(|edge| edge.from == node_id || edge.to == node_id)
            .cloned()
            .collect();
        
        for edge in &edges_to_remove {
            self.remove_edge(edge);
        }
        
        // 重新连接输入和输出节点
        self.reconnect_nodes_after_removal(node_id, &inputs, &outputs, &control_inputs, &control_outputs);
        
        // 移除节点
        self.nodes.remove(&node_id);
        
        Ok(())
    }

    /// 在移除节点后重新连接其输入输出节点
    fn reconnect_nodes_after_removal(
        &mut self,
        removed_node_id: SonNodeId,
        inputs: &[SonNodeId],
        outputs: &[SonNodeId],
        control_inputs: &[SonNodeId],
        control_outputs: &[SonNodeId],
    ) {
        // 收集需要添加的新边，避免在遍历时修改
        let mut new_data_edges = Vec::new();
        let mut new_control_edges = Vec::new();
        
        // 对于每个输入节点，将其输出重新连接到输出节点
        for &input_id in inputs {
            if let Some(input_node) = self.nodes.get_mut(&input_id) {
                // 移除对已删除节点的输出引用
                input_node.outputs.retain(|&id| id != removed_node_id);
                
                // 将输入节点的输出重新连接到输出节点
                for &output_id in outputs {
                    if output_id != input_id && !input_node.outputs.contains(&output_id) {
                        input_node.outputs.push(output_id);
                        new_data_edges.push((input_id, output_id));
                    }
                }
            }
        }
        
        // 对于每个输出节点，将其输入重新连接到输入节点
        for &output_id in outputs {
            if let Some(output_node) = self.nodes.get_mut(&output_id) {
                // 移除对已删除节点的输入引用
                output_node.inputs.retain(|&id| id != removed_node_id);
                
                // 将输出节点的输入重新连接到输入节点
                for &input_id in inputs {
                    if input_id != output_id && !output_node.inputs.contains(&input_id) {
                        output_node.inputs.push(input_id);
                    }
                }
            }
        }
        
        // 处理控制流连接
        for &control_input_id in control_inputs {
            if let Some(control_input_node) = self.nodes.get_mut(&control_input_id) {
                control_input_node.control_outputs.retain(|&id| id != removed_node_id);
                
                for &control_output_id in control_outputs {
                    if control_output_id != control_input_id && !control_input_node.control_outputs.contains(&control_output_id) {
                        control_input_node.control_outputs.push(control_output_id);
                        new_control_edges.push((control_input_id, control_output_id));
                    }
                }
            }
        }
        
        for &control_output_id in control_outputs {
            if let Some(control_output_node) = self.nodes.get_mut(&control_output_id) {
                control_output_node.control_inputs.retain(|&id| id != removed_node_id);
                
                for &control_input_id in control_inputs {
                    if control_input_id != control_output_id && !control_output_node.control_inputs.contains(&control_input_id) {
                        control_output_node.control_inputs.push(control_input_id);
                    }
                }
            }
        }
        
        // 在遍历完成后添加新的边
        for (from, to) in new_data_edges {
            let new_edge = SonEdge::new(from, to, EdgeType::Data);
            self.add_edge(new_edge);
        }
        
        for (from, to) in new_control_edges {
            let new_control_edge = SonEdge::new(from, to, EdgeType::Control);
            self.add_edge(new_control_edge);
        }
    }

    /// 查找连接到指定节点的所有边
    pub fn get_edges_connected_to_node(&self, node_id: SonNodeId) -> Vec<&SonEdge> {
        self.edges.iter()
            .filter(|edge| edge.from == node_id || edge.to == node_id)
            .collect()
    }

    /// 查找从指定节点出发的所有边
    pub fn get_edges_from_node(&self, node_id: SonNodeId) -> Vec<&SonEdge> {
        self.edges.iter()
            .filter(|edge| edge.from == node_id)
            .collect()
    }

    /// 查找到达指定节点的所有边
    pub fn get_edges_to_node(&self, node_id: SonNodeId) -> Vec<&SonEdge> {
        self.edges.iter()
            .filter(|edge| edge.to == node_id)
            .collect()
    }

    /// 检查两个节点之间是否存在边
    pub fn has_edge(&self, from: SonNodeId, to: SonNodeId, edge_type: EdgeType) -> bool {
        self.edges.iter()
            .any(|edge| edge.from == from && edge.to == to && edge.edge_type == edge_type)
    }

    /// 获取两个节点之间的所有边
    pub fn get_edges_between(&self, from: SonNodeId, to: SonNodeId) -> Vec<&SonEdge> {
        self.edges.iter()
            .filter(|edge| edge.from == from && edge.to == to)
            .collect()
    }

    /// 创建常量节点
    pub fn create_constant_node(&mut self, value: ConstantValue, typ: Type) -> SonNodeId {
        let constant_kind = SonNodeKind::with_data(OpCode::Constant, NodeData::Constant { value, typ });
        let constant_node = SonNode::new(0, constant_kind); // ID will be set by add_node
        self.add_node(constant_node)
    }

    /// 获取节点的所有前驱节点（输入节点）
    pub fn get_predecessors(&self, node_id: SonNodeId) -> Vec<SonNodeId> {
        self.nodes.get(&node_id)
            .map(|node| node.inputs.clone())
            .unwrap_or_default()
    }

    /// 获取节点的所有后继节点（输出节点）
    pub fn get_successors(&self, node_id: SonNodeId) -> Vec<SonNodeId> {
        self.nodes.get(&node_id)
            .map(|node| node.outputs.clone())
            .unwrap_or_default()
    }

    /// 获取节点的所有控制流前驱节点
    pub fn get_control_predecessors(&self, node_id: SonNodeId) -> Vec<SonNodeId> {
        self.nodes.get(&node_id)
            .map(|node| node.control_inputs.clone())
            .unwrap_or_default()
    }

    /// 获取节点的所有控制流后继节点
    pub fn get_control_successors(&self, node_id: SonNodeId) -> Vec<SonNodeId> {
        self.nodes.get(&node_id)
            .map(|node| node.control_outputs.clone())
            .unwrap_or_default()
    }

    /// 递归杀死节点：杀死指定节点及其所有未使用的输入节点
    pub fn kill_node_recursively(&mut self, node_id: SonNodeId) -> Result<(), String> {
        let node = self.nodes.get(&node_id)
            .ok_or_else(|| format!("Node {} does not exist", node_id))?;
        
        // 如果节点被标记为重要，不能杀死
        if node.is_keep {
            return Ok(());
        }
        
        // 收集所有输入节点
        let mut inputs_to_check = Vec::new();
        inputs_to_check.extend(node.inputs.iter().cloned());
        inputs_to_check.extend(node.control_inputs.iter().cloned());
        
        // 先杀死当前节点
        self.remove_node(node_id)?;
        
        // 递归检查输入节点是否可以杀死
        for input_id in inputs_to_check {
            if let Some(input_node) = self.nodes.get(&input_id) {
                // 如果输入节点没有其他输出，且不是重要节点，则递归杀死
                if !input_node.is_keep && 
                   input_node.outputs.is_empty() && 
                   input_node.control_outputs.is_empty() {
                    self.kill_node_recursively(input_id)?;
                }
            }
        }
        
        Ok(())
    }

    /// 检查节点是否可以被杀死（没有输出且不是重要节点）
    pub fn can_kill_node(&self, node_id: SonNodeId) -> bool {
        if let Some(node) = self.nodes.get(&node_id) {
            !node.is_keep && 
            node.outputs.is_empty() && 
            node.control_outputs.is_empty()
        } else {
            false
        }
    }
}

// 为Type实现Display trait以支持to_string()
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::IntType => write!(f, "IntType"),
            Type::FloatType => write!(f, "FloatType"),
            Type::BoolType => write!(f, "BoolType"),
            Type::CharType => write!(f, "CharType"),
            Type::VoidType => write!(f, "VoidType"),
            Type::ArrayType { element_type, array_size } => {
                if let Some(size) = array_size {
                    write!(f, "ArrayType[{}; {}]", element_type, size)
                } else {
                    write!(f, "ArrayType[{}]", element_type)
                }
            }
            Type::PointerType { target_type } => {
                write!(f, "PointerType({})", target_type)
            }
            Type::FunctionType { return_type, parameter_types } => {
                write!(f, "FunctionType({:?} -> {})", parameter_types, return_type)
            }
        }
    }
}

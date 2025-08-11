use crate::frontend::ast::{Ast, AstKind, Expression, Statement, BinaryOperator, UnaryOperator, Literal, Type};
use super::son_ir::{SonNodeKind, OpCode, NodeData, ConstantValue};

/// AST 到 Sea of Nodes IR 的节点映射器
pub struct NodeMapping;

impl NodeMapping {
    /// 将 AST 节点类型转换为 Sea of Nodes 节点类型
    pub fn ast_to_son_kind(ast: &Ast) -> Option<SonNodeKind> {
        match &ast.kind {
            AstKind::Program { .. } => {
                Some(SonNodeKind::new(OpCode::Start))
            }
            AstKind::Function { function_name, return_type, .. } => {
                let data = NodeData::Call {
                    function_name: function_name.clone(),
                    return_type: return_type.clone().unwrap_or(Type::VoidType),
                    arguments: Vec::new(),
                };
                Some(SonNodeKind::with_data(OpCode::Call, data))
            }
            AstKind::VariableDeclaration { variable_name, variable_type, .. } => {
                let data = NodeData::Local {
                    name: variable_name.clone(),
                    typ: variable_type.clone(),
                };
                Some(SonNodeKind::with_data(OpCode::Local, data))
            }
            AstKind::Statement(stmt) => {
                Self::statement_to_son_kind(stmt)
            }
            AstKind::Expression(expr) => {
                Self::expression_to_son_kind(expr)
            }
            AstKind::Type(_) => None,
        }
    }
    
    /// 将语句转换为 Sea of Nodes 节点类型
    fn statement_to_son_kind(stmt: &Statement) -> Option<SonNodeKind> {
        match stmt {
            Statement::Compound { .. } => {
                Some(SonNodeKind::new(OpCode::Region))
            }
            Statement::ExpressionStatement { .. } => {
                Some(SonNodeKind::new(OpCode::Region))
            }
            Statement::Return { .. } => {
                Some(SonNodeKind::new(OpCode::Return))
            }
            Statement::If { .. } => {
                Some(SonNodeKind::new(OpCode::If))
            }
            Statement::While { .. } => {
                Some(SonNodeKind::new(OpCode::Loop))
            }
            Statement::For { .. } => {
                Some(SonNodeKind::new(OpCode::Loop))
            }
            Statement::Break => {
                Some(SonNodeKind::new(OpCode::Break))
            }
            Statement::Continue => {
                Some(SonNodeKind::new(OpCode::Continue))
            }
            Statement::Empty => None,
        }
    }
    
    /// 将表达式转换为 Sea of Nodes 节点类型
    fn expression_to_son_kind(expr: &Expression) -> Option<SonNodeKind> {
        match expr {
            Expression::Literal(literal) => {
                Self::literal_to_son_kind(literal)
            }
            Expression::Identifier { name } => {
                let data = NodeData::Local {
                    name: name.clone(),
                    typ: Type::IntType, // 类型将在语义分析后填充
                };
                Some(SonNodeKind::with_data(OpCode::Local, data))
            }
            Expression::BinaryOperation { operator, .. } => {
                Self::binary_operator_to_son_kind(operator)
            }
            Expression::UnaryOperation { operator, .. } => {
                Self::unary_operator_to_son_kind(operator)
            }
            Expression::Assignment { .. } => {
                Some(SonNodeKind::new(OpCode::Store))
            }
            Expression::FunctionCall { function_name, .. } => {
                let data = NodeData::Call {
                    function_name: function_name.clone(),
                    return_type: Type::IntType, // 类型将在语义分析后填充
                    arguments: Vec::new(),
                };
                Some(SonNodeKind::with_data(OpCode::Call, data))
            }
            Expression::ArrayAccess { .. } => {
                Some(SonNodeKind::new(OpCode::ArrayAccess))
            }
            Expression::MemberAccess { .. } => {
                Some(SonNodeKind::new(OpCode::MemberAccess))
            }
            Expression::InitializerList { .. } => None,
        }
    }
    
    /// 将字面量转换为 Sea of Nodes 节点类型
    fn literal_to_son_kind(literal: &Literal) -> Option<SonNodeKind> {
        let (value, typ) = match literal {
            Literal::IntegerLiteral(n) => {
                (ConstantValue::Integer(*n as i64), Type::IntType)
            }
            Literal::FloatLiteral(f) => {
                (ConstantValue::Float(*f as f64), Type::FloatType)
            }
            Literal::StringLiteral(s) => {
                (ConstantValue::String(s.clone()), Type::CharType)
            }
            Literal::BooleanLiteral(b) => {
                (ConstantValue::Boolean(*b), Type::BoolType)
            }
        };
        
        let data = NodeData::Constant { value, typ };
        Some(SonNodeKind::with_data(OpCode::Constant, data))
    }
    
    /// 将二元运算符转换为 Sea of Nodes 节点类型
    fn binary_operator_to_son_kind(operator: &BinaryOperator) -> Option<SonNodeKind> {
        let opcode = match operator {
            BinaryOperator::Add => OpCode::Add,
            BinaryOperator::Subtract => OpCode::Subtract,
            BinaryOperator::Multiply => OpCode::Multiply,
            BinaryOperator::Divide => OpCode::Divide,
            BinaryOperator::Modulo => OpCode::Modulo,
            BinaryOperator::Equal => OpCode::Equal,
            BinaryOperator::NotEqual => OpCode::NotEqual,
            BinaryOperator::LessThan => OpCode::LessThan,
            BinaryOperator::LessEqual => OpCode::LessEqual,
            BinaryOperator::GreaterThan => OpCode::GreaterThan,
            BinaryOperator::GreaterEqual => OpCode::GreaterEqual,
            BinaryOperator::LogicalAnd => OpCode::LogicalAnd,
            BinaryOperator::LogicalOr => OpCode::LogicalOr,
            BinaryOperator::Assign => OpCode::Store,
            BinaryOperator::AddAssign => OpCode::Add,
            BinaryOperator::SubtractAssign => OpCode::Subtract,
            BinaryOperator::MultiplyAssign => OpCode::Multiply,
            BinaryOperator::DivideAssign => OpCode::Divide,
            BinaryOperator::ModuloAssign => OpCode::Modulo,
        };
        
        Some(SonNodeKind::new(opcode))
    }
    
    /// 将一元运算符转换为 Sea of Nodes 节点类型
    fn unary_operator_to_son_kind(operator: &UnaryOperator) -> Option<SonNodeKind> {
        let opcode = match operator {
            UnaryOperator::Minus => OpCode::Minus,
            UnaryOperator::LogicalNot => OpCode::LogicalNot,
            UnaryOperator::BitwiseNot => OpCode::BitwiseNot,
            UnaryOperator::Plus => OpCode::Add,
            UnaryOperator::Dereference => OpCode::Load,
            UnaryOperator::AddressOf => OpCode::Store,
            UnaryOperator::Increment => OpCode::Add,
            UnaryOperator::Decrement => OpCode::Subtract,
        };
        
        Some(SonNodeKind::new(opcode))
    }
    
    /// 创建参数节点
    pub fn create_parameter_node(name: String, typ: Type) -> SonNodeKind {
        let data = NodeData::Parameter { name, typ };
        SonNodeKind::with_data(OpCode::Parameter, data)
    }
    
    /// 创建常量节点
    pub fn create_constant_node(value: ConstantValue, typ: Type) -> SonNodeKind {
        let data = NodeData::Constant { value, typ };
        SonNodeKind::with_data(OpCode::Constant, data)
    }
    
    /// 创建二元运算节点
    pub fn create_binary_op_node(opcode: OpCode, left: Option<usize>, right: Option<usize>) -> SonNodeKind {
        let data = NodeData::BinaryOp { left, right };
        SonNodeKind::with_data(opcode, data)
    }
    
    /// 创建一元运算节点
    pub fn create_unary_op_node(opcode: OpCode, operand: Option<usize>) -> SonNodeKind {
        let data = NodeData::UnaryOp { operand };
        SonNodeKind::with_data(opcode, data)
    }
    
    /// 创建函数调用节点
    pub fn create_call_node(function_name: String, return_type: Type, arguments: Vec<usize>) -> SonNodeKind {
        let data = NodeData::Call {
            function_name,
            return_type,
            arguments,
        };
        SonNodeKind::with_data(OpCode::Call, data)
    }
    
    /// 创建类型转换节点
    pub fn create_cast_node(from_type: Type, to_type: Type, value: Option<usize>) -> SonNodeKind {
        let data = NodeData::Cast {
            from_type,
            to_type,
            value,
        };
        SonNodeKind::with_data(OpCode::Cast, data)
    }
    
    /// 创建 Phi 节点
    pub fn create_phi_node(label: String, typ: Type, inputs: Vec<Option<usize>>) -> SonNodeKind {
        let data = NodeData::Phi { label, typ, inputs, region: None };
        SonNodeKind::with_data(OpCode::Phi, data)
    }
    
    /// 创建投影节点
    pub fn create_proj_node(index: usize, label: String) -> SonNodeKind {
        let data = NodeData::Proj { index, label };
        SonNodeKind::with_data(OpCode::Proj, data)
    }
    
    /// 创建控制投影节点
    pub fn create_cproj_node(index: usize, label: String) -> SonNodeKind {
        let data = NodeData::CProj { index, label };
        SonNodeKind::with_data(OpCode::CProj, data)
    }
    
    /// 创建内存加载节点
    pub fn create_load_node(name: String, alias: u32, declared_type: Type, 
                           mem: Option<usize>, ptr: Option<usize>, offset: Option<usize>) -> SonNodeKind {
        let data = NodeData::Load {
            name,
            alias,
            declared_type,
            mem,
            ptr,
            offset,
        };
        SonNodeKind::with_data(OpCode::Load, data)
    }
    
    /// 创建内存存储节点
    pub fn create_store_node(name: String, alias: u32, declared_type: Type,
                            mem: Option<usize>, ptr: Option<usize>, offset: Option<usize>, 
                            value: Option<usize>, init: bool) -> SonNodeKind {
        let data = NodeData::Store {
            name,
            alias,
            declared_type,
            mem,
            ptr,
            offset,
            value,
            init,
        };
        SonNodeKind::with_data(OpCode::Store, data)
    }
    
    /// 创建数组访问节点
    pub fn create_array_access_node(array: Option<usize>, index: Option<usize>) -> SonNodeKind {
        let data = NodeData::ArrayAccess { array, index };
        SonNodeKind::with_data(OpCode::ArrayAccess, data)
    }
    
    /// 创建成员访问节点
    pub fn create_member_access_node(object: Option<usize>, field: String) -> SonNodeKind {
        let data = NodeData::MemberAccess { object, field };
        SonNodeKind::with_data(OpCode::MemberAccess, data)
    }
    
    /// 创建新对象节点
    pub fn create_new_node(ptr_type: Type, inputs: Vec<Option<usize>>) -> SonNodeKind {
        // 这里需要根据具体的指针类型来创建
        // 暂时使用一个通用的新节点
        let data = NodeData::None; // 可以根据需要扩展
        SonNodeKind::with_data(OpCode::New, data)
    }
    
    /// 创建浮点转换节点
    pub fn create_to_float_node(operand: Option<usize>) -> SonNodeKind {
        let data = NodeData::UnaryOp { operand };
        SonNodeKind::with_data(OpCode::ToFloat, data)
    }
    
    /// 创建浮点舍入节点
    pub fn create_round_f32_node(operand: Option<usize>) -> SonNodeKind {
        let data = NodeData::UnaryOp { operand };
        SonNodeKind::with_data(OpCode::RoundF32, data)
    }
    
    /// 检查节点是否为控制流节点
    pub fn is_control_flow_node(kind: &SonNodeKind) -> bool {
        kind.is_control_flow()
    }
    
    /// 检查节点是否为数据流节点
    pub fn is_data_flow_node(kind: &SonNodeKind) -> bool {
        kind.is_data_flow()
    }
    
    /// 获取节点的操作码
    pub fn get_opcode(kind: &SonNodeKind) -> &OpCode {
        &kind.opcode
    }
    
    /// 获取节点的标签
    pub fn get_label(kind: &SonNodeKind) -> String {
        kind.label()
    }
}

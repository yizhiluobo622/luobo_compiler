use crate::frontend::span::Span;

/// 语义信息
#[derive(Debug, Clone)]
pub struct SemanticInfo {
    /// 推导出的类型
    pub deduced_type: Option<Type>,
    /// 符号名称（如果是标识符）
    pub symbol_name: Option<String>,
    /// 语义错误
    pub semantic_errors: Vec<String>,
    /// 是否已进行语义分析
    pub analyzed: bool,
}

impl SemanticInfo {
    /// 创建空的语义信息
    pub fn new() -> Self {
        Self {
            deduced_type: None,
            symbol_name: None,
            semantic_errors: Vec::new(),
            analyzed: false,
        }
    }
    
    /// 设置推导类型
    pub fn set_deduced_type(&mut self, typ: Type) {
        self.deduced_type = Some(typ);
        self.analyzed = true;
    }
    
    /// 设置符号名称
    pub fn set_symbol_name(&mut self, name: String) {
        self.symbol_name = Some(name);
        self.analyzed = true;
    }
    
    /// 添加语义错误
    pub fn add_error(&mut self, error: String) {
        self.semantic_errors.push(error);
        self.analyzed = true;
    }
    
    /// 检查是否有语义错误
    pub fn has_errors(&self) -> bool {
        !self.semantic_errors.is_empty()
    }
    
    /// 获取推导类型
    pub fn get_deduced_type(&self) -> Option<&Type> {
        self.deduced_type.as_ref()
    }
}

/// 抽象语法树节点
#[derive(Debug, Clone)]
pub struct Ast {
    pub kind: AstKind,
    pub span: Span,
    /// 语义信息
    pub semantic_info: SemanticInfo,
}

/// AST节点类型
#[derive(Debug, Clone)]
pub enum AstKind {
    // 程序根节点
    Program {
        functions: Vec<Ast>,
        global_variables: Vec<Ast>,
    },
    
    // 函数定义
    Function {
        function_name: String,
        parameters: Vec<Ast>,
        return_type: Option<Type>,
        function_body: Box<Ast>,
    },
    
    // 变量声明
    VariableDeclaration {
        variable_name: String,
        variable_type: Type,
        initial_value: Option<Box<Ast>>,
        is_const: bool,
    },
    
    // 语句
    Statement(Statement),
    
    // 表达式
    Expression(Expression),
    
    // 类型
    Type(Type),
}

/// 语句类型
#[derive(Debug, Clone)]
pub enum Statement {
    // 复合语句 (代码块)
    Compound {
        statements: Vec<Ast>,
    },
    
    // 表达式语句
    ExpressionStatement {
        expression: Box<Ast>,
    },
    
    // 返回语句
    Return {
        value: Option<Box<Ast>>,
    },
    
    // if语句
    If {
        condition: Box<Ast>,
        then_branch: Box<Ast>,
        else_branch: Option<Box<Ast>>,
    },
    
    // while循环
    While {
        condition: Box<Ast>,
        body: Box<Ast>,
    },
    
    // for循环
    For {
        initialization: Option<Box<Ast>>,
        condition: Option<Box<Ast>>,
        update: Option<Box<Ast>>,
        body: Box<Ast>,
    },
    
    // break语句
    Break,
    
    // continue语句
    Continue,
    
    // 空语句
    Empty,
}

/// 表达式类型
#[derive(Debug, Clone)]
pub enum Expression {
    // 字面量
    Literal(Literal),
    
    // 标识符
    Identifier {
        name: String,
    },
    
    // 二元运算
    BinaryOperation {
        operator: BinaryOperator,
        left_operand: Box<Ast>,
        right_operand: Box<Ast>,
    },
    
    // 一元运算
    UnaryOperation {
        operator: UnaryOperator,
        operand: Box<Ast>,
    },
    
    // 函数调用
    FunctionCall {
        function_name: String,
        arguments: Vec<Ast>,
    },
    
    // 赋值
    Assignment {
        target: Box<Ast>,
        value: Box<Ast>,
    },
    
    // 初始化列表 { a, b, { c, d } }
    InitializerList {
        elements: Vec<Ast>,
    },
    
    // 数组访问
    ArrayAccess {
        array: Box<Ast>,
        index: Box<Ast>,
    },
    
    // 成员访问 (结构体)
    MemberAccess {
        object: Box<Ast>,
        member_name: String,
    },
}

/// 字面量类型
#[derive(Debug, Clone)]
pub enum Literal {
    IntegerLiteral(i32),
    FloatLiteral(f32),
    StringLiteral(String),
    BooleanLiteral(bool),
}

/// 二元运算符
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,           // +
    Subtract,      // -
    Multiply,      // *
    Divide,        // /
    Modulo,        // %
    Equal,         // ==
    NotEqual,      // !=
    LessThan,      // <
    LessEqual,     // <=
    GreaterThan,   // >
    GreaterEqual,  // >=
    LogicalAnd,    // &&
    LogicalOr,     // ||
    Assign,        // =
    AddAssign,     // +=
    SubtractAssign, // -=
    MultiplyAssign, // *=
    DivideAssign,   // /=
    ModuloAssign,   // %=
}

/// 一元运算符
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Plus,          // +
    Minus,         // -
    LogicalNot,    // !
    BitwiseNot,    // ~
    Dereference,   // *
    AddressOf,     // &
    Increment,     // ++
    Decrement,     // --
}

/// 类型
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    IntType,
    FloatType,
    VoidType,
    CharType,
    BoolType,
    ArrayType {
        element_type: Box<Type>,
        array_size: Option<usize>,
    },
    PointerType {
        target_type: Box<Type>,
    },
    FunctionType {
        parameter_types: Vec<Type>,
        return_type: Box<Type>,
    },
}

impl Type {
    /// 检查类型是否为常量类型
    pub fn is_constant(&self) -> bool {
        match self {
            Type::IntType | Type::FloatType | Type::CharType | Type::BoolType => true,
            Type::ArrayType { element_type, .. } => element_type.is_constant(),
            Type::PointerType { target_type } => target_type.is_constant(),
            Type::FunctionType { .. } => false,
            Type::VoidType => false,
        }
    }

    /// 检查类型是否兼容（可以安全地转换）
    pub fn is_compatible_with(&self, other: &Type) -> bool {
        match (self, other) {
            (t1, t2) => t1 == t2 || {
                // 检查是否可以安全转换
                match (t1, t2) {
                    (Type::IntType, Type::FloatType) => true,
                    (Type::CharType, Type::IntType) => true,
                    (Type::BoolType, Type::IntType) => true,
                    _ => false,
                }
            }
        }
    }
}

impl Ast {
    /// 创建新的AST节点
    pub fn new(kind: AstKind, span: Span) -> Self {
        Self {
            kind,
            span,
            semantic_info: SemanticInfo::new(),
        }
    }
    
    /// 获取节点类型
    pub fn kind(&self) -> &AstKind {
        &self.kind
    }
    
    /// 获取位置信息
    pub fn span(&self) -> &Span {
        &self.span
    }
}



impl From<Statement> for AstKind {
    fn from(stmt: Statement) -> Self {
        AstKind::Statement(stmt)
    }
}

impl From<Expression> for AstKind {
    fn from(expr: Expression) -> Self {
        AstKind::Expression(expr)
    }
}

impl From<Type> for AstKind {
    fn from(typ: Type) -> Self {
        AstKind::Type(typ)
    }
}

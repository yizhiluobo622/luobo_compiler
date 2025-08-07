use crate::frontend::span::Span;

/// 抽象语法树节点
#[derive(Debug, Clone)]
pub struct Ast {
    pub kind: AstKind,
    pub span: Span,
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

/// 类型系统
#[derive(Debug, Clone)]
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

impl Ast {
    /// 创建新的AST节点
    pub fn new(kind: AstKind, span: Span) -> Self {
        Self { kind, span }
    }
    
    /// 获取节点类型
    pub fn kind(&self) -> &AstKind {
        &self.kind
    }
    
    /// 获取节点位置信息
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

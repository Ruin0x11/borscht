use super::lexical::PrimitiveToken;
use std::borrow::Cow;

pub type AstNodeRef<'a> = Box<AstNode<'a>>;

#[derive(Clone, Debug)]
pub struct IfStatementNode<'a> {
    pub primitive: PrimitiveToken<'a>,
    pub arg: Option<AstNodeRef<'a>>,
    pub if_block: AstNodeRef<'a>,
    pub else_block: Option<AstNodeRef<'a>>,
}

#[derive(Clone, Debug)]
pub struct AssignmentNode<'a> {
    pub var: AstNodeRef<'a>,
    pub operator: PrimitiveToken<'a>,
    pub argument: Option<AstNodeRef<'a>> // Is None in cases like `x++`
}

#[derive(Clone, Debug)]
pub enum LiteralNode<'a> {
    Integer(i32),
    Double(f32),
    String(Cow<'a, str>)
}

#[derive(Clone, Debug)]
pub struct VariableNode<'a> {
    pub ident: PrimitiveToken<'a>,
    pub arg: Option<AstNodeRef<'a>>
}

#[derive(Clone, Debug)]
pub struct ExpressionNode<'a> {
    pub lhs: AstNodeRef<'a>,
    pub op: Option<PrimitiveToken<'a>>,
    pub rhs: Option<AstNodeRef<'a>>
}

#[derive(Clone, Debug)]
pub struct ArgumentNode<'a> {
    pub exps: Vec<AstNodeRef<'a>>,
    pub has_bracket: bool,
    pub first_arg_is_null: bool
}

#[derive(Clone, Debug)]
pub struct FunctionNode<'a> {
    pub ident: PrimitiveToken<'a>,
    pub arg: Option<AstNodeRef<'a>>
}

#[derive(Clone, Debug)]
pub struct OnStatementNode<'a> {
    pub primitive: PrimitiveToken<'a>,
    pub exp: Option<AstNodeRef<'a>>,
    pub func: Option<AstNodeRef<'a>>,
}

#[derive(Clone, Debug)]
pub struct OnEventStatementNode<'a> {
    pub primitive: PrimitiveToken<'a>,
    pub func: Option<AstNodeRef<'a>>,
}

#[derive(Clone, Debug)]
pub struct BlockStatementNode<'a> {
    pub nodes: Vec<AstNodeRef<'a>>
}

#[derive(Clone, Debug)]
pub struct LabelDeclarationNode<'a> {
    pub primitive: PrimitiveToken<'a>,
}

#[derive(Clone, Debug)]
pub struct McallStatementNode<'a> {
    pub primitive: PrimitiveToken<'a>,
    pub var: AstNodeRef<'a>,
    pub primary_exp: AstNodeRef<'a>,
    pub secondary_exps: Vec<AstNodeRef<'a>>,
}

#[derive(Clone, Debug)]
pub struct CommandStatementNode<'a> {
    pub func: AstNodeRef<'a>,
}

#[derive(Clone, Debug)]
pub enum AstNodeKind<'a> {
    IfStatement(IfStatementNode<'a>),
    Assignment(AssignmentNode<'a>),
    Literal(LiteralNode<'a>),
    Variable(VariableNode<'a>),
    Expression(ExpressionNode<'a>),
    Argument(ArgumentNode<'a>),
    Function(FunctionNode<'a>),
    OnStatement(OnStatementNode<'a>),
    OnEventStatement(OnEventStatementNode<'a>),
    BlockStatement(BlockStatementNode<'a>),
    LabelDeclaration(LabelDeclarationNode<'a>),
    CommandStatement(CommandStatementNode<'a>),
    McallStatement(McallStatementNode<'a>),
}

#[derive(Clone, Debug)]
pub struct AstNode<'a> {
    pub token_offset: u32,
    pub tab_count: u32,
    pub visible: bool,
    pub errors: Vec<String>,
    pub comments: Vec<String>,
    pub kind: AstNodeKind<'a>
}

impl<'a> AstNode<'a> {
    pub fn new(token_offset: u32, kind: AstNodeKind<'a>) -> Self {
        AstNode {
            token_offset: token_offset,
            tab_count: 0,
            visible: true,
            errors: Vec::new(),
            comments: Vec::new(),
            kind: kind
        }
    }
}

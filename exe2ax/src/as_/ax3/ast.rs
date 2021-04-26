use super::lexical::PrimitiveToken;

pub type AstNodeRef<'a> = Box<AstNode<'a>>;

pub struct IfStatementNode<'a> {
    pub primitive: PrimitiveToken<'a>,
    pub arg: AstNodeRef<'a>,
    pub if_block: AstNodeRef<'a>,
    pub else_block: Option<AstNodeRef<'a>>,
}

pub struct AssignmentNode<'a> {
    pub var: AstNodeRef<'a>,
    pub operator: PrimitiveToken<'a>,
    pub argument: Option<AstNodeRef<'a>> // Is None in cases like `x++`
}

pub struct VariableNode<'a> {
    pub ident: PrimitiveToken<'a>,
    pub arg: Option<AstNodeRef<'a>>
}

pub struct OnStatementNode<'a> {
    pub primitive: PrimitiveToken<'a>,
    pub exp: Option<AstNodeRef<'a>>,
    pub func: Option<AstNodeRef<'a>>,
}

pub struct OnEventStatementNode<'a> {
    pub primitive: PrimitiveToken<'a>,
    pub func: Option<AstNodeRef<'a>>,
}

pub struct BlockStatementNode<'a> {
    pub nodes: Vec<AstNodeRef<'a>>
}

pub struct LabelDeclarationNode<'a> {
    pub primitive: PrimitiveToken<'a>,
}

pub struct McallStatementNode<'a> {
    pub primitive: PrimitiveToken<'a>,
    pub var: AstNodeRef<'a>,
    pub primary_exp: AstNodeRef<'a>,
    pub secondary_exps: Vec<AstNodeRef<'a>>,
}

pub struct CommandStatementNode<'a> {
    pub func: AstNodeRef<'a>,
}

pub enum AstNodeKind<'a> {
    IfStatement(IfStatementNode<'a>),
    Assignment(AssignmentNode<'a>),
    Variable(VariableNode<'a>),
    OnStatement(OnStatementNode<'a>),
    OnEventStatement(OnEventStatementNode<'a>),
    BlockStatement(BlockStatementNode<'a>),
    LabelDeclaration(LabelDeclarationNode<'a>),
    CommandStatement(CommandStatementNode<'a>),
    McallStatement(McallStatementNode<'a>),
}

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
            tab_count: tab_count,
            visible: true,
            errors: Vec::new(),
            comments: Vec::new(),
            kind: kind
        }
    }
}

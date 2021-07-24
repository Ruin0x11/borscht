use super::lexical::{PrimitiveToken, PrimitiveTokenKind};
use std::borrow::Cow;
use std::fmt;

pub type AstNodeRef<'a> = Box<AstNode<'a>>;

#[derive(Clone, Debug)]
pub struct IfStatementNode<'a> {
    pub primitive: PrimitiveToken<'a>,
    pub arg: Option<AstNodeRef<'a>>,
    pub if_block: AstNodeRef<'a>,
    pub else_primitive: Option<PrimitiveToken<'a>>,
    pub else_block: Option<AstNodeRef<'a>>,
}

impl<'a> fmt::Display for IfStatementNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.arg {
            Some(arg) => write!(f, "{} ({}) {}", self.primitive, arg, self.if_block)?,
            None => write!(f, "{} {}", self.primitive, self.if_block)?
        }
        if let Some(else_block) = &self.else_block {
            write!(f, "{} {}", self.else_primitive.as_ref().unwrap(), else_block)?
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct AssignmentNode<'a> {
    pub var: AstNodeRef<'a>,
    pub operator: PrimitiveToken<'a>,
    pub argument: Option<AstNodeRef<'a>> // is None in cases like `x++`
}

impl<'a> fmt::Display for AssignmentNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.argument {
            Some(arg) => write!(f, "{} {} {}", self.var, self.operator, arg),
            None => write!(f, "{}{}", self.var, self.operator)
        }
    }
}

#[derive(Clone, Debug)]
pub enum LiteralNode<'a> {
    Integer(i32),
    Double(f32),
    String(Cow<'a, str>)
}

impl<'a> fmt::Display for LiteralNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralNode::Integer(i) => write!(f, "{}", i),
            LiteralNode::Double(d) => write!(f, "{}", d),
            LiteralNode::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(Clone, Debug)]
pub struct VariableNode<'a> {
    pub ident: PrimitiveToken<'a>,
    pub arg: Option<AstNodeRef<'a>>
}

impl<'a> fmt::Display for VariableNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let PrimitiveTokenKind::GlobalVariable(name) = &self.ident.kind {
            match &self.arg {
                Some(arg) => write!(f, "{}({})", name, arg),
                None => write!(f, "{}", name)
            }
        }
        else {
            unreachable!()
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExpressionNode<'a> {
    pub lhs: AstNodeRef<'a>,
    pub op: Option<PrimitiveToken<'a>>,
    pub rhs: Option<AstNodeRef<'a>>
}

impl<'a> fmt::Display for ExpressionNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.rhs {
            Some(rhs) => match &self.op {
                Some(op) => write!(f, "{} {} {}", self.lhs, op, rhs),
                None => unreachable!()
            }
            None => match &self.op {
                Some(op) => write!(f, "{}{}", self.lhs, op),
                None => write!(f, "{}", self.lhs)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct ArgumentNode<'a> {
    pub exps: Vec<AstNodeRef<'a>>,
    pub has_bracket: bool,
    pub first_arg_is_null: bool
}

impl<'a> fmt::Display for ArgumentNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.has_bracket {
            write!(f, "(")?;
        } else {
            write!(f, " ")?;
        }

        if self.first_arg_is_null {
            write!(f, ", ")?;
        }

        for (i, exp) in self.exps.iter().enumerate() {
            write!(f, "{}", exp)?;
            if i < self.exps.len() - 1 {
                write!(f, ", ")?;
            }
        }

        if self.has_bracket {
            write!(f, ")")?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct FunctionNode<'a> {
    pub ident: PrimitiveToken<'a>,
    pub arg: Option<AstNodeRef<'a>>
}

impl<'a> fmt::Display for FunctionNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ident)?;
        if let Some(arg) = &self.arg {
            write!(f, "{}", arg)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct OnStatementNode<'a> {
    pub primitive: PrimitiveToken<'a>,
    pub exp: Option<AstNodeRef<'a>>,
    pub func: Option<AstNodeRef<'a>>,
}

impl<'a> fmt::Display for OnStatementNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.primitive)?;

        if let Some(exp) = &self.exp {
            write!(f, " {}", exp)?;
        }

        if let Some(func) = &self.func {
            write!(f, " {}", func)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct OnEventStatementNode<'a> {
    pub primitive: PrimitiveToken<'a>,
    pub func: Option<AstNodeRef<'a>>,
}

impl<'a> fmt::Display for OnEventStatementNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.primitive)?;
        if let Some(func) = &self.func {
            write!(f, " {}", func)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct BlockStatementNode<'a> {
    pub nodes: Vec<AstNodeRef<'a>>
}

impl<'a> fmt::Display for BlockStatementNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{\n")?;
        for exp in self.nodes.iter() {
            write!(f, "{}\n", exp)?;
        }
        write!(f, "\n}}\n")
    }
}

#[derive(Clone, Debug)]
pub struct LabelDeclarationNode<'a> {
    pub primitive: PrimitiveToken<'a>,
}

impl<'a> fmt::Display for LabelDeclarationNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.primitive)
    }
}

#[derive(Clone, Debug)]
pub struct McallStatementNode<'a> {
    pub primitive: PrimitiveToken<'a>,
    pub var: AstNodeRef<'a>,
    pub primary_exp: AstNodeRef<'a>,
    pub arg: Option<AstNodeRef<'a>>,
}

impl<'a> fmt::Display for McallStatementNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}->{}", self.primitive, self.var)
    }
}

#[derive(Clone, Debug)]
pub struct CommandStatementNode<'a> {
    pub func: AstNodeRef<'a>,
}

impl<'a> fmt::Display for CommandStatementNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.func)
    }
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

impl<'a> fmt::Display for AstNodeKind<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstNodeKind::IfStatement(node) => write!(f, "{}", node),
            AstNodeKind::Assignment(node) => write!(f, "{}", node),
            AstNodeKind::Literal(node) => write!(f, "{}", node),
            AstNodeKind::Variable(node) => write!(f, "{}", node),
            AstNodeKind::Expression(node) => write!(f, "{}", node),
            AstNodeKind::Argument(node) => write!(f, "{}", node),
            AstNodeKind::Function(node) => write!(f, "{}", node),
            AstNodeKind::OnStatement(node) => write!(f, "{}", node),
            AstNodeKind::OnEventStatement(node) => write!(f, "{}", node),
            AstNodeKind::BlockStatement(node) => write!(f, "{}", node),
            AstNodeKind::LabelDeclaration(node) => write!(f, "{}", node),
            AstNodeKind::CommandStatement(node) => write!(f, "{}", node),
            AstNodeKind::McallStatement(node) => write!(f, "{}", node)
        }
    }
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

impl<'a> fmt::Display for AstNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

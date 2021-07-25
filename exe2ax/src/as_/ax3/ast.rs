use super::lexical::{PrimitiveToken, PrimitiveTokenKind};
use std::borrow::Cow;
use std::fmt;
use super::{Ax3File};

pub type AstNodeRef<'a> = Box<AstNode<'a>>;

pub trait AstPrintable<'a> {
    fn print_code(&self, f: &mut fmt::Formatter<'_>, tab_count: u32, file: &'a Ax3File<'a>) -> fmt::Result;
}

#[derive(Clone, Debug)]
pub struct IfStatementNode<'a> {
    pub primitive: PrimitiveToken<'a>,
    pub arg: Option<AstNodeRef<'a>>,
    pub if_block: AstNodeRef<'a>,
    pub else_primitive: Option<PrimitiveToken<'a>>,
    pub else_block: Option<AstNodeRef<'a>>,
}

pub fn print_tabs(f: &mut fmt::Formatter<'_>, tab_count: u32) -> fmt::Result {
    write!(f, "{:\t<1$}", "", tab_count as usize)
}

impl<'a> AstPrintable<'a> for IfStatementNode<'a> {
    fn print_code(&self, f: &mut fmt::Formatter<'_>, tab_count: u32, file: &'a Ax3File<'a>) -> fmt::Result {
        match &self.arg {
            Some(arg) => {
                write!(f, "{} (", self.primitive)?;
                arg.print_code(f, tab_count, file)?;
                write!(f, " ) ")?;
                self.if_block.print_code(f, tab_count, file)?;
            }
            None => {
                write!(f, "{} ", self.primitive)?;
                self.if_block.print_code(f, tab_count, file)?;
            }
        }
        if let Some(else_block) = &self.else_block {
            write!(f, " {} ", self.else_primitive.as_ref().unwrap())?;
            else_block.print_code(f, tab_count, file)?;
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

impl<'a> AstPrintable<'a> for AssignmentNode<'a> {
    fn print_code(&self, f: &mut fmt::Formatter<'_>, tab_count: u32, file: &'a Ax3File<'a>) -> fmt::Result {
        match &self.argument {
            Some(arg) => {
                self.var.print_code(f, tab_count, file)?;
                write!(f, " {}", self.operator);
                arg.print_code(f, tab_count, file)
            }
            None => {
                self.var.print_code(f, tab_count, file)?;
                write!(f, "{}", self.operator)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum LiteralNode<'a> {
    Integer(i32),
    Double(f32),
    String(Cow<'a, str>),
    Label(String, u32),
    Symbol(String),
}

impl<'a> AstPrintable<'a> for LiteralNode<'a> {
    fn print_code(&self, f: &mut fmt::Formatter<'_>, tab_count: u32, file: &'a Ax3File<'a>) -> fmt::Result {
        match self {
            LiteralNode::Integer(i) => write!(f, "{}", i),
            LiteralNode::Double(d) => write!(f, "{}", d),
            LiteralNode::String(s) => write!(f, "\"{}\"", s),
            LiteralNode::Label(l, _) => write!(f, "{}", l),
            LiteralNode::Symbol(s) => if s == "?" {
                Ok(())
            } else {
                write!(f, "{}", s)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct VariableNode<'a> {
    pub ident: PrimitiveToken<'a>,
    pub arg: Option<AstNodeRef<'a>>
}

impl<'a> AstPrintable<'a> for VariableNode<'a> {
    fn print_code(&self, f: &mut fmt::Formatter<'_>, tab_count: u32, file: &'a Ax3File<'a>) -> fmt::Result {
        match &self.ident.kind {
            PrimitiveTokenKind::GlobalVariable(name) => {
                match &self.arg {
                    Some(arg) => {
                        write!(f, "{}", name)?;
                        arg.print_code(f, tab_count, file)
                    },
                    None => write!(f, "{}", name)
                }
            },
            PrimitiveTokenKind::Parameter(param) => {
                match &self.arg {
                    Some(arg) => {
                        write!(f, "{}", param.get_param(file).unwrap())?;
                        arg.print_code(f, tab_count, file)
                    },
                    None => write!(f, "{}", param.get_param(file).unwrap())
                }
            },
            _ => unreachable!()

        }
    }
}

#[derive(Clone, Debug)]
pub struct ExpressionNode<'a> {
    pub lhs: AstNodeRef<'a>,
    pub op: Option<PrimitiveToken<'a>>,
    pub rhs: Option<AstNodeRef<'a>>,
    pub nested: bool
}

impl<'a> AstPrintable<'a> for ExpressionNode<'a> {
    fn print_code(&self, f: &mut fmt::Formatter<'_>, tab_count: u32, file: &'a Ax3File<'a>) -> fmt::Result {
        match &self.rhs {
            Some(rhs) => match &self.op {
                Some(op) => {
                    if self.nested {
                        write!(f, "(")?;
                    }
                    self.lhs.print_code(f, tab_count, file)?;
                    write!(f, " {} ", op)?;
                    rhs.print_code(f, tab_count, file)?;
                    if self.nested {
                        write!(f, ")")?;
                    }
                    Ok(())
                }
                None => unreachable!()
            }
            None => match &self.op {
                Some(op) => {
                    self.lhs.print_code(f, tab_count, file)?;
                    write!(f, "{}", op)
                }
                None => self.lhs.print_code(f, tab_count, file)
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

impl<'a> AstPrintable<'a> for ArgumentNode<'a> {
    fn print_code(&self, f: &mut fmt::Formatter<'_>, tab_count: u32, file: &'a Ax3File<'a>) -> fmt::Result {
        if self.has_bracket {
            write!(f, "(")?;
        } else {
            write!(f, " ")?;
        }

        if self.first_arg_is_null {
            write!(f, ", ")?;
        }

        for (i, exp) in self.exps.iter().enumerate() {
            exp.print_code(f, tab_count, file)?;
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

impl<'a> AstPrintable<'a> for FunctionNode<'a> {
    fn print_code(&self, f: &mut fmt::Formatter<'_>, tab_count: u32, file: &'a Ax3File<'a>) -> fmt::Result {
        match self.ident.kind {
            PrimitiveTokenKind::DllFunction(func) => {
                let name = func.get_default_name(file).unwrap();
                write!(f, "{}", name)?
            },
            _ => write!(f, "{}", self.ident)?
        };
        if let Some(arg) = &self.arg {
            arg.print_code(f, tab_count, file)?;
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

impl<'a> AstPrintable<'a> for OnStatementNode<'a> {
    fn print_code(&self, f: &mut fmt::Formatter<'_>, tab_count: u32, file: &'a Ax3File<'a>) -> fmt::Result {
        write!(f, "{}", self.primitive)?;

        if let Some(exp) = &self.exp {
            exp.print_code(f, tab_count, file)?;
        }

        if let Some(func) = &self.func {
            func.print_code(f, tab_count, file)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct OnEventStatementNode<'a> {
    pub primitive: PrimitiveToken<'a>,
    pub func: Option<AstNodeRef<'a>>,
}

impl<'a> AstPrintable<'a> for OnEventStatementNode<'a> {
    fn print_code(&self, f: &mut fmt::Formatter<'_>, tab_count: u32, file: &'a Ax3File<'a>) -> fmt::Result {
        write!(f, "{}", self.primitive)?;
        if let Some(func) = &self.func {
            write!(f, " ")?;
            func.print_code(f, tab_count, file)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct BlockStatementNode<'a> {
    pub nodes: Vec<AstNodeRef<'a>>
}

impl<'a> AstPrintable<'a> for BlockStatementNode<'a> {
    fn print_code(&self, f: &mut fmt::Formatter<'_>, tab_count: u32, file: &'a Ax3File<'a>) -> fmt::Result {
        write!(f, "{{\n")?;
        for (i, exp) in self.nodes.iter().enumerate() {
            print_tabs(f, tab_count)?;
            exp.print_code(f, tab_count, file)?;
            write!(f, "\n")?;
        }
        print_tabs(f, tab_count-1)?;
        write!(f, "}}")
    }
}

#[derive(Clone, Debug)]
pub struct LabelDeclarationNode {
    pub name: String
}

impl<'a> AstPrintable<'a> for LabelDeclarationNode {
    fn print_code(&self, f: &mut fmt::Formatter<'_>, tab_count: u32, file: &'a Ax3File<'a>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone, Debug)]
pub struct McallStatementNode<'a> {
    pub primitive: PrimitiveToken<'a>,
    pub var: AstNodeRef<'a>,
    pub primary_exp: AstNodeRef<'a>,
    pub arg: Option<AstNodeRef<'a>>,
}

impl<'a> AstPrintable<'a> for McallStatementNode<'a> {
    fn print_code(&self, f: &mut fmt::Formatter<'_>, tab_count: u32, file: &'a Ax3File<'a>) -> fmt::Result {
        write!(f, "{}->", self.primitive)?;
        self.var.print_code(f, tab_count, file)?;
        write!(f, " ")?;
        self.primary_exp.print_code(f, tab_count, file)?;
        if let Some(arg) = &self.arg {
            arg.print_code(f, tab_count, file)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct CommandStatementNode<'a> {
    pub func: AstNodeRef<'a>,
}

impl<'a> AstPrintable<'a> for CommandStatementNode<'a> {
    fn print_code(&self, f: &mut fmt::Formatter<'_>, tab_count: u32, file: &'a Ax3File<'a>) -> fmt::Result {
        self.func.print_code(f, tab_count, file)
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
    LabelDeclaration(LabelDeclarationNode),
    CommandStatement(CommandStatementNode<'a>),
    McallStatement(McallStatementNode<'a>),
}

impl<'a> AstPrintable<'a> for AstNodeKind<'a> {
    fn print_code(&self, f: &mut fmt::Formatter<'_>, tab_count: u32, file: &'a Ax3File<'a>) -> fmt::Result {
        match self {
            AstNodeKind::IfStatement(node) => node.print_code(f, tab_count, file),
            AstNodeKind::Assignment(node) => node.print_code(f, tab_count, file),
            AstNodeKind::Literal(node) => node.print_code(f, tab_count, file),
            AstNodeKind::Variable(node) => node.print_code(f, tab_count, file),
            AstNodeKind::Expression(node) => node.print_code(f, tab_count, file),
            AstNodeKind::Argument(node) => node.print_code(f, tab_count, file),
            AstNodeKind::Function(node) => node.print_code(f, tab_count, file),
            AstNodeKind::OnStatement(node) => node.print_code(f, tab_count, file),
            AstNodeKind::OnEventStatement(node) => node.print_code(f, tab_count, file),
            AstNodeKind::BlockStatement(node) => node.print_code(f, tab_count, file),
            AstNodeKind::LabelDeclaration(node) => node.print_code(f, tab_count, file),
            AstNodeKind::CommandStatement(node) => node.print_code(f, tab_count, file),
            AstNodeKind::McallStatement(node) => node.print_code(f, tab_count, file),
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
    pub fn new(token_offset: u32, kind: AstNodeKind<'a>, tab_count: u32) -> Self {
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

impl<'a> AstPrintable<'a> for AstNode<'a> {
    fn print_code(&self, f: &mut fmt::Formatter<'_>, tab_count: u32, file: &'a Ax3File<'a>) -> fmt::Result {
        self.kind.print_code(f, self.tab_count, file)?;
        Ok(())
    }
}

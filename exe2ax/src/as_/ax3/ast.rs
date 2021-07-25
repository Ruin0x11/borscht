use std::borrow::Cow;
use std::fmt;
use std::io::{Write, self};
use encoding_rs::SHIFT_JIS;
use super::lexical::{PrimitiveToken, PrimitiveTokenKind};
use super::{Ax3File, Ax3Function, Ax3FunctionFlags, Ax3FunctionType, Ax3Parameter, Hsp3As, Ax3Dll, Ax3DllType, Ax3Label, ResolvedLabel};

pub type AstNodeRef<'a> = Box<AstNode<'a>>;

pub trait AstPrintable<'a> {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error>;
}

#[derive(Clone, Debug)]
pub struct CommentLineNode {
    pub content: String,
}

impl<'a> AstPrintable<'a> for CommentLineNode {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
        write!(f, "{}", self.content)
    }
}

#[derive(Clone, Debug)]
pub struct IfStatementElsePart<'a> {
    pub primitive: PrimitiveToken<'a>,
    pub block: AstNodeRef<'a>
}

#[derive(Clone, Debug)]
pub struct IfStatementNode<'a> {
    pub primitive: PrimitiveToken<'a>,
    pub arg: Option<AstNodeRef<'a>>,
    pub if_block: AstNodeRef<'a>,
    pub else_part: Option<IfStatementElsePart<'a>>
}

pub fn print_tabs<W: Write>(f: &mut W, tab_count: u32) -> Result<(), io::Error> {
    write!(f, "{:\t<1$}", "", tab_count as usize)
}

impl<'a> AstPrintable<'a> for IfStatementNode<'a> {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
        match &self.arg {
            Some(arg) => {
                write!(f, "{} (", self.primitive)?;
                arg.print_code(f, tab_count, ctxt)?;
                write!(f, " ) ")?;
                self.if_block.print_code(f, tab_count, ctxt)?;
            }
            None => {
                write!(f, "{} ", self.primitive)?;
                self.if_block.print_code(f, tab_count, ctxt)?;
            }
        }
        if let Some(else_part) = &self.else_part {
            write!(f, " {} ", else_part.primitive)?;
            else_part.block.print_code(f, tab_count, ctxt)?;
        }
        Ok(())
    }
}

fn get_op<'a>(op: &PrimitiveToken<'a>, is_assignment: bool, has_expression: bool) -> String {
    let raw = op.dict_value.name.as_str();

    if is_assignment {
        if !has_expression && raw == "+" {
            "++".to_string()
        } else if !has_expression && raw == "-" {
            "--".to_string()
        } else {
            match raw {
                "=" |
                ">" |
                "<" => raw.to_string(),
                _ => format!("{}=", raw)
            }
        }
    } else {
        match raw {
            "=" | "!" => format!("{}=", raw),
            _ => raw.to_string()
        }
    }
}

#[derive(Clone, Debug)]
pub struct AssignmentNode<'a> {
    pub var: AstNodeRef<'a>,
    pub operator: PrimitiveToken<'a>,
    pub argument: Option<AstNodeRef<'a>> // is None in cases like `x++`
}

impl<'a> AstPrintable<'a> for AssignmentNode<'a> {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
        match &self.argument {
            Some(arg) => {
                self.var.print_code(f, tab_count, ctxt)?;
                write!(f, " {}", get_op(&self.operator, true, true));
                arg.print_code(f, tab_count, ctxt)
            }
            None => {
                self.var.print_code(f, tab_count, ctxt)?;
                write!(f, "{}", get_op(&self.operator, true, false))
            }
        }
    }
}

fn escape_string(value: &str) -> Cow<str> {
    let bytes = value.as_bytes();
    for (index, byte) in bytes.iter().enumerate() {
        match byte {
            0..=0x1F | b'"' | b'\\' => {
                return Cow::Owned(escape_string_inner(&bytes[0..index], &bytes[index..]))
            }
            _ => {}
        }
    }
    Cow::Borrowed(value)
}

fn escape_string_inner(start: &[u8], rest: &[u8]) -> String {
    let mut escaped = Vec::with_capacity(start.len() + rest.len() + 1);
    escaped.extend(start);

    for byte in rest {
        match byte {
            b'"' => escaped.extend(b"\\\""),
            b'\\' => escaped.extend(b"\\\\"),
            0x08 => escaped.extend(b"\\b"),
            0x0C => escaped.extend(b"\\f"),
            b'\n' => escaped.extend(b"\\n"),
            b'\r' => (),
            b'\t' => escaped.extend(b"\\t"),
            0..=0x1F => escaped.extend(format!("\\u{:04x}", byte).bytes()),
            _ => escaped.push(*byte),
        }
    }

    debug_assert!(std::str::from_utf8(&escaped).is_ok());
    unsafe { String::from_utf8_unchecked(escaped) }
}

#[derive(Clone, Debug)]
pub enum LiteralNode<'a> {
    Integer(i32),
    Double(f64),
    String(Cow<'a, str>),
    Label(ResolvedLabel<'a>),
    Symbol(String),
}


impl<'a> AstPrintable<'a> for LiteralNode<'a> {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
        match self {
            LiteralNode::Integer(i) => write!(f, "{}", i),
            LiteralNode::Double(d) => {
                if d.fract() == 0.0 {
                    write!(f, "{}.0", d)
                } else {
                    write!(f, "{:.}", d)
                }
            }
            LiteralNode::String(s) => {
                let shift_jis = true;

                let s = escape_string(s);
                if shift_jis {
                    let (bytes, _, errors) = SHIFT_JIS.encode(&s);
                    assert!(!errors, "Cannot encode string as SHIFT_JIS");
                    write!(f, "\"")?;
                    f.write_all(&bytes)?;
                    write!(f, "\"")
                } else {
                    write!(f, "\"{}\"", s)
                }
            },
            LiteralNode::Label(l) => write!(f, "{}", ctxt.label_names.get(&l).unwrap()),
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
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
        match &self.ident.kind {
            PrimitiveTokenKind::GlobalVariable(name) => {
                let (bytes, _, errors) = SHIFT_JIS.encode(&name);
                assert!(!errors, "Cannot encode string as SHIFT_JIS");

                match &self.arg {
                    Some(arg) => {
                        f.write_all(&bytes)?;
                        arg.print_code(f, tab_count, ctxt)
                    },
                    None => f.write_all(&bytes)
                }
            },
            PrimitiveTokenKind::Parameter(param) => {
                let param_name = ctxt.param_names.get(param).unwrap();
                match &self.arg {
                    Some(arg) => {
                        write!(f, "{}", param_name)?;
                        arg.print_code(f, tab_count, ctxt)
                    },
                    None => write!(f, "{}", param_name)
                }
            },
            _ => unreachable!()

        }
    }
}

fn operand_priority<'a>(node: &'a AstNodeRef<'a>) -> i32 {
    match &node.kind {
        AstNodeKind::Literal(n) => {
            match n {
                LiteralNode::Integer(i) => if *i < 0 { -1 } else { 100 },
                LiteralNode::Double(d) => if *d < 0.0 { -1 } else { 100 },
                _ => 100
            }
        },
        AstNodeKind::Function(_) => 100,
        AstNodeKind::Variable(_) => 100,
        AstNodeKind::Expression(e) => e.op.as_ref().unwrap().dict_value.priority as i32,
        _ => unreachable!()
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
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
        let p1 = operand_priority(&self.lhs);
        match &self.rhs {
            Some(rhs) => match &self.op {
                Some(op) => {
                    if op.dict_value.name == "*" {
                        if let AstNodeKind::Literal(l) = &self.lhs.kind {
                            if let AstNodeKind::Variable(_) = &rhs.kind {
                                if let LiteralNode::Integer(-1) = l {
                                    write!(f, "-")?;
                                    return rhs.print_code(f, tab_count, ctxt)
                                }
                            }
                        }
                        if let AstNodeKind::Literal(l) = &rhs.kind {
                            if let AstNodeKind::Variable(_) = &self.lhs.kind {
                                if let LiteralNode::Integer(-1) = l {
                                    write!(f, "-")?;
                                    return self.lhs.print_code(f, tab_count, ctxt)
                                }
                            }
                        }
                    }

                    let p2 = operand_priority(rhs);
                    let op_priority = op.dict_value.priority as i32;

                    if p1 < op_priority {
                        write!(f, "(")?;
                        self.lhs.print_code(f, tab_count, ctxt)?;
                        write!(f, ")")?;
                    } else {
                        self.lhs.print_code(f, tab_count, ctxt)?;
                    }

                    write!(f, " {} ", get_op(op, false, true))?;

                    if p2 <= op_priority {
                        write!(f, "(")?;
                        rhs.print_code(f, tab_count, ctxt)?;
                        write!(f, ")")?;
                    } else {
                        rhs.print_code(f, tab_count, ctxt)?;
                    }

                    Ok(())
                }
                None => unreachable!()
            }
            None => match &self.op {
                Some(op) => {
                    write!(f, "{}", get_op(op, false, false))?;
                    self.lhs.print_code(f, tab_count, ctxt)
                }
                None => self.lhs.print_code(f, tab_count, ctxt)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct ArgumentNode<'a> {
    pub exps: Vec<AstNodeRef<'a>>,
    pub has_bracket: bool,
    pub first_arg_is_null: bool,
    pub mcall: bool,
}

impl<'a> AstPrintable<'a> for ArgumentNode<'a> {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
        if self.has_bracket {
            write!(f, "(")?;
        } else {
            write!(f, " ")?;
        }

        if self.first_arg_is_null && !self.mcall {
            write!(f, ", ")?;
        }

        for (i, exp) in self.exps.iter().enumerate() {
            exp.print_code(f, tab_count, ctxt)?;
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
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
        match self.ident.kind {
            PrimitiveTokenKind::UserFunction(func) |
            PrimitiveTokenKind::DllFunction(func) |
            PrimitiveTokenKind::ComFunction(func) => {
                let name = ctxt.function_names.get(func).unwrap();
                write!(f, "{}", name)?
            },
            _ => write!(f, "{}", self.ident)?
        };
        if let Some(arg) = &self.arg {
            arg.print_code(f, tab_count, ctxt)?;
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
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
        write!(f, "{}", self.primitive)?;

        if let Some(exp) = &self.exp {
            exp.print_code(f, tab_count, ctxt)?;
        }

        if let Some(func) = &self.func {
            func.print_code(f, tab_count, ctxt)?;
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
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
        write!(f, "{}", self.primitive)?;
        if let Some(func) = &self.func {
            write!(f, " ")?;
            func.print_code(f, tab_count, ctxt)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct BlockStatementNode<'a> {
    pub nodes: Vec<AstNodeRef<'a>>
}

impl<'a> AstPrintable<'a> for BlockStatementNode<'a> {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
        write!(f, "{{\r\n")?;
        for (i, exp) in self.nodes.iter().enumerate() {

            // HACK
            if let AstNodeKind::LabelDeclaration(node) = &exp.kind {
                if ctxt.label_names.get(&node.label).is_none() {
                    continue;
                }
            }

            print_tabs(f, exp.tab_count)?;
            exp.print_code(f, exp.tab_count, ctxt)?;
            write!(f, "\r\n")?;
        }
        print_tabs(f, tab_count-1)?;
        write!(f, "}}")
    }
}

#[derive(Clone, Debug)]
pub struct LabelDeclarationNode<'a> {
    pub label: ResolvedLabel<'a>
}

impl<'a> AstPrintable<'a> for LabelDeclarationNode<'a> {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
        write!(f, "{}", ctxt.label_names.get(&self.label).unwrap())
    }
}

fn write_func_param<'a, W: Write>(f: &mut W, func: &'a Ax3Function, param: &Ax3Parameter, index: usize, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
    let remove_type = false;
    let parameter_names = match func.get_type() {
        Ax3FunctionType::Func |
        Ax3FunctionType::CFunc => false,
        _ => true
    };

    let mut wrote = false;
    if !remove_type {
        let type_name = param.get_type_name(ctxt.file).unwrap();
        if type_name == "NULL" {
            panic!("Null parameter type name");
        } else {
            write!(f, "{}", type_name)?;
            wrote = true;
        }
    }

    assert!(param.is_module_type(ctxt.file) == false);
    // let func = if param.is_module_type(ctxt.file) {
    //     param.get_module(ctxt.file).unwrap()
    // } else {
    //     func
    // };

    if parameter_names {
        if wrote {
            write!(f, " ")?;
        }

        let param_name = ctxt.param_names.get(param).unwrap();
        write!(f, "{}", param_name)?;
    }

    Ok(())
}

#[derive(Clone, Debug)]
pub struct FunctionDeclarationNode<'a> {
    pub func: &'a Ax3Function
}

impl<'a> FunctionDeclarationNode<'a> {
    pub fn get_name(&self, ctxt: &'a Hsp3As<'a>) -> Cow<'a, str> {
        if let Some(name) = ctxt.function_names.get(&self.func) {
            return Cow::Borrowed(name);
        }
        match self.func.get_type() {
            Ax3FunctionType::ComFunc => Cow::Owned(format!("comfunc_{}", self.func.function_index)),
            _ => self.func.get_default_name(ctxt.file).unwrap()
        }
    }
}

impl<'a> AstPrintable<'a> for FunctionDeclarationNode<'a> {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
        let name = self.get_name(ctxt);
        let mut param_start = 0;

        match self.func.get_type() {
            Ax3FunctionType::Func => {
                write!(f, "#func {} ", name)?;
                if self.func.flags.contains(Ax3FunctionFlags::OnExit) {
                    write!(f, "onexit ")?;
                }
                write!(f, "\"{}\"", self.func.get_default_name(ctxt.file).unwrap())?;
            },
            Ax3FunctionType::CFunc => {
                write!(f, "#cfunc {} \"{}\"", name, self.func.get_default_name(ctxt.file).unwrap())?;
            },
            Ax3FunctionType::DefFunc => {
                write!(f, "#deffunc {}", name)?;
                if self.func.flags.contains(Ax3FunctionFlags::OnExit) {
                    write!(f, " onexit")?;
                }
            },
            Ax3FunctionType::DefCFunc => write!(f, "#defcfunc {}", name)?,
            Ax3FunctionType::ComFunc => {
                param_start = 1;
                write!(f, "#comfunc {} {}", name, self.func.label_index)?;
            },
            Ax3FunctionType::Module => write!(f, "TODO module")?,
            _ => unreachable!()
        }

        let params = self.func.get_params(ctxt.file);
        if params.len() > param_start {
            for i in param_start..params.len() {
                if i != param_start {
                    write!(f, ",")?;
                }
                write!(f, " ")?;
                write_func_param(f, self.func, &params[i], i, ctxt)?;
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct UsedllDeclarationNode<'a> {
    pub dll: Ax3Dll,
    pub funcs: Vec<&'a Ax3Function>
}

impl<'a> AstPrintable<'a> for UsedllDeclarationNode<'a> {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
        let name = self.dll.get_name(ctxt.file).unwrap();
        match self.dll.dll_type {
            Ax3DllType::Uselib => {
                write!(f, "#uselib \"{}\"", name)
            },
            Ax3DllType::Usecom => {
                write!(f, "#usecom")?;
                if self.funcs.len() > 0 {
                    write!(f, " {}", ctxt.function_names[&self.funcs[0]])?;
                } else {
                    write!(f, " /*関数なし*/")?;
                }
                write!(f, " \"{}\" \"{}\"", name, self.dll.get_cls_name(ctxt.file).unwrap())
            },
            _ => unreachable!()
        }
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
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
        self.var.print_code(f, tab_count, ctxt)?;
        write!(f, "->")?;
        self.primary_exp.print_code(f, tab_count, ctxt)?;
        if let Some(arg) = &self.arg {
            arg.print_code(f, tab_count, ctxt)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct CommandStatementNode<'a> {
    pub func: AstNodeRef<'a>,
}

impl<'a> AstPrintable<'a> for CommandStatementNode<'a> {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
        self.func.print_code(f, tab_count, ctxt)
    }
}

#[derive(Clone, Debug)]
pub enum AstNodeKind<'a> {
    CommentLine(CommentLineNode),
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
    FunctionDeclaration(FunctionDeclarationNode<'a>),
    UsedllDeclaration(UsedllDeclarationNode<'a>),
    CommandStatement(CommandStatementNode<'a>),
    McallStatement(McallStatementNode<'a>),
}

impl<'a> AstPrintable<'a> for AstNodeKind<'a> {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
        match self {
            AstNodeKind::CommentLine(node) => node.print_code(f, tab_count, ctxt),
            AstNodeKind::IfStatement(node) => node.print_code(f, tab_count, ctxt),
            AstNodeKind::Assignment(node) => node.print_code(f, tab_count, ctxt),
            AstNodeKind::Literal(node) => node.print_code(f, tab_count, ctxt),
            AstNodeKind::Variable(node) => node.print_code(f, tab_count, ctxt),
            AstNodeKind::Expression(node) => node.print_code(f, tab_count, ctxt),
            AstNodeKind::Argument(node) => node.print_code(f, tab_count, ctxt),
            AstNodeKind::Function(node) => node.print_code(f, tab_count, ctxt),
            AstNodeKind::OnStatement(node) => node.print_code(f, tab_count, ctxt),
            AstNodeKind::OnEventStatement(node) => node.print_code(f, tab_count, ctxt),
            AstNodeKind::BlockStatement(node) => node.print_code(f, tab_count, ctxt),
            AstNodeKind::LabelDeclaration(node) => node.print_code(f, tab_count, ctxt),
            AstNodeKind::FunctionDeclaration(node) => node.print_code(f, tab_count, ctxt),
            AstNodeKind::UsedllDeclaration(node) => node.print_code(f, tab_count, ctxt),
            AstNodeKind::CommandStatement(node) => node.print_code(f, tab_count, ctxt),
            AstNodeKind::McallStatement(node) => node.print_code(f, tab_count, ctxt),
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
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As<'a>) -> Result<(), io::Error> {
        self.kind.print_code(f, self.tab_count, ctxt)?;
        Ok(())
    }
}

use std::borrow::Cow;
use std::fmt;
use std::io::{Write, self};
use encoding_rs::SHIFT_JIS;
use enum_as_inner::EnumAsInner;
use super::lexical::{PrimitiveToken, PrimitiveTokenKind};
use super::{Ax3File, Ax3Function, Ax3FunctionFlags, Ax3FunctionType, Ax3Parameter, Hsp3As, Ax3Dll, Ax3DllType, Ax3Label, ResolvedLabel, ResolvedParameter, Ax3Plugin, Ax3Cmd};

pub type AstNodeRef = Box<AstNode>;

pub trait AstPrintable<'a> {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error>;
}

impl<'a, T: AstPrintable<'a>> AstPrintable<'a> for Box<T> {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
        (**self).print_code(f, tab_count, ctxt)
    }
}

#[derive(Clone, Debug)]
pub struct ProgramNode {
    pub block: AstNodeRef
}

impl<'a> AstPrintable<'a> for ProgramNode {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
        self.block.print_code(f, tab_count, ctxt)
    }
}

#[derive(Clone, Debug)]
pub struct CommentLineNode {
    pub content: String,
}

impl<'a> AstPrintable<'a> for CommentLineNode {
    fn print_code<W: Write>(&self, f: &mut W, _tab_count: u32, _ctxt: &'a Hsp3As) -> Result<(), io::Error> {
        write!(f, "{}", self.content)
    }
}

#[derive(Clone, Debug)]
pub struct IfStatementElsePart {
    pub primitive: PrimitiveToken,
    pub block: AstNodeRef
}

#[derive(Clone, Debug)]
pub struct IfStatementNode {
    pub primitive: PrimitiveToken,
    pub arg: Option<AstNodeRef>,
    pub if_block: AstNodeRef,
    pub else_part: Option<IfStatementElsePart>
}

pub fn print_tabs<W: Write>(f: &mut W, tab_count: u32) -> Result<(), io::Error> {
    write!(f, "{:\t<1$}", "", tab_count as usize)
}

impl<'a> AstPrintable<'a> for IfStatementNode {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
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
            write!(f, "\n")?;
            print_tabs(f, tab_count)?;
            write!(f, "{} ", else_part.primitive)?;
            else_part.block.print_code(f, tab_count, ctxt)?;
        }
        Ok(())
    }
}

fn get_op(op: &PrimitiveToken, is_assignment: bool, has_expression: bool) -> String {
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
pub struct AssignmentNode {
    pub var: AstNodeRef,
    pub operator: PrimitiveToken,
    pub argument: Option<AstNodeRef> // is None in cases like `x++`
}

impl<'a> AstPrintable<'a> for AssignmentNode {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
        match &self.argument {
            Some(arg) => {
                self.var.print_code(f, tab_count, ctxt)?;
                write!(f, " {}", get_op(&self.operator, true, true))?;
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

#[derive(Clone, Debug, EnumAsInner)]
pub enum LiteralNode {
    Integer(i32),
    Double(f64),
    String(String),
    Label(ResolvedLabel),
    Symbol(String),
}

impl<'a> AstPrintable<'a> for LiteralNode {
    fn print_code<W: Write>(&self, f: &mut W, _tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
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
pub struct VariableNode {
    pub ident: PrimitiveToken,
    pub arg: Option<AstNodeRef>
}

impl<'a> AstPrintable<'a> for VariableNode {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
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

fn operand_priority(node: &AstNodeRef) -> i32 {
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
pub struct ExpressionNode {
    pub lhs: AstNodeRef,
    pub op: Option<PrimitiveToken>,
    pub rhs: Option<AstNodeRef>,
    pub nested: bool
}

impl<'a> AstPrintable<'a> for ExpressionNode {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
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
pub struct ArgumentNode {
    pub exps: Vec<AstNodeRef>,
    pub has_bracket: bool,
    pub first_arg_is_null: bool,
    pub mcall: bool,
}

impl<'a> AstPrintable<'a> for ArgumentNode {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
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
pub struct FunctionNode {
    pub ident: PrimitiveToken,
    pub arg: Option<AstNodeRef>
}

impl<'a> AstPrintable<'a> for FunctionNode {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
        match self.ident.kind {
            PrimitiveTokenKind::UserFunction(func) |
            PrimitiveTokenKind::DllFunction(func) |
            PrimitiveTokenKind::ComFunction(func) => {
                match ctxt.function_names.get(&func) {
                    Some(name) => write!(f, "{}", name)?,
                    None => write!(f, "NULL")?
                }
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
pub struct OnStatementNode {
    pub primitive: PrimitiveToken,
    pub exp: Option<AstNodeRef>,
    pub func: Option<AstNodeRef>,
}

impl<'a> AstPrintable<'a> for OnStatementNode {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
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
pub struct OnEventStatementNode {
    pub primitive: PrimitiveToken,
    pub func: Option<AstNodeRef>,
}

impl<'a> AstPrintable<'a> for OnEventStatementNode {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
        write!(f, "{}", self.primitive)?;
        if let Some(func) = &self.func {
            write!(f, " ")?;
            func.print_code(f, tab_count, ctxt)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct BlockStatementNode {
    pub nodes: Vec<AstNodeRef>,
    pub braces: bool
}

impl<'a> AstPrintable<'a> for BlockStatementNode {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
        if self.braces {
            write!(f, "{{\r\n")?;
        }
        for exp in self.nodes.iter() {
            print_tabs(f, exp.tab_count)?;
            exp.print_code(f, exp.tab_count, ctxt)?;
            write!(f, "\r\n")?;
        }
        if self.braces {
            print_tabs(f, tab_count-1)?;
            write!(f, "}}")?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct LabelDeclarationNode {
    pub label: ResolvedLabel
}

impl<'a> AstPrintable<'a> for LabelDeclarationNode {
    fn print_code<W: Write>(&self, f: &mut W, _tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
        write!(f, "{}", ctxt.label_names.get(&self.label).unwrap())
    }
}

fn write_func_param<'a, W: Write>(f: &mut W, func: &'a Ax3Function, param: &ResolvedParameter, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
    let remove_type = false;
    let parameter_names = match func.get_type() {
        Ax3FunctionType::Func |
        Ax3FunctionType::CFunc => false,
        _ => true
    };

    let mut wrote = false;
    if !remove_type {
        if &param.type_name == "NULL" {
            panic!("Null parameter type name");
        } else {
            write!(f, "{}", param.type_name)?;
            wrote = true;
        }
    }

    // assert!(param.is_module_type(&ctxt.file) == false);
    // let func = if param.is_module_type(ctxt.file) {
    //     param.get_module(ctxt.file).unwrap()
    // } else {
    //     func
    // };

    if parameter_names {
        if wrote {
            write!(f, " ")?;
        }

        let param_name = ctxt.param_names.get(param);
        if (param_name.is_some()) {
            write!(f, "{}", param_name.unwrap())?;
        }
    }

    Ok(())
}

#[derive(Clone, Debug)]
pub struct FunctionDeclarationNode {
    pub func: Ax3Function,
    pub default_name: String,
    pub params: Vec<ResolvedParameter>
}

impl FunctionDeclarationNode {
    pub fn get_name<'a>(&self, ctxt: &'a Hsp3As) -> String {
        if let Some(name) = ctxt.function_names.get(&self.func) {
            return name.to_string()
        }
        match self.func.get_type() {
            Ax3FunctionType::ComFunc => format!("comfunc_{}", self.func.function_index),
            _ => self.default_name.clone()
        }
    }
}

impl<'a> AstPrintable<'a> for FunctionDeclarationNode {
    fn print_code<W: Write>(&self, f: &mut W, _tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
        let name = self.get_name(ctxt);
        let mut param_start = 0;

        match self.func.get_type() {
            Ax3FunctionType::Func => {
                write!(f, "#func {} ", name)?;
                if self.func.flags.contains(Ax3FunctionFlags::OnExit) {
                    write!(f, "onexit ")?;
                }
                write!(f, "\"{}\"", self.default_name)?
            },
            Ax3FunctionType::CFunc => {
                write!(f, "#cfunc {} \"{}\"", name, self.default_name)?
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
            Ax3FunctionType::None => write!(f, "#none")?
        }

        if self.params.len() > param_start {
            for i in param_start..self.params.len() {
                if i != param_start {
                    write!(f, ",")?;
                }
                write!(f, " ")?;
                write_func_param(f, &self.func, &self.params[i], ctxt)?;
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct UsedllDeclarationNode {
    pub dll: Ax3Dll,
    pub name: String,
    pub cls_name: Option<String>,
    pub funcs: Vec<Ax3Function>
}

impl<'a> AstPrintable<'a> for UsedllDeclarationNode {
    fn print_code<W: Write>(&self, f: &mut W, _tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
        let name = &self.name;
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
                write!(f, " \"{}\" \"{}\"", name, self.cls_name.as_ref().unwrap())
            },
            _ => unreachable!()
        }
    }
}

#[derive(Clone, Debug)]
pub struct ReglibDeclarationNode {
    pub plugin: Ax3Plugin,
    pub dll_name: String,
    pub export_name: String,
}

impl<'a> AstPrintable<'a> for ReglibDeclarationNode {
    fn print_code<W: Write>(&self, f: &mut W, _tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
        write!(f, "#regcmd \"{}\" \"{}\"", &self.dll_name, &self.export_name)
    }
}

#[derive(Clone, Debug)]
pub struct CmdDeclarationNode {
    pub cmd: Ax3Cmd,
}

impl<'a> AstPrintable<'a> for CmdDeclarationNode {
    fn print_code<W: Write>(&self, f: &mut W, _tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
        write!(f, "#cmd cmd_{}_{}", self.cmd.plugin_index, self.cmd.method_index)
    }
}

#[derive(Clone, Debug)]
pub struct McallStatementNode {
    pub primitive: PrimitiveToken,
    pub var: AstNodeRef,
    pub primary_exp: AstNodeRef,
    pub arg: Option<AstNodeRef>,
}

impl<'a> AstPrintable<'a> for McallStatementNode {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
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
pub struct CommandStatementNode {
    pub func: AstNodeRef,
}

impl<'a> AstPrintable<'a> for CommandStatementNode {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
        self.func.print_code(f, tab_count, ctxt)
    }
}

#[derive(Clone, Debug, EnumAsInner)]
pub enum AstNodeKind {
    Program(ProgramNode),
    CommentLine(CommentLineNode),
    IfStatement(IfStatementNode),
    Assignment(AssignmentNode),
    Literal(LiteralNode),
    Variable(VariableNode),
    Expression(ExpressionNode),
    Argument(ArgumentNode),
    Function(FunctionNode),
    OnStatement(OnStatementNode),
    OnEventStatement(OnEventStatementNode),
    BlockStatement(BlockStatementNode),
    LabelDeclaration(LabelDeclarationNode),
    FunctionDeclaration(FunctionDeclarationNode),
    UsedllDeclaration(UsedllDeclarationNode),
    CommandStatement(CommandStatementNode),
    McallStatement(McallStatementNode),
}

impl<'a> AstPrintable<'a> for AstNodeKind {
    fn print_code<W: Write>(&self, f: &mut W, tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
        match self {
            AstNodeKind::Program(node) => node.print_code(f, tab_count, ctxt),
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
pub struct AstNode {
    pub token_offset: u32,
    pub tab_count: u32,
    pub visible: bool,
    pub errors: Vec<String>,
    pub comments: Vec<String>,
    pub kind: AstNodeKind
}

impl AstNode {
    pub fn new(token_offset: u32, kind: AstNodeKind, tab_count: u32) -> Self {
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

impl<'a> AstPrintable<'a> for AstNode {
    fn print_code<W: Write>(&self, f: &mut W, _tab_count: u32, ctxt: &'a Hsp3As) -> Result<(), io::Error> {
        self.kind.print_code(f, self.tab_count, ctxt)?;
        Ok(())
    }
}

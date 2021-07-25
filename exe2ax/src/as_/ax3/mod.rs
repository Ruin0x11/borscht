pub mod view;
pub mod dictionary;
pub mod lexical;
pub mod ast;
mod util;

use std::fs::File;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::io::{self, Read, Write, Seek, Cursor};
use encoding_rs::SHIFT_JIS;
use byteorder::{LittleEndian, ReadBytesExt};
use anyhow::{Result, anyhow};
use bitflags::bitflags;

use as_::dictionary::{HspCodeExtraFlags, HspCodeType};
use std::iter::Peekable;

use self::ast::*;
use self::dictionary::Hsp3Dictionary;
use self::lexical::PrimitiveTokenKind;
use self::view::Ax3View;
use super::DecodeOptions;

#[repr(C)]
#[derive(Debug)]
pub struct Ax3Header {
    pub _unknown1: u32,
    pub _unknown2: u32,
    pub _unknown3: u32,
    pub all_data_size: u32,
    pub code_offset: u32,
    pub code_size: u32,
    pub literal_offset: u32,
    pub literal_size: u32,
    pub label_offset: u32,
    pub label_size: u32,
    pub debug_offset: u32,
    pub debug_size: u32,
    pub dll_offset: u32,
    pub dll_size: u32,
    pub function_offset: u32,
    pub function_size: u32,
    pub parameter_offset: u32,
    pub parameter_size: u32,
    pub _unknown_offset: u32,
    pub _unknown_size: u32,
    pub plugin_offset: u32,
    pub plugin_size: u16,
    pub plugin_parameter_count: u16,
    pub _unknown4: u32,
    pub runtime_offset: u32,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ax3Label {
    pub token_offset: u32
}

/// Struct for disambiguating labels with the same offset but different
/// positions in the labels list.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResolvedLabel<'a> {
    pub index: usize,
    pub label: &'a Ax3Label
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct Ax3Cmd {
    pub plugin_index: u32,
    pub method_index: u32,
}

#[repr(u32)]
#[derive(Clone, Debug)]
pub enum Ax3DllType {
    None = 0x00,
    Uselib = 0x01,
    Usecom = 0x02
}

#[repr(C)]
#[derive(Clone, Debug)]
pub struct Ax3Dll {
    pub dll_type: Ax3DllType,
    pub name_offset: u32,
    pub _unknown1: u32,
    pub cls_name_offset: u32
}

impl Ax3Dll {
    fn get_name<'a>(&self, file: &'a Ax3File) -> Option<Cow<'a, str>> {
        match self.dll_type {
            Ax3DllType::Usecom => Some(file.read_iid_code_literal(self.name_offset as usize)),
            Ax3DllType::Uselib => Some(file.read_str_literal(self.name_offset as usize)),
            _ => None
        }
    }

    fn get_cls_name<'a>(&self, file: &'a Ax3File) -> Option<Cow<'a, str>> {
        match self.dll_type {
            Ax3DllType::Usecom => Some(file.read_str_literal(self.cls_name_offset as usize)),
            _ => None
        }
    }
}

#[repr(C)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ax3Parameter {
    pub param_type: u16,
    pub deffunc_index: i16,
    pub param_start_byte: u32
}

impl Ax3Parameter {
    pub fn is_module_type<'a>(&self, file: &'a Ax3File<'a>) -> bool {
        match self.get_type_name(file) {
            Some(s) => {
                let s = s.as_str();
                s == "modvar" || s == "modinit" || s == "modterm" || s == "struct"
            }
            _ => false
        }
    }

    pub fn get_type_name<'a>(&self, file: &'a Ax3File<'a>) -> Option<&'a String> {
        let param_type = self.param_type as u32;
        file.dict.params.get(&param_type)
    }

    pub fn get_module<'a>(&self, file: &'a Ax3File<'a>) -> Option<&'a Ax3Function> {
        if self.deffunc_index < 0 {
            return None
        }

        file.functions.get(self.deffunc_index as usize)
    }
}

#[repr(u8)]
#[derive(Clone, Debug)]
pub enum Ax3FunctionType {
    None = 0x00,
    Func = 0x01,
    CFunc = 0x02,
    DefFunc = 0x03,
    DefCFunc = 0x04,
    ComFunc = 0x05,
    Module = 0x06
}

bitflags! {
    pub struct Ax3FunctionFlags: u16 {
        const None = 0x00;
        const OnExit = 0x01;
    }
}

#[repr(C)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ax3Function {
    pub dll_index: i16,
    pub function_index: i16,
    pub param_start: u32,
    pub param_count: u32,
    pub str_index: u32,
    pub param_size_sum: u32,
    pub label_index: u32,
    pub _unknown1: u16,
    pub flags: Ax3FunctionFlags,
}

impl Ax3Function {
    pub fn get_type(&self) -> Ax3FunctionType {
        match self.dll_index {
            -1 => Ax3FunctionType::DefFunc,
            -2 => Ax3FunctionType::DefCFunc,
            -3 => Ax3FunctionType::Module,
            _ => {
                if self.function_index == -7 {
                    Ax3FunctionType::ComFunc
                } else {
                    match self.label_index {
                        2 | 3 => Ax3FunctionType::Func,
                        4 => Ax3FunctionType::CFunc,
                        _ => Ax3FunctionType::None,
                    }
                }
            }
        }
    }

    pub fn get_params<'a>(&self, file: &'a Ax3File) -> &'a [Ax3Parameter] {
        let size = self.param_start as usize;
        let count = self.param_count as usize;
        &file.parameters[size..size+count]
    }

    pub fn get_parent_module<'a>(&self, file: &'a Ax3File) -> Option<&'a Ax3Function> {
        let params = self.get_params(file);
        if params.len() == 0 {
            None
        } else if !params[0].is_module_type(file) {
            None
        } else {
            params[0].get_module(file)
        }
    }

    pub fn get_default_name<'a>(&self, file: &'a Ax3File) -> Option<Cow<'a, str>> {
        if self.str_index < 0 {
            None
        } else {
            Some(file.read_str_literal(self.str_index as usize))
        }
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct Ax3Plugin {
    pub _unknown1: u32,
    pub dll_name_offset: u32,
    pub export_name_offset: u32,
    pub _unknown2: u32,
}

#[repr(C)]
#[derive(Debug)]
pub struct Ax3File<'a> {
    pub header: &'a Ax3Header,
    pub dict: &'a Hsp3Dictionary,
    pub code: &'a [u8],
    pub literals: &'a [u8],
    pub labels: &'a [Ax3Label],
    pub dlls: &'a [Ax3Dll],
    pub parameters: &'a [Ax3Parameter],
    pub functions: &'a [Ax3Function],
    pub plugins: &'a [Ax3Plugin],
    pub variable_names: Vec<Cow<'a, str>>
}

#[repr(C)]
#[derive(Debug)]
pub struct Ax3Data {
    pub dict: Hsp3Dictionary,
    pub resolved_fn_names: HashMap<usize, String>
}

fn read_str_bytes<'a>(input: &'a [u8]) -> &'a [u8] {
    for (i, byte) in input.iter().enumerate() {
        if *byte == 0 {
            return &input[..i];
        }
    }

    panic!("No null byte in input");
}

fn read_str_literal<'a>(input: &'a [u8], offset: usize) -> Cow<'a, str> {
    let slice = &input[offset..];

    let bytes = read_str_bytes(slice);
    let (cow, _encoding_used, had_errors) = SHIFT_JIS.decode(bytes);
    assert!(!had_errors);
    cow
}

fn read_iid_code_literal<'a>(input: &'a [u8], offset: usize) -> Cow<'a, str> {
    let slice = &input[offset..offset+0x10];

    Cow::Owned(format!("{{{:02X}{:02X}{:02X}{:02X}-{:02X}{:02X}-{:02X}{:02X}-{:02X}{:02X}-{:02X}{:02X}{:02X}{:02X}{:02X}{:02X}}}",
                       slice[0x03],
                       slice[0x02],
                       slice[0x01],
                       slice[0x00],
                       slice[0x05],
                       slice[0x04],
                       slice[0x07],
                       slice[0x06],
                       slice[0x08],
                       slice[0x09],
                       slice[0x0A],
                       slice[0x0B],
                       slice[0x0C],
                       slice[0x0D],
                       slice[0x0E],
                       slice[0x0F]
    ))
}

impl<'a> Ax3File<'a> {
    pub fn read_str_literal(&self, offset: usize) -> Cow<'a, str> {
        read_str_literal(&self.literals, offset)
    }

    pub fn read_double_literal(&self, offset: usize) -> f64 {
        let slice = &self.literals[offset..];

        let mut cursor = Cursor::new(slice);
        cursor.read_f64::<LittleEndian>().unwrap()
    }

    pub fn read_iid_code_literal(&self, offset: usize) -> Cow<'a, str> {
        read_iid_code_literal(&self.literals, offset)
    }

    pub fn find_label(&self, label: &'a Ax3Label) -> ResolvedLabel<'a> {
        let index = self.labels.iter().position(|l| l == label).unwrap();
        ResolvedLabel {
            index: index,
            label: label
        }
    }
}

fn find_noncolliding_name(default_name: &str, function_names: &HashSet<String>) -> String {
    let mut new_name = default_name.to_string();
    let mut i = 1;
    while function_names.contains(&new_name.to_lowercase()) {
        new_name = format!("{}_{}", default_name, i);
        i = i + 1;
    }
    new_name
}

fn find_noncolliding_name_func(prefix: &str, function_names: &HashSet<String>) -> String {
    let mut new_name;
    let mut i = 1;
    loop {
        new_name = format!("{}_{}", prefix, i);
        i = i + 1;
        if !function_names.contains(&new_name.to_lowercase()) {
            break
        }
    }
    new_name
}

fn rename_functions<'a>(file: &'a Ax3File<'a>) -> (HashMap<&'a Ax3Function, String>, HashMap<&'a Ax3Parameter, String>) {
    let mut function_names = HashSet::new();

    let mut dll_funcs = Vec::new();
    let mut com_funcs = Vec::new();
    let mut initializers = Vec::new();

    let mut resolved = HashMap::new();

    let dict_funcnames = file.dict.get_all_function_names();
    for name in dict_funcnames {
        function_names.insert(name);
    }

    for (i, func) in file.functions.iter().enumerate() {
        match func.get_type() {
            Ax3FunctionType::CFunc | Ax3FunctionType::Func => {
                dll_funcs.push((func, i))
            },
            Ax3FunctionType::ComFunc => {
                com_funcs.push((func, i))
            },
            Ax3FunctionType::DefCFunc |
            Ax3FunctionType::DefFunc |
            Ax3FunctionType::Module => {
                match func.get_parent_module(file) {
                    Some(_) => initializers.push((func, i)),
                    None => {
                        if let Some(default_name) = func.get_default_name(file) {
                            let s = default_name.as_ref().to_string();
                            function_names.insert(s.to_lowercase());
                            resolved.insert(func, s);
                        }
                    }
                }
            },
            _ => ()
        }
    }

    for (func, i) in initializers.into_iter() {
        let default_name = func.get_default_name(file).unwrap();
        let defname = default_name.as_ref().to_string();

        if !function_names.contains(&defname.to_lowercase()) {
            function_names.insert(defname.to_lowercase());
            resolved.insert(func, defname);
        } else {
            let new_name = find_noncolliding_name(&defname, &function_names);
            function_names.insert(new_name.to_lowercase());
            resolved.insert(func, new_name);
        }
    }

    for (func, i) in dll_funcs.into_iter() {
        let default_name = func.get_default_name(file).unwrap();
        let mut new_name = default_name.to_string();

        if new_name.starts_with("_") && new_name.len() > 1 {
            new_name = new_name[1..].to_string();
        }

        if let Some(pos) = new_name.find("@") {
            new_name = new_name[..pos].to_string()
        }

        if !function_names.contains(&new_name.to_lowercase()) {
            function_names.insert(new_name.to_lowercase());
            resolved.insert(func, new_name);
        } else {
            let new_name = find_noncolliding_name_func("func", &function_names);
            function_names.insert(new_name.to_lowercase());
            resolved.insert(func, new_name);
        }
    }

    for (func, i) in com_funcs.into_iter() {
        let new_name = find_noncolliding_name_func("comfunc", &function_names);
        function_names.insert(new_name.to_lowercase());
        resolved.insert(func, new_name);
    }

    let mut params = HashMap::new();
    let funcnamed_params = false;

    if funcnamed_params {
        for func in file.functions.iter() {
            for (i, param) in func.get_params(file).iter().enumerate() {
                params.insert(param, format!("{}_prm{}", resolved[func], i));
            }
        }
    } else {
        for (i, param) in file.parameters.iter().enumerate() {
            params.insert(param, format!("prm_{}", i));
        }
    }

    (resolved, params)
}

fn rename_labels<'a>(file: &'a Ax3File<'a>, label_usage: &HashMap<ResolvedLabel<'a>, u32>) -> HashMap<ResolvedLabel<'a>, String> {
    let _count = file.labels.len();
    let mut labels = Vec::new();
    let mut result = HashMap::new();

    for (label, _) in label_usage.iter() {
        labels.push(label)
    }

    labels.sort_by(|a, b| {
        let ord1 = a.label.token_offset.cmp(&b.label.token_offset);
        if ord1 != Ordering::Equal {
            return ord1;
        }
        a.index.cmp(&b.index)
    });

    // let keta = f32::log10(count as f32) as usize + 1;

    for (i, l) in labels.into_iter().enumerate() {
        let new_name = format!("*label_{:0>4}", i+1);
        result.insert(*l, new_name);
    }

    result
}

fn read_variable_names<'a>(slice: &'a [u8], header: &'a Ax3Header, literals: &'a [u8]) -> Result<Vec<Cow<'a, str>>> {
    let mut result = Vec::new();

    let offset = header.debug_offset as usize;
    let size = header.debug_size as usize;
    let slice_range = &slice[offset..size+offset];
    let mut cursor = Cursor::new(slice_range);

    while let Ok(b) = cursor.read_u8() {
        match b {
            252 => {
                cursor.read_u8()?;
                cursor.read_u8()?;
            },
            253 => {
                let b1 = cursor.read_u8()? as u32;
                let b2 = cursor.read_u8()? as u32;
                let b3 = cursor.read_u8()? as u32;

                let literal_offset = b1 ^ (b2 << 8) ^ (b3 << 16);
                let s = read_str_literal(literals, literal_offset as usize);
                result.push(s);

                cursor.read_u8()?;
                cursor.read_u8()?;
            },
            254 => {
                cursor.read_u8()?;
                cursor.read_u8()?;
                cursor.read_u8()?;
                cursor.read_u8()?;
                cursor.read_u8()?;
            },
            255 => break,
            _ => unreachable!()
        }
    }

    Ok(result)
}

fn get_jump_to_offset<'a>(primitive: &lexical::PrimitiveToken<'a>) -> u32 {
    if let PrimitiveTokenKind::IfStatement(offset) = &primitive.kind {
        *offset as u32 + primitive.token_offset
    } else {
        unreachable!()
    }
}

fn fix_if_else_stmts<'a>(nodes: &mut Vec<AstNode<'a>>) {
    let mut found = Vec::new();
    for i in 0..nodes.len() {
        let node = &nodes[i];
        if let AstNodeKind::IfStatement(n) = &node.kind {
            if n.primitive.dict_value.code_type == HspCodeType::IfStatement {
                assert!(n.else_part.is_none());
                let jump_to_offset = get_jump_to_offset(&n.primitive);
                for j in i+1..nodes.len()-1 {
                    let other = &nodes[j];
                    if let AstNodeKind::IfStatement(ne) = &other.kind {
                        if other.token_offset == jump_to_offset && ne.primitive.dict_value.code_type == HspCodeType::ElseStatement {
                            found.push((i, j));
                            break;
                        }
                        else if other.token_offset > jump_to_offset {
                            break;
                        }
                    }
                }
            }
        }
    }

    while found.len() > 0 {
        let (left, right) = found.pop().unwrap();
        {
            let else_ = nodes.remove(right);
            let if_ = &mut nodes[left];

            if let AstNodeKind::IfStatement(ref mut n) = &mut if_.kind {
                assert!(n.primitive.dict_value.code_type == HspCodeType::IfStatement);
                if let AstNodeKind::IfStatement(ne) = else_.kind {
                    assert!(ne.primitive.dict_value.code_type == HspCodeType::ElseStatement);
                    n.else_part = Some(IfStatementElsePart {
                        primitive: ne.primitive,
                        block: ne.if_block,
                    });
                } else {
                    unreachable!()
                }
            } else {
                unreachable!()
            }
        }
        for p in found.iter_mut() {
            if p.0 > right {
                p.0 -= 1;
            }
            if p.1 > right {
                p.1 -= 1;
            }
        }
    }
}

fn fix_else_stmt_labels<'a>(nodes: &mut Vec<AstNode<'a>>) {
    if nodes.len() < 3 {
        return;
    }

    let mut found = Vec::new();

    for i in 0..nodes.len()-2 {
        let node = &nodes[i];
        if let AstNodeKind::IfStatement(n) = &node.kind {
            if n.primitive.dict_value.code_type == HspCodeType::IfStatement {
                let other = &nodes[i+1];
                if let AstNodeKind::LabelDeclaration(_) = &other.kind {
                    let other2 = &nodes[i+2];
                    if let AstNodeKind::IfStatement(n) = &other2.kind {
                        if n.primitive.dict_value.code_type == HspCodeType::ElseStatement {
                            found.push(i);
                        }
                    }
                }
            }
        }
    }

    while found.len() > 0 {
        let i = found.pop().unwrap();
        let node = nodes.remove(i+1);
        if let AstNodeKind::IfStatement(ref mut n) = &mut nodes[i].kind {
            if let AstNodeKind::BlockStatement(ref mut b) = &mut n.if_block.kind {
                b.nodes.push(Box::new(node));
            }
        } else {
            unreachable!()
        }

        for p in found.iter_mut() {
            if *p > i {
                *p -= 1;
            }
        }
    }
}

fn fix_dangling_else_stmts<'a>(nodes: &mut Vec<AstNode<'a>>) {
    if nodes.len() < 2 {
        return;
    }

    let mut found = Vec::new();

    for i in 0..nodes.len()-1 {
        let node = &nodes[i];
        if let AstNodeKind::IfStatement(n) = &node.kind {
            if n.primitive.dict_value.code_type == HspCodeType::IfStatement {
                let other = &nodes[i+1];
                if let AstNodeKind::IfStatement(n) = &other.kind {
                    if n.primitive.dict_value.code_type == HspCodeType::ElseStatement {
                        found.push(i);
                    }
                }
            }
        }
    }

    while found.len() > 0 {
        let i = found.pop().unwrap();
        let mut node = nodes.remove(i+1);
        if let AstNodeKind::IfStatement(ref mut n) = &mut nodes[i].kind {
            assert!(n.primitive.dict_value.code_type == HspCodeType::IfStatement);
            if let AstNodeKind::IfStatement(ne) = &mut node.kind {
                assert!(ne.primitive.dict_value.code_type == HspCodeType::ElseStatement);
                if get_jump_to_offset(&n.primitive) == node.token_offset {
                    n.else_part = Some(IfStatementElsePart {
                        primitive: ne.primitive.clone(),
                        block: ne.if_block.clone(),
                    });
                } else {
                    if let AstNodeKind::BlockStatement(ref mut b) = &mut n.if_block.kind {
                        if let AstNodeKind::BlockStatement(ref mut be) = &mut ne.if_block.kind {
                            for inner in be.nodes.drain(0..) {
                                b.nodes.push(inner)
                            }
                            b.nodes.push(Box::new(node));
                        } else {
                            unreachable!()
                        }
                    } else {
                        unreachable!()
                    }
                }
            } else {
                unreachable!()
            }
        } else {
            unreachable!()
        }

        for p in found.iter_mut() {
            if *p > i {
                *p -= 1;
            }
        }
    }
}

fn resolve_scope<'a>(nodes: &mut Vec<AstNode<'a>>) {
    fix_else_stmt_labels(nodes);
    fix_if_else_stmts(nodes);
    fix_dangling_else_stmts(nodes);
}

/// Removes the autogenerated 'stop' function call and label at the end of the
/// program.
fn remove_trailing_stop<'a>(result: &mut Vec<AstNode<'a>>) {
    let node = result.pop().unwrap(); // goto
    if let AstNodeKind::Function(n) = node.kind {
        assert!(n.ident.dict_value.name == "goto");
    } else {
        unreachable!()
    }

    let node = result.pop().unwrap(); // stop
    if let AstNodeKind::Function(n) = node.kind {
        assert!(n.ident.dict_value.name == "stop");
        assert!(n.ident.dict_value.extra == HspCodeExtraFlags::HasGhostGoto);
    } else {
        unreachable!()
    }

    let node = result.pop().unwrap(); // label
    if let AstNodeKind::LabelDeclaration(_) = node.kind {
        ()
    } else {
        unreachable!()
    }
}

#[derive(Debug)]
enum ExprPart<'a> {
    Token(lexical::PrimitiveToken<'a>),
    Expr(AstNode<'a>)
}

#[derive(Debug)]
enum LabelKind<'a> {
    Label,
    Function(&'a Ax3Function)
}

pub struct Parser<'a, R: Read + Seek> {
    tokens: Peekable<lexical::TokenIterator<'a, R>>,
    labels: Vec<(usize, &'a Ax3Label, LabelKind<'a>)>,
    file: &'a Ax3File<'a>,
    label_usage: HashMap<ResolvedLabel<'a>, u32>,
    tab_count: u32
}

impl<'a, R: Read + Seek> Parser<'a, R> {
    pub fn new(tokens: lexical::TokenIterator<'a, R>,
               file: &'a Ax3File<'a>,
               function_names: HashMap<&'a Ax3Function, String>) -> Self {
        let functions = function_names.iter().map(|(f, n)| { (f.label_index as usize, *f) })
                                             .collect::<HashMap<usize, &'a Ax3Function>>();

        let mut labels = Vec::new();
        for (i, label) in file.labels.iter().enumerate() {
            let kind = match functions.get(&i) {
                Some(func) => {
                    match func.get_type() {
                        Ax3FunctionType::DefFunc |
                        Ax3FunctionType::DefCFunc => LabelKind::Function(func),
                        _ => LabelKind::Label
                    }
                }
                None => LabelKind::Label
            };
            labels.push((i, label, kind));
        }

        labels.sort_by(|a, b| {
            let ord1 = b.1.token_offset.cmp(&a.1.token_offset);
            if ord1 != Ordering::Equal {
                return ord1;
            }
            b.0.cmp(&a.0)
        });

        Parser {
            tokens: tokens.peekable(),
            labels: labels,
            file: file,
            label_usage: HashMap::new(),
            tab_count: 1
        }
    }

    fn parse_primary_expression(&mut self) -> AstNode<'a> {
        let token = self.tokens.peek().unwrap().clone();
        match &token.kind {
            PrimitiveTokenKind::Integer => {
                let next = self.tokens.next().unwrap();
                let kind = AstNodeKind::Literal(LiteralNode::Integer(next.value));
                AstNode::new(next.token_offset, kind, self.tab_count)
            },
            PrimitiveTokenKind::Double(d) => {
                let next = self.tokens.next().unwrap();
                let kind = AstNodeKind::Literal(LiteralNode::Double(*d));
                AstNode::new(next.token_offset, kind, self.tab_count)
            },
            PrimitiveTokenKind::String(s) => {
                let next = self.tokens.next().unwrap();
                let kind = AstNodeKind::Literal(LiteralNode::String(s.clone()));
                AstNode::new(next.token_offset, kind, self.tab_count)
            },
            PrimitiveTokenKind::Label(l) => {
                let next = self.tokens.next().unwrap();
                *self.label_usage.entry(*l).or_insert(0) += 1;
                let kind = AstNodeKind::Literal(LiteralNode::Label(*l));
                AstNode::new(next.token_offset, kind, self.tab_count)
            },
            PrimitiveTokenKind::Symbol => {
                let next = self.tokens.next().unwrap();
                let kind = AstNodeKind::Literal(LiteralNode::Symbol(token.dict_value.name.clone()));
                AstNode::new(next.token_offset, kind, self.tab_count)
            },
            PrimitiveTokenKind::Parameter(_) |
            PrimitiveTokenKind::GlobalVariable(_) => self.read_variable(),
            PrimitiveTokenKind::HspFunction |
            PrimitiveTokenKind::OnFunction |
            PrimitiveTokenKind::OnEventFunction |
            PrimitiveTokenKind::McallFunction |
            PrimitiveTokenKind::UserFunction(_) |
            PrimitiveTokenKind::DllFunction(_) |
            PrimitiveTokenKind::PlugInFunction(_) |
            PrimitiveTokenKind::ComFunction(_) => self.read_function(true),
            x => {
                panic!("Not found expr: {:?}", token);
            }
        }
    }

    fn next_is_end_of_stream(&mut self) -> bool {
        self.tokens.peek().is_none()
    }

    fn next_is_end_of_param(&mut self) -> bool {
        match self.tokens.peek() {
            Some(x) => x.is_end_of_param(),
            None => true
        }
    }

    fn next_is_end_of_line(&mut self) -> bool {
        match self.tokens.peek() {
            Some(x) => x.is_end_of_line(),
            None => true
        }
    }

    fn next_is_bracket_start(&mut self) -> bool {
        match self.tokens.peek() {
            Some(x) => x.is_bracket_start(),
            None => true
        }
    }

    fn next_is_bracket_end(&mut self) -> bool {
        match self.tokens.peek() {
            Some(x) => x.is_bracket_end(),
            None => true
        }
    }

    fn read_function(&mut self, has_bracket: bool) -> AstNode<'a> {
        let tok = self.tokens.next().unwrap();
        let mut tab_count = self.tab_count;

        // if tok.adds_tab() {
        //     self.tab_count += 1;
        // } else if tok.removes_tab() {
        //     self.tab_count -= 1;
        //     tab_count -= 1;
        // }

        let token_offset = tok.token_offset;
        if self.next_is_end_of_line() {
            let kind = AstNodeKind::Function(FunctionNode {
                ident: tok,
                arg: None
            });
            return AstNode::new(token_offset, kind, tab_count);
        }

        if tok.has_ghost_label() {
            self.tokens.next().unwrap();
            if self.next_is_end_of_line() {
                let kind = AstNodeKind::Function(FunctionNode {
                    ident: tok,
                    arg: None
                });
                return AstNode::new(token_offset, kind, tab_count);
            }
        }

        let arg = if self.next_is_bracket_start() || !has_bracket {
            Some(Box::new(self.read_argument(false)))
        } else {
            None
        };

        let kind = AstNodeKind::Function(FunctionNode {
            ident: tok,
            arg: arg
        });
        AstNode::new(token_offset, kind, tab_count)
    }

    fn read_expression(&mut self, priority: u32) -> AstNode<'a> {
        let mut terms = Vec::new();

        loop {
            if self.next_is_bracket_end() {
                break;
            }
            if let PrimitiveTokenKind::Operator = &self.tokens.peek().unwrap().kind {
                terms.push(ExprPart::Token(self.tokens.next().unwrap()));
            } else {
                terms.push(ExprPart::Expr(self.parse_primary_expression()));
            }
            if self.next_is_end_of_param() {
                break;
            }
        }

        let mut stack: Vec<AstNode<'a>> = Vec::new();

        while terms.len() > 0 {
            let term = terms.remove(0);

            match term {
                ExprPart::Token(op) => {
                    let rhs = stack.remove(stack.len() - 1);
                    let lhs = stack.remove(stack.len() - 1);

                    let token_offset = lhs.token_offset;
                    let tab_count = lhs.tab_count;
                    let errors = lhs.errors.clone();
                    let comments = lhs.comments.clone();

                    let new_kind = AstNodeKind::Expression(ExpressionNode {
                        lhs: Box::new(lhs),
                        op: Some(op),
                        rhs: Some(Box::new(rhs)),
                        nested: true
                    });

                    let new_node = AstNode {
                        token_offset: token_offset,
                        tab_count: tab_count,
                        visible: true,
                        errors: errors,
                        comments: comments,
                        kind: new_kind
                    };

                    stack.push(new_node);
                },
                ExprPart::Expr(a) => {
                    stack.push(a);
                }
            }
        }

        let mut expr = stack.remove(0);
        if let AstNodeKind::Expression(ref mut node) = expr.kind {
            node.nested = false;
        }

        expr
    }

    fn read_argument(&mut self, mcall: bool) -> AstNode<'a> {
        let has_bracket = self.tokens.peek().map_or(false, |x| x.is_bracket_start());
        self.tokens.next_if(|x| has_bracket);

        let token_offset = self.tokens.peek().unwrap().token_offset;
        let first_arg_is_null = self.next_is_end_of_param();
        let mut exps = Vec::new();

        while !self.next_is_end_of_line() {
            if has_bracket && self.next_is_bracket_end() {
                self.tokens.next().unwrap();
                break;
            }

            exps.push(Box::new(self.read_expression(0)));
        }

        let kind = AstNodeKind::Argument(ArgumentNode {
            exps: exps,
            has_bracket: has_bracket,
            first_arg_is_null: first_arg_is_null,
            mcall: mcall
        });
        AstNode::new(token_offset, kind, self.tab_count)
    }

    fn read_variable(&mut self) -> AstNode<'a> {
        let ident = self.tokens.next().unwrap();
        let token_offset = ident.token_offset;
        let next = self.tokens.peek().unwrap();
        let arg = if next.is_bracket_start() {
            // Array access
            Some(Box::new(self.read_argument(false)))
        } else {
            None
        };

        let kind = AstNodeKind::Variable(VariableNode {
            ident: ident,
            arg: arg
        });
        AstNode::new(token_offset, kind, self.tab_count)
    }

    fn read_block(&mut self, until: u32) -> AstNode<'a> {
        self.tab_count += 1;

        let mut exprs = Vec::new();
        let token_offset = 0;
        while let Some(token) = self.tokens.peek() {
            if token.token_offset == until && token.dict_value.code_type == crate::as_::dictionary::HspCodeType::ElseStatement {
                break;
            } else if token.token_offset > until + 1 {
                // eprintln!("Overshot if boundary: {} > {}", token.token_offset, until);
                break;
            }
            for expr in self.read_logical_line() {
                exprs.push(expr);
            }
        }

        resolve_scope(&mut exprs);

        let kind = AstNodeKind::BlockStatement(BlockStatementNode {
            nodes: exprs.into_iter().map(Box::new).collect::<_>(),
        });
        let node = AstNode::new(token_offset, kind, self.tab_count);

        self.tab_count -= 1;

        node
    }

    fn read_if_statement(&mut self) -> AstNode<'a> {
        let primitive = self.tokens.next().unwrap();

        let token_offset = primitive.token_offset;
        let arg = if self.next_is_end_of_line() {
            None
        } else {
            Some(Box::new(self.read_argument(false)))
        };

        let mut jump_offset = get_jump_to_offset(&primitive);

        let if_block = Box::new(self.read_block(jump_offset));

        let kind = AstNodeKind::IfStatement(IfStatementNode {
            primitive: primitive,
            arg: arg,
            if_block: if_block,
            else_part: None
        });
        AstNode::new(token_offset, kind, self.tab_count)
    }

    pub fn read_on_event(&mut self) -> AstNode<'a> {
        let tok = self.tokens.next().unwrap();
        let token_offset = tok.token_offset;

        match self.tokens.peek().unwrap().dict_value.name.as_str() {
            "gosub" | "goto" => {
                let func = if self.next_is_end_of_line() {
                    None
                } else {
                    Some(Box::new(self.read_function(false)))
                };

                let kind = AstNodeKind::OnEventStatement(OnEventStatementNode {
                    primitive: tok,
                    func: func
                });
                return AstNode::new(token_offset, kind, self.tab_count)
            },
            _ => {
                let token_offset = tok.token_offset;
                if self.next_is_end_of_line() {
                    let kind = AstNodeKind::Function(FunctionNode {
                        ident: tok,
                        arg: None
                    });
                    return AstNode::new(token_offset, kind, self.tab_count);
                }

                if tok.has_ghost_label() {
                    self.tokens.next().unwrap();
                    if self.next_is_end_of_line() {
                        let kind = AstNodeKind::Function(FunctionNode {
                            ident: tok,
                            arg: None
                        });
                        return AstNode::new(token_offset, kind, self.tab_count);
                    }
                }

                let arg = Some(Box::new(self.read_argument(false)));

                let kind = AstNodeKind::Function(FunctionNode {
                    ident: tok,
                    arg: arg
                });
                AstNode::new(token_offset, kind, self.tab_count)
            }
        }
    }

    pub fn read_assignment(&mut self) -> AstNode<'a> {
        let var = self.read_variable();
        let token_offset = var.token_offset;
        let operator = self.tokens.next().unwrap();

        let argument = if self.next_is_end_of_line() {
            None
        } else {
            Some(Box::new(self.read_argument(false)))
        };

        let kind = AstNodeKind::Assignment(AssignmentNode {
            var: Box::new(var),
            operator: operator,
            argument: argument
        });
        return AstNode::new(token_offset, kind, self.tab_count)
    }

    pub fn read_mcall(&mut self) -> AstNode<'a> {
        let primitive = self.tokens.next().unwrap();
        let token_offset = primitive.token_offset;

        let variable = self.read_variable();
        let exp = self.read_expression(0);

        let arg = if self.next_is_end_of_line() {
            None
        } else {
            Some(Box::new(self.read_argument(true)))
        };

        let kind = AstNodeKind::McallStatement(McallStatementNode {
            primitive: primitive,
            var: Box::new(variable),
            primary_exp: Box::new(exp),
            arg: arg,
        });
        return AstNode::new(token_offset, kind, self.tab_count)
    }

    pub fn read_logical_line(&mut self) -> Vec<AstNode<'a>> {
        let offset_here = self.tokens.peek().unwrap().token_offset;

        // Labels
        let mut result = Vec::new();
        while let Some(top) = self.labels.last() {
            let token_offset = top.1.token_offset;
            if token_offset > offset_here {
                break;
            }
            let (index, label, label_kind) = self.labels.pop().unwrap();
            let resolved = ResolvedLabel { index: index, label: label };
            match label_kind {
                LabelKind::Function(func) => {
                    let kind = AstNodeKind::LabelDeclaration(LabelDeclarationNode {
                        label: resolved
                    });
                    let node = AstNode::new(label.token_offset, kind, 0);
                    result.push(node);

                    let kind = AstNodeKind::FunctionDeclaration(FunctionDeclarationNode {
                        func: func
                    });
                    let node = AstNode::new(label.token_offset, kind, 0);
                    result.push(node);
                },
                LabelKind::Label => {
                    let kind = AstNodeKind::LabelDeclaration(LabelDeclarationNode {
                        label: resolved
                    });
                    let node = AstNode::new(label.token_offset, kind, 0);
                    result.push(node);
                }
            };
        }

        let add_tab = self.tokens.peek().unwrap().adds_tab();
        let remove_tab = self.tokens.peek().unwrap().removes_tab();

        if remove_tab {
            self.tab_count -= 1;
        }

        // Logical line
        let line = match &self.tokens.peek().unwrap().kind {
            PrimitiveTokenKind::Parameter(_) |
            PrimitiveTokenKind::GlobalVariable(_) => {
                self.read_assignment()
            },
            PrimitiveTokenKind::HspFunction |
            PrimitiveTokenKind::UserFunction(_) |
            PrimitiveTokenKind::DllFunction(_) |
            PrimitiveTokenKind::PlugInFunction(_) |
            PrimitiveTokenKind::ComFunction(_) => self.read_function(false),
            PrimitiveTokenKind::IfStatement(_) => {
                self.read_if_statement()
            },
            PrimitiveTokenKind::OnEventFunction => {
                self.read_on_event()
            },
            PrimitiveTokenKind::McallFunction => {
                self.read_mcall()
            },
            // PrimitiveTokenKind::OnFunction |
            // PrimitiveTokenKind::McallFunction |
            x => {
                panic!("Not found: {:?}", x);
            }
        };

        if add_tab {
            self.tab_count += 1;
        }

        result.push(line);
        result
    }

    fn add_early_decls(&mut self, result: &mut Vec<AstNode<'a>>) {
        for (i, dll) in self.file.dlls.iter().enumerate() {
            let functions = self.file.functions.iter().filter(|f| {
                f.dll_index >= 0 && (f.dll_index as usize) == i
            }).collect::<Vec<&'a Ax3Function>>();

            let kind = AstNodeKind::UsedllDeclaration(UsedllDeclarationNode {
                dll: dll.clone(),
                funcs: functions.clone()
            });
            result.push(AstNode::new(0, kind, 0));

            for func in functions.iter() {
                let kind = AstNodeKind::FunctionDeclaration(FunctionDeclarationNode {
                    func: func.clone(),
                });
                result.push(AstNode::new(0, kind, 0));
            }

            result.push(AstNode::new(0, AstNodeKind::CommentLine(CommentLineNode{ content: String::new() }), 0));
        }

        if self.file.plugins.len() > 0 {
            panic!("Plugins are not implemented yet.");
        }
    }

    pub fn parse(&mut self) -> Vec<AstNode<'a>> {
        let mut result = Vec::new();

        self.add_early_decls(&mut result);

        while let Some(_) = self.tokens.peek() {
            for expr in self.read_logical_line() {
                result.push(expr);
            }
        }

        resolve_scope(&mut result);
        remove_trailing_stop(&mut result);

        result
    }
}

pub struct Hsp3As<'a> {
    nodes: Vec<AstNode<'a>>,
    file: &'a Ax3File<'a>,
    function_names: HashMap<&'a Ax3Function, String>,
    param_names: HashMap<&'a Ax3Parameter, String>,
    label_names: HashMap<ResolvedLabel<'a>, String>,
}

impl<'a> Hsp3As<'a> {
    fn write<W: Write>(&self, w: &mut W) -> Result<(), io::Error> {
        for (i, node) in self.nodes.iter().enumerate() {
            ast::print_tabs(w, node.tab_count)?;

            // HACK
            if let AstNodeKind::LabelDeclaration(node) = &node.kind {
                if self.label_names.get(&node.label).is_none() {
                    continue;
                }
            }

            node.print_code(w, node.tab_count, &self)?;
            write!(w, "\r\n")?;
        }
        Ok(())
    }
}

pub fn decode<'a, R: AsRef<[u8]>>(bytes: &'a R, opts: &DecodeOptions) -> Result<()> {
    let dict = Hsp3Dictionary::from_csv("Dictionary.csv")?;

    let slice = bytes.as_ref();
    let header: &'a Ax3Header = unsafe { &*(slice.as_ptr() as *const _) };

    let literals = util::get_slice(slice, header.literal_offset, header.literal_size);

    let variable_names = read_variable_names(slice, header, literals)?;

    let file = Ax3File {
        header: header,
        dict: &dict,
        code: util::get_slice(slice, header.code_offset, header.code_size),
        literals: literals,
        labels: util::transmute_slice::<Ax3Label>(slice, header.label_offset, header.label_size),
        dlls: util::transmute_slice::<Ax3Dll>(slice, header.dll_offset, header.dll_size),
        parameters: util::transmute_slice::<Ax3Parameter>(slice, header.parameter_offset, header.parameter_size),
        functions: util::transmute_slice::<Ax3Function>(slice, header.function_offset, header.function_size),
        plugins: util::transmute_slice::<Ax3Plugin>(slice, header.plugin_offset, header.plugin_size as u32),
        variable_names: variable_names
    };

    let (func_names, param_names) = rename_functions(&file);

    let reader = Cursor::new(file.code);

    let iter = lexical::TokenIterator {
        reader: reader,
        token_offset: 0,
        end_offset: header.code_size / 2,
        dict: &dict,
        file: &file
    };

    let mut parser = Parser::new(iter, &file, func_names.clone());
    let nodes = parser.parse();

    let label_names = rename_labels(&file, &parser.label_usage);

    let hsp3as = Hsp3As {
        nodes: nodes,
        file: &file,
        function_names: func_names,
        param_names: param_names,
        label_names: label_names
    };

    let mut file = File::create(&opts.output_file)?;
    hsp3as.write(&mut file)?;

    Ok(())
}

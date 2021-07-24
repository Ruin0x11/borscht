pub mod view;
pub mod dictionary;
pub mod lexical;
pub mod ast;
mod util;

use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::io::{Read, Seek, Cursor};
use encoding_rs::SHIFT_JIS;
use byteorder::{LittleEndian, ReadBytesExt};
use anyhow::{Result, anyhow};

use self::view::Ax3View;
use self::dictionary::Hsp3Dictionary;

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
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ax3Label {
    pub token_offset: u32
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct Ax3Cmd {
    pub plugin_index: u32,
    pub method_index: u32,
}

#[repr(u8)]
#[derive(Debug)]
pub enum Ax3DllType {
    None = 0x00,
    Uselib = 0x01,
    Usecom = 0x02
}

#[repr(C)]
#[derive(Debug)]
pub struct Ax3Dll {
    pub ret_type: u32,
    pub name_offset: u32,
    pub _unknown1: u32,
    pub cls_name_offset: u32
}

#[repr(C)]
#[derive(Debug)]
pub struct Ax3Parameter {
    pub param_type: u16,
    pub deffunc_index: i16,
    pub param_start_byte: u32
}

impl Ax3Parameter {
    pub fn is_module_type<'a>(&self, file: &'a Ax3File<'a>) -> bool {
        match self.get_param(file) {
            Some(s) => {
                let s = s.as_str();
                s == "modvar" || s == "modinit" || s == "modterm" || s == "struct"
            }
            _ => false
        }
    }

    pub fn get_param<'a>(&self, file: &'a Ax3File<'a>) -> Option<&'a String> {
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
#[derive(Debug)]
pub enum Ax3FunctionType {
    None = 0x00,
    Func = 0x01,
    CFunc = 0x02,
    DefFunc = 0x03,
    DefCFunc = 0x04,
    ComFunc = 0x05,
    Module = 0x06
}

#[repr(u8)]
#[derive(Debug)]
pub enum Ax3FunctionFlags {
    None = 0x00,
    OnExit = 0x01
}

#[repr(C)]
#[derive(Debug)]
pub struct Ax3Function {
    pub dll_index: i16,
    pub function_index: i16,
    pub param_start: u32,
    pub param_count: u32,
    pub str_index: u32,
    pub param_size_sum: u32,
    pub label_index: u32,
    pub _unknown1: u16,
    pub flags: u16,
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
    let (cow, encoding_used, had_errors) = SHIFT_JIS.decode(bytes);
    assert!(!had_errors);
    cow
}

impl<'a> Ax3File<'a> {
    pub fn read_str_literal(&self, offset: usize) -> Cow<'a, str> {
        read_str_literal(&self.literals, offset)
    }

    pub fn read_double_literal(&self, offset: usize) -> f32 {
        let slice = &self.literals[offset..];

        let mut cursor = Cursor::new(slice);
        cursor.read_f32::<LittleEndian>().unwrap()
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
    let mut new_name = String::new();
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

fn rename_functions<'a>(file: &'a Ax3File<'a>) -> HashMap<usize, String> {
    let mut function_names = HashSet::new();

    let mut dll_funcs = Vec::new();
    let mut com_funcs = Vec::new();
    let mut initializers = Vec::new();

    let mut resolved = HashMap::new();

    let mut dict_funcnames = file.dict.get_all_function_names();
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
                            resolved.insert(i, s);
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
            resolved.insert(i, defname);
        } else {
            let new_name = find_noncolliding_name(&defname, &function_names);
            function_names.insert(new_name.to_lowercase());
            resolved.insert(i, new_name);
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
            resolved.insert(i, new_name);
        } else {
            let new_name = find_noncolliding_name_func("func", &function_names);
            function_names.insert(new_name.to_lowercase());
            resolved.insert(i, new_name);
        }
    }

    for (func, i) in com_funcs.into_iter() {
        let new_name = find_noncolliding_name_func("comfunc", &function_names);
        function_names.insert(new_name.to_lowercase());
        resolved.insert(i, new_name);
    }

    resolved
}

fn rename_labels<'a>(file: &'a Ax3File<'a>) -> HashMap<usize, String> {
    let count = file.labels.len();
    let mut labels = Vec::new();
    let mut result = HashMap::new();

    for (i, label) in file.labels.iter().enumerate() {
        labels.push((i, label.token_offset))
    }

    labels.sort_by(|a, b| a.1.cmp(&b.1));

    let keta = f32::log10(count as f32) as usize + 1;

    for (i, _) in labels.iter().enumerate() {
        let new_name = format!("*label{:0>4}", i);
        result.insert(i, new_name);
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

use as_::dictionary::HspCodeExtraFlags;
use self::lexical::PrimitiveTokenKind;
use std::iter::Peekable;
use self::ast::*;

type Iter<'a, R> = Peekable<lexical::TokenIterator<'a, R>>;

fn read_primary_expression<'a, R: Read + Seek>(iter: &mut Iter<'a, R>) -> AstNode<'a> {
    let kind = iter.peek().unwrap().kind.clone();
    match kind {
        PrimitiveTokenKind::Integer => {
            let next = iter.next().unwrap();
            let kind = AstNodeKind::Literal(LiteralNode::Integer(next.value));
            AstNode::new(next.token_offset, kind)
        },
        PrimitiveTokenKind::Double(d) => {
            let next = iter.next().unwrap();
            let kind = AstNodeKind::Literal(LiteralNode::Double(d));
            AstNode::new(next.token_offset, kind)
        },
        PrimitiveTokenKind::String(s) => {
            let next = iter.next().unwrap();
            let kind = AstNodeKind::Literal(LiteralNode::String(s));
            AstNode::new(next.token_offset, kind)
        },
        PrimitiveTokenKind::GlobalVariable(ident) => read_variable(iter),
        PrimitiveTokenKind::HspFunction |
        PrimitiveTokenKind::OnFunction |
        PrimitiveTokenKind::OnEventFunction |
        PrimitiveTokenKind::McallFunction |
        PrimitiveTokenKind::UserFunction(_) |
        PrimitiveTokenKind::DllFunction(_) |
        PrimitiveTokenKind::PlugInFunction(_) |
        PrimitiveTokenKind::ComFunction(_) => read_function(iter),
        _ => unreachable!()
    }
}

fn priority(tok: &lexical::PrimitiveToken) -> (u32, u32) {
    match tok.kind {
        PrimitiveTokenKind::Operator => {
            match tok.dict_value.name.as_str() {
                "+" => (6, 6),
                _ => (1, 1),
            }
        },
        _ => unreachable!()
    }
}

fn read_expression_1<'a, R: Read + Seek>(iter: &mut Iter<'a, R>, lhs: AstNode<'a>, min_priority: u32) -> Option<AstNode<'a>> {
    if iter.peek().map_or(false, |x| x.is_end_of_param()) {
        return None;
    }

    let mut lookahead = iter.peek().unwrap().clone();

    let continue_lhs = |tok| {
        return priority(tok).0 >= min_priority
    };

    let continue_rhs = |tok, first_priority| {
        return priority(tok).1 >= first_priority
    };

    while priority(&lookahead).0 >= min_priority {
        let first_op = iter.next().unwrap();
        let first_prio = priority(&first_op).0;

        let mut rhs = read_primary_expression(iter);
        lookahead = iter.peek().unwrap().clone();

        while continue_rhs(&lookahead, first_prio) {
            let second_prio = priority(&lookahead).0;
            match read_expression_1(iter, rhs, second_prio) {
                Some(new_rhs) => {
                    rhs = new_rhs;
                    lookahead = iter.peek().unwrap().clone();
                },
                None => return Some(rhs)
            }
        }

        let new_kind = AstNodeKind::Expression(ExpressionNode {
            lhs: Box::new(lhs),
            op: Some(first_op),
            rhs: Some(Box::new(rhs))
        });

        lhs = AstNode {
            token_offset: lhs.token_offset,
            tab_count: lhs.tab_count,
            visible: true,
            errors: lhs.errors.clone(),
            comments: lhs.comments.clone(),
            kind: new_kind
        }
    }

    return Some(lhs);
}

fn read_expression<'a, R: Read + Seek>(iter: &mut Iter<'a, R>) -> AstNode<'a> {
    let primary = read_primary_expression(iter);
    match read_expression_1(iter, primary, 0) {
        Some(rhs) => rhs,
        None => primary
    }
}

fn read_argument<'a, R: Read + Seek>(iter: &mut Iter<'a, R>) -> AstNode<'a> {
    let has_bracket = iter.peek().map_or(false, |x| x.is_bracket_start());
    iter.next_if(|x| has_bracket);

    let next = iter.next().unwrap();
    let first_arg_is_null = next.is_end_of_param();
    let mut exps = Vec::new();

    while let Some(x) = iter.peek() {
        if has_bracket && x.is_bracket_end() {
            iter.next().unwrap();
            break;
        }
        exps.push(Box::new(read_expression(iter)));
    }

    let kind = AstNodeKind::Argument(ArgumentNode {
        exps: exps,
        has_bracket: has_bracket,
        first_arg_is_null: first_arg_is_null
    });
    AstNode::new(next.token_offset, kind)
}

fn read_function<'a, R: Read + Seek>(iter: &mut Iter<'a, R>) -> AstNode<'a> {
    let tok = iter.next().unwrap();
    let token_offset = tok.token_offset;
    if iter.peek().map_or(false, |x| x.is_end_of_stream()) {
        let kind = AstNodeKind::Function(FunctionNode {
            ident: tok,
            arg: None
        });
        return AstNode::new(token_offset, kind);
    }


    let kind = AstNodeKind::Function(FunctionNode {
        ident: tok,
        arg: None
    });
    return AstNode::new(token_offset, kind);
}

fn read_variable<'a, R: Read + Seek>(iter: &mut Iter<'a, R>) -> AstNode<'a> {
    let ident = iter.next().unwrap();
    let token_offset = ident.token_offset;
    let next = iter.peek().unwrap();
    let arg = if next.is_bracket_start() {
        // Array access
        Some(Box::new(read_argument(iter)))
    } else {
        None
    };

    let kind = AstNodeKind::Variable(VariableNode {
        ident: ident,
        arg: arg
    });
    AstNode::new(token_offset, kind)
}

fn read_nodes<'a, R: Read + Seek>(iter: &mut Iter<'a, R>) -> () {
    let mut tab_count = 0;
    while let Some(token) = iter.peek() {
        let token_offset = token.token_offset;
        let tab_count = tab_count;
        println!("{:?}", token);

        match token.kind {
            PrimitiveTokenKind::GlobalVariable(name) => {
                read_variable(iter);
            },
            _ => ()
        }
    }
}

pub fn decode<'a, R: AsRef<[u8]>>(bytes: &'a R) -> Result<()> {
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

    let func_names = rename_functions(&file);

    let label_names = rename_labels(&file);

    let reader = Cursor::new(file.code);

    let iter = lexical::TokenIterator {
        reader: reader,
        token_offset: 0,
        end_offset: header.code_size / 2,
        dict: &dict,
        file: &file
    };

    let nodes = read_nodes(&mut iter.peekable());

    Ok(())
}

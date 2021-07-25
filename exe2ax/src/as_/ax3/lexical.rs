use std::borrow::Cow;
use std::convert::TryFrom;
use std::fmt;
use std::io::{Read, Seek, SeekFrom};
use byteorder::{LittleEndian, ReadBytesExt};
use bitflags::bitflags;

use super::{Ax3Label, Ax3Function, Ax3Cmd, Ax3Parameter, Ax3File};

use crate::as_::dictionary::*;
use super::Hsp3Dictionary;

pub struct LexicalAnalyzer {

}

pub struct TokenIterator<'a, R: Read + Seek> {
    pub reader: R,
    pub token_offset: u32,
    pub end_offset: u32,
    pub dict: &'a Hsp3Dictionary,
    pub file: &'a Ax3File<'a>
}

bitflags! {
    pub struct PrimitiveTokenFlags: u8 {
        const None = 0x00;
        const IsLineHead = 0x20;
        const IsParamHead = 0x40;
        const HasLongTypeValue = 0x80;
    }
}

#[derive(Debug, Clone)]
pub enum PrimitiveTokenKind<'a> {
    Label(&'a Ax3Label),
    Integer,
    Double(f32),
    String(Cow<'a, str>),
    Symbol,
    Parameter(&'a Ax3Parameter),
    GlobalVariable(&'a Cow<'a, str>),
    Operator,
    IfStatement(u16),
    HspFunction,
    OnFunction,
    OnEventFunction,
    McallFunction,
    UserFunction(&'a Ax3Function),
    DllFunction(&'a Ax3Function),
    PlugInFunction(Ax3Cmd),
    ComFunction(&'a Ax3Function),
    Unknown,
}

#[derive(Debug, Clone)]
pub struct PrimitiveToken<'a> {
    pub token_offset: u32,
    pub type_: u8,
    pub flag: PrimitiveTokenFlags,
    pub value: i32,
    // name: &'a str,
    pub dict_value: HspDictionaryValue,
    pub kind: PrimitiveTokenKind<'a>
}

impl<'a> fmt::Display for PrimitiveToken<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.dict_value.name)
    }
}

impl<'a> PrimitiveToken<'a> {
    pub fn is_bracket_start(&self) -> bool {
        self.dict_value.extra.contains(HspCodeExtraFlags::BracketStart)
    }

    pub fn is_bracket_end(&self) -> bool {
        self.dict_value.extra.contains(HspCodeExtraFlags::BracketEnd)
    }

    pub fn has_ghost_label(&self) -> bool {
        self.dict_value.extra.contains(HspCodeExtraFlags::HasGhostLabel)
    }

    pub fn adds_tab(&self) -> bool {
        self.dict_value.extra.contains(HspCodeExtraFlags::AddTab)
    }

    pub fn removes_tab(&self) -> bool {
        self.dict_value.extra.contains(HspCodeExtraFlags::RemoveTab)
    }

    pub fn is_end_of_param(&self) -> bool {
        self.flag.contains(PrimitiveTokenFlags::IsLineHead) || self.flag.contains(PrimitiveTokenFlags::IsParamHead)
    }

    pub fn is_end_of_line(&self) -> bool {
        self.flag.contains(PrimitiveTokenFlags::IsLineHead)
    }
}

fn make_primitive<'a>(file: &'a Ax3File, v: HspDictionaryValue, token_offset: u32, type_: u8, flag: PrimitiveTokenFlags, value: i32, extra_value: Option<u16>) -> PrimitiveToken<'a>{
    let kind = match v.code_type {
        HspCodeType::None => PrimitiveTokenKind::Unknown,
        HspCodeType::Operator => PrimitiveTokenKind::Operator,
        HspCodeType::Symbol => PrimitiveTokenKind::Symbol,
        HspCodeType::Variable => {
            let name = &file.variable_names[value as usize];
            PrimitiveTokenKind::GlobalVariable(name)
        }
        HspCodeType::String => {
            let s = file.read_str_literal(value as usize);
            PrimitiveTokenKind::String(s)
        },
        HspCodeType::Double => {
            let d = file.read_double_literal(value as usize);
            PrimitiveTokenKind::Double(d)
        },
        HspCodeType::Integer => PrimitiveTokenKind::Integer,
        HspCodeType::Param => {
            let param = file.parameters.get(value as usize).unwrap();
            PrimitiveTokenKind::Parameter(param)
        },
        HspCodeType::Label => {
            let label = file.labels.get(value as usize).unwrap();
            PrimitiveTokenKind::Label(label)
        },
        HspCodeType::IfStatement |
        HspCodeType::ElseStatement => match extra_value {
            Some(ex) => PrimitiveTokenKind::IfStatement(ex),
            None => PrimitiveTokenKind::HspFunction
        },
        HspCodeType::HspFunction => PrimitiveTokenKind::HspFunction,
        HspCodeType::UserFunction => {
            let func = file.functions.get(value as usize).unwrap();
            PrimitiveTokenKind::UserFunction(func)
        },
        HspCodeType::DllFunction => {
            let func = file.functions.get(value as usize).unwrap();
            PrimitiveTokenKind::DllFunction(func)
        },
        HspCodeType::ComFunction => {
            let func = file.functions.get(value as usize - 0x1000).unwrap();
            PrimitiveTokenKind::ComFunction(func)
        },
        HspCodeType::PlugInFunction => {
            let cmd = Ax3Cmd {
                plugin_index: v.priority,
                method_index: value as u32
            };
            PrimitiveTokenKind::PlugInFunction(cmd)
        },
        HspCodeType::OnEventStatement => PrimitiveTokenKind::OnEventFunction,
        HspCodeType::OnStatement => PrimitiveTokenKind::OnFunction,
        HspCodeType::McallStatement => PrimitiveTokenKind::McallFunction,
    };

    PrimitiveToken {
        token_offset: token_offset,
        type_: type_,
        flag: flag,
        value: value,
        dict_value: v,
        kind: kind
    }
}

impl<'a, R: Read + Seek> Iterator for TokenIterator<'a, R> {
    type Item = PrimitiveToken<'a>;
    fn next(&mut self) -> Option<PrimitiveToken<'a>> {
        if self.token_offset >= self.end_offset {
            return None
        }

        let type_ = self.reader.read_u8().unwrap();
        let flag = PrimitiveTokenFlags::from_bits(self.reader.read_u8().unwrap()).unwrap();

        let token_offset = self.token_offset;
        self.token_offset += 1;

        let value = if flag.contains(PrimitiveTokenFlags::HasLongTypeValue) {
            self.token_offset += 2;
            self.reader.read_i32::<LittleEndian>().unwrap()
        } else {
            self.token_offset += 1;
            self.reader.read_u16::<LittleEndian>().unwrap() as i32
        };
        let value_ = if value == -1 {
            None
        } else {
            Some(value as u32)
        };

        let key = HspDictionaryKey {
            type_: type_,
            value: value_
        };

        match self.dict.lookup_code(&key) {
            Some(v) => {
                let extra_value = if v.extra.contains(HspCodeExtraFlags::HasExtraInt16) {
                    if flag.contains(PrimitiveTokenFlags::IsLineHead) {
                        let ev = self.reader.read_u16::<LittleEndian>().unwrap();
                        self.token_offset += 1;
                        Some(ev)
                    } else {
                        None
                    }
                } else {
                    None
                };

                let token = make_primitive(self.file, v, token_offset, type_, flag, value, extra_value);
                // println!("{:?}", token);
                Some(token)
            },
            None => {
                Some(PrimitiveToken {
                    token_offset: token_offset,
                    type_: type_,
                    flag: flag,
                    value: value,
                    dict_value: HspDictionaryValue::default(),
                    kind: PrimitiveTokenKind::Unknown
                })
            }
        }
    }
}

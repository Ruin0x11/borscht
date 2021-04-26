use std::collections::HashMap;
use bitflags::bitflags;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct HspDictionaryKey {
    pub type_: u8,
    pub value: Option<u32>,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, enum_utils::FromStr, enum_utils::TryFromRepr)]
pub enum HspCodeType {
    None = 0x00,
    Operator = 0xFF,
    Symbol = 0xFE,
    Variable = 0x01,
    String = 0x02,
    Double = 0x03,
    Integer = 0x04,
    Param = 0x05,
    Label = 0x07,
    HspFunction = 0x08,
    IfStatement = 0x0B,
    UserFunction = 0x0C,
    DllFunction = 0x10,
    ComFunction = 0x11,
    PlugInFunction = 0x12,
    OnEventStatement = 0x20,
    OnStatement = 0x21,
    ElseStatement = 0x22,
    McallStatement = 0x23,
}

bitflags! {
    pub struct HspCodeExtraFlags: u16 {
        const None = 0x00;
        const HasExtraInt16 = 0x0001;
        const HasGhostLabel = 0x0002;
        const HasGhostGoto  = 0x0004;
        const AddTab        = 0x0008;
        const RemoveTab     = 0x0010;
        const IsGhost       = 0x0020;
        const BracketStart  = 0x0040;
        const BracketEnd	  = 0x0080;
        const GotoFunction  = 0x0100;
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct HspDictionaryValue {
    pub name: String,
    pub code_type: HspCodeType,
    pub extra: HspCodeExtraFlags,
    pub priority: u32
}

impl Default for HspDictionaryValue {
    fn default() -> Self {
        HspDictionaryValue {
            name: String::new(),
            code_type: HspCodeType::None,
            extra: HspCodeExtraFlags::None,
            priority: 0
        }
    }
}

pub type HspCodeDictionary = HashMap<HspDictionaryKey, HspDictionaryValue>;

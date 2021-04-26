pub mod view;
pub mod dictionary;
mod util;

use std::borrow::Cow;
use encoding_rs::SHIFT_JIS;
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
#[derive(Debug)]
pub struct Ax3Label {
    pub token_offset: u32
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
    pub fn is_module_type<'a>(&self, data: &'a Ax3Data<'a>) -> bool {
        match self.get_param(data) {
            Some(s) => {
                let s = s.as_str();
                s == "modvar" || s == "modinit" || s == "modterm" || s == "struct"
            }
            _ => false
        }
    }

    pub fn get_param<'a>(&self, data: &'a Ax3Data<'a>) -> Option<&'a String> {
        let param_type = self.param_type as u32;
        data.dict.params.get(&param_type)
    }

    pub fn get_module<'a>(&self, data: &'a Ax3Data<'a>) -> Option<&'a Ax3Function> {
        if self.deffunc_index < 0 {
            return None
        }

        data.functions.get(self.deffunc_index as usize)
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

    pub fn get_params<'a>(&self, data: &'a Ax3Data) -> &'a [Ax3Parameter] {
        let size = self.param_start as usize;
        let count = self.param_count as usize;
        &data.parameters[size..size+count]
    }

    pub fn get_parent_module<'a>(&self, data: &'a Ax3Data) -> Option<&'a Ax3Function> {
        let params = self.get_params(data);
        if params.len() == 0 {
            None
        } else if !params[0].is_module_type(data) {
            None
        } else {
            params[0].get_module(data)
        }
    }

    pub fn get_default_name<'a>(&self, data: &'a Ax3Data) -> Option<Cow<'a, str>> {
        if self.str_index < 0 {
            None
        } else {
            Some(data.read_str_literal(self.str_index as usize))
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
pub struct Ax3Data<'a> {
    pub header: &'a Ax3Header,
    pub dict: &'a Hsp3Dictionary,
    pub literals: &'a [u8],
    pub labels: &'a [Ax3Label],
    pub dlls: &'a [Ax3Dll],
    pub parameters: &'a [Ax3Parameter],
    pub functions: &'a [Ax3Function],
    pub plugins: &'a [Ax3Plugin],
}

fn read_str_bytes<'a>(input: &'a [u8]) -> &'a [u8] {
    for (i, byte) in input.iter().enumerate() {
        if *byte == 0 {
            return &input[..i];
        }
    }

    panic!("No null byte in input");
}
impl<'a> Ax3Data<'a> {
    pub fn read_str_literal(&self, offset: usize) -> Cow<'a, str> {
        println!("READ {} {}", offset, self.header.literal_offset);

        let start = offset;
        let slice = &self.literals[start..];

        let bytes = read_str_bytes(slice);
        let (cow, encoding_used, had_errors) = SHIFT_JIS.decode(bytes);
        println!("{:?}", cow.as_ref());
        assert!(!had_errors);
        cow
    }
}

fn rename_functions<'a>(data: &'a mut Ax3Data<'a>) {
    let mut dll_funcs = Vec::new();
    let mut com_funcs = Vec::new();
    let mut initializers = Vec::new();

    for func in data.functions.iter() {
        match func.get_type() {
            Ax3FunctionType::CFunc | Ax3FunctionType::Func => {
                dll_funcs.push(func)
            },
            Ax3FunctionType::ComFunc => {
                com_funcs.push(func)
            },
            Ax3FunctionType::DefCFunc |
            Ax3FunctionType::DefFunc |
            Ax3FunctionType::Module => {
                match func.get_parent_module(data) {
                    Some(_) => initializers.push(func),
                    None => {
                        let default_name = func.get_default_name(data);
                    }
                }
            },
            _ => ()
        }
    }
}

pub fn decode<'a, R: AsRef<[u8]>>(bytes: &'a R) -> Result<()> {
    let mut dict = Hsp3Dictionary::from_csv("Dictionary.csv")?;
    // let ax3_view = view::parse_view(bytes);

    let slice = bytes.as_ref();
    let header: &'a Ax3Header = unsafe { &*(slice.as_ptr() as *const _) };

    let mut data = Ax3Data {
        header: header,
        dict: &dict,
        labels: util::transmute_slice::<Ax3Label>(slice, header.label_offset, header.label_size),
        literals: util::get_slice(slice, header.literal_offset, header.literal_size),
        dlls: util::transmute_slice::<Ax3Dll>(slice, header.dll_offset, header.dll_size),
        parameters: util::transmute_slice::<Ax3Parameter>(slice, header.parameter_offset, header.parameter_size),
        functions: util::transmute_slice::<Ax3Function>(slice, header.function_offset, header.function_size),
        plugins: util::transmute_slice::<Ax3Plugin>(slice, header.plugin_offset, header.plugin_size as u32),
    };

    {
        rename_functions(&mut data);
    }

    Ok(())
}

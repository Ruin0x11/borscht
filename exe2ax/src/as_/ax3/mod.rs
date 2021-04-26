pub mod view;
pub mod dictionary;
mod util;

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
    pub deffunc_index: u16,
    pub param_start_byte: u32
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
    pub function_index: u16,
    pub param_start: u32,
    pub param_count: u32,
    pub str_index: u32,
    pub param_size_sum: u32,
    pub label_index: u32,
    pub _unknown1: u16,
    pub flags: u16,
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
    header: &'a Ax3Header,
    labels: &'a [Ax3Label],
    dlls: &'a [Ax3Dll],
    parameters: &'a [Ax3Parameter],
    functions: &'a [Ax3Function],
    plugins: &'a [Ax3Plugin],
}

pub fn decode<'a, R: AsRef<[u8]>>(bytes: &'a R) -> Result<()> {
    let dict = Hsp3Dictionary::from_csv("Dictionary.csv");
    let ax3_view = view::parse_view(bytes);

    let slice = bytes.as_ref();
    let header: &'a Ax3Header = unsafe { &*(slice.as_ptr() as *const _) };

    let data = Ax3Data {
        header: header,
        labels: util::transmute_slice::<Ax3Label>(slice, header.label_offset, header.label_size),
        dlls: util::transmute_slice::<Ax3Dll>(slice, header.dll_offset, header.dll_size),
        parameters: util::transmute_slice::<Ax3Parameter>(slice, header.parameter_offset, header.parameter_size),
        functions: util::transmute_slice::<Ax3Function>(slice, header.function_offset, header.function_size),
        plugins: util::transmute_slice::<Ax3Plugin>(slice, header.plugin_offset, header.plugin_size as u32),
    };

    println!("{:#?}", data);

    Ok(())
}

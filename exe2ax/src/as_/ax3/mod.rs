pub mod view;
pub mod dictionary;
mod util;

use std::borrow::Cow;
use encoding_rs::SHIFT_JIS;
use std::collections::{HashMap, HashSet};
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
    pub literals: &'a [u8],
    pub labels: &'a [Ax3Label],
    pub dlls: &'a [Ax3Dll],
    pub parameters: &'a [Ax3Parameter],
    pub functions: &'a [Ax3Function],
    pub plugins: &'a [Ax3Plugin],
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
impl<'a> Ax3File<'a> {
    pub fn read_str_literal(&self, offset: usize) -> Cow<'a, str> {
        let start = offset;
        let slice = &self.literals[start..];

        let bytes = read_str_bytes(slice);
        let (cow, encoding_used, had_errors) = SHIFT_JIS.decode(bytes);
        assert!(!had_errors);
        cow
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

fn rename_functions<'a>(file: &'a mut Ax3File<'a>) -> HashMap<usize, String> {
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

pub fn decode<'a, R: AsRef<[u8]>>(bytes: &'a R) -> Result<()> {
    let mut dict = Hsp3Dictionary::from_csv("Dictionary.csv")?;
    // let ax3_view = view::parse_view(bytes);

    let slice = bytes.as_ref();
    let header: &'a Ax3Header = unsafe { &*(slice.as_ptr() as *const _) };

    let mut file = Ax3File {
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
        let func_names = rename_functions(&mut file);
        println!("{:#?}", func_names);
    }

    Ok(())
}

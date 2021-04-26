use std::slice;
use std::mem;
use bitflags::bitflags;
use anyhow::{Result, anyhow};

#[repr(C)]
#[derive(Debug)]
pub struct Ax3Header {
    pub _unknown1: u32,
    pub _unknown2: u32,
    pub _unknown3: u32,
    pub all_data_size: u32,
    pub script_offset: u32,
    pub script_size: u32,
    pub text_offset: u32,
    pub text_size: u32,
    pub label_offset: u32,
    pub label_size: u32,
    pub dll_offset: u32,
    pub dll_size: u32,
    pub func_offset: u32,
    pub func_size: u32,
    pub deffunc_offset: u32,
    pub deffunc_size: u32,
    pub module_offset: u32,
    pub module_size: u32,
}

#[repr(C)]
#[derive(Debug)]
pub struct Ax3Label {
    pub index: u32,
}

#[repr(C)]
#[derive(Debug)]
pub struct Ax3Dll {
    pub name: [u8; 24]
}

bitflags! {
    pub struct Ax3HikiFlags: u32 {
        const Val = 0x1;
        const Str = 0x2;
        const Int = 0x4;
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct Ax3Hiki {
    pub flags: Ax3HikiFlags
}

#[repr(C)]
#[derive(Debug)]
pub struct Ax3Func {
    pub dll_index: u32,
    pub hiki: Ax3Hiki,
    pub name_offset: u32,
    pub _pad: u32,
}

#[repr(C)]
#[derive(Debug)]
pub struct Ax3Deffunc {
    pub label_index: u32,
    pub hiki_type: u16,
    pub hiki_count: u16,
    pub name_offset: u32,
}

#[repr(C)]
#[derive(Debug)]
pub struct Ax3Module {
    pub name: [u8; 24]
}

#[repr(C)]
#[derive(Debug)]
pub struct Ax3Data<'a> {
    pub header: &'a Ax3Header,
    pub script: &'a [u8],
    pub dlls: &'a [Ax3Dll],
    pub funcs: &'a [Ax3Func],
    pub deffuncs: &'a [Ax3Deffunc],
    pub modules: &'a [Ax3Module],
    pub labels: &'a [Ax3Label],
    pub texts: &'a [u8],
}

fn get_slice<'a>(slice: &'a [u8], offset: u32, size: u32) -> &'a [u8] {
    let offset = offset as usize;
    let size = size as usize;

    &slice[offset..(offset+size)]
}

fn transmute_slice<'a, T>(slice: &'a [u8], offset: u32, size: u32) -> &'a [T] {
    let t_slice = get_slice(slice, offset, size);
    unsafe { slice::from_raw_parts(t_slice.as_ptr() as *const _, size as usize / mem::size_of::<T>()) }
}

pub fn parse_data<'a, R: AsRef<[u8]>>(bytes: &'a R) -> Ax3Data<'a> {
    let slice = bytes.as_ref();
    let header: &'a Ax3Header = unsafe { &*(slice.as_ptr() as *const _) };

    Ax3Data {
        header: header,
        script: get_slice(slice, header.script_offset, header.script_size),
        labels: transmute_slice::<Ax3Label>(slice, header.label_offset, header.label_size),
        dlls: transmute_slice::<Ax3Dll>(slice, header.dll_offset, header.dll_size),
        funcs: transmute_slice::<Ax3Func>(slice, header.func_offset, header.func_size),
        deffuncs: transmute_slice::<Ax3Deffunc>(slice, header.deffunc_offset, header.deffunc_size),
        modules: transmute_slice::<Ax3Module>(slice, header.module_offset, header.module_size),
        texts: get_slice(slice, header.text_offset, header.text_size),
    }
}

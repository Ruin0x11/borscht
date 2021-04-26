use std::slice;
use std::mem;
use bitflags::bitflags;
use anyhow::{Result, anyhow};

use super::util;

#[repr(C)]
#[derive(Debug)]
pub struct Ax3ViewHeader {
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
pub struct Ax3View<'a> {
    pub header: &'a Ax3ViewHeader,
    pub script: &'a [u8],
    pub dlls: &'a [Ax3Dll],
    pub funcs: &'a [Ax3Func],
    pub deffuncs: &'a [Ax3Deffunc],
    pub modules: &'a [Ax3Module],
    pub labels: &'a [Ax3Label],
    pub texts: &'a [u8],
}

pub fn parse_view<'a, R: AsRef<[u8]>>(bytes: &'a R) -> Ax3View<'a> {
    let slice = bytes.as_ref();
    let header: &'a Ax3ViewHeader = unsafe { &*(slice.as_ptr() as *const _) };

    Ax3View {
        header: header,
        script: util::get_slice(slice, header.script_offset, header.script_size),
        labels: util::transmute_slice::<Ax3Label>(slice, header.label_offset, header.label_size),
        dlls: util::transmute_slice::<Ax3Dll>(slice, header.dll_offset, header.dll_size),
        funcs: util::transmute_slice::<Ax3Func>(slice, header.func_offset, header.func_size),
        deffuncs: util::transmute_slice::<Ax3Deffunc>(slice, header.deffunc_offset, header.deffunc_size),
        modules: util::transmute_slice::<Ax3Module>(slice, header.module_offset, header.module_size),
        texts: util::get_slice(slice, header.text_offset, header.text_size),
    }
}

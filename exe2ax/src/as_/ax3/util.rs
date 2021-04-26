use std::slice;
use std::mem;

pub(crate) fn get_slice<'a>(slice: &'a [u8], offset: u32, size: u32) -> &'a [u8] {
    let offset = offset as usize;
    let size = size as usize;

    &slice[offset..(offset+size)]
}

pub(crate) fn transmute_slice<'a, T>(slice: &'a [u8], offset: u32, size: u32) -> &'a [T] {
    let t_slice = get_slice(slice, offset, size);
    unsafe { slice::from_raw_parts(t_slice.as_ptr() as *const _, size as usize / mem::size_of::<T>()) }
}

use std::ffi::{CStr};
use std::path::PathBuf;
use std::io::{Read, Seek, SeekFrom, Cursor};
use std::str::{self, FromStr};
use anyhow::{Result, anyhow};

use crate::ax::Ax;

pub mod dictionary;
pub mod ax3;

pub struct DecodeOptions {}

pub fn ax_to_as(ax: Ax, opts: &DecodeOptions) -> Result<ax3::Hsp3As> {
    match ax {
        Ax::Ax2(_) => Err(anyhow!("asd")),
        Ax::Ax3(data) => Ok(self::ax3::decode(data, opts)?)
    }
}

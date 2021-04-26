use std::ffi::{CStr};
use std::io::{Read, Seek, SeekFrom, Cursor};
use std::str::{self, FromStr};
use anyhow::{Result, anyhow};

use crate::ax::Ax;

pub mod dictionary;
pub mod ax3;

pub fn ax_to_as(ax: &Ax) -> Result<()> {
    match ax {
        Ax::Ax2(_) => Err(anyhow!("asd")),
        Ax::Ax3(data) => {
            let ax3_data = self::ax3::data::parse_data(&data);
            // println!("{:#?}", ax3_data);
            Ok(())
        }
    }
}

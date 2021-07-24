use std::ffi::{CStr};
use std::io::{Read, Seek, SeekFrom, Cursor};
use std::str::{self, FromStr};
use anyhow::{Result, anyhow};

use crate::dpm::Dpm;
use crate::crypt;
use crate::error;

#[derive(Clone, Debug)]
pub enum Ax {
    Ax2(Vec<u8>),
    Ax3(Vec<u8>)
}

pub enum AxType {
    Ax2,
    Ax3
}

impl FromStr for AxType {
    type Err = error::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "HSP2" => Ok(AxType::Ax2),
            "HSP3" => Ok(AxType::Ax3),
            _ => Err(error::Error::new(format!("Unknown AX format {}", s)))

        }
    }
}

impl AsRef<[u8]> for Ax {
    fn as_ref(&self) -> &[u8] {
        match self {
            Ax::Ax2(b) => &b,
            Ax::Ax3(b) => &b
        }
    }
}

pub fn dpm_to_ax(dpm: &Dpm) -> Result<Ax> {
    let start_ax = dpm.get_file_data("start.ax").ok_or_else(|| anyhow!("No start.ax found in DPM file"))?;

    let buffer = if start_ax.file.encryption_key.is_some() {
        crypt::decrypt(&start_ax.data)?
    } else {
        start_ax.data.clone()
    };

    bytes_to_ax(buffer)
}

pub fn bytes_to_ax(buffer: Vec<u8>) -> Result<Ax> {
    let mut stream = Cursor::new(&buffer);

    let mut header = [0; 4];
    stream.read_exact(&mut header)?;
    let s = str::from_utf8(&header)?;

    AxType::from_str(s).map(|t| match t {
        AxType::Ax2 => Ax::Ax2(buffer),
        AxType::Ax3 => Ax::Ax3(buffer)
    })
    .map_err(|e| anyhow!(e))
}

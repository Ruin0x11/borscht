use std::ffi::{CStr};
use std::io::{Read, Seek, SeekFrom, Cursor};
use std::str::{self, FromStr};
use anyhow::{Result, anyhow};

use crate::dpm::Dpm;
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

    if start_ax.file.encryption_key.is_some() {
        return Err(anyhow!("Encrypted files are not supported"))
    }

    let mut stream = Cursor::new(&start_ax.data);

    let mut header = [0; 4];
    stream.read_exact(&mut header)?;
    let s = str::from_utf8(&header)?;

    AxType::from_str(s).map(|t| match t {
        AxType::Ax2 => Ax::Ax2(start_ax.data.clone()),
        AxType::Ax3 => Ax::Ax3(start_ax.data.clone())
    })
    .map_err(|e| anyhow!(e))
}

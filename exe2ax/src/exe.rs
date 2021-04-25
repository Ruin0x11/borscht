use std::io::{Read, Seek, SeekFrom};
use anyhow::Result;

#[derive(Clone, Debug)]
pub struct Dpm(pub Vec<u8>);

pub static DPM_HEADER: &'static str = "DPMX";

fn seek_to_dpm_start<R: Read + Seek>(stream: &mut R) -> Result<bool> {
    let len = stream.stream_len()?;
    let mut header = [0; 4];

    if len >= 0x25004 {
        stream.seek(SeekFrom::Start(0x25000))?;
        if let Ok(_) = stream.read_exact(&mut header) {
            if &header == DPM_HEADER.as_bytes() {
                stream.seek(SeekFrom::Start(0x25000))?;
                return Ok(true)
            }
        }
    } else if len >= 0x1BE04 {
        stream.seek(SeekFrom::Start(0x1BE00))?;
        if let Ok(_) = stream.read_exact(&mut header) {
            if &header == DPM_HEADER.as_bytes() {
                stream.seek(SeekFrom::Start(0x25000))?;
                return Ok(true)
            }
        }
    }

    stream.seek(SeekFrom::Start(0x0))?;

    while let Ok(_) = stream.read_exact(&mut header) {
        if &header == DPM_HEADER.as_bytes() {
            stream.seek(SeekFrom::Current(-0x4))?;
            return Ok(true)
        }
    }

    stream.seek(SeekFrom::Start(0x0))?;
    Ok(false)
}

pub fn exe_to_dpm<R: Read + Seek>(stream: &mut R) -> Result<Option<Dpm>> {
    if !seek_to_dpm_start(stream)? {
        return Ok(None)
    }

    let mut buf = Vec::new();
    stream.read_to_end(&mut buf)?;

    Ok(Some(Dpm(buf)))
}

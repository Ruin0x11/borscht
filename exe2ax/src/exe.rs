use std::io::{Read, Seek, SeekFrom};
use anyhow::Result;

pub struct Dpm(pub Vec<u8>);

static DPM_HEADER: &'static str = "DPMX";

fn seek_to_dpm_start<R: Read + Seek>(stream: &mut R) -> Result<bool> {
    let len = stream.stream_len()?;
    let mut bytes = [0; 4];

    if len >= 0x25004 {
        stream.seek(SeekFrom::Start(0x25000))?;
        if let Ok(_) = stream.read(&mut bytes) {
            if &bytes == DPM_HEADER.as_bytes() {
                stream.seek(SeekFrom::Start(0x25000))?;
                return Ok(true)
            }
        }
    } else if len >= 0x1BE04 {
        stream.seek(SeekFrom::Start(0x1BE00))?;
        if let Ok(_) = stream.read_exact(&mut bytes) {
            if &bytes == DPM_HEADER.as_bytes() {
                stream.seek(SeekFrom::Start(0x25000))?;
                return Ok(true)
            }
        }
    }

    stream.seek(SeekFrom::Start(0x0))?;

    while let Ok(_) = stream.read_exact(&mut bytes) {
        if &bytes == DPM_HEADER.as_bytes() {
            return Ok(true)
        }
    }

    stream.seek(SeekFrom::Start(0x0))?;
    Ok(false)
}

pub fn exe_to_dpm<R: Read + Seek>(stream: &mut R) -> Result<Dpm> {
    seek_to_dpm_start(stream)?;

    let mut buf = Vec::new();
    stream.read_to_end(&mut buf)?;

    Ok(Dpm(buf))
}

use std::ffi::{CStr};
use std::io::{Read, Seek, SeekFrom};
use std::str;
use byteorder::{LittleEndian, ReadBytesExt};
use pelite::pe32::{PeFile, Pe};
use anyhow::{Result, anyhow};

static DPM_HEADER: &'static str = "DPMX";

#[derive(Clone, Debug)]
pub struct DpmFile {
    pub name: String,
    pub unknown: u32,
    pub encryption_key: Option<u32>,
    pub file_offset: u64,
    pub file_size: u64
}

#[derive(Clone, Debug)]
pub struct DpmHeader {
    pub file_offset_start: u64,
    pub files: Vec<DpmFile>
}

#[derive(Clone, Debug)]
pub struct Dpm {
    pub header: DpmHeader,
    pub file_data: Vec<Vec<u8>>
}

pub struct DpmFileRef<'a> {
    pub file: &'a DpmFile,
    pub data: &'a Vec<u8>
}

impl Dpm {
    pub fn get_file_data<'a>(&'a self, file_name: &str) -> Option<DpmFileRef<'a>> {
        let mut ind = None;

        for (i, file) in self.header.files.iter().enumerate() {
            if &file.name == file_name {
                ind = Some(i);
                break;
            }
        }

        ind.map(|i| DpmFileRef { file: &self.header.files[i], data: &self.file_data[i] })
    }

    pub fn iter_files<'a>(&'a self) -> std::slice::Iter<'_, DpmFile> {
        self.header.files.iter()
    }
}

// unsafe: s must contain a null byte
unsafe fn str_from_null_terminated_utf8(s: &[u8]) -> &str {
    CStr::from_ptr(s.as_ptr() as *const _).to_str().unwrap()
}

fn str_from_null_terminated_utf8_safe(s: &[u8]) -> &str {
    if s.iter().any(|&x| x == 0) {
        unsafe { str_from_null_terminated_utf8(s) }
    } else {
        str::from_utf8(s).unwrap()
    }
}

fn read_dpm_header<R: Read + Seek>(stream: &mut R) -> Result<DpmHeader> {
    let mut header = [0; 4];

    stream.read_exact(&mut header)?;
    stream.seek(SeekFrom::Start(0))?;

    if header[0] == ('M' as u8) && header[1] == ('Z' as u8) {
        let mut buf = Vec::new();
        stream.read_to_end(&mut buf)?;
        let pe_file = PeFile::from_bytes(&buf)?;
        let headers = pe_file.section_headers().image();

        let last_header = headers[headers.len() - 1];
        let end_of_exe = last_header.PointerToRawData + last_header.SizeOfRawData;
        stream.seek(SeekFrom::Start(end_of_exe as u64))?;
        stream.read_exact(&mut header)?;
        stream.seek(SeekFrom::Current(-0x4))?;
    }

    if &header != DPM_HEADER.as_bytes() {
        return Err(anyhow!("No DPM header found"));
    }

    let start_position = stream.stream_position()?;

    stream.read_u32::<LittleEndian>()?;
    stream.read_u32::<LittleEndian>()?;
    let file_count = stream.read_u32::<LittleEndian>()? as u64;
    stream.read_u32::<LittleEndian>()?;
    println!("{:?}", file_count);

    let file_offset_start = start_position + 0x10 + file_count * 0x20;

    let mut files = Vec::new();
    let mut name_buf = [0; 16];

    for _ in 0..file_count {
        stream.read_exact(&mut name_buf)?;
        let name = str_from_null_terminated_utf8_safe(&name_buf).to_string();
        let unknown = stream.read_u32::<LittleEndian>()?;
        let encryption_key = stream.read_u32::<LittleEndian>()?;
        let file_offset = stream.read_u32::<LittleEndian>()? as u64;
        let file_size = stream.read_u32::<LittleEndian>()? as u64;

        if file_offset + file_size > stream.stream_len()? {
            return Err(anyhow!("DPM file overflowed bounds of stream"));
        }

        let encryption_key_ = if encryption_key != 0 {
            Some(encryption_key)
        } else {
            None
        };

        let file = DpmFile {
            name: name,
            unknown: unknown,
            encryption_key: encryption_key_,
            file_offset: file_offset,
            file_size: file_size,
        };
        println!("{:?}", file);

        files.push(file);
    }

    Ok(DpmHeader {
        file_offset_start: file_offset_start,
        files: files
    })
}

pub fn exe_to_dpm<R: Read + Seek>(stream: &mut R) -> Result<Dpm> {
    let header = read_dpm_header(stream)?;

    let mut file_data = Vec::new();

    for file in header.files.iter() {
        stream.seek(SeekFrom::Start(header.file_offset_start + file.file_offset))?;
        let mut data = Vec::new();
        stream.take(file.file_size).read_to_end(&mut data)?;
        file_data.push(data);
    }

    Ok(Dpm {
        header: header,
        file_data: file_data
   })
}

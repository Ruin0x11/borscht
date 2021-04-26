use std::collections::HashMap;
use std::convert::TryFrom;
use std::fs::File;
use std::io::{Read, Seek, SeekFrom, Cursor};
use std::path::Path;
use encoding_rs::SHIFT_JIS;
use encoding_rs_io::DecodeReaderBytesBuilder;
use anyhow::{Result, anyhow};

use crate::as_::dictionary::*;

#[derive(Debug)]
pub struct Hsp3Dictionary {
    pub codes: HspCodeDictionary,
    pub params: HashMap<u32, String>
}

fn find_subsequence(haystack: &[u8], needle: &[u8]) -> Option<usize> {
    haystack.windows(needle.len()).position(|window| window == needle)
}

fn extract_range<'a>(bytes: &'a [u8], start: &str, end: &str) -> Result<&'a [u8]> {
    let start_pos = find_subsequence(bytes, start.as_bytes()).ok_or_else(|| anyhow!("No {} found in dictionary", start))?;
    let end_offset = find_subsequence(&bytes[start_pos..], end.as_bytes()).ok_or_else(|| anyhow!("No {} found in dictionary", end))?;
    Ok(&bytes[start_pos..start_pos+end_offset])
}

fn parse_hex_u8(raw: &str) -> u8 {
    let without_prefix = raw.trim_start_matches("0x");
    u8::from_str_radix(without_prefix, 16).unwrap()
}

fn parse_hex_i32(raw: &str) -> i32 {
    let without_prefix = raw.trim_start_matches("0x");
    i32::from_str_radix(without_prefix, 16).unwrap()
}

fn parse_hex_u32(raw: &str) -> u32 {
    let without_prefix = raw.trim_start_matches("0x");
    u32::from_str_radix(without_prefix, 16).unwrap()
}

fn parse_extra(s: &str) -> (HspCodeExtraFlags, u32) {
    if s.starts_with("Priority_") {
        (HspCodeExtraFlags::None, s.strip_prefix("Priority_").unwrap().parse::<u32>().unwrap())
    } else {
        let map = |flag| match flag {
            "HasExtraInt16" => HspCodeExtraFlags::HasExtraInt16,
            "HasGhostLabel" => HspCodeExtraFlags::HasGhostLabel,
            "HasGhostGoto" => HspCodeExtraFlags::HasGhostGoto,
            "AddTab" => HspCodeExtraFlags::AddTab,
            "RemoveTab" => HspCodeExtraFlags::RemoveTab,
            "IsGhost" => HspCodeExtraFlags::IsGhost,
            "BracketStart" => HspCodeExtraFlags::BracketStart,
            "BracketEnd" => HspCodeExtraFlags::BracketEnd,
            "GotoFunction" => HspCodeExtraFlags::GotoFunction,
            _ => HspCodeExtraFlags::None,
        };
        let spl = s.split(" ").map(map).fold(HspCodeExtraFlags::None, |acc, f| acc | f);
        (spl, 0)
    }
}

fn make_csv_reader<'a>(bytes: &'a [u8]) -> csv::Reader<encoding_rs_io::DecodeReaderBytes<&'a [u8], Vec<u8>>> {
    let transcoded = DecodeReaderBytesBuilder::new()
        .encoding(Some(SHIFT_JIS))
        .build(bytes);

    csv::ReaderBuilder::new()
        .delimiter(b',')
        .comment(Some(b'#'))
        .flexible(true)
        .from_reader(transcoded)
}

fn extract_code<'a>(bytes: &'a [u8]) -> Result<HspCodeDictionary> {
    let range = extract_range(bytes, "$Code", "$End")?;

    let mut rdr = make_csv_reader(range);

    let mut dict = HspCodeDictionary::new();

    for result in rdr.records() {
        let r = result?;

        let type_ = r.get(0).map(|i| parse_hex_u8(i)).ok_or_else(|| anyhow!("Missing type"))?;
        let value_ = r.get(1).map(|i| parse_hex_i32(i)).ok_or_else(|| anyhow!("Missing value"))?;
        let value = if value_ == -1 {
            None
        } else {
            Some(value_ as u32)
        };
        let dict_key = HspDictionaryKey { type_, value };

        let name = r.get(2).ok_or_else(|| anyhow!("Missing name"))?.to_string();
        let code_type_ = r.get(3).ok_or_else(|| anyhow!("Missing code type"))?;
        let code_type: HspCodeType = code_type_.parse::<HspCodeType>().map_err(|_| anyhow!("Failed to parse code type {}", code_type_))?;
        let (extra, priority) = match r.get(4) {
            Some(s) => parse_extra(s),
            None => (HspCodeExtraFlags::None, 0)
        };

        let dict_value = HspDictionaryValue { name, code_type, extra, priority };
        dict.insert(dict_key, dict_value);
    }

    Ok(dict)
}

fn extract_param_type<'a>(bytes: &'a [u8]) -> Result<HashMap<u32, String>> {
    let range = extract_range(bytes, "$ParamType", "$End")?;

    let mut rdr = make_csv_reader(range);

    let mut dict = HashMap::new();

    for result in rdr.records() {
        let r = result?;

        let key = r.get(0).map(|i| parse_hex_u32(i)).ok_or_else(|| anyhow!("Missing key"))?;
        let value = r.get(1).ok_or_else(|| anyhow!("Missing value"))?.to_string();
        dict.insert(key, value);
    }

    Ok(dict)
}

impl Hsp3Dictionary {
    pub fn from_csv<P: AsRef<Path>>(path: P) -> Result<Self> {
        let mut file = File::open(path.as_ref())?;
        let mut buf = Vec::new();
        file.read_to_end(&mut buf)?;

        let codes = extract_code(&buf)?;
        let params = extract_param_type(&buf)?;

        Ok(Hsp3Dictionary {
            codes: codes,
            params: params
        })
    }

    pub fn lookup_code(&self, key: &HspDictionaryKey) -> Option<HspDictionaryValue> {
        match self.codes.get(key) {
            Some(v) => Some(v.clone()),
            None => {
                let key = HspDictionaryKey { type_: key.type_, value: None };
                match self.codes.get(&key) {
                    Some(v) => Some(v.clone()),
                    None => {
                        if key.type_ == 0x11 && key.value >= Some(0x1000) {
                            // ComFunction
                            Some(HspDictionaryValue {
                                name: "comfunc".to_string(),
                                code_type: HspCodeType::ComFunction,
                                extra: HspCodeExtraFlags::None,
                                priority: 0
                            })
                        } else if key.type_ >= 0x12 {
                            // PlugInFunction
                            Some(HspDictionaryValue {
                                name: "pluginFunction".to_string(),
                                code_type: HspCodeType::try_from(key.type_ as u8 - 0x12).unwrap(),
                                extra: HspCodeExtraFlags::None,
                                priority: 0
                            })
                        } else {
                            None
                        }
                    }
                }
            }
        }
    }
}

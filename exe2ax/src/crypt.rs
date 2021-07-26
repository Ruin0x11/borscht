use std::io::{Read, Seek, SeekFrom, Cursor};
use anyhow::{anyhow, Result};

#[derive(Debug)]
struct XorAddTransform {
    xor_byte: u8,
    add_byte: u8,
    xor_sum: bool
}

impl XorAddTransform {
    pub fn encode(&self, b: u8) -> u8 {
        if self.xor_sum {
            ((b ^ self.xor_byte).overflowing_add(self.add_byte)).0
        } else {
            (self.add_byte.overflowing_add(b).0) ^ self.xor_byte
        }
    }

    pub fn decode(&self, b: u8) -> u8 {
        if self.xor_sum {
            (b.overflowing_sub(self.add_byte).0) ^ self.xor_byte
        } else {
            (b ^ self.xor_byte).overflowing_sub(self.add_byte).0
        }
    }
}

pub fn get_xor_byte(add: u8, plain: u8, encrypted: u8, xor_sum: bool) -> u8 {
    if xor_sum {
        (encrypted.overflowing_sub(add).0) ^ plain
    } else {
        encrypted ^ (plain.overflowing_add(add).0)
    }
}

fn brute_force_xor_key(encrypted: &[u8]) -> Result<XorAddTransform> {
    let mut diff_buffer: Vec<u8> = Vec::new();
    let mut prev = 0u8;
    let mut and = 0xFFu8;
    let mut or = 0x00u8;
    let plain = [ 0x48u8, 0x53u8, 0x50u8, 0x33u8 ]; // HSP3

    for byte in plain.iter() {
        let next = (*byte).overflowing_sub(prev).0;
        diff_buffer.push(next);
        prev = *byte;
        and &= next;
        or |= next;
    }

    if and != 0x00 || or != 0xFF {
        return Err(anyhow!("Failed to decode encryption diff buffer"))
    }

    for i in 0..0xFF {
        let add_byte = i & 0x7F;
        let xor_sum = i >= 0x80;
        let xoradd = XorAddTransform {
            xor_byte: get_xor_byte(add_byte, diff_buffer[0], encrypted[0], xor_sum),
            add_byte: add_byte,
            xor_sum: xor_sum
        };

        let mut found = true;
        for (i, _byte) in plain.iter().enumerate() {
            if encrypted[i] != xoradd.encode(diff_buffer[i]) {
                found = false;
                break;
            }
        }

        if found {
            return Ok(xoradd);
        }
    }

    Err(anyhow!("Failed to find encryption key"))
}

pub fn decrypt(buf: &[u8]) -> Result<Vec<u8>> {
    let xoradd = brute_force_xor_key(buf)?;
    println!("{:?}", xoradd);

    let mut result = Vec::new();

    let mut prev = 0u8;
    for encoded in buf.iter() {
        let plain = xoradd.decode(*encoded);
        let decoded = plain.overflowing_add(prev).0;
        prev = decoded;
        result.push(decoded)
    }

    Ok(result)
}

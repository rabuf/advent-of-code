use std::fs::File;
use std::io::{BufRead, BufReader};

fn get_input() -> (u32, Vec<u64>) {
    let filename = "../input/03.txt";
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let mut width: u32 = 0;
    let codes: Vec<u64> = reader
        .lines()
        .map(|l| {
            let line = l.unwrap();
            let line = line.trim();
            width = width.max(line.len() as u32);
            u64::from_str_radix(line, 2).unwrap()
        })
        .collect();
    (width, codes)
}

fn gamma(codes: &[u64], width: u32) -> u64 {
    let mut result = 0;
    let mut mask = 1;
    for _ in 0..width {
        if codes.iter().filter(|c| **c & mask != 0).count() >= codes.len() / 2 {
            result += mask;
        }
        mask = mask * 2;
    }
    return result;
}

pub fn day03_01() -> u64 {
    let (width, codes) = get_input();

    let g = gamma(&codes, width);
    let e = !g & (2_u64.pow(width) - 1);

    return g * e;
}

pub fn day03_02() -> u64 {
    let (width, mut codes) = get_input();

    codes.sort();
    let mut o_lower = 0;
    let mut o_upper = codes.len();
    let mut c_lower = 0;
    let mut c_upper = codes.len();
    for i in (0..width).rev() {
        let mask = 2_u64.pow(i);
        if o_upper - o_lower > 1 {
            let mut o_mid = o_lower + 1;
            while o_mid < o_upper && codes[o_mid] & mask == codes[o_mid - 1] & mask {
                o_mid = o_mid + 1;
            }
            if o_mid - o_lower <= o_upper - o_mid {
                o_lower = o_mid;
            } else {
                o_upper = o_mid;
            }
        }
        if c_upper - c_lower > 1 {
            let mut c_mid = c_lower + 1;
            while c_mid < c_upper && codes[c_mid] & mask == codes[c_mid - 1] & mask {
                c_mid = c_mid + 1;
            }
            if c_mid - c_lower > c_upper - c_mid {
                c_lower = c_mid;
            } else {
                c_upper = c_mid;
            }
        }
    }
    return codes[c_lower] * codes[o_lower];
}

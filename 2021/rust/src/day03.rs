use std::fs::File;
use std::io::{BufRead, BufReader};

fn get_input(filename: &str) -> (u32, Vec<u64>) {
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
    result
}

fn power_consumption(codes: &[u64], width: u32) -> u64 {
    let g = gamma(&codes, width);
    let e = !g & ((1 << width) - 1);
    g * e
}

pub fn part1() -> u64 {
    let (width, codes) = get_input("../input/03.txt");
    power_consumption(&codes, width)
}

pub fn part2() -> u64 {
    let (width, mut codes) = get_input("../input/03.txt");

    codes.sort();
    let mut o_lower = 0;
    let mut o_upper = codes.len();
    let mut c_lower = 0;
    let mut c_upper = codes.len();
    for i in (0..width).rev() {
        let mask = 1 << i;
        if o_upper - o_lower > 1 {
            let mid = binary_search(&codes, o_lower, o_upper, mask);
            if mid - o_lower <= o_upper - mid {
                o_lower = mid;
            } else {
                o_upper = mid;
            }
        }
        if c_upper - c_lower > 1 {
            let mid = binary_search(&codes, c_lower, c_upper, mask);
            if mid - c_lower > c_upper - mid {
                c_lower = mid;
            } else {
                c_upper = mid;
            }
        }
    }
    codes[c_lower] * codes[o_lower]
}

fn binary_search(codes: &[u64], lower: usize, upper: usize, mask: u64) -> usize {
    let mut mid = (upper - lower) / 2 + lower;
    let mut lower = lower;
    let mut upper = upper;
    while lower + 1 != upper {
        if codes[mid] & mask > 0 {
            upper = mid;
            mid = (lower + mid) / 2;
        } else {
            lower = mid;
            mid = (upper + mid) / 2;
        }
    }
    mid + 1
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn gamma_test() {
        let (width, codes) = get_input("../test/03.txt");
        let g = gamma(&codes, width);
        assert_eq!(22, g);
    }

    #[test]
    fn epsilon_test() {
        let (width, codes) = get_input("../test/03.txt");
        let g = gamma(&codes, width);
        let e = !g & ((1 << width) - 1);
        assert_eq!(9, e);
    }

    #[test]
    fn power_consumption_test() {
        let (width, codes) = get_input("../test/03.txt");
        assert_eq!(198, power_consumption(&codes, width))
    }
}

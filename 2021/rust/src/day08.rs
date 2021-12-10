use itertools::Itertools;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn get_input(filename: &str) -> (Vec<Vec<String>>, Vec<Vec<String>>) {
    let file = File::open(filename).unwrap();
    let lines = BufReader::new(file).lines();

    let mut displays = vec![];
    let mut codes = vec![];

    for line in lines {
        let line = line.unwrap();
        let mut parts = line.split(" | ");
        let cs = parts
            .next()
            .unwrap()
            .split(' ')
            .map(|s| s.chars().sorted().collect())
            .collect();
        codes.push(cs);
        let ds = parts
            .next()
            .unwrap()
            .split(' ')
            .map(|s| s.chars().sorted().collect())
            .collect();
        displays.push(ds);
    }
    (codes, displays)
}

/**
Part 1 is straightforward: going through the display portions, count how many 2, 3, 4, and 7
length strings appear.
*/

pub fn part1() -> usize {
    let (_displays, codes) = get_input("../input/08.txt");
    codes
        .iter()
        .map(|s| {
            s.iter()
                .filter(|s| s.len() == 2 || s.len() == 3 || s.len() == 4 || s.len() == 7)
                .count()
        })
        .sum()
}

fn clever_decode(codes: &[String], display: &[String]) -> usize {
    let mut codes: Vec<_> = codes.iter().collect();
    let default = "".to_string();
    let mut decoder: Vec<&String> = vec![&default; 10];
    // 1, 4, 7, 8 are straight forward
    codes.iter().for_each(|&code| match code.len() {
        2 => decoder[1] = code,
        3 => decoder[7] = code,
        4 => decoder[4] = code,
        7 => decoder[8] = code,
        _ => (),
    });
    // remove the discovered values
    codes.retain(|c| !decoder.contains(c));
    codes.iter().for_each(|&code| {
        if code.len() == 6 && decoder[4].chars().all(|c| code.contains(c)) {
            decoder[9] = code;
        }
    });
    codes.retain(|c| !decoder.contains(c));
    codes.iter().for_each(|&code| {
        if code.len() == 6 && decoder[7].chars().all(|c| code.contains(c)) {
            decoder[0] = code;
        }
    });
    codes.retain(|c| !decoder.contains(c));
    codes.iter().for_each(|&code| {
        if code.len() == 6 {
            decoder[6] = code;
        }
    });
    codes.retain(|c| !decoder.contains(c));
    codes.iter().for_each(|&code| {
        if decoder[7].chars().all(|c| code.contains(c)) {
            decoder[3] = code;
        }
    });
    codes.retain(|c| !decoder.contains(c));
    codes.iter().for_each(|&code| {
        if code.chars().all(|c| decoder[6].contains(c)) {
            decoder[5] = code;
        }
    });
    codes.retain(|c| !decoder.contains(c));
    decoder[2] = codes[0];
    let mut result = 0;
    for d in display.iter() {
        result *= 10;
        let i = decoder.iter().position(|&s| s == d).or(Some(0)).unwrap();
        result += i;
    }
    result
}

pub fn part2() -> usize {
    let (codes, displays) = get_input("../input/08.txt");
    codes
        .iter()
        .zip(displays)
        .map(|(c, d)| clever_decode(c, &d))
        .sum()
}

#[cfg(test)]
mod test {
    use super::*;
}

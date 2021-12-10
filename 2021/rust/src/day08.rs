use itertools::Itertools;
use std::collections::HashSet;
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

fn translate(perm: &[char], codes: &[String]) -> Vec<String> {
    let mut translated: Vec<String> = vec![];
    // for each code, use the index of the code in the permutation
    // as the basis for mapping. That is, if 'g' is in the first index
    // it becomes 'a' when translated
    for code in codes.iter() {
        let t = code
            .chars()
            .map(|c| {
                let i = perm.iter().position(|p| *p == c).unwrap();
                char::from_u32((i as u32) + ('a' as u32)).unwrap()
            })
            .sorted()
            .collect::<String>();
        translated.push(t);
    }

    translated
}

fn decode_line(codes: &[String], display: &[String]) -> usize {
    let canonical_strings: [&str; 10] = [
        "abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg",
    ];

    let canonical_set: HashSet<_> = canonical_strings.iter().map(|s| s.to_string()).collect();

    for perm in "abcdefg".chars().permutations(7) {
        let codes = translate(&perm, codes);

        if 10 == codes.iter().filter(|&s| canonical_set.contains(s)).count() {
            let displays = translate(&perm, display);
            let result = displays
                .iter()
                .map(|d| canonical_strings.iter().position(|&s| s == d).unwrap())
                .fold(0, |acc, val| acc * 10 + val);
            return result;
        }
    }
    0 // should never happen, but if it can't be decoded just returns 0
}

pub fn part2() -> usize {
    let (codes, displays) = get_input("../input/08.txt");
    codes
        .iter()
        .zip(displays)
        .map(|(c, d)| decode_line(c, &d))
        .sum()
}

#[cfg(test)]
mod test {
    use super::*;
}

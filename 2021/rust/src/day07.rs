use std::fs::File;
use std::io::{BufRead, BufReader};

fn get_input(filename: &str) -> Vec<isize> {
    let file = File::open(filename).unwrap();
    let line = BufReader::new(file).lines().next().unwrap().unwrap();

    let crabs = line
        .split(',')
        .map(|f| f.parse::<isize>().unwrap())
        .collect();
    crabs
}

pub fn part1() -> isize {
    let mut crabs = get_input("../input/07.txt");
    let n = crabs.len() / 2;
    crabs.select_nth_unstable(n);
    let x = crabs[n];
    crabs.iter().map(|c| (*c - x).abs()).sum()
}

fn triangle(n: isize) -> isize {
    n * (n + 1) / 2
}

pub fn part2() -> isize {
    let crabs = get_input("../input/07.txt");
    let mean = crabs.iter().sum::<isize>() / (crabs.len() as isize);
    let mut min = isize::MAX;
    for pos in (mean - 1)..=(mean + 1) {
        let fuel = crabs.iter().map(|c| triangle((*c - pos).abs())).sum();
        min = min.min(fuel)
    }
    min
}

#[cfg(test)]
mod test {
    use super::*;
}

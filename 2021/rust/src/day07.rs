use std::fs::File;
use std::io::{BufRead, BufReader};

fn get_input(filename: &str) -> (Vec<isize>, isize, isize) {
    let file = File::open(filename).unwrap();
    let line = BufReader::new(file).lines().next().unwrap().unwrap();
    let mut upper = isize::MIN;
    let mut lower = isize::MAX;
    let crabs = line.split(",")
        .map(|f| {

            let x = isize::from_str_radix(f,10).unwrap();
            upper = upper.max(x);
            lower = lower.min(x);
            x
        })
        .collect();
    (crabs, lower, upper)
}

pub fn part1() -> isize {
    let (mut crabs, _lower, _upper) = get_input("../input/07.txt");
    let n = crabs.len()/2;
    crabs.select_nth_unstable(n);
    let x = crabs[n];
    crabs.iter().map(|c| (*c-x).abs()).sum()
}

fn triangle(n : isize) -> isize {
    n * (n + 1) / 2
}

pub fn part2() -> isize {
    let (crabs, lower, upper) = get_input("../input/07.txt");
    let mut min = isize::MAX;
    for pos in lower..=upper {
        let fuel = crabs.iter().map(|c| triangle((*c - pos).abs())).sum();
        min = min.min(fuel)
    }
    min
}

#[cfg(test)]
mod test {
    use super::*;
}

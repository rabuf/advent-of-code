use std::fs::File;
use std::io::{BufRead, BufReader};

fn get_input(filename: &str) -> usize {
    let file = File::open(filename).unwrap();
    let lines = BufReader::new(file).lines();

    lines.count()
}

pub fn part1() -> usize {
    get_input("../input/09.txt")
}

pub fn part2() -> usize {
    get_input("../input/09.txt") + 1
}

#[cfg(test)]
mod test {
    use super::*;
}

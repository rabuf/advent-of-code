use std::fs::File;
use std::io::{BufRead, BufReader};

pub fn part1() -> i64 {
    let filename = "../input/01.txt";
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);

    reader
        .lines()
        .map(|l| l.unwrap().parse::<i64>().unwrap())
        .fold((0_i64, i64::MAX), |(sum, prev), curr| {
            (sum + if prev < curr { 1 } else { 0 }, curr)
        })
        .0
}
pub fn part2() -> i64 {
    let filename = "../input/01.txt";
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);

    reader
        .lines()
        .map(|l| l.unwrap().parse::<i64>().unwrap())
        .fold(
            (0_i64, (i64::MAX, i64::MAX, i64::MAX)),
            |(sum, (prev, b, c)), curr| (sum + if prev < curr { 1 } else { 0 }, (b, c, curr)),
        )
        .0
}

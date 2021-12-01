use std::fs::File;
use std::io::{BufRead, BufReader};

fn day01_01() -> i64 {
    let filename = "../input/01.txt";
    let file = match File::open(filename) {
        Ok(f) => f,
        Err(_) => panic!("{} not found.", filename),
    };
    let reader = BufReader::new(file);
    reader
        .lines()
        .map(|l| l.unwrap().parse::<i64>().unwrap())
        .fold((0 as i64, i64::max_value()), |(sum, prev), curr| {
            (sum + if prev < curr { 1 } else { 0 }, curr)
        })
        .0
}
fn day01_02() -> i64 {
    let filename = "../input/01.txt";
    let file = match File::open(filename) {
        Ok(f) => f,
        Err(_) => panic!("{} not found.", filename),
    };
    let reader = BufReader::new(file);
    reader
        .lines()
        .map(|l| l.unwrap().parse::<i64>().unwrap())
        .fold(
            (
                0 as i64,
                (i64::max_value(), i64::max_value(), i64::max_value()),
            ),
            |(sum, (prev, b, c)), curr| (sum + if prev < curr { 1 } else { 0 }, (b, c, curr)),
        )
        .0
}
fn main() {
    println!("Hello AoC 2021!");
    println!("Day 01 Part 01: {0}", day01_01());
    println!("Day 01 Part 02: {0}", day01_02());
}

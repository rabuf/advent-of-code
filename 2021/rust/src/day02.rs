use std::fs::File;
use std::io::{BufRead, BufReader};

pub fn day02_01() -> i64 {
    let filename = "../input/02.txt";
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let (mut h, mut d) = (0, 0);
    for line in reader.lines() {
        let line = line.unwrap();
        let parts: Vec<_> = line.split_whitespace().collect();
        let distance = parts[1].parse::<i64>().unwrap();
        match parts[0] {
            "forward" => h = h + distance,
            "up" => d = d - distance,
            "down" => d = d + distance,
            _ => (),
        }
    }
    return h * d;
}
pub fn day02_02() -> i64 {
    let filename = "../input/02.txt";
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let (mut h, mut d, mut aim) = (0, 0, 0);
    for line in reader.lines() {
        let line = line.unwrap();
        let parts: Vec<_> = line.split_whitespace().collect();
        let distance = parts[1].parse::<i64>().unwrap();
        match parts[0] {
            "forward" => {
                h = h + distance;
                d = d + distance * aim;
            }
            "up" => aim = aim - distance,
            "down" => aim = aim + distance,
            _ => (),
        }
    }
    return h * d;
}

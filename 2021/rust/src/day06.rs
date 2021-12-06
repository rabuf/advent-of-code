use std::fs::File;
use std::io::{BufRead, BufReader};

fn get_input(filename: &str) -> Vec<usize> {
    let file = File::open(filename).unwrap();
    let line = BufReader::new(file).lines().next().unwrap().unwrap();
    let mut fishes = vec![0;9];
    line.split(",")
        .map(|f| usize::from_str_radix(f,10).unwrap())
        .for_each(|n| fishes[n] = fishes[n] + 1);
    fishes
}

fn simulator (fish: &mut [usize], generations: usize) {
    let mut temp = vec![0;9];
    for _i in 0..generations {
        fish.rotate_left(1);
        fish[6] = fish[6] + fish[0];
        for j in 0..9 {
            temp[j] = fish[(j + 1) % 9];
        }
        temp[6] = temp[6] + fish[0];
        fish.copy_from_slice(&temp);
    }
}

pub fn part1() -> usize {
    let mut fish = get_input("../input/06.txt");
    simulator(&mut fish, 80);
    fish.iter().sum()
}

pub fn part2() -> usize {
    let mut fish = get_input("../input/06.txt");
    simulator(&mut fish, 256);
    fish.iter().sum()
}

#[cfg(test)]
mod test {
    use super::*;
}

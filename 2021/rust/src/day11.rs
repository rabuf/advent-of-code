use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug, Copy, Clone)]
enum Octopus {
    Flashed,
    PoweringUp(u32),
}

type OctopusGrid = Vec<Vec<Octopus>>;

fn get_input(filename: &str) -> OctopusGrid {
    let file = File::open(filename).unwrap();
    let lines = BufReader::new(file).lines();
    let mut octopi = vec![];
    for line in lines {
        let mut row = vec![];
        for c in line.unwrap().chars() {
            row.push(Octopus::PoweringUp(c.to_digit(10).unwrap()));
        }
        octopi.push(row);
    }
    octopi
}

fn flash_dance(octopi: &mut OctopusGrid) -> usize {
    let mut count = 0;
    let mut flashed = vec![];
    // Increment all octopi, possibly marking some as having flashed
    for (i, row) in octopi.iter_mut().enumerate() {
        for (j, octopus) in row.iter_mut().enumerate() {
            if let Octopus::PoweringUp(9) = octopus {
                *octopus = Octopus::Flashed;
                flashed.push((i, j));
                count += 1;
            } else if let Octopus::PoweringUp(n) = octopus {
                *octopus = Octopus::PoweringUp(*n + 1);
            }
        }
    }
    while !flashed.is_empty() {
        let (i, j) = flashed.pop().unwrap();
        for dx in -1..=1 {
            if j == 0 && dx == -1 {
                continue;
            }
            if j == octopi[i].len() - 1 && dx == 1 {
                continue;
            }
            let j = (j as i32 + dx) as usize;
            for dy in -1..=1 {
                if i == 0 && dy == -1 {
                    continue;
                }
                if i == octopi.len() - 1 && dy == 1 {
                    continue;
                }
                let i = (i as i32 + dy) as usize;
                if let Octopus::PoweringUp(9) = octopi[i][j] {
                    octopi[i][j] = Octopus::Flashed;
                    flashed.push((i, j));
                    count += 1;
                }
                if let Octopus::PoweringUp(n) = octopi[i][j] {
                    octopi[i][j] = Octopus::PoweringUp(n + 1);
                }
            }
        }
    }
    // Convert all flashed octopi to PoweringUp(0)
    for row in octopi.iter_mut() {
        for octopus in row.iter_mut() {
            if let Octopus::Flashed = octopus {
                *octopus = Octopus::PoweringUp(0);
            }
        }
    }
    count
}

pub fn part1() -> usize {
    let mut octopi = get_input("../input/11.txt");
    let mut flashes = 0;
    for _ in 0..100 {
        flashes += flash_dance(&mut octopi);
    }
    flashes
}

pub fn part2() -> u64 {
    let mut octopi = get_input("../input/11.txt");
    let octopi_count = octopi.len() * octopi[0].len();
    let mut steps = 1;
    while octopi_count != flash_dance(&mut octopi) {
        steps += 1;
    }
    steps
}
#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn incomplete_score() {}
}

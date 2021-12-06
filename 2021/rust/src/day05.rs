use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

type Point = (i64, i64);
type Segment = (Point, Point);

fn get_input(filename: &str) -> Vec<Segment> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let lines = reader.lines();
    let re = Regex::new(r"(\d+),(\d+) -> (\d+),(\d+)").unwrap();
    let mut segments: Vec<Segment> = vec![];
    for line in lines.map(|l| l.unwrap()) {
        match re.captures(&line) {
            None => (),
            Some(cap) => {
                segments.push((
                    (
                        i64::from_str_radix(&cap[1], 10).unwrap(),
                        i64::from_str_radix(&cap[2], 10).unwrap(),
                    ),
                    (
                        i64::from_str_radix(&cap[3], 10).unwrap(),
                        i64::from_str_radix(&cap[4], 10).unwrap(),
                    ),
                ));
            }
        }
    }
    segments
}

fn make_grid(segments: &[Segment]) -> HashMap<Point, u64> {
    let mut grid: HashMap<Point, u64> = HashMap::new();
    for ((x0, y0), (x1, y1)) in segments {
        let dx = i64::signum(x1 - x0);
        let dy = i64::signum(y1 - y0);
        let distance = i64::max(i64::abs(x1 - x0), i64::abs(y1 - y0));

        for i in 0..=distance {
            let p = (x0 + dx * i, y0 + dy * i);
            match grid.get_mut(&p) {
                None => {
                    grid.insert(p, 1);
                } //.insert(p, 1).unwrap(),
                Some(x) => *x = *x + 1,
            }
        }
    }
    grid
}

fn count_overlapping(grid: HashMap<Point, u64>) -> usize {
    grid.iter().filter(|(_k, v)| **v > 1).count()
}

pub fn part1() -> usize {
    let segments = get_input("../input/05.txt");
    let filtered = segments
        .into_iter()
        .filter(|((x0, y0), (x1, y1))| x0 == x1 || y0 == y1)
        .collect::<Vec<Segment>>();
    let grid = make_grid(&filtered);
    count_overlapping(grid)
}

pub fn part2() -> usize {
    let segments = get_input("../input/05.txt");
    let grid = make_grid(&segments);
    count_overlapping(grid)
}

#[cfg(test)]
mod test {
    use super::*;
}

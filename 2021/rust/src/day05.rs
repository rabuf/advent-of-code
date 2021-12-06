use regex::Regex;
use std::fs::File;
use std::io::{BufRead, BufReader};

type Point = (isize, isize);
type Segment = (Point, Point);

fn get_input(filename: &str) -> (Vec<Segment>, (usize, usize)) {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let lines = reader.lines();
    let re = Regex::new(r"(\d+),(\d+) -> (\d+),(\d+)").unwrap();
    let mut segments: Vec<Segment> = vec![];
    let mut max_x = 0;
    let mut max_y = 0;
    for line in lines.map(|l| l.unwrap()) {
        match re.captures(&line) {
            None => (),
            Some(cap) => {
                let x0 = isize::from_str_radix(&cap[1], 10).unwrap();
                let y0 = isize::from_str_radix(&cap[2], 10).unwrap();
                let x1 = isize::from_str_radix(&cap[3], 10).unwrap();
                let y1 = isize::from_str_radix(&cap[4], 10).unwrap();
                segments.push(((x0,y0),(x1,y1)));
                max_x = max_x.max(x0.max(x1));
                max_y = max_y.max(y0.max(y1));
            }
        }
    }
    (segments, ((max_x + 1) as usize, (max_y + 1) as usize))
}

fn make_grid(segments: &[Segment], x: usize, y: usize) -> Vec<Vec<usize>> {
    let mut grid= vec![vec![0;y];x];

    for ((x0, y0), (x1, y1)) in segments {
        let dx = (x1 - x0).signum();
        let dy = (y1 - y0).signum();
        let distance = (x1 - x0).abs().max((y1 - y0).abs());

        for i in 0..=distance {
            let (x,y) = ((x0 + dx * i) as usize, (y0 + dy * i) as usize);
            grid[x][y] = grid[x][y] + 1;
        }
    }
    grid
}

fn count_overlapping(grid: &Vec<Vec<usize>>) -> isize {
    let mut count = 0;
    for row in grid.iter() {
        for cell in row.iter() {
            if *cell > 1 {
                count = count + 1;
            }
        }
    }
    count
}

pub fn part1() -> isize {
    let (segments, (x, y)) = get_input("../input/05.txt");
    let filtered = segments
        .into_iter()
        .filter(|((x0, y0), (x1, y1))| x0 == x1 || y0 == y1)
        .collect::<Vec<Segment>>();
    let grid = make_grid(&filtered, x, y);
    count_overlapping(&grid)
}

pub fn part2() -> isize {
    let (segments, (x, y)) = get_input("../input/05.txt");
    let grid = make_grid(&segments, x, y);
    count_overlapping(&grid)
}

#[cfg(test)]
mod test {
    use super::*;
}

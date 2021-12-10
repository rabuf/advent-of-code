use itertools::Itertools;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn get_input(filename: &str) -> Vec<Vec<u8>> {
    let file = File::open(filename).unwrap();
    let lines = BufReader::new(file).lines();

    let mut grid: Vec<Vec<u8>> = vec![];

    for line in lines {
        let line = line.unwrap();
        let line = line.as_bytes();
        let mut row = vec![];

        for n in line {
            row.push(n - b'0');
        }

        grid.push(row);
    }

    grid
}

fn get_neighbors(i: usize, j: usize, grid: &[Vec<u8>]) -> Vec<u8> {
    let mut neighbors = vec![];
    if i + 1 < grid.len() {
        neighbors.push(grid[i + 1][j]);
    }
    if 0 < i {
        neighbors.push(grid[i - 1][j]);
    }
    if j + 1 < grid.len() {
        neighbors.push(grid[i][j + 1]);
    }
    if 0 < j {
        neighbors.push(grid[i][j - 1]);
    }
    neighbors
}

fn is_lowpoint(i: usize, j: usize, grid: &[Vec<u8>]) -> bool {
    get_neighbors(i, j, grid).iter().all(|&v| v > grid[i][j])
}

pub fn part1() -> usize {
    let grid = get_input("../input/09.txt");
    let mut lowpoints = 0;
    for i in 0..grid.len() {
        for j in 0..grid[i].len() {
            if is_lowpoint(i, j, &grid) {
                lowpoints += 1 + grid[i][j] as usize;
            }
        }
    }
    lowpoints
}

fn fill_basin(i: usize, j: usize, grid: &mut [Vec<u8>]) -> usize {
    if grid[i][j] == 9 {
        return 0;
    }
    grid[i][j] = 9;
    let mut size = 1;
    if 0 < i {
        size += fill_basin(i - 1, j, grid);
    }
    if i + 1 < grid.len() {
        size += fill_basin(i + 1, j, grid);
    }
    if 0 < j {
        size += fill_basin(i, j - 1, grid);
    }
    if j + 1 < grid[i].len() {
        size += fill_basin(i, j + 1, grid);
    }
    size
}

pub fn part2() -> usize {
    let mut grid = get_input("../input/09.txt");
    let mut basins = vec![];
    for i in 0..grid.len() {
        for j in 0..grid[i].len() {
            if is_lowpoint(i, j, &grid) {
                basins.push(fill_basin(i, j, &mut grid));
            }
        }
    }
    basins.iter().sorted().rev().take(3).product()
}

#[cfg(test)]
mod test {
    use super::*;
}

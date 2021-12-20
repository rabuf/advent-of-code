use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};
use itertools::Itertools;

type Point = (isize, isize);

enum Fold {
    Up(isize),
    Left(isize),
}

fn get_input(filename: &str) -> (HashSet<Point>, Vec<Fold>) {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let mut lines = reader.lines();
    let mut points: HashSet<Point> = HashSet::new();

    loop {
        let line = lines.next().unwrap().unwrap();
        if line.is_empty() { break; }
        let p: Vec<_> = line.split(',').collect();
        let point = (p[0].parse::<isize>().unwrap(), p[1].parse::<isize>().unwrap());
        points.insert(point);
    }
    let mut folds: Vec<Fold> = vec![];
    loop {
        let line = lines.next();
        if line.is_none() { break; }
        let line = line.unwrap().unwrap();
        let split: Vec<_> = line.split_whitespace().collect();
        let rule: Vec<_> = split[2].split('=').collect();
        let at = rule[1].parse().unwrap();
        match rule[0] {
            "x" => folds.push(Fold::Up(at)),
            "y" => folds.push(Fold::Left(at)),
            _ => ()
        }
    }
    (points, folds)
}

impl Fold {
    fn apply(&self, point: Point) -> Option<Point> {
        if let Fold::Up(at) = self {
            if point.0 == *at { return None; }
            if point.0 < *at { return Some(point); }
            return Some((2 * at - point.0, point.1));
        }
        if let Fold::Left(at) = self {
            if point.1 == *at { return None; }
            if point.1 < *at { return Some(point); }
            return Some((point.0, 2 * at - point.1));
        }
        None
    }
}

pub fn part1() -> usize {
    let (points, folds) = get_input("../input/13.txt");
    points.iter().filter_map(|&p| folds[0].apply(p)).unique().count()
}

pub fn part2() -> usize {
    let (mut points, folds) = get_input("../input/13.txt");
    folds.iter().for_each(|f| {
        points = points.iter().filter_map(|&p| f.apply(p)).collect();
    });
    for j in 0..6 {
        for i in 0..39 {
            if points.contains(&(i as isize,j as isize)) {
                print!("#");
            } else {
                print!(" ");
            }
        }
        println!();
    }
    points.len()
}

#[cfg(test)]
mod test {
    use super::*;
}

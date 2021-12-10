use std::fs::File;
use std::io::{BufRead, BufReader, Lines};

fn get_input(filename: &str) -> Lines<BufReader<File>> {
    let file = File::open(filename).unwrap();
    BufReader::new(file).lines()
}

#[derive(Debug)]
enum Status {
    Corrupt(u64),
    Incomplete(u64),
}

fn is_opener(c: char) -> bool {
    matches!(c, '(' | '[' | '{' | '<')
}

fn to_closer(c: char) -> char {
    match c {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        '<' => '>',
        _ => '?',
    }
}

fn corruption_score(c: char) -> u64 {
    match c {
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        _ => 0,
    }
}

fn incomplete_score(stack: &[char]) -> u64 {
    stack.iter().rev().fold(0, |acc, c| {
        acc * 5
            + match c {
                ')' => 1,
                ']' => 2,
                '}' => 3,
                '>' => 4,
                _ => 0,
            }
    })
}

fn process_line(line: &str) -> Status {
    let mut stack = vec![];
    for c in line.chars() {
        if is_opener(c) {
            stack.push(to_closer(c));
        } else if stack.pop().unwrap() != c {
            return Status::Corrupt(corruption_score(c));
        }
    }
    Status::Incomplete(incomplete_score(&stack))
}

pub fn part1() -> u64 {
    let lines = get_input("../input/10.txt");
    lines
        .map(|l| process_line(&l.unwrap()))
        .filter_map(|status| match status {
            Status::Corrupt(score) => Some(score),
            _ => None,
        })
        .sum()
}

pub fn part2() -> u64 {
    let lines = get_input("../input/10.txt");
    let mut scores: Vec<_> = lines
        .map(|l| process_line(&l.unwrap()))
        .filter_map(|status| match status {
            Status::Incomplete(score) => Some(score),
            _ => None,
        })
        .collect();
    let mid = scores.len() / 2;
    *scores.select_nth_unstable(mid).1
}
#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn incomplete_score() {
        let s = "<{([{{}}[<[[[<>{}]]]>[]]";
        let score = process_line(s);
        match score {
            Status::Incomplete(s) => assert_eq!(294, s),
            _ => assert!(false),
        }
    }
}

use std::fs::File;
use std::io::{BufRead, BufReader};

type Card = Vec<Vec<u64>>;

fn get_input(filename: &str) -> (Vec<u64>, Vec<Card>) {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let mut lines = reader.lines();

    let line = lines.next().unwrap().unwrap();
    let numbers = line
        .split(",")
        .map(|s| u64::from_str_radix(s, 10).unwrap())
        .collect();

    let mut cards: Vec<Vec<Vec<u64>>> = vec![];
    'read_cards: loop {
        //let line = lines.next();
        match lines.next() {
            Some(_) => (),
            None => break 'read_cards,
        }
        let mut card: Card = vec![];
        for _i in 0..5 {
            let line = lines.next().unwrap().unwrap();
            let row = line
                .split_whitespace()
                .map(|s| u64::from_str_radix(s, 10).unwrap())
                .collect();
            card.push(row);
        }
        cards.push(card);
    }
    (numbers, cards)
}

fn score_card(c: &Card, n: u64) -> u64 {
    let mut sum = 0;
    for row in c.into_iter() {
        for i in row.into_iter() {
            sum = sum + i;
        }
    }
    return sum * n;
}
fn is_winner(c: Card) -> bool {
    for i in 0..5 {
        let mut won = true;
        for j in 0..5 {
            won = won && c[i][j] == 0;
        }
        if won {
            return true;
        }
        won = true;
        for j in 0..5 {
            won = won && c[j][i] == 0;
        }
        if won {
            return true;
        }
    }
    return false;
}

fn mark_card(c: &mut Card, n: u64) {
    for i in 0..5 {
        for j in 0..5 {
            if c[i][j] == n {
                c[i][j] = 0;
            }
        }
    }
}

pub fn part1() -> u64 {
    let (numbers, mut cards) = get_input("../input/04.txt");
    for n in numbers.iter() {
        for c in cards.iter_mut() {
            mark_card(&mut *c, *n);
            if is_winner(c.to_vec()) {
                return score_card(c, *n);
            }
        }
    }
    0
}

pub fn part2() -> u64 {
    let (numbers, mut cards) = get_input("../input/04.txt");
    let mut score = 0;
    for n in numbers.iter() {
        for c in cards.iter_mut() {
            mark_card(&mut *c, *n);
        }
        let mut i = 0;
        while i < cards.len() {
            if is_winner(cards[i].to_vec()) {
                score = score_card(&cards[i], *n);
                cards.remove(i);
            } else {
                i = i + 1;
            }
        }
    }
    score
}

#[cfg(test)]
mod test {
    use super::*;
}

use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufRead, BufReader};

fn is_small(name: &str) -> bool {
    name.chars().all(|c| c.is_ascii_lowercase())
}

fn get_input(filename: &str) -> HashMap<String, Vec<String>> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let lines = reader.lines();
    let re = Regex::new(r"([a-zA-Z]+)-([a-zA-Z]+)").unwrap();
    let mut cave_network = HashMap::new();
    for line in lines.map(|l| l.unwrap()) {
        match re.captures(&line) {
            None => (),
            Some(cap) => {
                let left = cap[1].to_string();
                let right = cap[2].to_string();
                create_connection(&mut cave_network, &left, &right);
                create_connection(&mut cave_network, &right, &left);
            }
        }
    }
    cave_network
}

fn create_connection(cave_network: &mut HashMap<String, Vec<String>>, from: &str, to: &str) {
    if let Some(c) = cave_network.get_mut(&from.to_string()) {
        c.push(to.to_string());
    } else {
        let v = vec![to.to_string()];
        cave_network.insert(from.to_string(), v);
    }
}

// I'll record a set of small caves visited up to a particular point, this will make it easier
// than my mutation heavy version for CL (where I removed and then re-inserted small caves)



fn recursive_search(cave_network: &HashMap<String, Vec<String>>) -> usize {
    let mut visited = HashSet::new();
    fn traverse(cave_network: &HashMap<String, Vec<String>>, position: String, visited: &mut HashSet<String>) -> usize {
        if position == "end" {
            return 1; // we reached the target
        }
        if is_small(&position) && visited.contains(&position) {
            return 0; // can't revisit
        }
        if is_small(&position) {
            visited.insert(position.clone());
            let mut count = 0;
            for cave in cave_network.get(&position).unwrap() {
                count += traverse(cave_network, cave.clone(), visited);
            }
            visited.remove(&position);
            return count;
        }
        let mut count = 0;
        for cave in cave_network.get(&position).unwrap() {
            count += traverse(cave_network, cave.clone(), visited)
        }
        count
    }
    traverse(cave_network,"start".to_string(), &mut visited)
}

pub fn part1() -> usize {
    let cave_network = get_input("../input/12.txt");
    recursive_search(&cave_network)
}

fn revisit_search(cave_network: &HashMap<String, Vec<String>>) -> usize {
    let mut visited = HashSet::new();
    visited.insert("start".to_string());
    // returns the count of paths + whether the node was revisited
    fn traverse(cave_network: &HashMap<String, Vec<String>>, position: String, visited: &mut HashSet<String>, revisit: &str, did_revisit: bool) -> usize {
        if position == "end" {
            if revisit.is_empty() {
                return 1; // we reached the target
            }
            return if did_revisit {
                1
            } else {
                0
            }
        }
        if is_small(&position) && visited.contains(&position) {
            return 0; // can't revisit
        }
        if revisit.is_empty() && is_small(&position) {
            // Perform two traversals. First, without reinserting this node act as if in part 1
            visited.insert(position.clone());
            let mut count = 0;
            for cave in cave_network.get(&position).unwrap() {
                count += traverse(cave_network, cave.clone(), visited, "", false);
            }
            visited.remove(&position);
            // count only those times it was revisited, to avoid double counting
            for cave in cave_network.get(&position).unwrap() {
                count += traverse(cave_network, cave.clone(), visited, &position, false);
            }
            return count;
        }
        if revisit == position {
            visited.insert(position.clone());
            let mut count = 0;
            for cave in cave_network.get(&position).unwrap() {
                count += traverse(cave_network, cave.clone(), visited, revisit, true);
            }
            visited.remove(&position);
            return count;
        }
        if is_small(&position) {
            visited.insert(position.clone());
            let mut count = 0;
            for cave in cave_network.get(&position).unwrap() {
                count += traverse(cave_network, cave.clone(), visited, revisit, did_revisit);
            }
            visited.remove(&position);
            return count;
        }
        let mut count = 0;
        for cave in cave_network.get(&position).unwrap() {
            count += traverse(cave_network, cave.clone(), visited, revisit, did_revisit);
        }
        count
    }
    // Simplifying `traverse` by handling "start" outside the recursive function
    let mut count = 0;
    for cave in cave_network.get(&"start".to_string()).unwrap() {
        count += traverse(cave_network,cave.clone(), &mut visited, "", false);
    }
    count
}

pub fn part2() -> usize {
    let cave_network = get_input("../input/12.txt");
    revisit_search(&cave_network)
}

#[cfg(test)]
mod test {
    use super::*;
}

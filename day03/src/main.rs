use std::fs;
use std::str::Lines;

fn priority(item: u8) -> usize {
    match item {
        97..=122 => item - 96, // lower-case ascii
        _ => item - 65 + 27,   // assumes uppercase
    }
    .into()
}

fn shared(rucksack: &str) -> u8 {
    let (first, second) = rucksack.as_bytes().split_at(rucksack.len() / 2);
    for item in first {
        if second.contains(item) {
            return *item;
        }
    }
    panic!("Invalid input")
}

fn part1(input: Lines) -> usize {
    input.map(shared).map(priority).sum()
}

fn badge(group: &[&str]) -> u8 {
    *group[0]
        .as_bytes()
        .iter()
        .filter(|item| group[1].as_bytes().contains(item))
        .filter(|item| group[2].as_bytes().contains(item))
        .next()
        .unwrap()
}

fn part2(input: Lines) -> usize {
    input
        .collect::<Vec<_>>() // need to collect into vector to use chunks
        .chunks_exact(3)
        .map(badge)
        .map(priority)
        .sum()
}

fn main() {
    let input = fs::read_to_string("./input.txt").expect("Missing input.txt");

    println!("part1 answer: {:?}", part1(input.lines()));
    println!("part2 answer: {:?}", part2(input.lines()));
}

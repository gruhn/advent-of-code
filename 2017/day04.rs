use itertools::Itertools;
use std::collections::HashSet;
use std::fs;

fn has_no_duplicates(words: &Vec<String>) -> bool {
    let mut seen = HashSet::new();

    for word in words {
        if seen.contains(word) {
            return false;
        }

        seen.insert(word);
    }

    return true;
}

fn sort_chars(word: String) -> String {
    word.chars().sorted().collect::<String>()
}

fn main() {
    let input = fs::read_to_string("input/04.txt").unwrap();
    let lines: Vec<Vec<String>> = input
        .split('\n')
        .map(|line| line.split(' ').map(|word| word.to_string()).collect_vec())
        .collect_vec();

    let part1 = lines
        .iter()
        .filter(|words| has_no_duplicates(words))
        .count();

    println!("Part 1: {part1}");

    let part2 = lines
        .iter()
        .map(|line| {
            line.iter()
                .map(|word| sort_chars(word.to_string()))
                .collect_vec()
        })
        .filter(|words| has_no_duplicates(words))
        .count();

    println!("Part 2: {part2}");
}

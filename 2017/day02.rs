use core::panic;
use std::{
    cmp::{max, min},
    fs,
};

use itertools::Itertools;

fn parse(input: String) -> Vec<Vec<u32>> {
    input
        .split('\n')
        .map(|row| row.split('\t').map(|cell| cell.parse().unwrap()).collect())
        .collect()
}

fn min_max_diff(vec: &Vec<u32>) -> u32 {
    vec.iter().max().unwrap() - vec.iter().min().unwrap()
}

fn divisor_diff(vec: &Vec<u32>) -> u32 {
    for pair in vec.iter().combinations(2) {
        let x = max(pair[0], pair[1]);
        let y = min(pair[0], pair[1]);

        if x % y == 0 {
            return x / y;
        }
    }

    panic!("No evenly divisible values in given row");
}

fn main() {
    let input = fs::read_to_string("input/02.txt").unwrap();
    let rows = parse(input);

    let part1: u32 = rows.iter().map(min_max_diff).sum();
    println!("{:#?}", part1);

    let part2: u32 = rows.iter().map(divisor_diff).sum();
    println!("{:#?}", part2);
}

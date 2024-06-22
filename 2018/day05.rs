use std::fs;

fn react(char1: char, char2: char) -> bool {
    let same_letter = char1.to_lowercase().to_string() == char2.to_lowercase().to_string();
    let different_casing = char1 != char2;
    same_letter && different_casing
}

fn collapse(str: &str) -> String {
    let chars: Vec<char> = str.chars().collect();
    let mut chars_slice: &[char] = &chars[..];
    let mut result: String = "".to_string();

    loop {
        match chars_slice {
            [] => break,
            [char1] => {
                result.push(*char1);
                break;
            }
            [char1, char2, ref rest @ ..] if react(*char1, *char2) => {
                chars_slice = rest;
            }
            [char1, ref rest @ ..] => {
                result.push(*char1);
                chars_slice = rest;
            }
        }
    }

    result
}

fn main() {
    // let input = fs::read_to_string("input/05.txt").unwrap();
    let input = String::from("dabAcCaCBAcCcaDA");
    let result = collapse(&input);
    let len = result.len();
    println!("Part 1: {result} {len}");
}

use std::fs;

fn captcha<F>(vec: &Vec<u32>, partner_of: F) -> u32
where
    F: Fn(usize) -> u32,
{
    vec.iter()
        .enumerate()
        .filter_map(|(i, digit)| {
            if *digit == partner_of(i) {
                Some(digit)
            } else {
                None
            }
        })
        .sum()
}

fn main() {
    let input = fs::read_to_string("input/01.txt").unwrap();
    let digits: Vec<u32> = input.chars().map_while(|char| char.to_digit(10)).collect();

    let part1_partner_of = |i| digits[(i + 1) % digits.len()];
    let part1: u32 = captcha(&digits, part1_partner_of);

    println!("Part 1: {part1}");

    let part2_get_partner = |i| digits[(i + digits.len() / 2) % digits.len()];
    let part2: u32 = captcha(&digits, part2_get_partner);

    println!("Part 2: {part2}");
}

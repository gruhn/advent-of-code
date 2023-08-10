use std::fs;

fn part1(vec: &Vec<i32>) -> u32 {
    let mut index: i32 = 0;
    let mut step_count: u32 = 0;
    let mut vec_mut = vec.clone();

    loop {
        if 0 <= index && index < (vec_mut.len() as i32) {
            let offset = vec_mut[index as usize];

            vec_mut[index as usize] += 1;
            step_count += 1;
            index = index + offset;
        } else {
            break step_count;
        }
    }
}

fn part2(vec: &Vec<i32>) -> u32 {
    let mut index: i32 = 0;
    let mut step_count: u32 = 0;
    let mut vec_mut = vec.clone();

    loop {
        if 0 <= index && index < (vec_mut.len() as i32) {
            let offset = vec_mut[index as usize];

            if offset >= 3 {
                vec_mut[index as usize] -= 1;
            } else {
                vec_mut[index as usize] += 1;
            }

            step_count += 1;
            index = index + offset;
        } else {
            break step_count;
        }
    }
}

fn main() {
    let input: Vec<i32> = fs::read_to_string("input/05.txt")
        .unwrap()
        .split('\n')
        .map(|line| line.parse::<i32>().unwrap())
        .collect();

    println!("Part 1: {:#?}", part1(&input));
    println!("Part 2: {:#?}", part2(&input));
}

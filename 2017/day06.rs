use itertools::iterate;
use std::collections::HashSet;
use std::fs;

fn index_of_max<T: Ord>(vec: &Vec<T>) -> Vec<usize> {
    let max = vec.iter().max();

    vec.iter()
        .enumerate()
        .filter(|(_, val)| Some(*val) == max)
        .map(|(i, _)| i)
        .collect()
}

fn step(mem: &Vec<usize>) -> Vec<usize> {
    let index = index_of_max(mem)[0];
    let max_value = mem[index];

    let get_value = |i: usize| if i == index { 0 } else { mem[i] };

    mem.iter()
        .enumerate()
        .map(|(i, _)| {
            // i relative to `index`, as if `index+1` would be 0.
            let i_rel = ((i as i32) - ((index + 1) as i32)).rem_euclid(mem.len() as i32) as usize;

            if i_rel < (max_value % mem.len()) {
                get_value(i) + max_value / mem.len() + 1
            } else {
                get_value(i) + max_value / mem.len()
            }
        })
        .collect()
}

fn main() {
    let input: Vec<usize> = fs::read_to_string("input/06.txt")
        .unwrap()
        .split('\t')
        .map(|str| str.parse::<usize>().unwrap())
        .collect();

    let mut steps = iterate(input, step);
    let mut seen: HashSet<Vec<usize>> = HashSet::new();
    let mut step_count = 0;

    let first_repeat = loop {
        let mem = steps.next().unwrap();

        if seen.contains(&mem) {
            break mem;
        } else {
            seen.insert(mem);
            step_count += 1;
        }
    };

    println!("Part 1: {:?}", step_count);

    step_count = 0;
    for mem in steps {
        step_count += 1;

        if mem == first_repeat {
            break;
        }
    }

    println!("Part 2: {:?}", step_count);
}

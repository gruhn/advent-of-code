use core::panic;
use std::{collections::HashSet, fs};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Point {
    x: i64,
    y: i64,
}

enum Region {
    Finite { points: HashSet<Point> },
    Infinite
}

fn parse_line(line: &str) -> Point {
    let cols = line.split(", ").collect::<Vec<&str>>();
    let nums = cols
        .into_iter()
        .map(|col| col.parse::<i64>())
        .collect::<Result<Vec<i64>, _>>();

    match nums {
        Err(_) => panic!("non-integer column"),
        Ok(res) => match res[..] {
            [] => panic!("line without data"),
            [_] => panic!("line with only one coodinate"),
            [x, y] => Point { x, y },
            _ => panic!("line with more than two coodinates"),
        },
    }
}

fn parse_file(str: &str) -> Vec<Point> {
    str.lines().map(parse_line).collect()
}

// manhattan distance
fn dist(p1: Point, p2: Point) -> i64 {
    (p1.x - p2.x).abs() + (p1.y - p2.y).abs()
}

fn neighbors(p: &Point) -> Vec<Point> {
    vec![
        Point { x: p.x, y: p.y - 1 },
        Point { x: p.x, y: p.y + 1 },
        Point { x: p.x - 1, y: p.y },
        Point { x: p.x + 1, y: p.y }
    ]
}

fn expand(center: Point, other_centers: &Vec<Point>) -> HashSet<Point> {
    let mut all_points: HashSet<Point> = HashSet::new();
    let mut frontier = HashSet::new();

    frontier.insert(center);
    while !frontier.is_empty() {
        for point in frontier.iter() {
            all_points.insert(*point);
        }

        // frontier.iter().flat_map(neighbors)
    }

    all_points
}

fn region() -> Region {
    Region::Infinite
}

fn main() {
    let input = fs::read_to_string("input/06.txt").unwrap();
    let initial_points = parse_file(&input);

    let regions: Vec<HashSet<Point>> = initial_points
        .into_iter()
        .map(|point| {
            let mut set = HashSet::new();
            set.insert(point);
            set
        })
        .collect();

    println!("Part 1: {:?}", input);

    // for region in regions {}
}

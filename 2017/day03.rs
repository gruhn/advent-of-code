use std::collections::HashMap;

type Point = (i32, i32);

fn manhattan_dist((x, y): Point) -> i32 {
    x.abs() + y.abs()
}

fn index_to_point(index_one_based: i32) -> Point {
    if index_one_based <= 0 {
        panic!("not defined for integers <= 0");
    } else if index_one_based == 1 {
        (0, 0)
    } else {
        // make index zero-based to simplify calculations
        let index = index_one_based - 1;

        // distance from center to the "spiral shell" where `index` is located
        let radius = (((index as f32 + 1.0).sqrt() - 1.0) / 2.0).ceil() as i32;

        let outer_shell_side_length = 2 * radius + 1;
        let inner_shell_side_length = 2 * (radius - 1) + 1;

        // compute the index relativ to just the shell
        let inner_shell_area = inner_shell_side_length.pow(2);
        let index_on_shell = index - inner_shell_area;

        // determinte on which "edge" of the shell the index is located, i.e.
        // left, top, right, or bottom edge. The edges are considered forward
        // shifted, so
        //   * the top-right corner is part of the right edge
        //   * top-left corner is part of the top edge
        //   * the bottom-left corner is part of the left edge and
        //   * the bottom-right corner is part of the bottom edge
        let outer_shell_edge = index_on_shell / (outer_shell_side_length - 1);

        // compute the index relative to just the specific edge of the shell
        let index_on_edge = index_on_shell % (outer_shell_side_length - 1);

        // `index_on_edge` points from the rim inwards, e.g. on the top
        // edge of the shell it points from the right end to the left. But to get
        // a coordinate out of it, we must adjust it so it points from the center
        // outward (either left or right), since the coordinate origin is in the
        // center of the edge.
        let index_on_edge_from_origin = index_on_edge - radius + 1;

        match outer_shell_edge {
            0 => (radius, index_on_edge_from_origin),   // right edge
            1 => (-index_on_edge_from_origin, radius),  // top edge
            2 => (-radius, -index_on_edge_from_origin), // left edge
            3 => (index_on_edge_from_origin, -radius),  // bottom edge
            _ => panic!("undefined"),
        }
    }
}

fn neighbors_of((x, y): Point) -> Vec<Point> {
    vec![
        (x - 1, y + 1),
        (x, y + 1),
        (x + 1, y + 1),
        (x - 1, y),
        (x + 1, y),
        (x - 1, y - 1),
        (x, y - 1),
        (x + 1, y - 1),
    ]
}

fn main() {
    let input = 368078;

    let part1 = manhattan_dist(index_to_point(input));
    println!("Part 1: {part1}");

    let mut value_table: HashMap<Point, i32> = HashMap::new();
    value_table.insert((0, 0), 1);
    let part2 = (2..)
        .map(|index| {
            let point = index_to_point(index);
            let value: i32 = neighbors_of(point)
                .iter()
                .map(|neighbor| value_table.get(neighbor).unwrap_or(&0))
                .sum();

            value_table.insert(point, value);
            value
        })
        .find(|value| *value > input)
        .unwrap();

    println!("Part 2: {part2}");
}

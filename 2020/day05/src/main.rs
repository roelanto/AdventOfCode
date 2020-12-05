use std::io::{self, BufRead};

pub fn parse_file(filename: &str) -> Vec<String> {
    //    let mut v = Vec::new();
    let lines = aoc_lib::read_file(filename);
    let cursor = io::Cursor::new(lines);
    let lines_iter = cursor.lines().map(|l| l.unwrap()).collect::<Vec<String>>();
    return lines_iter;
}

fn decide_substring(input: &str) -> usize {
    let mut a = 0;
    for x in 0..input.len() {
        a = a << 1
            | match input.chars().nth(x).unwrap() {
                'F' => 0,
                'L' => 0,
                _ => 1,
            }
    }

    return a;
}

fn decide_string(input: &String) -> (usize, usize) {
    return (
        decide_substring(&input[0..7]),
        decide_substring(&input[7..10]),
    );
}

fn solve_first_puzzle(filename: &str) -> Vec<usize> {
    let v = parse_file(filename);
    let mut vv = Vec::new();
    for input in v {
        let (a, b) = decide_string(&input);
        vv.push(a * 8 + b);
    }
    vv.sort();
    vv.reverse();
    return vv;
}

fn solve_second_puzzle(vv: Vec<usize>) {
    let vv_filtered = vv
        .iter()
        .filter(|a| !(vv.contains(&(*a - 1)) && vv.contains(&(*a + 1))))
        .collect::<Vec<&usize>>();
    println!("{:?}", vv_filtered[1] - 1);
}

fn main() {
    println!("Sample");
    let filename = "sample.txt";
    solve_first_puzzle(filename);
    let filename = "input_1.txt";
    println!("Puzzle 1");
    let vv = solve_first_puzzle(filename);
    println!("{:?}", vv[0]);
    println!("Puzzle 2");
    solve_second_puzzle(vv);
}

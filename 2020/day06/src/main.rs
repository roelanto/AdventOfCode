use std::collections::HashSet;
use std::io::{self, BufRead};

pub fn parse_file(filename: &str) -> Vec<String> {
    //    let mut v = Vec::new();
    let lines = aoc_lib::read_file(filename);
    let cursor = io::Cursor::new(lines);
    let lines_iter = cursor.lines().map(|l| l.unwrap()).collect::<Vec<String>>();
    return lines_iter;
}

fn solve_first_puzzle(filename: &str) -> Vec<usize> {
    let v = parse_file(filename);
    let mut vv = Vec::new();
    let mut chars = HashSet::new();
    for input in v {
        let isnewline = input.len() == 0;
        match isnewline {
            true => {
                vv.push(chars.len());
                chars = HashSet::new();
            }
            false => {
                for c in input.chars() {
                    chars.insert(c);
                }
            }
        }
    }
    vv.push(chars.len());
    return vv;
}

fn solve_second_puzzle(filename: &str) -> Vec<usize> {
    let v = parse_file(filename);
    let mut vv = Vec::new();
    let mut lineno = 0;
    let mut chars = HashSet::new();
    let mut localchars = HashSet::new();
    for input in v {
        let isnewline = input.len() == 0;
        match isnewline {
            true => {
                lineno = 0;
                vv.push(chars.len());
                chars = HashSet::new();
            }
            false => {
                localchars = HashSet::new();
                for c in input.chars() {
                    localchars.insert(c);
                    if lineno == 0 {
                        chars.insert(c);
                    }
                }
                lineno = lineno + 1;
                chars = chars.intersection(&localchars).copied().collect();
            }
        }
    }
    vv.push(chars.len());
    return vv;
}

fn main() {
    println!("Sample");
    let filename = "sample.txt";
    let vv = solve_first_puzzle(filename);
    println!("{:?}", vv.iter().fold(0, |x, acc| acc + x));

    let filename = "input_1.txt";
    println!("Puzzle 1");
    let vv = solve_first_puzzle(filename);
    println!("{:?}", vv.iter().fold(0, |x, acc| acc + x));

    //    println!("{:?}", vv[0]);
    println!("Puzzle 2");
    //    let filename = "sample.txt";
    let vv = solve_second_puzzle(filename);
    println!("{:?}", vv.iter().fold(0, |x, acc| acc + x));
}

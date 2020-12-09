use regex::Regex;
use std::collections::HashSet;
use std::io::{self, BufRead};

pub fn parse_file(filename: &str) -> Vec<String> {
    //    let mut v = Vec::new();
    let lines = aoc_lib::read_file(filename);
    let cursor = io::Cursor::new(lines);
    let lines_iter = cursor.lines().map(|l| l.unwrap()).collect::<Vec<String>>();
    return lines_iter;
}

pub fn parse_line(st: &str) -> (String, String) {
    let re1 = Regex::new(r"^([^ ]*) (.*)$").unwrap();
    let mut head = "".to_string();
    let mut tail = "".to_string();
    if re1.is_match(&st) {
        head = re1
            .captures(&st)
            .unwrap()
            .get(1)
            .unwrap()
            .as_str()
            .to_string();
        tail = re1
            .captures(&st)
            .unwrap()
            .get(2)
            .unwrap()
            .as_str()
            .to_string();
    }
    return (head, tail);
}

fn read_numbers(filename: &str) -> Vec<usize> {
    let v = parse_file(filename);
    let numbers = v
        .iter()
        .map(|x| x.parse::<usize>().unwrap())
        .collect::<Vec<usize>>();
    return numbers;
}

fn solve_first_puzzle(filename: &str, seglength: usize) -> usize {
    let numbers = read_numbers(filename);
    for i in seglength..numbers.len() {
        let mut sums: HashSet<usize> = HashSet::new();
        for x in i - seglength..i {
            for y in i - seglength..i {
                if x != y {
                    sums.insert(numbers[x] + numbers[y]);
                }
            }
        }
        if !sums.contains(&numbers[i]) {
            return numbers[i];
        }
    }
    return 0;
}

fn solve_second_puzzle(filename: &str, seglength: usize, value: usize) -> usize {
    let mut valid_starts: HashSet<(usize, usize)> = HashSet::new();
    let numbers = read_numbers(filename);
    for i in seglength..numbers.len() {
        if numbers[i] < value {
            valid_starts.insert((i, numbers[i]));
        }
    }
    for (start, _) in valid_starts {
        let mut sums: HashSet<usize> = HashSet::new();
        for i in (0..start).rev() {
            if start - i >= 2 {
                let totalsum = numbers[i..start].iter().fold(0, |acc, x| acc + x);
                if totalsum > value {
                    break;
                }
                sums.insert(totalsum);
                if totalsum == value {
                    let mut finalrange =
                        numbers[i..start].iter().map(|x| *x).collect::<Vec<usize>>();
                    finalrange.sort();
                    return finalrange[0] + finalrange[finalrange.len() - 1];
                }
            }
        }
    }

    return 0;
}

fn main() {
    println!("Sample");
    let filename = "sample.txt";
    let vv = solve_first_puzzle(filename, 5);
    println!("{:?}", vv);

    let filename = "input.txt";
    println!("Puzzle 1");
    let vv = solve_first_puzzle(filename, 25);
    println!("{:?}", vv);

    //    println!("{:?}", vv[0]);
    println!("Puzzle 2");
    //    let filename = "sample.txt";
    let vv = solve_second_puzzle(filename, 0, vv);
    println!("{:?}", vv);
}

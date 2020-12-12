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

fn read_numbers(filename: &str) -> HashSet<usize> {
    let v = parse_file(filename);
    let numbers = v
        .iter()
        .map(|x| x.parse::<usize>().unwrap())
        .collect::<HashSet<usize>>();
    return numbers;
}

fn compute_solution(numbers: &Vec<usize>) -> Vec<usize> {
    let mut result = Vec::new();
    let mut lastval = 0;
    for x in numbers {
        result.push(*x - lastval);
        lastval = *x;
    }
    result.push(3);
    return result;
}

fn solve_first_puzzle(filename: &str) -> (Vec<usize>, usize) {
    let numbers = read_numbers(filename);
    let mut chain_head: Vec<usize> = Vec::new();
    let mut chain_tail: Vec<usize> = Vec::new();
    let maxnum = numbers.iter().max().unwrap().clone();
    //numbers.insert(maxnum + 3);
    chain_head.push(0);
    chain_tail.push(maxnum + 3);
    //  return find_next_step(&numbers, chain_head, chain_tail, 0, diffs, maxnum + 3, true);
    let mut n_vec = numbers.iter().map(|x| *x).collect::<Vec<usize>>();
    n_vec.sort();
    let vv = compute_solution(&n_vec);
    return (vv, 0);
}

fn reduce_to_two(numbers: &Vec<usize>, pos: usize) -> Option<Vec<usize>> {
    let mut res: Vec<usize> = Vec::new();
    for x in 0..pos {
        res.push(numbers[x]);
    }
    //    println!("HEAD: {:?}", res);
    let mut changed = false;
    if pos + 1 < numbers.len() {
        let val = match numbers[pos] + numbers[pos + 1] <= 3 {
            true => {
                changed = true;
                numbers[pos] + numbers[pos + 1]
            }
            false => numbers[pos],
        };
        //        println!("VAL: {:?}", val);
        res.push(val);
    }
    if pos + 2 < numbers.len() {
        for x in pos + 2..numbers.len() {
            res.push(numbers[x]);
        }
    }

    return match changed {
        true => Some(res.clone()),
        false => None,
    };
}

fn iterate(numbers: &Vec<usize>, start: usize) -> HashSet<Vec<usize>> {
    let mut result: HashSet<Vec<usize>> = HashSet::new();
    for x in (start..numbers.len()).rev() {
        match reduce_to_two(&numbers, x) {
            Some(x) => {
                result.insert(x.clone());
            }
            None => {}
        }
    }
    return result;
}

fn solve_second_puzzle(_numbers: &Vec<usize>) -> usize {
    let mut result: HashSet<Vec<usize>> = HashSet::new();
    let mut totals: Vec<usize> = Vec::new();
    result.insert(_numbers.to_vec());
    let mut segment: Vec<usize> = Vec::new();
    for x in _numbers {
        match x {
            1 => segment.push(x.clone()),
            3 => {
                segment.push(x.clone());
                let mut res: HashSet<Vec<usize>> = HashSet::new();
                for z in iterate(&segment, 0) {
                    res.insert(z.clone());
                }

                for y in res.clone() {
                    for z in iterate(&y, 0) {
                        res.insert(z);
                    }
                }

                totals.push(res.len() + 1);
                segment = [].to_vec();
            }
            _ => {}
        }
    }
    return totals.iter().fold(1, |acc, x| acc * x);
}

fn main() {
    println!("Sample");
    let filename = "sample_0.txt";
    let vv = solve_first_puzzle(filename);
    println!(
        "Solution first: {:?} x {:?}",
        vv.0.iter()
            .filter(|x| **x == 1 as usize)
            .fold(0, |acc, _| acc + 1),
        vv.0.iter()
            .filter(|x| **x == 3 as usize)
            .fold(0, |acc, _| acc + 1)
    );
    let vv = solve_second_puzzle(&vv.0);
    println!("Solution second: {:?}", vv);
    let filename = "sample_1.txt";
    let vv = solve_first_puzzle(filename);
    println!(
        "Solution first: {:?} {:?}",
        vv.0.iter()
            .filter(|x| **x == 1 as usize)
            .fold(0, |acc, _| acc + 1),
        vv.0.iter()
            .filter(|x| **x == 3 as usize)
            .fold(0, |acc, _| acc + 1)
    );

    let vv = solve_second_puzzle(&vv.0);
    println!("Solution second: {:?}", vv);
    let filename = "input.txt";
    println!("Puzzle 1");
    let vv = solve_first_puzzle(filename);
    println!(
        "Solution first: {:?} {:?} = {:?}",
        vv.0.iter()
            .filter(|x| **x == 1 as usize)
            .fold(0, |acc, _| acc + 1),
        vv.0.iter()
            .filter(|x| **x == 3 as usize)
            .fold(0, |acc, _| acc + 1),
        vv.0.iter()
            .filter(|x| **x == 1 as usize)
            .fold(0, |acc, _| acc + 1)
            * vv.0
                .iter()
                .filter(|x| **x == 3 as usize)
                .fold(0, |acc, _| acc + 1)
    );
    let vv = solve_second_puzzle(&vv.0);
    println!("Solution second: {:?}", vv);

    //    println!("{:?}", vv[0]);
    //    println!("Puzzle 2");
    //    let filename = "sample.txt";
    //    let vv = solve_second_puzzle(filename, 0, vv);
    //    println!("{:?}", vv);
}

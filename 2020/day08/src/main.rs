use regex::Regex;
use std::collections::HashSet;
use std::io::{self, BufRead};
use std::panic;

pub fn parse_file(filename: &str) -> Vec<String> {
    //    let mut v = Vec::new();
    let lines = aoc_lib::read_file(filename);
    let cursor = io::Cursor::new(lines);
    let lines_iter = cursor.lines().map(|l| l.unwrap()).collect::<Vec<String>>();
    return lines_iter;
}

pub fn parse_line(st: &str) -> (String, bool, i32) {
    let re1 = Regex::new(r"^([^ ]*) (.)(.*)$").unwrap();
    let mut head = "".to_string();
    let mut tail = 0;
    let mut oper = "".to_string();
    if re1.is_match(&st) {
        head = re1
            .captures(&st)
            .unwrap()
            .get(1)
            .unwrap()
            .as_str()
            .to_string();
        oper = re1
            .captures(&st)
            .unwrap()
            .get(2)
            .unwrap()
            .as_str()
            .to_string();
        tail = re1
            .captures(&st)
            .unwrap()
            .get(3)
            .unwrap()
            .as_str()
            .parse::<i32>()
            .unwrap();
    }
    return (head, oper == "+", tail);
}

fn read_instructions(filename: &str) -> Vec<(String, bool, i32)> {
    let v = parse_file(filename);
    let instructions = v
        .iter()
        .map(|x| parse_line(x))
        .collect::<Vec<(String, bool, i32)>>();
    //    println!("{:?} ", instructions);
    return instructions;
}

fn find_solution(
    instructions: &Vec<(String, bool, i32)>,
    _cur_ptr: i32,
    _acc: i32,
    _visited: &HashSet<i32>,
    level: i32,
) -> (i32, i32) {
    let mut cur_ptr: i32 = _cur_ptr;
    let mut acc: i32 = _acc;
    let mut visited: HashSet<i32> = _visited.clone();
    //    println!("find_solution called, cur_ptr = {:?}", _cur_ptr);
    //    println!("find_solution called, instructions = {:?}", instructions);
    loop {
        if cur_ptr >= instructions.len() as i32 {
            println!("END OF PROGRAM, ACC = {:?}", acc);
            break;
        }
        let mut instruction = &instructions[cur_ptr as usize].0;
        let oper = instructions[cur_ptr as usize].1;
        let val = instructions[cur_ptr as usize].2;
        let mut newacc = acc;
        /*        println!(
                    "Current ptr: {:?}: {:?}  acc: {:?}, {:?}",
                    cur_ptr,
                    instructions[cur_ptr as usize],
                    acc,
                    (match instructions[cur_ptr as usize].1 {
                        true => 1,
                        false => -1 as i32,
                    } * (instructions[cur_ptr as usize].2) as i32)
                );
        */
        let prevlen = visited.len();
        visited.insert(cur_ptr);
        cur_ptr = match &instructions[cur_ptr as usize].0[..] {
            "nop" => {
                if level == 0 {
                    let new_cur_ptr = cur_ptr
                        + (match instructions[cur_ptr as usize].1 {
                            true => 1,
                            false => -1 as i32,
                        } * (instructions[cur_ptr as usize].2) as i32)
                            as i32;

                    let mut new_instructions = instructions.clone();
                    new_instructions[cur_ptr as usize].0 = "jmp".to_string();
                    //                    println!("Changed to new instructions {:?}", new_instructions);
                    /*                    println!(
                        "Running with new ptr: {:?} (was: {:?})",
                        new_cur_ptr, cur_ptr
                    );*/
                    let result = panic::catch_unwind(|| {
                        /*                        println!(
                            "{:?}",
                            find_solution(&new_instructions, new_cur_ptr, acc, &visited, 1)
                        );*/
                    });
                    if result.is_err() {
                        //                        println!("NEW INSTRUCTIONS {:?}", new_instructions);
                        //                        println!("ACC: {:?}", acc);
                        //                        println!("ERROR {:?}", result);
                        break;
                    }
                }
                cur_ptr + 1
            }
            "jmp" => {
                if level == 0 {
                    let new_cur_ptr = cur_ptr + 1;
                    let mut new_instructions = instructions.clone();
                    new_instructions[cur_ptr as usize].0 = "nop".to_string();
                    let result = panic::catch_unwind(|| {
                        find_solution(&new_instructions, new_cur_ptr, acc, &visited, 1)
                    });
                    if result.is_err() {
                        //                        println!("NEW INSTRUCTIONS B {:?}", new_instructions);
                        //                      println!("ERROR {:?}", result);
                        //                    println!("ACC: {:?}", acc);
                    }
                    //                println!(
                    //                  "Curptr update {:?} {:?}",
                    //                    cur_ptr
                    //                      + match instructions[cur_ptr as usize].1 {
                    //                        true => 1,
                    //                      false => -1 as i32,
                    //                },
                    //          -1 * (instructions[cur_ptr as usize].2) as i32
                    //    );
                }

                cur_ptr
                    + (match instructions[cur_ptr as usize].1 {
                        true => 1,
                        false => -1 as i32,
                    } * (instructions[cur_ptr as usize].2) as i32) as i32
            }
            "acc" => {
                newacc = newacc
                    + (match instructions[cur_ptr as usize].1 {
                        true => 1,
                        false => -1,
                    } * instructions[cur_ptr as usize].2 as i32) as i32;
                cur_ptr + 1
            }

            _ => cur_ptr,
        };
        if prevlen == visited.len() {
            break;
        }
        acc = newacc;
    }
    return (cur_ptr, acc);
}

fn solve_first_puzzle(filename: &str) -> i32 {
    let visited: HashSet<i32> = HashSet::new();
    let instructions = read_instructions(filename);
    let (_, result) = find_solution(&instructions, 0, 0, &visited, 1);
    return (result);
}

fn solve_second_puzzle(filename: &str) -> i32 {
    let visited: HashSet<i32> = HashSet::new();
    let instructions = read_instructions(filename);
    let (_, result) = find_solution(&instructions, 0, 0, &visited, 0);
    return (result);
}

fn main() {
    println!("Sample");
    let filename = "sample.txt";
    let vv = solve_first_puzzle(filename);
    println!("{:?}", vv);
    if 2 == 1 {
        std::process::exit(0);
    }

    let filename = "input.txt";
    println!("Puzzle 1");
    let vv = solve_first_puzzle(filename);
    println!("{:?}", vv);

    //    println!("{:?}", vv[0]);
    println!("Puzzle 2");
    //    let filename = "sample.txt";
    let vv = solve_second_puzzle(filename);
    println!("{:?}", vv);
}

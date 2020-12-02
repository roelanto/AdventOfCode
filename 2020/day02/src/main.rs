use aoc_lib;

use regex::Regex;
use std::io::{self, BufRead};
fn read_and_parse(st: &str) -> Vec<String> {
    let split = st.split(" ").collect::<Vec<&str>>();
    let splitst = split.iter().map(|x| String::from(*x)).collect();
    return splitst;
}

fn create_regexp(minmax: &str, _character: &str) -> String {
    let split = minmax.split("-").collect::<Vec<&str>>();
    let character = _character.split(":").collect::<Vec<&str>>()[0];
    let min = split[0].parse::<i32>().unwrap();
    let max = split[1].parse::<i32>().unwrap();
    let mut retval: String = format!("{}{}{}", "^[^", character, "]*");
    for _x in 0..min {
        retval = retval + character + "[^" + character + "]*";
    }
    for _x in min..max {
        retval = retval + character + "?[^" + character + "]*";
    }
    retval = retval + "$";
    return retval;
}

fn create_regexp2(minmax: &str, _character: &str) -> String {
    let split = minmax.split("-").collect::<Vec<&str>>();
    let character = _character.split(":").collect::<Vec<&str>>()[0];
    let min = split[0].parse::<i32>().unwrap();
    let max = split[1].parse::<i32>().unwrap();
    let mut retval: String = format!("{}", "^");
    for _x in 0..(min - 1) {
        retval = retval + ".";
    }
    retval = retval + character;
    for _x in min..(max - 1) {
        retval = retval + "."
    }
    retval = retval + "[^" + character + "]";
    retval = retval + ".*$";
    let mut retval2 = format!("{}|", retval);
    let mut retval: String = format!("{}", "^");
    for _x in 0..(min - 1) {
        retval = retval + ".";
    }
    retval = retval + "[^" + character + "]";
    for _x in min..(max - 1) {
        retval = retval + "."
    }
    retval = retval + character;
    retval = retval + ".*$";
    retval2 = format!("{}{}", retval2, retval);
    return retval2;
}

fn check_regexp(regexp: &String, password: &String) -> bool {
    let re = Regex::new(regexp).unwrap();
    re.is_match(password)
}

fn solve_first_puzzle(filename: &str) {
    let content = aoc_lib::read_file(filename);
    let cursor = io::Cursor::new(content);
    let lines_iter = cursor.lines().map(|l| l.unwrap());
    let result = lines_iter
        .map(|x| {
            println!("{:?} ", read_and_parse(&x));
            let regexp = create_regexp(&read_and_parse(&x)[0], &read_and_parse(&x)[1]);
            println!(
                "Matching {:?} with regexp {:?}, result: {:?}",
                &read_and_parse(&x)[2],
                regexp,
                check_regexp(&regexp, &read_and_parse(&x)[2])
            );
            check_regexp(&regexp, &read_and_parse(&x)[2])
        })
        .collect::<Vec<bool>>();
    println!(
        "{:?}",
        result.iter().fold(0, |acc, x| acc
            + match x {
                true => 1,
                false => 0,
            })
    );

    //    let numbers: Vec<i32> = lines_iter.map(|x| x.parse::<i32>().unwrap()).collect();
}

fn solve_second_puzzle(filename: &str) {
    let content = aoc_lib::read_file(filename);
    let cursor = io::Cursor::new(content);
    let lines_iter = cursor.lines().map(|l| l.unwrap());
    let result = lines_iter
        .map(|x| {
            println!("{:?} ", read_and_parse(&x));
            let regexp = create_regexp2(&read_and_parse(&x)[0], &read_and_parse(&x)[1]);
            println!(
                "Matching {:?} with regexp {:?}, result: {:?}",
                &read_and_parse(&x)[2],
                regexp,
                check_regexp(&regexp, &read_and_parse(&x)[2])
            );
            check_regexp(&regexp, &read_and_parse(&x)[2])
        })
        .collect::<Vec<bool>>();
    println!(
        "{:?}",
        result.iter().fold(0, |acc, x| acc
            + match x {
                true => 1,
                false => 0,
            })
    );
}

fn main() {
    let filename = "input_0.txt";
    let filename = "input_1.txt";
    println!("Puzzle 1");
    solve_first_puzzle(filename);
    println!("Puzzle 2");
    solve_second_puzzle(filename);
}

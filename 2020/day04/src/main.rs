use regex::Regex;
use std::env;
use std::fs;
use std::io::{self, BufRead};

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct Passport {
    byr: Option<usize>,
    iyr: Option<usize>,
    eyr: Option<usize>,
    hgt: Option<String>,
    hcl: Option<String>,
    ecl: Option<String>,
    pid: Option<String>,
    cid: Option<usize>,
}

pub fn extract_from_string_vec<'a>(v: &'a Vec<String>, st: &str) -> Option<String> {
    let result = v
        .iter()
        .filter(|&x| x.split(":").collect::<Vec<&str>>()[0] == st)
        .map(|x| match x.len() {
            0 => None,
            _ => Some(x.split(":").collect::<Vec<&str>>()[1]),
        })
        .collect::<Vec<_>>();

    if result.len() == 0 {
        return None;
    }
    match result[0] {
        Some(x) => Some(x.to_string()),
        _ => None,
    }
}

pub fn extract_from_string_vec_i(v: &Vec<String>, st: &str) -> Option<usize> {
    let res = extract_from_string_vec(v, st);
    let x = match res {
        Some(x) => Some(x.parse::<usize>().unwrap()),
        None => None,
    };
    return x;
}

pub fn parse_file(filename: &str) -> Vec<Passport> {
    let mut v = Vec::new();
    let lines = aoc_lib::read_file(filename);
    let cursor = io::Cursor::new(lines);
    let mut lines_iter = cursor.lines().map(|l| l.unwrap()).enumerate();
    let mut vv: Vec<String> = Vec::new();
    for (y, l) in lines_iter {
        let mut stri = String::new();
        let line_iter = l.chars().enumerate();
        let mut str_idx = 0;
        for (x, c) in line_iter {
            //            println!("Char read: {:?} {:?} {:?}", x, c, str_idx);
            match c {
                ' ' => {
                    vv.push(stri);
                    stri = String::new();
                }
                '\n' => {
                    println!("PUSH");
                }
                _ => stri = format!("{}{}", stri, c),
            };
        }
        vv.push(stri);

        if l.chars().collect::<Vec<_>>().len() == 0 {
            v.push(Passport {
                byr: extract_from_string_vec_i(&vv, "byr"),
                iyr: extract_from_string_vec_i(&vv, "iyr"),
                eyr: extract_from_string_vec_i(&vv, "eyr"),
                hgt: extract_from_string_vec(&vv, "hgt"),
                hcl: extract_from_string_vec(&vv, "hcl"),
                ecl: extract_from_string_vec(&vv, "ecl"),
                pid: extract_from_string_vec(&vv, "pid"),
                cid: extract_from_string_vec_i(&vv, "cid"),
            });
            //            println!("Records: {:?}", v);
            vv = Vec::new();
        }

        //        println!("Strings: {:?}", vv);
    }
    v.push(Passport {
        byr: extract_from_string_vec_i(&vv, "byr"),
        iyr: extract_from_string_vec_i(&vv, "iyr"),
        eyr: extract_from_string_vec_i(&vv, "eyr"),
        hgt: extract_from_string_vec(&vv, "hgt"),
        hcl: extract_from_string_vec(&vv, "hcl"),
        ecl: extract_from_string_vec(&vv, "ecl"),
        pid: extract_from_string_vec(&vv, "pid"),
        cid: extract_from_string_vec_i(&vv, "cid"),
    });

    return v;
}

fn validate_byr(st: usize) -> bool {
    st >= 1920 && st <= 2002
}

fn validate_iyr(st: usize) -> bool {
    st >= 2010 && st <= 2020
}

fn validate_eyr(st: usize) -> bool {
    st >= 2020 && st <= 2030
}

fn validate_hgt(st: String) -> bool {
    let re1 = Regex::new(r"(\d(3))cm").unwrap();
    let re2 = Regex::new(r"\d(2)in").unwrap();
    let mut is_valid1 = false;
    let mut is_valid2 = true;
    let numcaptures1 = match re1.captures(&st) {
        Some(x) => x.len(),
        _ => 0,
    };
    let numcaptures2 = match re2.captures(&st) {
        Some(x) => x.len(),
        _ => 0,
    };
    if re1.is_match(&st) {
        let length = re1
            .captures(&st)
            .unwrap()
            .get(1)
            .unwrap()
            .as_str()
            .parse::<i32>();
        is_valid1 = match length {
            Ok(x) => (x >= 150 && x <= 193),
            Err(e) => false,
        }
    } else if re2.is_match(&st) {
        let length = re2
            .captures(&st)
            .unwrap()
            .get(1)
            .unwrap()
            .as_str()
            .parse::<i32>();

        is_valid2 = match length {
            Ok(x) => (x >= 59 && x <= 76),
            Err(e) => false,
        }
    }
    println!("validate hgt: {:?}", is_valid1 | is_valid2);
    return is_valid1 | is_valid2;
}

fn validate_hcl(st: String) -> bool {
    let re1 = Regex::new(r"^#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$").unwrap();
    println!("validate hcl: {:?}", re1.is_match(&st));
    re1.is_match(&st)
}

fn validate_ecl(st: String) -> bool {
    let re1 = Regex::new(r"^amb|blu|brn|gry|grn|hzl|oth$").unwrap();
    re1.is_match(&st)
}

fn validate_pid(st: String) -> bool {
    let re1 = Regex::new(r"^\d{9}$").unwrap();
    re1.is_match(&st)
}

fn validate(pp: &Passport) -> bool {
    println!("Validate: {:?}", pp);
    let result = validate_byr(pp.byr.unwrap())
        & validate_iyr(pp.iyr.unwrap())
        & validate_eyr(pp.eyr.unwrap())
        & validate_hgt(pp.hgt.as_ref().unwrap().to_string())
        & validate_hcl(pp.hcl.as_ref().unwrap().to_string())
        & validate_ecl(pp.ecl.as_ref().unwrap().to_string())
        & validate_pid(pp.pid.as_ref().unwrap().to_string());
    println!("Result: {:?}", result);
    return (result);
}

fn solve_first_puzzle(filename: &str) {
    let v = parse_file(filename);
    println!("Passports: {:?}", v.len());
    println!(
        "{:?} invalid",
        v.iter()
            .filter(|&x| x.byr == None
                || x.iyr == None
                || x.eyr == None
                || x.hgt == None
                || x.hcl == None
                || x.ecl == None
                || x.pid == None)
            .fold(0, |acc, x| acc + 1)
    );
}

fn solve_second_puzzle(filename: &str) {
    let v = parse_file(filename);
    println!("Passports: {:?}", v.len());
    let num_valid = v
        .iter()
        .filter(|&x| {
            !(x.byr == None
                || x.iyr == None
                || x.eyr == None
                || x.hgt == None
                || x.hcl == None
                || x.ecl == None
                || x.pid == None)
        })
        .filter(|&x| validate(x))
        .fold(0, |acc, x| acc + 1);
    println!("Num valid: {:?}", num_valid);
}

fn main() {
    let filename = "sample.txt";
    solve_first_puzzle(filename);
    let filename = "input_1.txt";
    println!("Puzzle 1");
    solve_first_puzzle(filename);
    //    let filename = "sample2.txt";
    println!("Puzzle 2");
    solve_second_puzzle(filename);
}

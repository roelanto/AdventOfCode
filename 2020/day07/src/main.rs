use regex::Regex;
use std::collections::HashSet;
use std::io::{self, BufRead};

pub fn parse_file(filename: &str) -> Vec<String> {
    let lines = aoc_lib::read_file(filename);
    let cursor = io::Cursor::new(lines);
    let lines_iter = cursor.lines().map(|l| l.unwrap()).collect::<Vec<String>>();
    return lines_iter;
}

fn parse_line(st: &str) -> Vec<(String, String, usize)> {
    let re1 = Regex::new(r"^(.*) bags contain (.*)\.$").unwrap();
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

    let tail_units = tail.split(", ").collect::<Vec<&str>>();
    let tl = tail_units
        .iter()
        .map(|x| {
            let re = Regex::new("^(no other|[0-9]+) ?(.*),?.* bags?.*$").unwrap();
            return (
                match re.captures(x).unwrap().get(1).unwrap().as_str() {
                    "no other" => 0,
                    _ => re
                        .captures(x)
                        .unwrap()
                        .get(1)
                        .unwrap()
                        .as_str()
                        .parse::<usize>()
                        .unwrap(),
                },
                re.captures(x).unwrap().get(2).unwrap().as_str().to_string(),
            );
        })
        .collect::<Vec<(usize, String)>>();
    let mut result: Vec<(String, String, usize)> = Vec::new();
    for l in tl {
        result.push((head.clone(), l.1, l.0));
    }

    return result;
}

fn create_table(filename: &str) -> Vec<(String, String, usize)> {
    let v = parse_file(filename);
    let mut table = Vec::new();
    for line in v {
        for pl in parse_line(&line) {
            table.push(pl)
        }
    }
    return table;
}

fn solve_first_puzzle(filename: &str) {
    let table = create_table(filename);
    let mut destinations: HashSet<&String> = HashSet::new();
    let mut rules: Vec<&(String, String, usize)> = table
        .iter()
        .filter(|(_, destination, _)| destination == "shiny gold")
        .collect();
    for s in rules.iter().map(|(x, _, _)| x).collect::<Vec<&String>>() {
        destinations.insert(s);
    }

    for x in table
        .iter()
        .filter(|(_, destination, _)| destinations.contains(&destination))
        .collect::<Vec<&(String, String, usize)>>()
    {
        rules.push(x)
    }
    for s in rules.iter().map(|(x, _, _)| x).collect::<Vec<&String>>() {
        destinations.insert(s);
    }
    let mut old_destinations_ct;
    loop {
        old_destinations_ct = destinations.len();
        for x in table
            .iter()
            .filter(|(_origin, destination, _number)| destinations.contains(&destination))
            .collect::<Vec<&(String, String, usize)>>()
        {
            rules.push(x)
        }
        for s in rules.iter().map(|(x, _, _)| x).collect::<Vec<&String>>() {
            destinations.insert(s);
        }
        if destinations.len() == old_destinations_ct {
            break;
        }
    }
    println!("{:?}", destinations.len());
}

fn solve_second_puzzle(filename: &str) {
    let table = create_table(filename);
    let mut destinations: HashSet<(&String, usize)> = HashSet::new();
    let mut rules: HashSet<&(String, String, usize)> = table
        .iter()
        .filter(|(origin, _destination, _number)| origin == "shiny gold")
        .collect();
    for s in rules
        .iter()
        .map(|(_x, y, z)| (y, *z))
        .collect::<Vec<(&String, usize)>>()
    {
        destinations.insert(s);
    }

    let destinations_strings = destinations
        .iter()
        .map(|(x, _y)| *x)
        .collect::<Vec<&String>>();
    for x in table
        .iter()
        .filter(|(origin, _destination, _number)| destinations_strings.contains(&origin))
        .collect::<Vec<&(String, String, usize)>>()
    {
        rules.insert(x);
    }
    for s in rules
        .iter()
        .map(|(_x, y, z)| (y, *z))
        .collect::<Vec<(&String, usize)>>()
    {
        destinations.insert(s);
    }
    let mut old_destinations_ct: usize;
    loop {
        old_destinations_ct = destinations.len();
        let destinations_strings = destinations
            .iter()
            .map(|(x, _y)| *x)
            .collect::<Vec<&String>>();
        for x in table
            .iter()
            .filter(|(origin, _destination, _number)| destinations_strings.contains(&origin))
            .collect::<Vec<&(String, String, usize)>>()
        {
            rules.insert(x);
        }
        for s in rules
            .iter()
            .map(|(x, _y, z)| (x, *z))
            .collect::<Vec<(&String, usize)>>()
        {
            destinations.insert(s);
        }
        if destinations.len() == old_destinations_ct {
            break;
        }
    }
    println!(
        "{:?}",
        get_number_of_rules(&table, &"shiny gold".to_string(), 0)
    );
}

fn get_number_of_rules(table: &Vec<(String, String, usize)>, st: &String, curval: usize) -> usize {
    let rules: Vec<&(String, String, usize)> = table
        .iter()
        .filter(|(origin, _destination, _number)| origin == st)
        .collect();
    let mut total = curval;
    if rules.len() > 0 {
        for (_origin, destination, number) in rules {
            let multiplier = match &st[..] {
                "shiny gold" => 1,
                _ => curval,
            };
            total = total + get_number_of_rules(table, destination, multiplier * *number);
        }
    }
    return total;
}

fn main() {
    println!("Sample");
    let filename = "sample.txt";
    println!("Puzzle 1");
    solve_first_puzzle(filename);
    println!("Puzzle 2 - sample");
    solve_second_puzzle(filename);
    let filename = "sample_1.txt";
    println!("Puzzle 2 - sample 2");
    solve_second_puzzle(filename);
    let filename = "input.txt";
    println!("Puzzle 2 ");
    solve_second_puzzle(filename);
}

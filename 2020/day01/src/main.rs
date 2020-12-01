use std::io::{self, BufRead};

fn read_and_parse(st: &str) -> Vec<String> {
    let split = st.split(",").collect::<Vec<&str>>();
    let splitst = split.iter().map(|x| String::from(*x)).collect();
    return splitst;
}

fn solve_first_puzzle(filename: &str) {
    let content = day01::read_file(filename);
    let cursor = io::Cursor::new(content);
    let lines_iter = cursor.lines().map(|l| l.unwrap());
    let numbers: Vec<i32> = lines_iter.map(|x| x.parse::<i32>().unwrap()).collect();
    let small_numbers: Vec<i32> = numbers
        .into_iter()
        .filter(|x| *x < 2020 as i32)
        .collect::<Vec<i32>>();
    let true_numbers: Vec<i32> = small_numbers
        .iter()
        .map(|x| {
            let addition_result = small_numbers
                .iter()
                .filter(|y| *y + *x == 2020)
                .take(1)
                .collect::<Vec<_>>();
            if addition_result.len() > 0 {
                if addition_result[0] > x {
                    println!(
                        "Found! {:?} {:?} = {:?}",
                        x,
                        addition_result[0],
                        x * addition_result[0]
                    );
                }
                return *addition_result[0];
            }
            return 0;
        })
        //        .map(|x| small_numbers.iter().map(|y| *y + *x).collect::<Vec<i32>>())
        .collect::<Vec<_>>();
}

fn solve_second_puzzle(filename: &str) {
    let content = day01::read_file(filename);
    let cursor = io::Cursor::new(content);
    let lines_iter = cursor.lines().map(|l| l.unwrap());
    let numbers: Vec<i32> = lines_iter.map(|x| x.parse::<i32>().unwrap()).collect();
    let small_numbers: Vec<i32> = numbers
        .into_iter()
        .filter(|x| *x < 2020 as i32)
        .collect::<Vec<i32>>();
    let true_numbers: Vec<i32> = small_numbers
        .iter()
        .map(|x| {
            small_numbers
                .iter()
                .map(|z| {
                    let addition_result = small_numbers
                        .iter()
                        .filter(|y| *y + *x + *z == 2020)
                        .collect::<Vec<_>>();
                    if x > z {
                        if addition_result.len() > 0 {
                            println!(
                                "Found! {:?} {:?} {:?} = {:?}",
                                x,
                                z,
                                addition_result[0],
                                z * x * addition_result[0]
                            );
                        }
                    }

                    return 1;
                })
                .fold(0, |sum, i| sum + i)
        })
        //        .map(|x| small_numbers.iter().map(|y| *y + *x).collect::<Vec<i32>>())
        .collect::<Vec<_>>();
}

fn main() {
    let filename = "input_0.txt";
    let filename = "input_1.txt";
    println!("Puzzle 1");
    solve_first_puzzle(filename);
    println!("Puzzle 2");
    solve_second_puzzle(filename);
    //    solve_second_puzzle(filename);
    //    println!("Hello, world!");
}

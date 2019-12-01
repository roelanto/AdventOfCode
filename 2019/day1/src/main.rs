use std::env;
use std::io::{self, BufRead};

fn compute_required_fuel(mass:i32) -> i32{
    let result = mass / 3; // divide by 3
    let result = result - 2; // subtract 2
    return result;
}
    
	

fn main() {
    let content = day1::read_file("input.txt");
    let cursor = io::Cursor::new(content);
    let mut lines_iter = cursor.lines().map(|l| l.unwrap());
    let numbers: Vec<i32> = lines_iter.map(|x| compute_required_fuel(x.parse::<i32>().unwrap())).collect();
    println!("Sum of numbers: {}", numbers.iter().sum::<i32>());

}


use std::env;
use std::io::{self, BufRead};
use math::round;

fn compute_required_fuel(mass:i32) -> i32{
    let result = math::round::floor((mass / 3) as f64, 0) ; // divide by 3
    let result = result as i32 - 2; // subtract 2
    return result;
}
    
	
fn compute_required_fuel_b(mass:i32) -> i32{
    let mut new_mass = compute_required_fuel(mass);
    let mut sum_of_mass = 0;
    while new_mass > 0 {
	sum_of_mass = new_mass + sum_of_mass;
	new_mass = compute_required_fuel(new_mass);
    }
    return sum_of_mass;
}
    
fn main() {
    let content = day1::read_file("input.txt");
    let cursor = io::Cursor::new(content);
    let mut lines_iter = cursor.lines().map(|l| l.unwrap());
    let numbers: Vec<i32> = lines_iter.map(|x| compute_required_fuel_b(x.parse::<i32>().unwrap())).collect();
    println!("Sum of mass: {}", numbers.iter().sum::<i32>());

}


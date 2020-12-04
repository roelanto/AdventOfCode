use std::env;
use std::fs;
use std::io::{self, BufRead};

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub struct Point {
    is_visible: Option<bool>,
    x: i32,
    y: i32,
}

fn compute_slope<'a>(reference_point: &Point, point: &'a Point) -> (f32, f32, &'a Point) {
    let slope = (point.y as f32 - reference_point.y as f32) as f32
        / (point.x as f32 - (0.000 + reference_point.x as f32)) as f32;
    let intercept = point.y as f32 - (point.x as f32 * slope);
    /*    println!("Computed slope for reference point {:?},{:?} and point {:?},{:?}: {:?}",
         reference_point.x, reference_point.y,
         point.x, point.y,
    slope);*/
    let distance = (((reference_point.x - point.x) as f32).powf(2.0)
        + ((reference_point.y - point.y) as f32).powf(2.0))
    .sqrt();
    return (slope, distance, point);
}

pub fn parse_file(filename: &str) -> Vec<Point> {
    let mut v = Vec::new();
    let lines = aoc_lib::read_file(filename);
    let cursor = io::Cursor::new(lines);
    let mut lines_iter = cursor.lines().map(|l| l.unwrap()).enumerate();
    for (y, l) in lines_iter {
        let line_iter = l.chars().enumerate();
        for (x, c) in line_iter {
            v.push(Point {
                is_visible: match (c) {
                    '#' => Some(true),
                    '.' => Some(false),
                    _ => None,
                },
                x: x as i32,
                y: y as i32,
            });
            //		println!("Found: {:?},{:?}", x, y);
        }
    }
    return v;
}

fn points_on_slope(begin: &Point, depth: &Point, left: usize, down: usize) -> Vec<Point> {
    let mut curPoint: Point = begin.clone();
    let mut vec: Vec<Point> = Vec::new();
    for x in 0..(depth.y / down as i32) {
        vec.push(Point {
            is_visible: None,
            x: curPoint.x + left as i32,
            y: curPoint.y + down as i32,
        });
        curPoint.x = curPoint.x + left as i32;
        curPoint.y = curPoint.y + down as i32;
    }
    return (vec);
}

fn how_many_trees(filename: &str, left: usize, down: usize) -> usize {
    let mut v = parse_file(filename);
    let width = 11;
    let height = 11;
    let width = 31;
    let height = 323;

    let points: Vec<Point> = points_on_slope(
        &Point {
            x: 0,
            y: 0,
            is_visible: None,
        },
        &Point {
            x: width,
            y: height,
            is_visible: None,
        },
        left,
        down,
    );

    let results = points
        .iter()
        .map(|point| {
            for pb in &v {
                if pb.x == (point.x % width) && pb.y == point.y {
                    return pb.is_visible.unwrap();
                }
            }
            return false;
        })
        .collect::<Vec<bool>>();
    println!("Querying {:?} points", points.len());
    println!("Querying {:?} board points", v.len());
    results.iter().fold(0, |acc, x| {
        acc + match x {
            true => 1,
            false => 0,
        }
    })
}

fn solve_first_puzzle(filename: &str) {
    println!("{:?}", how_many_trees(filename, 3, 1));
}

fn solve_second_puzzle(filename: &str) {
    println!(
        "{:?} {:?} {:?} {:?} {:?} {:?}",
        how_many_trees(filename, 3, 1),
        how_many_trees(filename, 1, 1),
        how_many_trees(filename, 5, 1),
        how_many_trees(filename, 7, 1),
        how_many_trees(filename, 1, 2),
        how_many_trees(filename, 3, 1)
            * how_many_trees(filename, 1, 1)
            * how_many_trees(filename, 5, 1)
            * how_many_trees(filename, 7, 1)
            * how_many_trees(filename, 1, 2),
    );
}

fn main() {
    let filename = "input_1.txt";
    println!("Puzzle 1");
    solve_first_puzzle(filename);
    println!("Puzzle 2");
    solve_second_puzzle(filename);
}

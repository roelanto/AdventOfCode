use std::fs;
use std::env;
use std::io::{self, BufRead};

#[derive(Debug,Ord,PartialOrd,Eq,PartialEq,Clone)]
pub struct Point {
    is_visible: Option<bool>,
    x: i32,
    y: i32,
}

pub fn read_file(filename: &str) -> std::string::String {
    let contents = fs::read_to_string(filename.to_string()).expect("Error reading file");
    return contents;
}

pub fn parse_file(filename: &str) -> Vec<Point> {
    let mut v = Vec::new();
    let lines = read_file(filename);
    let cursor = io::Cursor::new(lines);
    let mut lines_iter = cursor.lines().map(|l| l.unwrap()).enumerate();
    for (y,l) in lines_iter {
	let line_iter = l.chars().enumerate();
	for (x,c) in line_iter {
	    if c == '#' {
		v.push(Point{is_visible: None, x: x as i32, y: y as i32});
//		println!("Found: {:?},{:?}", x, y);
	    }
	}
    }
    return v;
}    

fn compute_slope<'a>(reference_point: &Point, point: &'a Point) -> (f32, f32, &'a Point) {
    let slope = (point.y as f32  - reference_point.y as f32) as f32 / (point.x as f32 - (0.000 + reference_point.x as f32)) as f32 ;
    let intercept = point.y as f32 - (point.x as f32 * slope);
/*    println!("Computed slope for reference point {:?},{:?} and point {:?},{:?}: {:?}",
	     reference_point.x, reference_point.y,
	     point.x, point.y,
    slope);*/
    let distance = (((reference_point.x - point.x) as f32).powf(2.0) + ((reference_point.y - point.y) as f32).powf(2.0)).sqrt();
    return (slope, distance, point);
}


fn compute_number_of_unique_slopes<'a>(reference_point: &Point, points: &'a Vec<Point>) -> Vec<(f32, f32, &'a Point)> {
    let filtered_points:Vec<&Point> = points.iter().filter(|x| x != &reference_point).collect();
    let pos_points =  filtered_points.iter().filter(|x| x.x >= reference_point.x).collect::<Vec<&&Point>>();
    let neg_points =  filtered_points.iter().filter(|x| x.x < reference_point.x).collect::<Vec<&&Point>>();
    let mut pos_unique_slopes : Vec<_> = pos_points.iter().map(|x| compute_slope(&reference_point, &x)).collect();
    let mut neg_unique_slopes : Vec<_> = neg_points.iter().map(|x| compute_slope(&reference_point, &x)).collect();

    pos_unique_slopes.sort_by(|a, b| a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal));
    pos_unique_slopes.dedup_by(|a, b| a.0 == b.0);
    neg_unique_slopes.sort_by(|a, b| a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal));
    neg_unique_slopes.dedup_by(|a, b| a.0 == b.0);
    pos_unique_slopes.append(&mut neg_unique_slopes);
    return pos_unique_slopes;
}

#[derive(Ord,PartialOrd,PartialEq,Eq)]
struct Resultpair <'a>(i32, &'a Point);

fn has_planet(v : &Vec<(f32, f32, &Point)>, p : &Point) -> bool {
    println!("Equality: {:?}", v.iter().filter(|a| a.2 == p).collect::<Vec<_>>().len() );
    return v.iter().filter(|a| a.2 == p).collect::<Vec<_>>().len() > 0;
}

fn main() {
    let mut v = parse_file("input.txt");
    let mut resultVec : Vec<Resultpair> = Vec::new();
    // take any planet
    for reference_planet in v.iter() {
	let result = compute_number_of_unique_slopes(&reference_planet, &v);
	let resultPair = Resultpair(result.len() as i32, &reference_planet);
	resultVec.push(resultPair);
    }
    resultVec.sort();
    resultVec.iter().map(|x| x).inspect(|x| println!("Number of planets: {:?}, {:?},{:?}", x.0, x.1.x, x.1.y)).collect::<Vec<_>>();

    // part two
    let mut numzapped = 0;
    let mut v2 = parse_file("input.txt");
    let mut v3 = v2.clone();
    let reference_point = Point{is_visible: None, x: 13, y: 17};
    let zapped_planets = compute_number_of_unique_slopes(&reference_point, &v2);
    for zapped_planet in zapped_planets.iter() {
	v3.retain(|p| p != zapped_planet.2);
	numzapped = numzapped + 1;
	println!("{:?}: Zapped planet {:?}, v2 now has {:?} planets", numzapped, zapped_planet.2, v3.len());
    }
}

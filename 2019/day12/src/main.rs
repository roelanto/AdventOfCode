use gnuplot::{Figure, Caption, Color,Graph,AxesCommon};
use std::collections::HashSet;
use indexmap::IndexSet;
use num::integer::lcm;

#[derive(Debug,Clone)]
pub struct Moon {
    x: f32,
    y: f32,
    z: f32,
    velx : f32,
    vely : f32,
    velz : f32,
}

#[derive(Debug,Clone)]
pub struct Velocity {
    x: f32,
    y: f32,
    z: f32
}


fn apply_velocity(moon: &Moon) -> Moon {
    Moon{x: moon.x + moon.velx,
	 y: moon.y + moon.vely,
	 z: moon.z + moon.velz,
	 velx: moon.velx,
	 vely: moon.vely,
	 velz: moon.velz,}
}

fn compute_pot(moon: &Moon) -> f32 {
    moon.x.abs() + moon.y.abs() + moon.z.abs()
}

fn compute_kin(moon: &Moon) -> f32 {
    moon.velx.abs() + moon.vely.abs() + moon.velz.abs()
}

fn perform_steps(_moons : Vec<Moon>, num : i32) {
    println!("Step {}:", 0);
    let mut moons = _moons.clone();
    moons.iter().inspect(|a| println!("{:?}", a)).collect::<Vec<_>>();

    for i in 1..num {
//	print!("{}", termion::clear::All);
	moons = moons.iter().map(|m1| {
	    let moon_gravity = moons.iter()
		.filter(|m2| !(m2.x == m1.x && m2.y == m1.y && m2.z == m1.z))
		.map(|m2| {
		    let x_gravity = if m1.x < m2.x {1.0} else if m1.x == m2.x {0.0} else {-1.0};
		    let y_gravity = if m1.y < m2.y {1.0} else if m1.y == m2.y {0.0} else {-1.0};
		    let z_gravity = if m1.z < m2.z {1.0} else if m1.z == m2.z {0.0} else {-1.0};
		    Moon{x:0.0, y:0.0, z:0.0, velx: x_gravity, vely: y_gravity, velz:z_gravity}})
		.fold(Moon{x:m1.x, y:m1.y, z: m1.z, velx:m1.velx, vely:m1.vely, velz:m1.velz},
		      |acc, x| Moon{x: acc.x, y: acc.y, z: acc.z, velx: acc.velx + x.velx, vely: acc.vely + x.vely, velz: acc.velz + x.velz});
	    moon_gravity
	}).collect::<Vec<Moon>>();
	moons = vec![apply_velocity(&moons[0]),
			 apply_velocity(&moons[1]),
			 apply_velocity(&moons[2]),
		     apply_velocity(&moons[3])];
	println!("Step {}: ", i);
	moons.iter().inspect(|a| println!("{:?}", a)).collect::<Vec<_>>();

    	println!("Total: {}", moons.iter().inspect(|a| println!("{:?} * {:?} = {:?}", compute_pot(a),  compute_kin(a), compute_pot(a) * compute_kin(a)))
		 .fold(0.0, |acc, x| acc + compute_pot(x) * compute_kin(x)));
    }
}

fn extract_tuple(moons: &Vec<Moon>, which:i32) -> String {
    let tuple = match which {
	0 => moons.iter().map(|x| (x.x as i32, x.velx as i32)).collect::<Vec<_>>(),
	1 => moons.iter().map(|x| (x.y as i32, x.vely as i32)).collect::<Vec<_>>(),
	2 => moons.iter().map(|x| (x.z as i32, x.velz as i32)).collect::<Vec<_>>(),
	_ => panic!("aargh"),
    };
    let stringrep = format!("{:?}", tuple);
    return stringrep;
}

fn find_repeats(_moons: Vec<Moon>, which:i32) -> (i32, i32) {
    let mut moons = _moons.clone();
    let mut old_len = 0;
    let mut tuplevec = IndexSet::new();
    tuplevec.insert(extract_tuple(&moons, which));
    let mut i = 1;
    let mut resval = (0,0);
    while old_len == 0 || old_len < tuplevec.len() {
	moons = moons.iter().map(|m1| {
	    let moon_gravity = moons.iter()
		.filter(|m2| !(m2.x == m1.x && m2.y == m1.y && m2.z == m1.z))
		.map(|m2| {
		    let x_gravity = if m1.x < m2.x {1.0} else if m1.x == m2.x {0.0} else {-1.0};
		    let y_gravity = if m1.y < m2.y {1.0} else if m1.y == m2.y {0.0} else {-1.0};
		    let z_gravity = if m1.z < m2.z {1.0} else if m1.z == m2.z {0.0} else {-1.0};
		    Moon{x:0.0, y:0.0, z:0.0, velx: x_gravity, vely: y_gravity, velz:z_gravity}})
		.fold(Moon{x:m1.x, y:m1.y, z: m1.z, velx:m1.velx, vely:m1.vely, velz:m1.velz},
		      |acc, x| Moon{x: acc.x, y: acc.y, z: acc.z, velx: acc.velx + x.velx, vely: acc.vely + x.vely, velz: acc.velz + x.velz});
	    moon_gravity
	}).collect::<Vec<Moon>>();
	moons = vec![apply_velocity(&moons[0]),
			 apply_velocity(&moons[1]),
			 apply_velocity(&moons[2]),
		     apply_velocity(&moons[3])];
//	println!("Step {}: ", i);
	let tuple = extract_tuple(&moons, which);
//	println!("{} {:?}", i, tuple);
	old_len = tuplevec.len();
	tuplevec.insert(tuple.clone());
//	println!("Inserted into hashset, len was {} and is now {}", old_len, tuplevec.len());
	if old_len == tuplevec.len() {
//	    println!("FOUND, {:?} repeats between {:?} and {:?}", tuple, i, tuplevec.get_full(&tuple).unwrap().0);
	    resval = (i as i32, tuplevec.get_full(&tuple).unwrap().0 as i32);
	}
//	moons.iter().inspect(|a| println!("{:?}", a)).collect::<Vec<_>>();

//    	println!("Total: {}", moons.iter().inspect(|a| println!("{:?} * {:?} = {:?}", compute_pot(a),  compute_kin(a), compute_pot(a) * compute_kin(a)))
//		 .fold(0.0, |acc, x| acc + compute_pot(x) * compute_kin(x)));
	i = i+1;
    }
    return resval;
}
    

fn main() {
    // sample 1
    let mut moons = vec![Moon{x:-1.0, y:0.0, z:2.0, velx: 0.0, vely: 0.0, velz: 0.0},
		     Moon{x:2.0, y:-10.0, z: -7.0, velx: 0.0, vely: 0.0, velz: 0.0},
		     Moon{x: 4.0, y: -8.0, z: 8.0 , velx: 0.0, vely: 0.0, velz: 0.0},
    		     Moon{x: 3.0, y:5.0, z: -1.0, velx: 0.0, vely: 0.0, velz: 0.0}];
    // sample 2
    let mut moons = vec![Moon{x:-8.0, y:-10.0, z:0.0, velx: 0.0, vely: 0.0, velz: 0.0},
			 Moon{x:5.0, y:5.0, z:10.0, velx: 0.0, vely: 0.0, velz: 0.0},
			 Moon{x:2.0, y:-7.0, z:3.0 , velx: 0.0, vely: 0.0, velz: 0.0},
    			 Moon{x:9.0, y:-8.0, z:-3.0, velx: 0.0, vely: 0.0, velz: 0.0}];
    // true puzzle input
    let mut moons = vec![Moon{x:14.0, y:9.0, z:14.0, velx: 0.0, vely: 0.0, velz: 0.0},
			 Moon{x:9.0, y:11.0, z:6.0, velx: 0.0, vely: 0.0, velz: 0.0},
			 Moon{x:-6.0, y:14.0, z:-4.0 , velx: 0.0, vely: 0.0, velz: 0.0},
    			 Moon{x:4.0, y:-4.0, z:-3.0, velx: 0.0, vely: 0.0, velz: 0.0}];


    //    perform_steps(moons, 1001);
    println!("{:?}", find_repeats(moons.clone(), 0));
    println!("{:?}", find_repeats(moons.clone(), 1));
    println!("{:?}", find_repeats(moons.clone(), 2));

    // correct answer is the lowest common denominator of these numbers

    let num1 = find_repeats(moons.clone(), 0).0 as i64;
    let num2 = find_repeats(moons.clone(), 1).0 as i64;
    let num3 = find_repeats(moons.clone(), 2).0 as i64;
    let lcm1 = lcm(num1, num2);
    let lcm2 = lcm(lcm1, num3);
    println!("GCD: {}", lcm2);
	

    

}


    

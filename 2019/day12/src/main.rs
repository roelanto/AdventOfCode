use gnuplot::{Figure, Caption, Color,Graph,AxesCommon};

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

    //let moons = vec![Moon{x:3.0, y:0.0, z:0.0}, Moon{x:5.0, y:0.0, z:0.0}];

    println!("Step {}:", 0);
    moons.iter().inspect(|a| println!("{:?}", a)).collect::<Vec<_>>();

    for i in 1..1001 {
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

//    	println!("Total: {}", moons.iter().inspect(|a| println!("{:?} * {:?} = {:?}", compute_pot(a),  compute_kin(a), compute_pot(a) * compute_kin(a)))
//	    .fold(0.0, |acc, x| acc + compute_pot(x) * compute_kin(x)));
    }
    let x = [0u32, 1, 2];
    let y = [3u32, 4, 5];
    let mut fg = Figure::new();
//    fg.set_terminal("pngcairo nocrop enhanced font \"verdana,8\"", "/tmp/sample2.png");
//    fg.set_pre_commands("set terminal png nocrop enhanced font \"verdana,8\" size 640,300");
    fg.axes2d()
	.set_title("A plot", &[])
	.set_legend(Graph(0.5), Graph(0.9), &[], &[])
	.set_x_label("x", &[])
	.set_y_label("y^2", &[])
	.lines(&x, &y, &[Caption("A line"), Color("black")])
	.points(&[1,2],&[2,3], &[Caption("Points")])
	;
   // fg.show();
//    fg.save_to_eps("/tmp/sample1.eps", 512, 512);
}

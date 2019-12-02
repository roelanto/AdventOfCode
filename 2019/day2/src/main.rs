fn run_on_vector  (v: &mut Vec<u32>) -> u32 {
    let opcode_add = |x : u32, y: u32| {
	x + y
    };

    let opcode_multiply = |x : u32, y: u32| {
	x * y
    };

    for i in 0..v.len() {
	let opcode_idx = i;
	if opcode_idx % 4 == 0 {
	    let opcode = v[opcode_idx];
	  //  println!("Opcode: {:?} with idx {:?}", opcode, opcode_idx);
	    let opcode_arg1 = v[opcode_idx+1];
	    let opcode_arg1_val = v[opcode_arg1 as usize];
	    let opcode_arg2 = v[opcode_idx+2];
	    let opcode_arg2_val = v[opcode_arg2 as usize];
	    let opcode_arg3 = v[opcode_idx+3];
	    if opcode != 99 {
		let newval = match opcode {
		    1 => opcode_add(opcode_arg1_val as u32, opcode_arg2_val as u32),
		    2 => opcode_multiply(opcode_arg1_val as u32, opcode_arg2_val as u32),
		    _ => 0
		};
		v[opcode_arg3 as usize] = newval;
	//	println!("Opcodes: {:?}", v);
	    } else {
		return v[0];
	    }
	}
    }
    return 0;
}


fn compute_with_initial_values(x:u32, y:u32, orig_v:&Vec<u32>) -> u32 {
    let mut v = orig_v.clone();
    v[1] = x;
    v[2] = y;
    run_on_vector(&mut v)
}
    
fn main() {
    let v = vec![1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,1,13,23,27,1,6,27,31,2,31,13,35,1,9,35,39,2,39,13,43,1,43,10,47,1,47,13,51,2,13,51,55,1,55,9,59,1,59,5,63,1,6,63,67,1,13,67,71,2,71,10,75,1,6,75,79,1,79,10,83,1,5,83,87,2,10,87,91,1,6,91,95,1,9,95,99,1,99,9,103,2,103,10,107,1,5,107,111,1,9,111,115,2,13,115,119,1,119,10,123,1,123,10,127,2,127,10,131,1,5,131,135,1,10,135,139,1,139,2,143,1,6,143,0,99,2,14,0,0];

    println!("First answer: {}", compute_with_initial_values(12, 2, &v));
    
    for x in 0..99 {
	for y in 0..99 {
	    let retval = compute_with_initial_values(x, y, &v);
	    if (retval == 19690720) {
		println!("Second answer: 100 * noun + verb is: 100 * {} + {} = {} ", x,y,100*x+y);
	    }
	}
    }
}

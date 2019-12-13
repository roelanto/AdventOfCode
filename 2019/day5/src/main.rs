fn run_on_vector  (v: &mut Vec<i32>, input: Vec<i32>) -> i32 {
    let opcode_add = |x : i32, y: i32| {
	x + y
    };

    let opcode_multiply = |x : i32, y: i32| {
	x * y
    };

    let opcode_save_int = |x : i32, y:i32| {
    };
    
    let mut input_iter = input.iter();

    let mut i = 0;
    let mut output = Vec::new();
    println!("{:?}", v);
    while i < v.len() {
	let opcode_idx = i;
	let opcode = v[opcode_idx];
	println!("Processing opcode: {:?} at idx {:?}", opcode, opcode_idx);
	if opcode != 99 {
	    match determine_opcode(opcode) {
		1 => {
		    let opcode_arg1 = v[opcode_idx+1];
		    let opcode_arg1_val = match determine_parameter_modes(opcode).0 {
			0 => v[opcode_arg1 as usize],
			_ => opcode_arg1,
		    };
		    let opcode_arg2 = v[opcode_idx+2];
		    let opcode_arg2_val = match determine_parameter_modes(opcode).1 {
			0 => v[opcode_arg2 as usize],
			_ => opcode_arg2,
		    };
		    let opcode_arg3 = v[opcode_idx+3];
		    i = i+4;
		    v[opcode_arg3 as usize] = opcode_add(opcode_arg1_val as i32, opcode_arg2_val as i32)},
		2 => {
		    let opcode_arg1 = v[opcode_idx+1];
		    let opcode_arg1_val = match determine_parameter_modes(opcode).0 {
			0 => v[opcode_arg1 as usize],
			_ => opcode_arg1,
		    };
		    let opcode_arg2 = v[opcode_idx+2];
		    let opcode_arg2_val = match determine_parameter_modes(opcode).1 {
			0 => v[opcode_arg2 as usize],
			_ => opcode_arg2,
		    };
		    let opcode_arg3 = v[opcode_idx+3];
		    i = i+4;
		    v[opcode_arg3 as usize] = opcode_multiply(opcode_arg1_val as i32, opcode_arg2_val as i32)},
		3 => {
		    let opcode_arg1 = v[opcode_idx+1];
		    i = i+2;
		    println!("opcode 3: {}", opcode_arg1);
		    if opcode_arg1 as usize<= i {
			i = i+1;
		    }
		    v.insert(opcode_arg1 as usize, *input_iter.next().unwrap())},
		4 => {
		    let opcode_arg1 = v[opcode_idx+1];
		    i = i+2;
		    println!("opcode 4: {}", opcode_arg1);
		    output.push(v[opcode_arg1 as usize])},
		_ => (),
	    };
	    println!("Finished processing: opcodes vector is now: {:?}", v);
	} else {
	    println!("Encountered opcode 99, exiting");
	    println!("Output vector is now: {:?}", output);
	    return v[0];
	}
    }
    return 0;
}

fn determine_opcode(parameter:i32) -> i32 {
    parameter % 100
}

fn determine_parameter_modes(parameter:i32) -> (i32, i32, i32) {
    (
	(parameter % 1000) / 100,
	(parameter % 10000) / 1000 ,
	(parameter % 100000) / 10000 )

}

fn compute_with_initial_values(orig_v:&Vec<i32>) -> i32 {
    let mut v = orig_v.clone();
    let input = vec![2,3,4,5];
    run_on_vector(&mut v, input)
}
    
fn main() {
    let v = vec![3,5,3,10,99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0];
    let v = vec![3,0,4,0,99];
    let v = vec![1002,4,3,4,33];
    let v = vec![3,225,1,225,6,6,1100,1,238,225,104,0,1102,16,13,225,1001,88,68,224,101,-114,224,224,4,224,1002,223,8,223,1001,224,2,224,1,223,224,223,1101,8,76,224,101,-84,224,224,4,224,102,8,223,223,101,1,224,224,1,224,223,223,1101,63,58,225,1102,14,56,224,101,-784,224,224,4,224,102,8,223,223,101,4,224,224,1,223,224,223,1101,29,46,225,102,60,187,224,101,-2340,224,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1102,60,53,225,1101,50,52,225,2,14,218,224,101,-975,224,224,4,224,102,8,223,223,1001,224,3,224,1,223,224,223,1002,213,79,224,101,-2291,224,224,4,224,102,8,223,223,1001,224,2,224,1,223,224,223,1,114,117,224,101,-103,224,224,4,224,1002,223,8,223,101,4,224,224,1,224,223,223,1101,39,47,225,101,71,61,224,101,-134,224,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,1102,29,13,225,1102,88,75,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1107,677,677,224,102,2,223,223,1006,224,329,1001,223,1,223,108,677,677,224,1002,223,2,223,1005,224,344,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,359,1001,223,1,223,1107,226,677,224,102,2,223,223,1006,224,374,1001,223,1,223,8,677,226,224,102,2,223,223,1006,224,389,101,1,223,223,8,226,226,224,102,2,223,223,1006,224,404,101,1,223,223,7,677,677,224,1002,223,2,223,1006,224,419,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,434,101,1,223,223,1108,677,226,224,1002,223,2,223,1006,224,449,1001,223,1,223,108,677,226,224,1002,223,2,223,1006,224,464,101,1,223,223,1108,226,677,224,1002,223,2,223,1006,224,479,101,1,223,223,1007,677,677,224,1002,223,2,223,1006,224,494,1001,223,1,223,107,226,226,224,102,2,223,223,1005,224,509,1001,223,1,223,1008,677,226,224,102,2,223,223,1005,224,524,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,539,101,1,223,223,1108,677,677,224,102,2,223,223,1005,224,554,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,569,101,1,223,223,1107,677,226,224,1002,223,2,223,1006,224,584,1001,223,1,223,7,226,677,224,102,2,223,223,1005,224,599,101,1,223,223,108,226,226,224,1002,223,2,223,1005,224,614,101,1,223,223,107,226,677,224,1002,223,2,223,1005,224,629,1001,223,1,223,107,677,677,224,1002,223,2,223,1006,224,644,101,1,223,223,1007,677,226,224,1002,223,2,223,1006,224,659,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,674,1001,223,1,223,4,223,99,226];

    println!("First answer: {}", compute_with_initial_values(&v));

    println!("{}", determine_opcode(12));
    println!("{}", determine_opcode(115));
    println!("{}", determine_opcode(12332));

    println!("{:?}", determine_parameter_modes(12));
    println!("{:?}", determine_parameter_modes(115));
    println!("{:?}", determine_parameter_modes(12332));

}

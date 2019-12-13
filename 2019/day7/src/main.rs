use std::collections::HashSet;

fn yield_argument (v : &Vec<i32>, idx: usize, mode : i32) -> i32 {
    let arg = v[idx];
    let val = match mode {
	0 => v[arg as usize],
	_ => arg,
    };
    val
}

fn yield_arguments_2 (v : &Vec<i32>, idx: usize, opcode : i32) -> (i32, i32) {
    let modes = determine_parameter_modes(opcode);
    (	yield_argument(v, idx+1, 1),
	yield_argument(v, idx+2, 1))
}

fn yield_arguments_3 (v : &Vec<i32>, idx: usize, opcode : i32) -> (i32, i32, i32) {
    let modes = determine_parameter_modes(opcode);
    (	yield_argument(v, idx+1, modes.0),
	yield_argument(v, idx+2, modes.1),
	yield_argument(v, idx+3, 1))
}

fn run_on_vector  (mut v: Vec<i32>, input: Vec<i32>, pointer:usize ) -> (Vec<i32>, Vec<i32>, bool, usize) {
    let opcode_add = |x : i32, y: i32| {
	x + y
    };

    let opcode_multiply = |x : i32, y: i32| {
	x * y
    };

    let opcode_save_int = |x : i32, y:i32| {
    };
    
    let mut input_iter = input.iter();

    let mut i = pointer;
    let mut output = Vec::new();
//    println!("{:?}", v);
    while i < v.len() {
	let opcode_idx = i;
	let opcode = v[opcode_idx];
//	println!("Processing opcode: {:?} at idx {:?}", opcode, opcode_idx);
	if opcode != 99 {
	    match determine_opcode(opcode) {
		1 => {
		    let args = yield_arguments_3(&v, i, opcode);
		    i = i+4;
		    v[args.2 as usize] = args.0 + args.1
		},
		2 => {
		    let args = yield_arguments_3(&v, i, opcode);
		    i = i+4;
		    v[args.2 as usize] = opcode_multiply(args.0, args.1)
		},
		3 => {
		    print!("opcode 3: READ ");
		    let args = yield_arguments_2(&v, i, opcode);
		    i = i+2;
		    let next_input = match input_iter.next() {
			Some(inner) => inner,
			None => {println!("-- detected empty buffer, returning"); return (output, v, false, i)},
		    };
//		    println!("value {} into position {}", &next_input, args.0);
		    v[args.0 as usize] = *next_input
		    },
		4 => {
		    let opcode_arg1 = v[opcode_idx+1];
		    let opcode_arg1_val = match determine_parameter_modes(opcode).0 {
			0 => v[opcode_arg1 as usize],
			_ => opcode_arg1,
		    };
		    i = i+2;
		    println!("Opcode 4: WRITE value {}", opcode_arg1_val);
		    output.push(opcode_arg1_val as i32);
		    return (output, v, false, i);
		},
		5 => {
		    println!("Opcode 5");

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
		    println!("Opcodearg2 = {}, interpretation is {} so resulting value is {}", opcode_arg2, determine_parameter_modes(opcode).1, opcode_arg2_val);
		    println!("jmp if true: opcode_arg1 = {} != 0, then jump to {} ", opcode_arg1_val, opcode_arg2_val);
		    if opcode_arg1_val > 0 {
			i = opcode_arg2_val as usize;
			println!("Jumping to {}", i);
		    } else {
			i = i+3;
		    }},
		6 => {
		    println!("Opcode 6");
		    
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
//		    println!("jmp if equal: opcode_arg1 = {}, val = {}", opcode_arg1, opcode_arg1_val);
		    if opcode_arg1_val == 0 {
			i = opcode_arg2_val as usize;
//			println!("Set index to {}, opcode_arg1_val is {}", i, opcode_arg1_val);
		    } else {
//			println!("Moving index from {} to {}", i, i+3);
			i = i+3;
		    }},
		7 => {
		    println!("Opcode 7");
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
		    let opcode_arg3_val = match determine_parameter_modes(opcode).2 {
			0 => v[opcode_arg3 as usize],
			_ => opcode_arg3,
		    };
		    i = i+4;
		    if opcode_arg1_val < opcode_arg2_val {
			v[opcode_arg3 as usize] = 1;
		    } else {
			v[opcode_arg3 as usize] = 0;
		    }
		    },
		8 => {
		    println!("Opcode 8");

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
		    let opcode_arg3_val = match determine_parameter_modes(opcode).2 {
			0 => v[opcode_arg3 as usize],
			_ => opcode_arg3,
		    };
		    i = i+4;
//		    println!("equals: if {:?} == {:?} write 1 to {:}", opcode_arg1_val, opcode_arg2_val, opcode_arg3);
		    if opcode_arg1_val == opcode_arg2_val {
			v[opcode_arg3 as usize] = 1;
		    } else {
			v[opcode_arg3 as usize] = 0;
		    }
		},
		    
		_ => (),
	    };
//	    println!("Finished processing: opcodes vector is now: {:?}", v);
	} else {
	    println!("Encountered opcode 99, exiting");
	    println!("Output vector is now: {:?}", output);
	    return (output, v, true, i);
	}
    }
    return (output, v, true, 0);
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

fn compute_with_initial_values(orig_v:&Vec<i32>, initial_value: Vec<i32>) -> Vec<i32> {
    println!("Compute with initial values: {:?}", initial_value);
    let mut v = orig_v.clone();
    let input = initial_value;
    run_on_vector(v, input, 0).0
}

fn compute_with_initial_values_fb(mut orig_v:Vec<i32>, initial_value: Vec<i32>, pointer:usize) -> (Vec<i32>, Vec<i32>, bool, usize) {
    println!("Start computation, input buffer is: {:?}", initial_value);
    let input = initial_value;
    let output = run_on_vector(orig_v, input, pointer);
    output
}

fn compute_for_all_thrusters(v:&Vec<i32>, inputs:(i32,i32,i32,i32,i32)) -> Vec<i32> {
    println!("Run with phases: {:?}", inputs);
    let output = compute_with_initial_values(&v, vec![inputs.0,0]);
    let output = compute_with_initial_values(&v, vec![inputs.1,output[0]]);
    let output = compute_with_initial_values(&v, vec![inputs.2,output[0]]);
    let output = compute_with_initial_values(&v, vec![inputs.3,output[0]]);
    let output = compute_with_initial_values(&v, vec![inputs.4,output[0]]);
    return output;
}

fn do_first_exercise () {
        let v = vec![3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0];
    let output = compute_for_all_thrusters(&v, (4,3,2,1,0));
    assert_eq!(output[0], 43210);
    println!("Final output: {:?}", output);

    let v = vec![3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0];
    let output = compute_for_all_thrusters(&v, (0,1,2,3,4));
    assert_eq!(output[0], 54321);
    println!("Final output: {:?}", output);

    let v = vec![3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0];
    let output = compute_for_all_thrusters(&v, (1,0,4,3,2));
    assert_eq!(output[0], 65210);

    println!("Final output: {:?}", output);

    let v = vec![3,8,1001,8,10,8,105,1,0,0,21,42,67,88,101,114,195,276,357,438,99999,3,9,101,3,9,9,1002,9,4,9,1001,9,5,9,102,4,9,9,4,9,99,3,9,1001,9,3,9,1002,9,2,9,101,2,9,9,102,2,9,9,1001,9,5,9,4,9,99,3,9,102,4,9,9,1001,9,3,9,102,4,9,9,101,4,9,9,4,9,99,3,9,101,2,9,9,1002,9,3,9,4,9,99,3,9,101,4,9,9,1002,9,5,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,99];

    println!("\n\n\n");
    println!("Initial: {:?}", compute_for_all_thrusters(&v, (0,1,2,3,4)));


//    compute_with_initial_values(&v, vec![5, 14118]);
    let mut result = Vec::new();
    let mut highest_val = 0;
    for a in 0..5 {
	for b in 0..5 {
	    for c in 0..5 {
		for d in 0..5 {
		    for e in 0..5 {
			let mut h = HashSet::new();
			h.insert(a);
			h.insert(b);
			h.insert(c);
			h.insert(d);
			h.insert(e);
			if (h.len() == 5) {
			    let val = compute_for_all_thrusters(&v, (a,b,c,d,e))[0];
			    if (val > highest_val) {
				highest_val = val;
			    }
			    result.push(val);
			    println!("\n**************\n{} {} {} {} {} {:?}\n*******\n\n", a, b, c, d, e, compute_for_all_thrusters(&v, (a,b,c,d,e)));
			}
		    }
		}
	    }
	}
    }
    println!("Highest val: {:?}", highest_val);
}    

fn run_feedback_loop(v: Vec<i32>, inputs:(i32,i32,i32,i32,i32)) -> i32 {
    let mut v_a = v.clone();
    let mut v_b = v.clone();
    let mut v_c = v.clone();
    let mut v_d = v.clone();
    let mut v_e = v.clone();
    let mut ptr_a = 0;
    let mut ptr_b = 0;
    let mut ptr_c = 0;
    let mut ptr_d = 0;
    let mut ptr_e = 0;
    //    let output = Vec::new();
    let mut output = vec![0];
    let mut finished = false;
    println!("Phases: {:?}", inputs);
    println!("Before A: {:?}", &v_a);
    let (_output, _v_a, _finished, _ptr_a) = compute_with_initial_values_fb(v_a, vec![inputs.0,output[0]], ptr_a);
    output = _output;
    v_a = _v_a;
    finished = _finished;
    ptr_a = _ptr_a;

    println!("After A: {:?}", &v_a);
    println!("After run of engine A output is {:?} and v is {:?}, ptr is {}\n\n", output, v_a, ptr_a);
    let (_output, _v_b, _finished, _ptr_b) = compute_with_initial_values_fb(v_b, vec![inputs.1,output[0]], ptr_b);
    output = _output;
    v_b = _v_b;
    finished = _finished;
    ptr_b = _ptr_b;

    println!("After run of engine B output is {:?} and v is {:?}\n\n", output, v_b);
    let (_output, _v_c, _finished, _ptr_c) = compute_with_initial_values_fb(v_c, vec![inputs.2,output[0]], ptr_c);
    output = _output;
    v_c = _v_c;
    finished = _finished;
    ptr_c = _ptr_c;

    println!("After run of engine C output is {:?} and v is {:?}\n\n", output, v_c);
    let (_output, _v_d, _finished, _ptr_d) = compute_with_initial_values_fb(v_d, vec![inputs.3,output[0]], ptr_d);
    output = _output;
    v_d = _v_d;
    finished = _finished;
    ptr_d = _ptr_d;

    println!("After run of engine D output is {:?} and v is {:?}\n\n", output, v_d);
    let (_output, _v_e, _finished, _ptr_e) = compute_with_initial_values_fb(v_e, vec![inputs.4,output[0]], ptr_e);
    output = _output;
    v_e = _v_e;
    finished = _finished;
    ptr_e = _ptr_e;

    println!("After run of engine E output is {:?} and v is {:?}\n\n", output, v_e);

    let mut last_known_output = 0;
    while finished != true {
	println!("While-loop for {:?}, finished = {}", inputs, finished);
	let mut input = Vec::new();
	if output.len() > 0 { input.push(output[0])};
	println!("Starting A with ptr {}", ptr_a);
	let (_output, _v_a, _finished, _ptr_a) = compute_with_initial_values_fb(v_a, input, ptr_a);
	output = _output;
	v_a = _v_a;
	finished = _finished;
	ptr_a = _ptr_a;
	println!("After run of engine A output is {:?} and v is {:?}, ptr is {}\n\n", output, v_a, ptr_a);

	
	let mut input = Vec::new();
	if output.len() > 0 { input.push(output[0])};
	let (_output, _v_b, _finished, _ptr_b) = compute_with_initial_values_fb(v_b, input, ptr_b);
	output = _output;
	v_b = _v_b;
	finished = _finished;
	ptr_b = _ptr_b;
	println!("After run of engine B output is {:?} and v is {:?}\n\n", output, v_b);
	let mut input = Vec::new();
	if output.len() > 0 { input.push(output[0])};
	let (_output, _v_c, _finished, _ptr_c) = compute_with_initial_values_fb(v_c, input, ptr_c);
	output = _output;
	v_c = _v_c;
	finished = _finished;
	ptr_c = _ptr_c;

	println!("After run of engine C output is {:?} and v is {:?}\n\n", output, v_c);
	let mut input = Vec::new();
	if output.len() > 0 { input.push(output[0])};
	let (_output, _v_d, _finished, _ptr_d) = compute_with_initial_values_fb(v_d, input, ptr_d);
	output = _output;
	v_d = _v_d;
	finished = _finished;
	ptr_d = _ptr_d;

	println!("After run of engine D output is {:?} and v is {:?}\n\n", output, v_d);
	let mut input = Vec::new();
	if output.len() > 0 { input.push(output[0])};
	let (_output, _v_e, _finished, _ptr_e) = compute_with_initial_values_fb(v_e, input, ptr_e);
	output = _output;
	v_e = _v_e;
	finished = _finished;
	ptr_e = _ptr_e;

	if output.len() > 0 {last_known_output = output[0]};
	println!("After run of engine E output is {:?} and v is {:?}, finished is {}\n\n", output, v_e, finished);
    }
    return last_known_output;
}    

fn main() {
    //    do_first_exercise();
    let v = vec![3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5];
    let inputs = (9,8,7,6,5);

  //  let output = compute_for_all_thrusters(&v, (9,8,7,6,5));
    assert_eq!(run_feedback_loop(v, inputs), 139629729);

    let v = vec![3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10];

    let inputs = (9,7,8,5,6);

  //  let output = compute_for_all_thrusters(&v, (9,8,7,6,5));
    assert_eq!(run_feedback_loop(v, inputs), 18216);
    println!("All tests OK");


    let v = vec![3,8,1001,8,10,8,105,1,0,0,21,42,67,88,101,114,195,276,357,438,99999,3,9,101,3,9,9,1002,9,4,9,1001,9,5,9,102,4,9,9,4,9,99,3,9,1001,9,3,9,1002,9,2,9,101,2,9,9,102,2,9,9,1001,9,5,9,4,9,99,3,9,102,4,9,9,1001,9,3,9,102,4,9,9,101,4,9,9,4,9,99,3,9,101,2,9,9,1002,9,3,9,4,9,99,3,9,101,4,9,9,1002,9,5,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,99];
    let mut result = Vec::new();
    let mut highest_val = 0;
    for a in 5..10 {
	for b in 5..10 {
	    for c in 5..10 {
		for d in 5..10 {
		    for e in 5..10 {
			let mut h = HashSet::new();
			h.insert(a);
			h.insert(b);
			h.insert(c);
			h.insert(d);
			h.insert(e);
			if (h.len() == 5) {
			    let val = run_feedback_loop(v.clone(), (a,b,c,d,e));
			    if (val > highest_val) {
				highest_val = val;
			    }
			    result.push(val);
			    println!("\n**************\n{} {} {} {} {} {:?}\n*******\n\n", a, b, c, d, e, compute_for_all_thrusters(&v, (a,b,c,d,e)));
			}
		    }
		}
	    }
	}
    }
    println!("Highest val: {:?}", highest_val);
    
}

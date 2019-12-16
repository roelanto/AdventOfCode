use std::collections::HashMap;
use std::cmp::min;
use std::cmp::max;
use std::fs;
use math::round::ceil;



fn read_file(filename: &str) -> std::string::String {
    let contents = fs::read_to_string(filename.to_string()).expect("Error reading file");
    return contents;
}

fn parse_rule_line(s: &str) -> (String, String) {
    let split = s.split(" => ");
    let vec:Vec<String> = split.map(|s| String::from(s)).collect();
    println!("Split: {:?}, {}", vec[1], vec.len());
    return (vec[0].clone(), vec[1].clone());
}


fn unit(s: &str) -> String {
//    println!("Finding unit of {}", s);
    let st = s.split(" ").collect::<Vec<_>>();
    st[1].to_string()
}

fn numunits(s: &str) -> usize {
    let st = s.split(" ").collect::<Vec<_>>();
    st[0].parse().unwrap()
}

fn rule_for_unit(rules: &Vec<(String, String)>, symbol: &str) -> (String, String) {
//    println!("Rule_for_unit {:?} symbol: {:?}", rules, symbol);
    let rule = rules.iter().filter(|a| unit(&a.1) == symbol).collect::<Vec<_>>()[0];
    rule.clone()
}

fn number_symbols_in_map(hm: &HashMap<String, i64>, symbol: &str) -> i64 {
    match hm.get(symbol) {
	None => 0,
	Some(x) => *x,
    }
}

fn total_non_ore_non_fuel_in_map(hm: &HashMap<String, i64>) -> i64 {
    hm.iter().filter(|a| *a.0 != "ORE").filter(|a| *a.0 != "FUEL").fold(0, |a, x| a + x.1)
}
fn next_symbol(hm: &HashMap<String, i64>) -> String {
    return match hm.iter().filter(|a| *a.1 > 0 ).filter(|a| *a.0 != "ORE").next() {
	None => "None".to_string(),
	Some(x) => x.0.to_string(),
    }
}


fn increase_alt_with<'b>(hm: &mut HashMap<String, i64>, symbol: String, withnum: i64) {
    let num = hm.entry(symbol.clone()).or_insert(0);
    *num += withnum;
//    hm
}


fn process_next_rule<'a>(rules: &Vec<(String, String)>, mut needed_symbols : HashMap<String, i64>, mut produced_symbols: HashMap<String, i64>) -> (HashMap<String, i64>, HashMap<String, i64>) {
    //    println!("# of symbols on the stack: {}, i: {}", symbols.len(), i);
    //    println!("\n\nNEXT RULE\n");
    //    println!("Needed symbols: {:?}\nProduced symbols: {:?}", needed_symbols, produced_symbols);
    if next_symbol(&needed_symbols) != "None" {
	let symbol = next_symbol(&needed_symbols);
	let numneeded = number_symbols_in_map(&needed_symbols, &symbol);
	//	println!("Symbol: {} * {}", symbol, numneeded);
	let rule = rule_for_unit(&rules, &symbol);
	//	println!("applying rule {:?}", rule);
	let multiplier = math::round::ceil(numneeded as f64 / numunits(&rule.1) as f64, 0) as i64;
	let lhs = rule.0;
	let slist = &lhs.split(", ").map(|a| (unit(a), numunits(a))).collect::<Vec<_>>();
	increase_alt_with(&mut produced_symbols, unit(&rule.1), multiplier * numunits(&rule.1) as i64);
	
	for s in slist {
	    //		    println!("  added needed symbol: {:?}", s.0.to_string());
	    increase_alt_with(&mut needed_symbols, s.0.to_string(), s.1 as i64 * multiplier);
	}
	//	println!("    .. before weeding: needed:   {:?} {:?}", new_needed_symbols.len(), new_needed_symbols);
	//	println!("    .. before weeding: produced: {:?} {:?}", produced_symbols.len(), produced_symbols);
	
	let psclone = produced_symbols.clone();
	for p in &psclone {
	    let n_num = needed_symbols.get(p.0).unwrap();
	    //	    println!("We need {} of just produced {}", n_num, p.0);
	    let num_corr = max(min(n_num.clone(), *p.1), 0);
	    //	    println!("We have {} of produced {}", n_num, p.0);
	    //	    println!("Correcting with {}", num_corr);
	    
	    // if there are more produced than needed: decrease both needed and produced with min (needed, produced).
	    *needed_symbols.entry(p.0.to_string()).or_insert(0) -= num_corr;
	    *produced_symbols.entry(p.0.to_string()).or_insert(0) -= num_corr;
	    //	    p.1 -= max(min(*n_num, p.1), 0);
	}	
    } 
    (needed_symbols, produced_symbols)
		
}


fn find_num_ore_needed_for_fuel(rules: &Vec<(String, String)>, fuelqty : i64) -> i64 {
    let mut exitcond = false;
    let mut produced_symbols_map = HashMap::new();
    let mut needed_symbols_map = HashMap::new();
    needed_symbols_map.insert("FUEL".to_string(), fuelqty as i64);
    while exitcond == false { 
	let retval = process_next_rule(&rules, needed_symbols_map, produced_symbols_map);
	needed_symbols_map = retval.0;
	produced_symbols_map = retval.1;
	let num_ore_found = number_symbols_in_map(&needed_symbols_map, "ORE");
	//	println!("Num fuel: {}", number_symbols_in_map(&needed_symbols_map, "FUEL"));
	let non_ore_produced = total_non_ore_non_fuel_in_map(&needed_symbols_map);
	//	println!("ore found until now: {}", num_ore_found);
	//	println!("non-ore found until now: {}", non_ore_produced);
	if num_ore_found > 0 && non_ore_produced == 0 {
//	    println!("EXIT CONDITION 2: {}: num ore {}, per fuel: {}, generates {}", fuelqty, num_ore_found, num_ore_found as f64 / fuelqty as f64, fuelqty);
	    exitcond = true;
	}
    }
    number_symbols_in_map(&needed_symbols_map, "ORE")
}    
#[allow(unused_variables, unused_mut)]
fn main() {
    let contents = read_file("input.txt");
    let rules = contents.split("\n").filter(|a| a.len() > 0).map(|a| parse_rule_line(a)).collect::<Vec<_>>();

    println!("Rules: {:?}", rules);

    let mut total_ore_per_fuel = find_num_ore_needed_for_fuel(&rules, 1);
    println!("The answer for part A is {}", total_ore_per_fuel);

    
    let mut stepsizes = vec![1000000,10000,1000,100,10,1];
    let mut lastval = 0;
    let mut start = ceil(1000000000000.0 / total_ore_per_fuel as f64, 0) as i64;
    let target_ore = 1000000000000;
    let mut answer = 0 as i64;
    for stepsize in stepsizes {
	let mut exitcond = false;
	let mut i = 0;
	while exitcond == false {
	    let num_ore_needed = find_num_ore_needed_for_fuel(&rules, start as i64 + (i*stepsize));
	    if num_ore_needed < target_ore {
		lastval = start as i64 + (i*stepsize);
		answer = num_ore_needed;
	    } else {
		exitcond = true;
	    }
	    i += 1;
	}
	start = lastval;
    }
    println!("The answer for part B is {}", lastval);
}    



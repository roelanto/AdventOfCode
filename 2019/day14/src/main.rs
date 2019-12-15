use std::collections::HashMap;
use std::fs;



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

// pub fn parse_lines(content: &String) -> Vec<(String, String)> {
//     let split = content.split("\n");
//     let mut rules = Vec::new();
//     for s in split {
// 	if s.len() > 1 {
// 	    planets.push(parse_planet_line(&s.to_string()));
// 	}
//     }
//     return planets;
// }

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

fn number_symbols_in_map(hm: &HashMap<&str, i32>, symbol: &str) -> i32 {
    match hm.get(symbol) {
	None => 0,
	Some(x) => *x,
    }
}

fn total_number_in_map(hm: &HashMap<&str, i32>) -> i32 {
    hm.iter().fold(0, |a, x| a + x.1)
}

fn total_non_ore_non_fuel_in_map(hm: &HashMap<&str, i32>) -> i32 {
    hm.iter().filter(|a| *a.0 != "ORE").filter(|a| *a.0 != "FUEL").fold(0, |a, x| a + x.1)
}
fn next_symbol(hm: &HashMap<&str, i32>) -> String {
    return match hm.iter().filter(|a| *a.1 > 0).filter(|a| *a.0 != "ORE").next() {
	None => "None".to_string(),
	Some(x) => x.0.to_string(),
    }
//    let (symbol, number) = hm.iter().filter(|a| *a.1 > 0).filter(|a| *a.0 != "ORE").next().unwrap();
//    symbol.to_string()
}

fn increase<'a>(mut hm: HashMap<&'a str, i32>, symbol: &'a str) -> HashMap<&'a str, i32> {
    let num = hm.entry(symbol.clone()).or_insert(1);
    *num += 1;
    hm
}

fn increase_alt<'b>(hm: &mut HashMap<&'b str, i32>, symbol: String) {
    let num = hm.entry(symbol.clone()).or_insert(1);
    *num += 1;
//    hm
}


#[allow(unused_variables, unused_mut)]
fn process_next_rule<'a>(rules: &Vec<(String, String)>, mut needed_symbols : HashMap<&'a str, i32>, mut produced_symbols: HashMap<&'a str, i32>) -> (HashMap<&'a str, i32>, HashMap<&'a str, i32>) {
    //    println!("# of symbols on the stack: {}, i: {}", symbols.len(), i);
//    println!("\n\nNEXT RULE\n");
    let next_needed_symbols = next_symbol(&needed_symbols);
    let num_ore_found = number_symbols_in_map(&needed_symbols, "ORE");
    let non_ore_produced = total_non_ore_non_fuel_in_map(&produced_symbols);
    println!("ore found until now: {}", num_ore_found);
    if num_ore_found > 0 && non_ore_produced == 0 {
	println!("EXIT CONDITION: num ore: {}", num_ore_found);
	panic!("YES");
    }
    if next_symbol(&needed_symbols) != "NONE" {
	let symbol = next_symbol(&needed_symbols);
	let rule = rule_for_unit(&rules, &symbol);
	println!("applying rule {:?}", rule);
	let mut produces = Vec::new();
	for i in 0..numunits(&rule.1) {
	    produces.push(unit(&rule.1));
	}
	//	println!("  rule produces: {:?}, adding to produced_units, which is now: {:?}", produces, produced_symbols);
//	let v = produces.iter().map(|a| {increase_alt(&mut produced_symbols, &a); a}).collect::<Vec<_>>();;
	for p in produces {
	    increase_alt(&mut produced_symbols, p.to_string());
	}
	let lhs = rule.0;
	let slist = &lhs.split(", ").map(|a| (unit(a), numunits(a))).collect::<Vec<_>>();
	for s in slist {
	    for a in 0..s.1 {
//		println!("  added needed symbol: {:?}", s.0.to_string());
//		increase_alt(&mut needed_symbols, &s.0);
	    }
	}
    }
//	println!("    .. before weeding: needed:   {:?} {:?}", new_needed_symbols.len(), new_needed_symbols);
//	println!("    .. before weeding: produced: {:?} {:?}", produced_symbols.len(), produced_symbols);
/*	let mut should_remove_ps = Vec::new();
	for p_idx in 0..produced_symbols.len() {
	    let p = produced_symbols[p_idx].clone();
	    let mut has_removed = false;
	    let mut should_remove_ns = Vec::new();
//	    println!("Considering what to do with produced symbol {}: {}", p_idx, p);
	    for n_idx in 0..new_needed_symbols.len() {
//		println!("Comparing {} == {} yields {}", p, new_needed_symbols[n_idx], p == new_needed_symbols[n_idx]);
		if p == new_needed_symbols[n_idx] {
		    should_remove_ns.push(n_idx);
//		    println!("Deleting Idx of needed symbols is {} in {:?}, should break now", n_idx, new_needed_symbols);
		    break;
		}
	    }
	    if should_remove_ns.len() == 0 {
//		new_needed_symbols.push(p.to_string());
	    } else {
		new_needed_symbols.remove(should_remove_ns[0]);
//		println!("Push idx {} to should_reove_ps ", p_idx);
		should_remove_ps.push(p_idx);
	    }
	}
//	println!("I think I need to remove idxes {:?}", should_remove_ps);
	should_remove_ps.reverse();
	for idx in should_remove_ps {
//	    println!("Removing idx {:?}", idx);
	    produced_symbols.remove(idx);
	}
	println!("    .. after weeding: needed:   {:?} ", new_needed_symbols.len());
	println!("    .. after weeding: produced: {:?} ", produced_symbols.len());

	return process_next_rule(rules, new_needed_symbols, produced_symbols);
    } else {
	println!("FOUND ORE");
	return (needed_symbols, produced_symbols);
    }
//    let rule = rule_for_unit(&rules, symbols[i]);
	//	println!("Next rule: {:?}", rule);*/
	(needed_symbols, produced_symbols)
}

#[allow(unused_variables, unused_mut)]
fn main() {
    let contents = read_file("sample0.txt");
    let rules = contents.split("\n").filter(|a| a.len() > 0).map(|a| parse_rule_line(a)).collect::<Vec<_>>();

    let mut thesymbol = String::new();
    let mut symbols = Vec::new();
    symbols.push("FUEL".to_string());
    let mut i = 0;
    println!("Rules: {:?}", rules);
    let mut num_ore = 0;
    let mut needs_symbols = true;
//    let mut produced_symbols = Vec::new();

    let mut produced_symbols_map = HashMap::new();
    let mut needed_symbols_map = HashMap::new();

  //  let mut needed_hm = HashMap::new();
/*    hm.insert("FUEL", 1);
    hm.insert("ORE", 2);
    hm.insert("IRON", 3);
    println!("length hm: {}", hm.len());
    println!("Numbwr of ore {:?}", number_symbols_in_map(&hm, "ORE"));
    println!("total number {:?}", total_number_in_map(&hm));
    println!("Next symbol {:?}", next_symbol(&hm));
*/
    process_next_rule(&rules, needed_symbols_map, produced_symbols_map);
}

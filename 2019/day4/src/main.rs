fn rle_reduce_int(num : &str) -> Vec<u32>{
    let digits: Vec<_> = num.to_string().chars().map(|d| d.to_digit(10).unwrap()).collect();
    return rle_reduce(digits);
}

fn rle_reduce(v:Vec<u32>) -> Vec<u32> {
    let mut prev_read = v[0]+1;
    let mut vnew:Vec<_> = Vec::new();
    let mut counter = 0;
    let mut runcounter = 0;
    for val in v.into_iter() {
	counter = counter + 1;
	if val != prev_read {
	    if runcounter > 0 {
		vnew.push(counter);
	    }
	    counter = 0;
	} 
	prev_read = val;
	runcounter = runcounter + 1;
    }
    vnew.push(counter+1);
    return vnew;
}

fn determine_order(st : &str) -> bool {
    let m1 = st.chars().map(|x| x.to_digit(10).unwrap())  ;
    let m1vec = m1.clone().collect::<Vec<u32>>();
    let mut m2vec = m1.clone().collect::<Vec<u32>>();
    m2vec.sort();
    m2vec==m1vec
}

fn has_multiples (v : Vec<u32>) -> u32 {
    v.iter().map(|x| if *x > 1 {1} else {0}).sum()
}

fn has_doubles (v : Vec<u32>) -> u32 {
    v.iter().map(|x| if *x == 2 {1} else {0}).sum()
}

fn main() {
    
    let v:Vec<_> = (240298..784956).map(|x| x).collect();

    let viter = v.iter();
    // determine which have double input

    let vfilter:Vec<&u32> = viter.filter(|x| has_multiples(rle_reduce_int(&x.to_string())) > 0).collect();
    println!("filter1: {:?}", vfilter.len());
    let vfilter2:Vec<&&u32> = vfilter.iter().filter(|x| determine_order(&x.to_string())).collect::<Vec<&&u32>>();
    println!("filter2: {:?} is the answer to the first question.", vfilter2.len());

    let vfilter3:Vec<&&&u32> = vfilter2.iter().filter(|x| has_doubles(rle_reduce_int(&x.to_string())) > 0).collect();
    println!("filter3: {:?} is the answer to the second question.", vfilter3.len());
}

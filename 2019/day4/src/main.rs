

fn rle_reduce_int(num : &str) -> Vec<u32>{
    let digits: Vec<_> = num.to_string().chars().map(|d| d.to_digit(10).unwrap()).collect();
    return rle_reduce(digits);
}

fn rle_reduce(v:Vec<u32>) -> Vec<u32> {
    let mut prev_read = v[0]+1;
    let mut vnew:Vec<_> = Vec::new();
    for val in v.into_iter() {
	if val != prev_read {
	    vnew.push(val);
	}
	prev_read = val;
    }
    return vnew;
}


//fn join(a: &[i32]) -> String {
//    a.iter().fold(String::new(),|mut s,&n| {write!(s,"{}",n).ok(); s})
//}
fn determine_order(st : &str) -> bool {
    let m1 = st.chars().map(|x| x.to_digit(10).unwrap())  ;
    let m1vec = m1.clone().collect::<Vec<u32>>();
    let mut m2vec = m1.clone().collect::<Vec<u32>>();
    m2vec.sort();
    m2vec==m1vec
}

fn main() {
    let v:Vec<_> = (240298..784956).map(|x| x).collect();
    //    let v = vec![1,2,3,4,5,6,7,8,9,11,10];
//    let v = vec![111111, 223450, 123789];
    let viter = v.iter();
    // determine which have double input
    let vfilter:Vec<&u32> = viter.filter(|x| (x.to_string()).len() != (rle_reduce_int(&x.to_string())).len()).collect();
    println!("filter1: {:?}", vfilter.len());
    let vfilter2:Vec<&&u32> = vfilter.iter().filter(|x| determine_order(&x.to_string())).collect::<Vec<&&u32>>();
    println!("filter2: {:?}", vfilter2.len());

    println!("rle: {:?}", rle_reduce(vec![1,2,3,3,2,1]));
    println!("rle: {:?}", rle_reduce(vec![0]));
    let num = "123321";
    println!("rle: {:?}", rle_reduce_int(num));

}

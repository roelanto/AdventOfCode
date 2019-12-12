use std::fs;

fn read_file(filename: &str) -> std::string::String {
    let contents = fs::read_to_string(filename.to_string()).expect("Error reading file");
    return contents;
}

fn num_containing_digits(layer: &Vec<char>, digit:char) -> usize {
    let occurrences = layer.into_iter().filter(|a| a==&&digit).collect::<Vec<_>>();
    occurrences.len()
}

fn pixel_at(layers: &Vec<Vec<char>>, pos:usize) -> char {
    let pixels = layers.iter().map(|a| a[pos]).filter(|a| a != &'2').collect::<Vec<_>>();
    pixels[0]
}

fn main() {
    let content = "123456789012".to_string();
    let size = 3*2;
    let width = 3;
    
    let content = "0222112222120000";
    let size = 2*2;
    let width = 2;

    let content = read_file("input.txt");
    let size = 25*6;
    let width = 25;

    let mut i = 0;
    let mut layer = Vec::new();
    let mut content_iter = content.chars();
    let mut layer_vec = Vec::new();
    while i < content.len() {
	layer.push(content_iter.next().unwrap());
	i = i+1;
	if i % size  == 0 {
	    layer_vec.push(layer.clone());
	    layer.clear();
	}
    }

    // answer 1
    let num_zeroes = layer_vec.iter().map(|l| num_containing_digits(l, '0')).collect::<Vec<_>>();
    let layer_with_most_zeroes = num_zeroes.iter().enumerate().map(|(x, y)| (y, x)).min().unwrap();
    println!("{:?}", layer_with_most_zeroes);
    println!("{:?}", num_containing_digits(&layer_vec[layer_with_most_zeroes.1], '1'));
    println!("{:?}", num_containing_digits(&layer_vec[layer_with_most_zeroes.1], '2'));
    println!("final answer: {:?}",
	     num_containing_digits(&layer_vec[layer_with_most_zeroes.1], '1') * 
	     num_containing_digits(&layer_vec[layer_with_most_zeroes.1], '2'));
    

    // answer 2
    for i in 0..size {
	if i % width == 0 {
	    println!("");
	}
	print!("{}", match pixel_at(&layer_vec, i) {
	    '0' => ' ',
	    '1' => '@',
	    _ => ' ',
	});
    }
    println!("");
}

use std::fs;

pub fn read_file(filename: &str) -> std::string::String {
    let contents = fs::read_to_string(filename.to_string()).expect("Error reading file");
    return contents;
    

}

use compiler_homework::parser::parser;
use std::io::Read;

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input).unwrap();
    let out = parser::program(&input);
    println!("{:?}", out);
}
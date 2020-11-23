use compiler_homework::parser::parser;
use compiler_homework::codegen::TranslationState;
use std::io::Read;
use std::fs::File;

fn main() {
    env_logger::init();

    let mut input = String::new();

    let mut file = File::open(std::env::args().nth(1).unwrap()).unwrap();
    file.read_to_string(&mut input).unwrap();
    drop(file);

    let out = parser::program(&input).expect("parsing failed");
    let mut trans = TranslationState::new();
    trans.generate_program(&out).expect("compilation failed");
    let image = trans.finalize().expect("finalize failed");
    image.run();
}
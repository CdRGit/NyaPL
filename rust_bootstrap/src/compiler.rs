use std::fs;

use crate::lexer;

pub fn compile(source: String) {
    let content = fs::read_to_string(source).expect("Could not read source file");

    let tokens = lexer::lex(&content).expect("Could not lex source file");

    println!("{content}");

    println!("{tokens:#?}");
}

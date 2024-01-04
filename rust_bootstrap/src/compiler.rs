use std::fs;

use crate::lexer;
use crate::parser;

pub fn compile(source: String) {
	let content = fs::read_to_string(source).expect("Could not read source file");
	println!("{content}");
	let tokens = lexer::lex(&content).expect("Could not lex source file");
	println!("{tokens:#?}");
	let ast = parser::parse(&tokens).expect("Could not parse source file");
	println!("{ast:#?}");
}

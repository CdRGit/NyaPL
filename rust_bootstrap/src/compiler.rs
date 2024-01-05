use std::fs;

use crate::lexer;
use crate::parser;

pub fn compile(source: String) {
	let content = fs::read_to_string(source).unwrap();
	println!("{content}");
	let tokens = lexer::lex(&content).unwrap();
	println!("{tokens:#?}");
	let ast = parser::parse(&tokens).unwrap();
	println!("{ast:#?}");
}

use std::fs;

use crate::lexer;
use crate::parser;
use crate::parsetree_pretty_print;

pub fn compile(source: String) {
	let content = fs::read_to_string(source).unwrap();
	println!("{content}");
	let tokens = lexer::lex(&content).unwrap();
	println!("{tokens:#?}");
	let ast = parser::parse(&tokens).unwrap();
	parsetree_pretty_print::print(ast);
}

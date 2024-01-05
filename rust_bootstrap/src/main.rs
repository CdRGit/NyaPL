use std::env;
mod compiler;
mod lexer;
mod parser;
mod parsetree_pretty_print;

fn main() {
	let source = env::args().skip(1).next().unwrap();
	compiler::compile(source);
}

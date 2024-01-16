use std::env;

mod source_span;
mod lexer;

mod ast;
mod parser;

mod parsetree_pretty_print;

mod compiler;

fn main() {
	let source = env::args().skip(1).next().unwrap();
	compiler::compile(source);
}

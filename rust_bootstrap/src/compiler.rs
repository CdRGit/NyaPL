use std::fs;

use crate::diagnostic::Diagnostic;
use crate::lexer;
use crate::parser;
use crate::typechecker;
use crate::parsetree_pretty_print;

pub fn compile(source: String) -> Box<[Diagnostic]> {
	let mut diagnostics = Vec::new();
	let content = fs::read_to_string(source.clone()).unwrap();
	println!("{content}");
	let (tokens, lex_diag) = lexer::lex(source.into(), &content);
	if let None = tokens {
		return lex_diag;
	}
	diagnostics.extend_from_slice(&lex_diag);
	let tokens = tokens.unwrap();
	println!("{tokens:#?}");
	let (ast, parse_diag) = parser::parse(&tokens);
	diagnostics.extend_from_slice(&parse_diag);
	if let None = ast {
		return diagnostics.into();
	}
	let ast = ast.unwrap();
	parsetree_pretty_print::print(&ast);
	let typed = typechecker::check_tree(&ast).unwrap();
	diagnostics.into()
}

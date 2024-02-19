use std::env;

mod source_span;
mod lexer;

mod ast;
mod parser;

mod typed_tree;
mod typechecker;

mod parsetree_pretty_print;

mod compiler;

mod diagnostic;

use diagnostic::{Severity, Diagnostic};
use source_span::SourceSpan;

fn main() {
	let source = env::args().skip(1).next().unwrap();
	print_diagnostics(compiler::compile(source));
}

fn print_diagnostics(diagnostics: Box<[Diagnostic]>) {
	for diag in diagnostics.into_iter() {
		let header = match diag.1 {
			Severity::Info => "[INFO]",
			Severity::Warn => "\x1b[33m[WARNING]",
			Severity::Error => "\x1b[31m[ERROR]",
		};
		let message = &diag.2;
		let location = format_location(&diag.0);
		println!("{}\x1b[39m: {} \x1b[34mat\x1b[36m {}", header, message, location);
		assert!(diag.3.len() == 0);
	}
}

fn format_location(span: &SourceSpan) -> Box<str> {
	format!("{}, line {}-{}, col {}-{}", span.path, span.start.line, span.end.line, span.start.col, span.end.col).into()
}

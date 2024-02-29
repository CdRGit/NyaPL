use crate::source_span::SourceSpan;

#[derive(Debug, Clone)]
pub struct Diagnostic(pub SourceSpan, pub Severity, pub Box<str>, pub Box<[Diagnostic]>);

impl Diagnostic {
	pub fn simple_error(message: Box<str>, span: SourceSpan) -> Self {
		Diagnostic(span, Severity::Error, message, Box::new([]))
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Severity {
	Info,
	Warn,
	Error,
}

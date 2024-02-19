use std::rc::Rc;

use crate::{source_span::{SourceSpan, SourceLoc}, diagnostic::{Severity, Diagnostic}};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
	If,
	Elif,
	Else,
	While,

	Platform,
	Unsafe,
	Intrinsic,
	Function,
	TypeDef,
	Let,
	Mutable,
	Return,

	False,
	True,

	Hole,
}

#[derive(Debug, Clone)]
pub struct Token {
	pub kind: TokenKind,
	pub span: SourceSpan,
}

impl Token {
	fn new(span: SourceSpan, kind: TokenKind) -> Token {
		return Token {span, kind}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
	Identifier(Rc<str>),
	Keyword(Keyword),

	Integer(u64),

	LParen,
	RParen,
	LCurly,
	RCurly,
	LSquare,
	RSquare,

	EqEq,
	BangEq,
	Greater,
	GreaterEq,
	Lesser,
	LesserEq,

	GreaterGreater,
	LesserLesser,

	And,
	AndAnd,
	Caret,
	Pipe,
	PipePipe,

	Assign,
	Bang,
	Question,

	Colon,
	SemiColon,
	Comma,

	Plus,
	Minus,
	Slash,
	Star,
	Percent,
}

pub fn lex(path: Rc<str>, source_text: &str) -> (Option<Box<[Token]>>, Box<[Diagnostic]>) {
	let mut diagnostics = Vec::new();
	let mut vec = Vec::new();
	let mut iter = source_text.chars().enumerate().peekable();
	let (mut line, mut col): (usize, usize) = (0, 0);
	while let Some((idx, c)) = iter.next() {
		let (s_line, s_col) = (line, col);
		let mut e_idx = idx;
		match c {
			'\r' => {
				if let Some((_, '\n')) = iter.peek() {
					iter.next();
				}
				line += 1;
				col = 0;
				continue;
			}
			'\n' => {
				line += 1;
				col = 0;
				continue;
			}
			_ => {
				col += 1;
			}
		}
		let kind = match c {
			' ' | '\t' | '\n' => continue,
			',' => TokenKind::Comma,
			':' => TokenKind::Colon,
			';' => TokenKind::SemiColon,
			'(' => TokenKind::LParen,
			')' => TokenKind::RParen,
			'{' => TokenKind::LCurly,
			'}' => TokenKind::RCurly,
			'[' => TokenKind::LSquare,
			']' => TokenKind::RSquare,
			'+' => TokenKind::Plus,
			'-' => TokenKind::Minus,
			'/' => TokenKind::Slash,
			'*' => TokenKind::Star,
			'%' => TokenKind::Percent,
			'?' => TokenKind::Question,
			'^' => TokenKind::Caret,
			'&' => {
				if let Some((e, '&')) = iter.peek() {
					e_idx = *e;
					col = col + 1;
					TokenKind::AndAnd
				} else {
					TokenKind::And
				}
			},
			'|' => {
				if let Some((e, '|')) = iter.peek() {
					e_idx = *e;
					col = col + 1;
					iter.next();
					TokenKind::PipePipe
				} else {
					TokenKind::Pipe
				}
			},
			'=' => {
				if let Some((e, '=')) = iter.peek() {
					e_idx = *e;
					col = col + 1;
					iter.next();
					TokenKind::EqEq
				} else {
					TokenKind::Assign
				}
			},
			'!' => {
				if let Some((e, '=')) = iter.peek() {
					e_idx = *e;
					col = col + 1;
					iter.next();
					TokenKind::BangEq
				} else {
					TokenKind::Bang
				}
			},
			'>' => {
				if let Some((e, '=')) = iter.peek() {
					e_idx = *e;
					col = col + 1;
					iter.next();
					TokenKind::GreaterEq
				} else if let Some((e, '>')) = iter.peek() {
					e_idx = *e;
					iter.next();
					TokenKind::GreaterGreater
				} else {
					TokenKind::Greater
				}
			},
			'<' => {
				if let Some((e, '=')) = iter.peek() {
					e_idx = *e;
					col = col + 1;
					iter.next();
					TokenKind::LesserEq
				} else if let Some((e, '<')) = iter.peek() {
					e_idx = *e;
					col = col + 1;
					iter.next();
					TokenKind::LesserLesser
				} else {
					TokenKind::Lesser
				}
			},
			'0'..='9' => {
				let mut val = 0;
				val += c as u64 - b'0' as u64;
				while let Some((e, c)) = iter.peek() {
					match c {
						'0'..='9' => {
							val *= 10;
							val += *c as u64 - b'0' as u64;
							e_idx = *e;
							col = col + 1;
							iter.next();
						},
						_ => break,
					}
				}

				TokenKind::Integer(val)
			},
			'a'..='z' | 'A'..='Z' | '_' => {
				let mut value = String::new();
				value.push(c);
				while let Some((e, c)) = iter.peek() {
					match c {
						'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
							e_idx = *e;
							col = col + 1;
							value.push(*c);
							iter.next();
						},
						_ => break,
					}
				}
				match value.as_str() {
					"if" => TokenKind::Keyword(Keyword::If),
					"elif" => TokenKind::Keyword(Keyword::Elif),
					"else" => TokenKind::Keyword(Keyword::Else),
					"while" => TokenKind::Keyword(Keyword::While),
					"platform" => TokenKind::Keyword(Keyword::Platform),
					"unsafe" => TokenKind::Keyword(Keyword::Unsafe),
					"intrinsic" => TokenKind::Keyword(Keyword::Intrinsic),
					"fn" => TokenKind::Keyword(Keyword::Function),
					"typedef" => TokenKind::Keyword(Keyword::TypeDef),
					"let" => TokenKind::Keyword(Keyword::Let),
					"mut" => TokenKind::Keyword(Keyword::Mutable),
					"return" => TokenKind::Keyword(Keyword::Return),
					"false" => TokenKind::Keyword(Keyword::False),
					"true" => TokenKind::Keyword(Keyword::True),
					"_" => TokenKind::Keyword(Keyword::Hole),
					_ => TokenKind::Identifier(value.into()),
				}
			},
			c => {
				let message = format!("Unexpected Character '{}'", c);
				diagnostics.push(
					Diagnostic(
						SourceSpan {path: path.clone(), start: SourceLoc {idx, col: s_col, line: s_line}, end: SourceLoc {idx, col: s_col, line: s_line}},
						Severity::Error,
						message.into(),
						Box::new([])
					)
				);
				continue;
			}
		};
		e_idx = e_idx + 1;
		vec.push(Token::new(SourceSpan {path: path.clone(), start: SourceLoc {idx, col: s_col, line: s_line}, end: SourceLoc {idx: e_idx, col, line}}, kind));
	}
	let any_errors = (&diagnostics).into_iter().any(|d| d.1 == Severity::Error);
	if any_errors {
		(None, diagnostics.into())
	} else {
		(Some(vec.into()), diagnostics.into())
	}
}

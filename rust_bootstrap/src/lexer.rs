use std::rc::Rc;

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
	pub offset: usize,
}

impl Token {
	fn new(offset: usize, kind: TokenKind) -> Token {
		return Token {offset, kind}
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

#[derive(Debug)]
pub enum LexError {
	UnexpectedChar(char, usize),
}

pub fn lex(source_text: &str) -> Result<Box<[Token]>, LexError> {
	let mut vec = Vec::new();
	let mut iter = source_text.chars().enumerate().peekable();
	while let Some((idx, c)) = iter.next() {
		vec.push(Token::new(idx, match c {
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
				if let Some((_, '&')) = iter.peek() {
					iter.next();
					TokenKind::AndAnd
				} else {
					TokenKind::And
				}
			},
			'|' => {
				if let Some((_, '|')) = iter.peek() {
					iter.next();
					TokenKind::PipePipe
				} else {
					TokenKind::Pipe
				}
			},
			'=' => {
				if let Some((_, '=')) = iter.peek() {
					iter.next();
					TokenKind::EqEq
				} else {
					TokenKind::Assign
				}
			},
			'!' => {
				if let Some((_, '=')) = iter.peek() {
					iter.next();
					TokenKind::BangEq
				} else {
					TokenKind::Bang
				}
			},
			'>' => {
				if let Some((_, '=')) = iter.peek() {
					iter.next();
					TokenKind::GreaterEq
				} else if let Some((_, '>')) = iter.peek() {
					iter.next();
					TokenKind::GreaterGreater
				} else {
					TokenKind::Greater
				}
			},
			'<' => {
				if let Some((_, '=')) = iter.peek() {
					iter.next();
					TokenKind::LesserEq
				} else if let Some((_, '<')) = iter.peek() {
					iter.next();
					TokenKind::LesserLesser
				} else {
					TokenKind::Lesser
				}
			},
			'0'..='9' => {
				let mut val = 0;
				val += c as u64 - b'0' as u64;
				while let Some((_, c)) = iter.peek() {
					match c {
						'0'..='9' => {
							val *= 10;
							val += *c as u64 - b'0' as u64;
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
				while let Some((_, c)) = iter.peek() {
					match c {
						'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
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
			_ => return Err(LexError::UnexpectedChar(c, idx)),
		}));
	}
	return Ok(vec.into());
}

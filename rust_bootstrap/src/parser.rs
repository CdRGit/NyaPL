use crate::lexer::{Token, TokenKind, Keyword};

use std::rc::Rc;
use std::iter::Peekable;

#[derive(Debug)]
pub enum AstNode {
	Root(Box<[AstNode]>),
	Function {
		kw: Token,
		effects: Box<[AstNode]>,
		name: Rc<str>,
		params: Box<[AstNode]>,
	},
	NamedEffect(Token),
}

#[derive(Debug)]
pub enum ParseError {
	UnexpectedToken(Token),
	ExpectedToken(&'static str, Token),
	UnexpectedEndOfInput,
}

pub fn parse_effect<'a, I>(tokens: &mut Peekable<I>) -> Result<AstNode, ParseError>
where I: Iterator<Item = &'a Token> {
	Ok(match tokens.next() {
		Some(tok)
			if matches!(tok, Token {kind: TokenKind::Identifier(_), offset: _})
				=> AstNode::NamedEffect(tok.clone()),
				Some(tok) => return Err(ParseError::UnexpectedToken(tok.clone())),
				None => return Err(ParseError::UnexpectedEndOfInput),
	})
}

pub fn parse_function<'a, I>(tokens: &mut Peekable<I>) -> Result<AstNode, ParseError>
where I: Iterator<Item = &'a Token> {
	// get the `fn`
	let kw = tokens.next().unwrap().clone();
	let mut effects = Vec::new();
	// do we have a `[`?
	if let Some(Token {kind: TokenKind::LSquare, offset: _}) = tokens.next() {
		loop {
			if let Some(Token {kind: TokenKind::RSquare, offset: _}) = tokens.peek() {
				tokens.next();
				break;
			} else if let None = tokens.peek() {
				return Err(ParseError::UnexpectedEndOfInput);
			}
			effects.push(parse_effect(tokens)?);
		}
	}
	// name
	let name;
	let tok = tokens.next().ok_or(ParseError::UnexpectedEndOfInput)?;
	if let Token {kind: TokenKind::Identifier(fn_name), offset: _} = tok {
		name = fn_name.clone();
	} else {
		return Err(ParseError::ExpectedToken("identifier", tok.clone()));
	}
	// parameters
	let mut params = Vec::new();
	tokens.next_if(|t| t.kind == TokenKind::LParen).ok_or(ParseError::UnexpectedEndOfInput)?;
	return Ok(AstNode::Function { kw, effects: effects.into(), name, params: params.into() });
}

pub fn parse(tokens: &[Token]) -> Result<AstNode, ParseError> {
	let mut iter = tokens.iter().peekable();
	let mut result = Vec::new();

	while let Some(tok) = iter.peek() {
		result.push(match tok.kind {
			TokenKind::Keyword(Keyword::Function) => parse_function(&mut iter)?,
			_ => return Err(ParseError::UnexpectedToken(tok.clone().clone())),
		});
	}
	return Ok(AstNode::Root(result.into()));
}

use crate::lexer::{Token, TokenKind, Keyword};

use std::rc::Rc;
use std::iter::Peekable;

#[derive(Debug)]
pub struct ParseTree {
	pub toplevel: Box<[TopLevelStmt]>,
}

#[derive(Debug)]
pub enum TopLevelStmt {
	Function(Function),
}

#[derive(Debug)]
pub struct Function {
	pub kw: Token,
	pub effects: Box<[Effect]>,
	pub name: Rc<str>,
	pub params: Box<[Parameter]>,
	pub ret_type: Type,
	pub children: Box<[Function]>,
	pub body: BlockExpr,
}

#[derive(Debug)]
pub enum Expr {
	Block(BlockExpr),
	Integer(IntExpr),
	Unit,
	Lookup(LookupExpr),
	Call(CallExpr),
	Return(ReturnExpr),
	If(IfExpr),
}

#[derive(Debug)]
pub struct BlockExpr(Box<[Expr]>);

#[derive(Debug)]
pub struct IntExpr(Token, u64);

#[derive(Debug)]
pub struct LookupExpr(Token, Rc<str>);

#[derive(Debug)]
pub struct CallExpr(Box<Expr>, Box<[Expr]>);

#[derive(Debug)]
pub struct ReturnExpr(Token, Box<Expr>);

#[derive(Debug)]
pub struct IfExpr {
	kw: Token,
	condition: Box<Expr>,
	body: BlockExpr,
}

#[derive(Debug)]
pub enum Effect {
	Named(Rc<str>),
}

#[derive(Debug)]
pub struct Parameter {
	pub name: Rc<str>,
	pub type_: Type,
}

#[derive(Debug)]
pub enum Type {
	Named(Rc<str>),
}

#[derive(Debug)]
pub enum ParseError {
	UnexpectedToken(Token),
	UnexpectedEndOfInput,
}

fn take<'a, I, F, U>(tokens: &mut Peekable<I>, selector: F) -> Result<U, ParseError>
where I: Iterator<Item = &'a Token>, F: Fn (&&Token) -> Result<U, ParseError> {
	let tok = tokens.peek().ok_or(ParseError::UnexpectedEndOfInput)?;
	let ret = selector(tok)?;
	tokens.next();
	return Ok(ret);
}

fn take_kind<'a, I>(tokens: &mut Peekable<I>, kind: TokenKind) -> Result<Token, ParseError>
where I: Iterator<Item = &'a Token> {
	return take(tokens, |t| if t.kind == kind { Ok(t.clone().clone()) } else { Err(ParseError::UnexpectedToken(t.clone().clone())) });
}

fn parse_effect<'a, I>(tokens: &mut Peekable<I>) -> Result<Effect, ParseError>
where I: Iterator<Item = &'a Token> {
	Ok(match tokens.next() {
		Some(Token {kind: TokenKind::Identifier(name), offset: _})
			=> Effect::Named(name.clone()),
		Some(tok) => return Err(ParseError::UnexpectedToken(tok.clone())),
		None => return Err(ParseError::UnexpectedEndOfInput),
	})
}

fn parse_type<'a, I>(tokens: &mut Peekable<I>) -> Result<Type, ParseError>
where I: Iterator<Item = &'a Token> {
	Ok(match tokens.next() {
		Some(Token {kind: TokenKind::Identifier(name), offset: _})
			=> Type::Named(name.clone()),
		Some(tok) => return Err(ParseError::UnexpectedToken(tok.clone())),
		None => return Err(ParseError::UnexpectedEndOfInput),
	})
}

fn parse_typetag<'a, I>(tokens: &mut Peekable<I>) -> Result<Type, ParseError>
where I: Iterator<Item = &'a Token> {
	take_kind(tokens, TokenKind::Colon)?;
	return parse_type(tokens);
}

fn parse_return<'a, I>(tokens: &mut Peekable<I>) -> Result<ReturnExpr, ParseError>
where I: Iterator<Item = &'a Token> {
	let kw = tokens.next().unwrap();
	let expr = parse_expr(tokens)?;
	Ok(ReturnExpr(kw.clone(), Box::new(expr)))
}

fn parse_if<'a, I>(tokens: &mut Peekable<I>) -> Result<IfExpr, ParseError>
where I: Iterator<Item = &'a Token> {
	let kw = tokens.next().unwrap();
	let cond = parse_expr(tokens)?;
	let body = parse_block_expr(tokens)?;

	return Ok(IfExpr { kw: kw.clone(), condition: Box::new(cond), body });
}

fn parse_block_expr<'a, I>(tokens: &mut Peekable<I>) -> Result<BlockExpr, ParseError>
where I: Iterator<Item = &'a Token> {
	take_kind(tokens, TokenKind::LCurly)?;
	// function bodies can first have as many child / helper functions as they want
	let body = if take_kind(tokens, TokenKind::RCurly).is_ok() {
		// implied Unit expression
		Box::new([Expr::Unit])
	} else {
		let body = parse_expr_list(tokens)?;
		take_kind(tokens, TokenKind::RCurly)?;
		body
	};
	return Ok(BlockExpr(body));
}

fn parse_atom_expr<'a, I>(tokens: &mut Peekable<I>) -> Result<Expr, ParseError>
where I: Iterator<Item = &'a Token> {
	let tok = tokens.peek().ok_or(ParseError::UnexpectedEndOfInput)?.clone().clone();
	Ok(match tok.kind.clone() {
		TokenKind::Integer(val) => { tokens.next(); Expr::Integer(IntExpr(tok, val)) },
		TokenKind::Keyword(Keyword::Return) => Expr::Return(parse_return(tokens)?),
		TokenKind::Keyword(Keyword::If) => Expr::If(parse_if(tokens)?),
		TokenKind::Identifier(name) => { tokens.next(); Expr::Lookup(LookupExpr(tok.clone(), name)) },
		_ => return Err(ParseError::UnexpectedToken(tok.clone().clone())),
	})
}

fn parse_primary_expr<'a, I>(tokens: &mut Peekable<I>) -> Result<Expr, ParseError>
where I: Iterator<Item = &'a Token> {
	let mut expr = parse_atom_expr(tokens)?;

	loop {
		let tok = tokens.peek().ok_or(ParseError::UnexpectedEndOfInput)?;
		expr = if let Token {kind: TokenKind::LParen, offset: _} = tok {
			let mut args = Vec::new();
			tokens.next();
			if take_kind(tokens, TokenKind::RParen).is_err() {
				loop {
					args.push(parse_expr(tokens)?);
					if take_kind(tokens, TokenKind::Comma).is_err() {
						take_kind(tokens, TokenKind::RParen)?;
						break;
					}
				}
			}

			Expr::Call(CallExpr(Box::new(expr), args.into()))
		} else {
			break;
		}
	}

	Ok(expr)
}

fn parse_binary_expr<'a, I>(tokens: &mut Peekable<I>) -> Result<Expr, ParseError>
where I: Iterator<Item = &'a Token> {
	let expr = parse_primary_expr(tokens)?;
	todo!();

	Ok(expr)
}

fn parse_expr<'a, I>(tokens: &mut Peekable<I>) -> Result<Expr, ParseError>
where I: Iterator<Item = &'a Token> {
	let expr = parse_binary_expr(tokens)?;

	Ok(expr)
}

fn parse_expr_list<'a, I>(tokens: &mut Peekable<I>) -> Result<Box<[Expr]>, ParseError>
where I: Iterator<Item = &'a Token> {
	let mut list = Vec::new();

	loop {
		list.push(parse_expr(tokens)?);
		// no semicolon
		if take_kind(tokens, TokenKind::SemiColon).is_err() {
			break;
		}
		// closing curly after semicolon?
		if tokens.peek().filter(|t| t.kind == TokenKind::RCurly).is_some() {
			// add the implied "unit" expression
			list.push(Expr::Unit);
			break;
		}
	}

	return Ok(list.into());
}

fn parse_function_body<'a, I>(tokens: &mut Peekable<I>) -> Result<(Box<[Function]>, BlockExpr), ParseError>
where I: Iterator<Item = &'a Token> {
	take_kind(tokens, TokenKind::LCurly)?;
	// function bodies can first have as many child / helper functions as they want
	let mut children = Vec::new();
	while let Some(tok) = tokens.peek() {
		children.push(match tok.kind {
			TokenKind::Keyword(Keyword::Function)
				=> parse_function(tokens)?,
			_ => break,
		});
	}
	let body = if take_kind(tokens, TokenKind::RCurly).is_ok() {
		// implied Unit expression
		Box::new([Expr::Unit])
	} else {
		let body = parse_expr_list(tokens)?;
		take_kind(tokens, TokenKind::RCurly)?;
		body
	};
	return Ok((children.into(), BlockExpr(body)));
}

fn parse_function<'a, I>(tokens: &mut Peekable<I>) -> Result<Function, ParseError>
where I: Iterator<Item = &'a Token> {
	// get the `fn`
	let kw = tokens.next().unwrap().clone();
	let mut effects = Vec::new();
	// do we have a `[`?
	if let Some(_) = tokens.next_if(|t| t.kind == TokenKind::LSquare) {
		loop {
			if let Some(_) = tokens.next_if(|t| t.kind == TokenKind::RSquare) {
				break;
			}
			effects.push(parse_effect(tokens)?);
		}
	}
	// name
	let name = take(tokens, |t| {
		if let Token {kind: TokenKind::Identifier(n), offset: _} = t {
			Ok(n.clone())
		} else {
			Err(ParseError::UnexpectedToken(t.clone().clone()))
		}
	})?;
	// parameters
	let mut params = Vec::new();
	take_kind(tokens, TokenKind::LParen)?;

	take_kind(tokens, TokenKind::RParen)?;
	let ret_type = parse_typetag(tokens)?;

	let (children, body) = parse_function_body(tokens)?;
	return Ok(Function { kw, effects: effects.into(), name, params: params.into(), ret_type, children, body });
}

pub fn parse(tokens: &[Token]) -> Result<ParseTree, ParseError> {
	let mut iter = tokens.iter().peekable();
	let mut toplevel = Vec::new();

	while let Some(tok) = iter.peek() {
		toplevel.push(match tok.kind {
			TokenKind::Keyword(Keyword::Function)
				=> TopLevelStmt::Function(parse_function(&mut iter)?),
			_ => return Err(ParseError::UnexpectedToken(tok.clone().clone())),
		});
	}
	return Ok(ParseTree { toplevel: toplevel.into() });
}

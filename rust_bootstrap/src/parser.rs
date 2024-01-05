use crate::lexer::{Token, TokenKind, Keyword};

use std::rc::Rc;
use std::collections::VecDeque;
use std::iter::Peekable;
use std::cmp::Ordering;
use std::error::Error;
use std::fmt::Display;

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

#[derive(Debug, Clone)]
pub enum Expr {
	Unit,
	Boolean(bool),
	Integer(IntExpr),
	Tuple(Box<[Expr]>),
	Block(BlockExpr),
	BinOp(BinExpr),
	PreOp(PreExpr),
	PostOp(PostExpr),
	Lookup(LookupExpr),
	Call(CallExpr),
	Return(ReturnExpr),
	If(IfExpr),
	While(WhileExpr),
	Let(LetExpr),
}

#[derive(Debug, Clone)]
pub struct BlockExpr(Box<[Expr]>);

#[derive(Debug, Clone)]
pub struct IntExpr(Token, u64);

#[derive(Debug, Clone)]
pub struct LookupExpr(Token, Rc<str>);

#[derive(Debug, Clone)]
pub struct CallExpr(Box<Expr>, Box<[Expr]>);

#[derive(Debug, Clone)]
pub struct BinExpr(InfixOp, Box<Expr>, Box<Expr>);

#[derive(Debug, Clone)]
pub enum InfixOp {
	Add,
	Sub,
	Mult,
	Div,
	Mod,

	BitAnd,
	BitOr,
	BitXor,

	LogAnd,
	LogOr,

	Assign,

	Equal,
	NotEqual,
	Greater,
	GreaterEqual,
	Lesser,
	LesserEqual,
}

#[derive(Debug, Clone)]
pub struct PreExpr(PrefixOp, Box<Expr>);

#[derive(Debug, Clone)]
pub enum PrefixOp {
	Identity,
	Negative,
	Not,

	Reference,
	Derference,
}

#[derive(Debug, Clone)]
pub struct PostExpr(PostfixOp, Box<Expr>);

#[derive(Debug, Clone)]
pub enum PostfixOp {
	// temporary
	Yell,
	Query,
}

#[derive(Debug, Clone)]
pub struct ReturnExpr(Token, Box<Expr>);

#[derive(Debug, Clone)]
pub struct IfExpr {
	kw: Token,
	condition: Box<Expr>,
	body: BlockExpr,
	else_: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct WhileExpr {
	kw: Token,
	condition: Box<Expr>,
	body: BlockExpr,
}

#[derive(Debug, Clone)]
pub struct LetExpr {
	kw: Token,
	target: Pattern,
	value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum Pattern {
	Named(Mutability, Rc<str>),
	Hole,
}

#[derive(Debug, Clone)]
pub enum Mutability {
	Immutable,
	Mutable,
}

#[derive(Debug, Clone)]
pub enum Effect {
	Named(Rc<str>),
}

#[derive(Debug, Clone)]
pub struct Parameter(Rc<str>, Type);

#[derive(Debug, Clone)]
pub enum Type {
	Named(Rc<str>),
}

#[derive(Debug)]
pub enum ParseError {
	UnexpectedToken(Token, ExpectedMessage),
	UnexpectedEndOfInput,
	AmbiguousOperatorOrder(Expr),
}

#[derive(Debug)]
pub enum ExpectedMessage {
	Text(&'static str),
	Kind(TokenKind),
}

impl Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "Parser Error")
    }
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
	return take(tokens, |t| if t.kind == kind { Ok(t.clone().clone()) } else { Err(ParseError::UnexpectedToken(t.clone().clone(), ExpectedMessage::Kind(kind.clone()))) });
}

fn parse_effect<'a, I>(tokens: &mut Peekable<I>) -> Result<Effect, ParseError>
where I: Iterator<Item = &'a Token> {
	Ok(match tokens.next() {
		Some(Token {kind: TokenKind::Identifier(name), offset: _})
			=> Effect::Named(name.clone()),
		Some(tok) => return Err(ParseError::UnexpectedToken(tok.clone(), ExpectedMessage::Text("effect"))),
		None => return Err(ParseError::UnexpectedEndOfInput),
	})
}

fn parse_type<'a, I>(tokens: &mut Peekable<I>) -> Result<Type, ParseError>
where I: Iterator<Item = &'a Token> {
	Ok(match tokens.next() {
		Some(Token {kind: TokenKind::Identifier(name), offset: _})
			=> Type::Named(name.clone()),
		Some(tok) => return Err(ParseError::UnexpectedToken(tok.clone(), ExpectedMessage::Text("type"))),
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

fn parse_pattern<'a, I>(tokens: &mut Peekable<I>) -> Result<Pattern, ParseError>
where I: Iterator<Item = &'a Token> {
	match tokens.next() {
		Some(Token { kind: TokenKind::Keyword(Keyword::Mutable), offset: _ }) => {
			match tokens.next() {
				Some(Token { kind: TokenKind::Identifier(n), offset: _}) => Ok(Pattern::Named(Mutability::Mutable, n.clone())),
				Some(t) => Err(ParseError::UnexpectedToken(t.clone(), ExpectedMessage::Text("pattern name"))),
				None => Err(ParseError::UnexpectedEndOfInput),
			}
		},
		Some(Token { kind: TokenKind::Identifier(n), offset: _ }) => Ok(Pattern::Named(Mutability::Immutable, n.clone())),
		Some(t) => Err(ParseError::UnexpectedToken(t.clone(), ExpectedMessage::Text("pattern"))),
		None => Err(ParseError::UnexpectedEndOfInput),
	}
}

fn parse_if<'a, I>(tokens: &mut Peekable<I>) -> Result<IfExpr, ParseError>
where I: Iterator<Item = &'a Token> {
	let kw = tokens.next().unwrap();
	let cond = parse_expr(tokens)?;
	let body = parse_block_expr(tokens)?;
	// else / elif block?
	let else_ = match tokens.peek() {
		Some(Token {kind: TokenKind::Keyword(Keyword::Else), offset: _}) => { tokens.next(); Some(Box::new(Expr::Block(parse_block_expr(tokens)?))) },
		Some(Token {kind: TokenKind::Keyword(Keyword::Elif), offset: _}) => { Some(Box::new(Expr::If(parse_if(tokens)?))) },
		_ => None,
	};

	return Ok(IfExpr { kw: kw.clone(), condition: cond.into(), body, else_ });
}

fn parse_while<'a, I>(tokens: &mut Peekable<I>) -> Result<WhileExpr, ParseError>
where I: Iterator<Item = &'a Token> {
	let kw = tokens.next().unwrap();
	let cond = parse_expr(tokens)?;
	let body = parse_block_expr(tokens)?;
	return Ok(WhileExpr { kw: kw.clone(), condition: cond.into(), body });
}

fn parse_let<'a, I>(tokens: &mut Peekable<I>) -> Result<LetExpr, ParseError>
where I: Iterator<Item = &'a Token> {
	let kw = tokens.next().unwrap();
	let pattern = parse_pattern(tokens)?;
	take_kind(tokens, TokenKind::Assign)?;
	let expr = parse_expr(tokens)?;
	Ok(LetExpr { kw: kw.clone(), target: pattern, value: expr.into() })
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
		TokenKind::Keyword(Keyword::True) => { tokens.next(); Expr::Boolean(true) },
		TokenKind::Keyword(Keyword::False) => { tokens.next(); Expr::Boolean(false) },
		TokenKind::Keyword(Keyword::Return) => Expr::Return(parse_return(tokens)?),
		TokenKind::Keyword(Keyword::If) => Expr::If(parse_if(tokens)?),
		TokenKind::Keyword(Keyword::While) => Expr::While(parse_while(tokens)?),
		TokenKind::Keyword(Keyword::Let) => Expr::Let(parse_let(tokens)?),
		TokenKind::Identifier(name) => { tokens.next(); Expr::Lookup(LookupExpr(tok.clone(), name)) },
		TokenKind::LCurly => { Expr::Block(parse_block_expr(tokens)?) },
		TokenKind::LParen => {
			// unit / parenthesized / tuple
			tokens.next();
			let mut args = Vec::new();
			// does this call have args?
			if take_kind(tokens, TokenKind::RParen).is_err() {
				loop {
					args.push(parse_expr(tokens)?);
					if take_kind(tokens, TokenKind::Comma).is_err() {
						// we need to have an rparen here now
						take_kind(tokens, TokenKind::RParen)?;
						break;
					}
				}
			}
			match &args[..] {
				[] => Expr::Unit,
				[expr] => expr.clone(),
				_ => Expr::Tuple(args.into()),
			}
		},
		_ => return Err(ParseError::UnexpectedToken(tok.clone().clone(), ExpectedMessage::Text("atom_expr"))),
	})
}

fn parse_suffix_expr<'a, I>(tokens: &mut Peekable<I>) -> Result<Expr, ParseError>
where I: Iterator<Item = &'a Token> {
	// parse core item
	let mut core = parse_atom_expr(tokens)?;

	// parse suffixes
	loop {
		let tok = tokens.peek();
		match tok {
			None => break,
			Some(tok) => {
				match tok.kind {
					TokenKind::Bang => {
						tokens.next();
						core = Expr::PostOp(PostExpr(PostfixOp::Yell, Box::new(core)));
					}
					TokenKind::Question => {
						tokens.next();
						core = Expr::PostOp(PostExpr(PostfixOp::Query, Box::new(core)));
					}
					TokenKind::LParen => {
						// call
						tokens.next();
						let mut args = Vec::new();
						// does this call have args?
						if take_kind(tokens, TokenKind::RParen).is_err() {
							loop {
								args.push(parse_expr(tokens)?);
								if take_kind(tokens, TokenKind::Comma).is_err() {
									// we need to have an rparen here now
									take_kind(tokens, TokenKind::RParen)?;
									break;
								}
							}
						}
						core = Expr::Call(CallExpr(Box::new(core), args.into()));
					},
					_ => break,
				}
			},
		}
	}
	Ok(core)
}

fn parse_prefix_expr<'a, I>(tokens: &mut Peekable<I>) -> Result<Expr, ParseError>
where I: Iterator<Item = &'a Token> {
	// parse prefixes onto a stack
	let mut prefix_stack = Vec::new();
	loop {
		let tok = tokens.peek();
		let prefix = match tok {
			None => break,
			Some(tok) => {
				match tok.kind {
					TokenKind::Plus => PrefixOp::Identity,
					TokenKind::Minus => PrefixOp::Negative,
					TokenKind::Bang => PrefixOp::Not,

					TokenKind::Star => PrefixOp::Derference,
					TokenKind::And => PrefixOp::Reference,
					TokenKind::AndAnd => {
						prefix_stack.push(PrefixOp::Reference);
						PrefixOp::Reference
					},
					_ => break,
				}
			},
		};
		prefix_stack.push(prefix);
		tokens.next();
	}

	// parse core item
	let mut core = parse_suffix_expr(tokens)?;

	// apply prefixes
	while let Some(prefix) = prefix_stack.pop() {
		core = Expr::PreOp(PreExpr(prefix, Box::new(core)));
	}

	Ok(core)
}

fn parse_bin_exprs<'a, I>(tokens: &mut Peekable<I>) -> Result<(Expr, Box<[(InfixOp, Expr)]>), ParseError>
where I: Iterator<Item = &'a Token> {
	let leftmost = parse_prefix_expr(tokens)?;
	let mut right = Vec::new();
	// parse infixes
	loop {
		let tok = tokens.peek();
		match tok {
			None => break,
			Some(tok) => {
				match tok.kind {
					// math
					TokenKind::Plus => {
						tokens.next();
						right.push((InfixOp::Add, parse_prefix_expr(tokens)?))
					}
					TokenKind::Minus => {
						tokens.next();
						right.push((InfixOp::Sub, parse_prefix_expr(tokens)?))
					}
					TokenKind::Star => {
						tokens.next();
						right.push((InfixOp::Mult, parse_prefix_expr(tokens)?))
					}
					TokenKind::Slash => {
						tokens.next();
						right.push((InfixOp::Div, parse_prefix_expr(tokens)?))
					}
					TokenKind::Percent => {
						tokens.next();
						right.push((InfixOp::Mod, parse_prefix_expr(tokens)?))
					}
					// reassignment
					TokenKind::Assign => {
						tokens.next();
						right.push((InfixOp::Assign, parse_prefix_expr(tokens)?))
					}
					// comparisons
					TokenKind::EqEq => {
						tokens.next();
						right.push((InfixOp::Equal, parse_prefix_expr(tokens)?))
					},
					TokenKind::BangEq => {
						tokens.next();
						right.push((InfixOp::NotEqual, parse_prefix_expr(tokens)?))
					},
					// bitwise logic
					TokenKind::Pipe => {
						tokens.next();
						right.push((InfixOp::BitOr, parse_prefix_expr(tokens)?))
					},
					TokenKind::And => {
						tokens.next();
						right.push((InfixOp::BitAnd, parse_prefix_expr(tokens)?))
					},
					// short-circuit logic
					TokenKind::PipePipe => {
						tokens.next();
						right.push((InfixOp::LogOr, parse_prefix_expr(tokens)?))
					},
					TokenKind::AndAnd => {
						tokens.next();
						right.push((InfixOp::LogAnd, parse_prefix_expr(tokens)?))
					},
					_ => break,
				}
			},
		}
	}

	Ok((leftmost, right.into()))
}

#[derive(PartialEq, PartialOrd, Eq, Ord)]
// later is tighter
enum Precedence {
	Assignment,
	LogicOr,
	LogicAnd,
	Comparison,
	BitOr,
	BitXor,
	BitAnd,
	BitShift,
	Additive,
	Multiplicative,
}

fn get_precedence(infix: &InfixOp) -> Precedence {
	use InfixOp as I;
	use Precedence as P;
	match infix {
		I::Assign => P::Assignment,
		I::LogOr  => P::LogicOr,
		I::LogAnd => P::LogicAnd,
		I::Equal
		| I::NotEqual
		| I::Lesser
		| I::Greater
		| I::LesserEqual
		| I::GreaterEqual => P::Comparison,
		I::BitOr  => P::BitOr,
		I::BitXor => P::BitXor,
		I::BitAnd => P::BitAnd,
		I::Add | I::Sub => P::Additive,
		I::Mult | I::Div | I::Mod => P::Multiplicative,
	}
}

enum Associativity {
	Left,
	Right,
	Explicit,
}

fn get_associavity(infix: &InfixOp) -> Associativity {
	use InfixOp as I;
	use Associativity as A;
	match infix {
		I::Assign => A::Right,
		I::LogOr  => A::Left,
		I::LogAnd => A::Left,
		I::Equal
		| I::NotEqual
		| I::Lesser
		| I::Greater
		| I::LesserEqual
		| I::GreaterEqual => A::Explicit,
		I::BitOr  => A::Left,
		I::BitXor => A::Left,
		I::BitAnd => A::Left,
		I::Add | I::Sub => A::Left,
		I::Mult | I::Div | I::Mod => A::Left,
	}
}

fn parse_expr<'a, I>(tokens: &mut Peekable<I>) -> Result<Expr, ParseError>
where I: Iterator<Item = &'a Token> {
	let bins = parse_bin_exprs(tokens)?;

	let mut working_stack = Vec::new();
	working_stack.push(bins.0);

	let mut operator_stack = Vec::new();
	for pair in bins.1.into_iter() {
		let prec = get_precedence(&pair.0);
		let assoc = get_associavity(&pair.0);
		while {
			if let Some(op) = operator_stack.last() {
				let prec2 = get_precedence(&op);
				match prec2.cmp(&prec) {
					Ordering::Less => false,
					Ordering::Equal => match assoc {
						Associativity::Left => true,
						Associativity::Right => false,
						Associativity::Explicit => return Err(ParseError::AmbiguousOperatorOrder(pair.1.clone())),
					},
					Ordering::Greater => true,
				}
			} else {
				false
			}
		} {
			// apply
			let op = operator_stack.pop().unwrap();
			let right = working_stack.pop().unwrap();
			let left = working_stack.pop().unwrap();
			let node = BinExpr(op, left.into(), right.into());
			working_stack.push(Expr::BinOp(node));
		}
		operator_stack.push(pair.0.clone());
		working_stack.push(pair.1.clone());
	}

	while let Some(op) = operator_stack.pop() {
		let right = working_stack.pop().unwrap();
		let left = working_stack.pop().unwrap();
		let node = BinExpr(op, left.into(), right.into());
		working_stack.push(Expr::BinOp(node));
	}

	match &working_stack[..] {
		[expr] => Ok(expr.clone()),
		_ => unreachable!("shunting yard should ensure the working stack has a length of 1 at the end"),
	}
}

fn requires_semicolon(expr: &Expr) -> bool {
	match expr {
		Expr::If(_) => false,
		Expr::While(_) => false,
		_ => true,
	}
}

fn parse_expr_list<'a, I>(tokens: &mut Peekable<I>) -> Result<Box<[Expr]>, ParseError>
where I: Iterator<Item = &'a Token> {
	let mut list = Vec::new();

	loop {
		let expr = parse_expr(tokens)?;
		// no semicolon
		if take_kind(tokens, TokenKind::SemiColon).is_err() && requires_semicolon(&expr) {
			list.push(expr);
			break;
		}
		list.push(expr);
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

fn parse_param<'a, I>(tokens: &mut Peekable<I>) -> Result<Parameter, ParseError>
where I: Iterator<Item = &'a Token> {
	let name = take(tokens, |t| {
		if let Token {kind: TokenKind::Identifier(n), offset: _} = t {
			Ok(n.clone())
		} else {
			Err(ParseError::UnexpectedToken(t.clone().clone(), ExpectedMessage::Text("parameter name")))
		}
	})?;

	let type_ = parse_typetag(tokens)?;
	Ok(Parameter(name, type_))
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
			Err(ParseError::UnexpectedToken(t.clone().clone(), ExpectedMessage::Text("function name")))
		}
	})?;
	// parameters
	let mut params = Vec::new();
	take_kind(tokens, TokenKind::LParen)?;

	if take_kind(tokens, TokenKind::RParen).is_err() {
		loop {
			params.push(parse_param(tokens)?);
			if take_kind(tokens, TokenKind::RParen).is_ok() {
				break;
			}
			take_kind(tokens, TokenKind::Comma)?;
		}
	}
	// end of params

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
			_ => return Err(ParseError::UnexpectedToken(tok.clone().clone(), ExpectedMessage::Text("parse_top_level"))),
		});
	}
	return Ok(ParseTree { toplevel: toplevel.into() });
}

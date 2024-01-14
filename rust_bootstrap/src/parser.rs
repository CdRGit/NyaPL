use crate::lexer::{Token, TokenKind, Keyword};

use std::rc::Rc;
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
	Hole,
	Boolean(bool),
	Integer(IntExpr),
	Tuple(Box<[Expr]>),
	Block(BlockExpr),
	Assign(Pattern, Box<Expr>),
	BinOp(BinExpr),
	PreOp(PreExpr),
	PostOp(PostExpr),
	Lookup(LookupExpr),
	Call(CallExpr),
	Return(ReturnExpr),
	If(IfExpr),
	While(WhileExpr),
	Let(LetExpr),
	Stmt(Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct BlockExpr(pub Box<[Expr]>);

#[derive(Debug, Clone)]
pub struct IntExpr(pub Token, pub u64);

#[derive(Debug, Clone)]
pub struct LookupExpr(pub Token, pub Rc<str>);

#[derive(Debug, Clone)]
pub struct CallExpr(pub Box<Expr>, pub Box<[Expr]>);

#[derive(Debug, Clone)]
pub struct BinExpr(pub InfixOp, pub Box<Expr>, pub Box<Expr>);

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

	ShiftRight,
	ShiftLeft,

	Equal,
	NotEqual,
	Greater,
	GreaterEqual,
	Lesser,
	LesserEqual,
}

#[derive(Debug, Clone)]
pub struct PreExpr(pub PrefixOp, pub Box<Expr>);

#[derive(Debug, Clone)]
pub enum PrefixOp {
	Identity,
	Negative,
	Not,

	Reference,
	Dereference,
}

#[derive(Debug, Clone)]
pub struct PostExpr(pub PostfixOp, pub Box<Expr>);

#[derive(Debug, Clone)]
pub enum PostfixOp {
	// temporary
	Yell,
	Query,
}

#[derive(Debug, Clone)]
pub struct ReturnExpr(pub Token, pub Box<Expr>);

#[derive(Debug, Clone)]
pub struct IfExpr {
	pub kw: Token,
	pub condition: Box<Expr>,
	pub body: BlockExpr,
	pub else_: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct WhileExpr {
	pub kw: Token,
	pub condition: Box<Expr>,
	pub body: BlockExpr,
}

#[derive(Debug, Clone)]
pub struct LetExpr {
	pub kw: Token,
	pub target: Pattern,
	pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum Pattern {
	Named(Mutability, Rc<str>, Option<Type>),
	Tuple(Box<[Pattern]>),
	Expression(Box<Expr>),
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
pub struct Parameter(pub Rc<str>, pub Type);

#[derive(Debug, Clone)]
pub enum Type {
	Named(Rc<str>),
	Tuple(Box<[Type]>),
	Function(Box<[Type]>, Box<Type>),
}

#[derive(Debug)]
pub enum ParseError {
	UnexpectedToken(Token, ExpectedMessage),
	UnexpectedEndOfInput,
	AmbiguousOperatorOrder(Expr),
	IllegalExpressionTerm(&'static str),
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
		Some(Token {kind: TokenKind::LParen, offset: _}) => {
			let mut params = Vec::new();
			// end early if we immediately hit an RParen
			if take_kind(tokens, TokenKind::RParen).is_err() {
				loop {
					params.push(parse_type(tokens)?);
					if take_kind(tokens, TokenKind::RParen).is_ok() {
						break;
					}
					take_kind(tokens, TokenKind::Comma)?;
				}
			}
			return Ok(Type::Tuple(params.into()));
		},
		Some(Token {kind: TokenKind::Keyword(Keyword::Function), offset: _}) => {
			// function pointer
			// grab LParen
			take_kind(tokens, TokenKind::LParen)?;
			let mut params = Vec::new();
			// end early if we immediately hit an RParen
			if take_kind(tokens, TokenKind::RParen).is_err() {
				loop {
					params.push(parse_type(tokens)?);
					if take_kind(tokens, TokenKind::RParen).is_ok() {
						break;
					}
					take_kind(tokens, TokenKind::Comma)?;
				}
			}
			let ret_type = parse_typetag(tokens)?;
			return Ok(Type::Function(params.into(), ret_type.into()));
		},
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
				Some(Token { kind: TokenKind::Identifier(n), offset: _}) => {
					let type_ = if tokens.peek().filter(|t| t.kind == TokenKind::Colon).is_some() {
						Some(parse_typetag(tokens)?)
					} else {
						None
					};
					Ok(Pattern::Named(Mutability::Mutable, n.clone(), type_))
				},
				Some(t) => Err(ParseError::UnexpectedToken(t.clone(), ExpectedMessage::Text("pattern name"))),
				None => Err(ParseError::UnexpectedEndOfInput),
			}
		},
		Some(Token { kind: TokenKind::LParen, offset: _}) => {
			let mut params = Vec::new();
			// end early if we immediately hit an RParen
			if take_kind(tokens, TokenKind::RParen).is_err() {
				loop {
					params.push(parse_pattern(tokens)?);
					if take_kind(tokens, TokenKind::RParen).is_ok() {
						break;
					}
					take_kind(tokens, TokenKind::Comma)?;
				}
			}
			return Ok(Pattern::Tuple(params.into()));
		},
		Some(Token { kind: TokenKind::Identifier(n), offset: _}) => {
			let type_ = if tokens.peek().filter(|t| t.kind == TokenKind::Colon).is_some() {
				Some(parse_typetag(tokens)?)
			} else {
				None
			};
			Ok(Pattern::Named(Mutability::Immutable, n.clone(), type_))
		},
		Some(Token { kind: TokenKind::Keyword(Keyword::Hole), offset: _}) => Ok(Pattern::Hole),
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
		let body = parse_stmt_list(tokens)?;
		take_kind(tokens, TokenKind::RCurly)?;
		body
	};
	return Ok(BlockExpr(body));
}

fn parse_atom_expr<'a, I>(tokens: &mut Peekable<I>) -> Result<Expr, ParseError>
where I: Iterator<Item = &'a Token> {
	let tok = tokens.peek().ok_or(ParseError::UnexpectedEndOfInput)?.clone().clone();
	Ok(match tok.kind.clone() {
		TokenKind::Keyword(Keyword::Hole) => { tokens.next(); Expr::Hole },
		TokenKind::Integer(val) => { tokens.next(); Expr::Integer(IntExpr(tok, val)) },
		TokenKind::Keyword(Keyword::True) => { tokens.next(); Expr::Boolean(true) },
		TokenKind::Keyword(Keyword::False) => { tokens.next(); Expr::Boolean(false) },
		TokenKind::Keyword(Keyword::Return) => Expr::Return(parse_return(tokens)?),
		TokenKind::Keyword(Keyword::If) => Expr::If(parse_if(tokens)?),
		TokenKind::Keyword(Keyword::While) => Expr::While(parse_while(tokens)?),
		TokenKind::Identifier(name) => { tokens.next(); Expr::Lookup(LookupExpr(tok.clone(), name)) },
		TokenKind::LCurly => { Expr::Block(parse_block_expr(tokens)?) },
		TokenKind::LParen => {
			// unit / parenthesized / tuple
			tokens.next();
			let mut args = Vec::new();
			// does this call have args?
			if take_kind(tokens, TokenKind::RParen).is_err() {
				loop {
					args.push(parse_pattern_or_expr(tokens)?);
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
								args.push(parse_pattern_or_expr(tokens)?);
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

					TokenKind::Star => PrefixOp::Dereference,
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
					},
					// comparisons
					TokenKind::EqEq => {
						tokens.next();
						right.push((InfixOp::Equal, parse_prefix_expr(tokens)?))
					},
					TokenKind::BangEq => {
						tokens.next();
						right.push((InfixOp::NotEqual, parse_prefix_expr(tokens)?))
					},
					TokenKind::Greater => {
						tokens.next();
						right.push((InfixOp::Greater, parse_prefix_expr(tokens)?))
					},
					TokenKind::GreaterEq => {
						tokens.next();
						right.push((InfixOp::GreaterEqual, parse_prefix_expr(tokens)?))
					},
					TokenKind::Lesser => {
						tokens.next();
						right.push((InfixOp::Lesser, parse_prefix_expr(tokens)?))
					},
					TokenKind::LesserEq => {
						tokens.next();
						right.push((InfixOp::LesserEqual, parse_prefix_expr(tokens)?))
					},
					// bitshifts
					TokenKind::GreaterGreater => {
						tokens.next();
						right.push((InfixOp::ShiftRight, parse_prefix_expr(tokens)?))
					},
					TokenKind::LesserLesser => {
						tokens.next();
						right.push((InfixOp::ShiftLeft, parse_prefix_expr(tokens)?))
					},
					// bitwise logic
					TokenKind::Pipe => {
						tokens.next();
						right.push((InfixOp::BitOr, parse_prefix_expr(tokens)?))
					},
					TokenKind::Caret => {
						tokens.next();
						right.push((InfixOp::BitXor, parse_prefix_expr(tokens)?))
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
		I::ShiftLeft | I::ShiftRight => P::BitShift,
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
		I::ShiftLeft | I::ShiftRight => A::Left,
		I::Add | I::Sub => A::Left,
		I::Mult | I::Div | I::Mod => A::Left,
	}
}

fn parse_bin_expr<'a, I>(tokens: &mut Peekable<I>) -> Result<Expr, ParseError>
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

fn validate_expr(expr: &Expr) -> Result<(), ParseError> {
	use Expr as E;
	match expr {
		E::Hole => Err(ParseError::IllegalExpressionTerm("Cannot use holes in expression")),
		E::Unit | E::Integer(_) | E::Boolean(_) => Ok(()),
		E::Lookup(_) => Ok(()),

		E::Assign(_, rhs) => validate_expr(&rhs),
		E::Tuple(exprs) => {
			let vals: Result<Vec<_>, ParseError> = exprs.iter().map(|e| validate_expr(e)).collect();
			vals?;
			Ok(())
		},
		E::Call(CallExpr(base, exprs)) => {
			validate_expr(base)?;
			let vals: Result<Vec<_>, ParseError> = exprs.iter().map(|e| validate_expr(e)).collect();
			vals?;
			Ok(())
		},
		E::BinOp(BinExpr(_, lhs, rhs)) => {
			validate_expr(lhs)?;
			validate_expr(rhs)
		},
		E::PreOp(PreExpr(_, base)) => {
			validate_expr(base)
		},
		E::PostOp(PostExpr(_, base)) => {
			validate_expr(base)
		},
		// these should be fine
		E::Block(_) => Ok(()),
		E::Return(_) => Ok(()),
		E::While(_) => Ok(()),
		E::If(_) => Ok(()),
		E::Let(_) => Ok(()),
		E::Stmt(e) => validate_expr(e),
	}
}

fn rewrite_to_pattern(expr: &Expr) -> Result<Pattern, ParseError> {
	use Expr as E;
	use Pattern as P;
	match expr {
		E::Lookup(LookupExpr(_, name)) => Ok(P::Named(Mutability::Mutable, name.clone(), None)),
		E::Hole => Ok(P::Hole),
		E::Tuple(exprs) => {
			let pats: Result<Vec<_>, _> = exprs.iter().map(|e| rewrite_to_pattern(e)).collect();
			Ok(P::Tuple(pats?.into()))
		},
		E::Unit | E::Boolean(_) | E::Integer(_) => Ok(P::Expression(Box::new(expr.clone()))),
		E::BinOp(_) | E::PreOp(_) | E::PostOp(_) |
		E::Assign(..) => Err(ParseError::IllegalExpressionTerm("no operators allowed in patterns")),
		E::Return(_) => Err(ParseError::IllegalExpressionTerm("no returns allowed in patterns")),
		E::If(_) => Err(ParseError::IllegalExpressionTerm("no ifs allowed in patterns")),
		E::While(_) => Err(ParseError::IllegalExpressionTerm("no whiles allowed in patterns")),
		E::Let(_) => Err(ParseError::IllegalExpressionTerm("no lets allowed in patterns")),
		E::Stmt(_) => Err(ParseError::IllegalExpressionTerm("no statements allowed in patterns")),
		E::Block(_) => todo!("block patterns"),
		E::Call(_) => todo!("call patterns"),
	}
}

fn parse_pattern_or_expr<'a, I>(tokens: &mut Peekable<I>) -> Result<Expr, ParseError>
where I: Iterator<Item = &'a Token> {
	let lhs = parse_bin_expr(tokens)?;

	if take_kind(tokens, TokenKind::Assign).is_ok() {
		let rhs = parse_expr(tokens)?;
		validate_expr(&rhs)?;
		return Ok(Expr::Assign(rewrite_to_pattern(&lhs)?, rhs.into()));
	}
	return Ok(lhs);
}

fn parse_expr<'a, I>(tokens: &mut Peekable<I>) -> Result<Expr, ParseError>
where I: Iterator<Item = &'a Token> {
	let lhs = parse_pattern_or_expr(tokens)?;

	validate_expr(&lhs)?;
	return Ok(lhs);
}

#[derive(Debug)]
enum ExprAllowed {
	Always,
	Semicolon,
	EndOfBlock,
}

fn restrictions(expr: &Expr) -> ExprAllowed {
	use Expr as E;
	use ExprAllowed as A;
	match expr {
		// this is only here for patterns
		// but if it occurs we need to panic
		E::Hole => panic!("hole somehow managed to pass through to the end"),
		// literals at end of block only
		E::Integer(_) | E::Boolean(_) | E::Tuple(_) | E::Unit => A::EndOfBlock,
		// variable lookups at end of block only
		E::Lookup(_) => A::EndOfBlock,
		// operator expressions at end of block only
		E::PostOp(_) | E::PreOp(_) | E::BinOp(_) => A::EndOfBlock,

		// allowed with semicolon (or at end of block)
		E::Call(_) | E::Return(_) | E::Let(_) | E::Assign(..) => A::Semicolon,

		// blocks and block-requiring expressions are always allowed
		E::Block(_) | E::If(_) | E::While(_) => A::Always,

		// should not be reachable, but the compiler doesn't know that
		E::Stmt(_) => unreachable!(),
	}
}

fn parse_stmt<'a, I>(tokens: &mut Peekable<I>) -> Result<Expr, ParseError>
where I: Iterator<Item = &'a Token> {
	if tokens.peek().filter(|t| t.kind == TokenKind::Keyword(Keyword::Let)).is_some() {
		// let
		return Ok(Expr::Let(parse_let(tokens)?));
	}
	let expr = parse_expr(tokens)?;
	return Ok(expr);
}

fn parse_stmt_list<'a, I>(tokens: &mut Peekable<I>) -> Result<Box<[Expr]>, ParseError>
where I: Iterator<Item = &'a Token> {
	let mut list = Vec::new();

	loop {
		let stmt = parse_stmt(tokens)?;
		let allowed = restrictions(&stmt);
		if let Some(tok) = tokens.peek() {
			match (allowed, tok) {
				(ExprAllowed::Always, _) => {
					// take as many semicolons as desired
					if take_kind(tokens, TokenKind::SemiColon).is_ok() {
						// grab all of the semicolons
						while take_kind(tokens, TokenKind::SemiColon).is_ok() {}
						list.push(Expr::Stmt(Box::new(stmt)));
					} else {
						list.push(stmt);
					}
				}
				(ExprAllowed::Semicolon, Token { kind: TokenKind::SemiColon, offset: _ }) => {
					// grab all the semicolons
					while take_kind(tokens, TokenKind::SemiColon).is_ok() {}
					list.push(Expr::Stmt(Box::new(stmt)));
				}
				(ExprAllowed::Semicolon, Token { kind: TokenKind::RCurly, offset: _ }) => {
					// end of block
					list.push(stmt);
				}
				(ExprAllowed::Semicolon, _) => {
					// expected a semicolon
					return Err(ParseError::UnexpectedToken(tok.clone().clone(), ExpectedMessage::Kind(TokenKind::SemiColon)));
				}
				(ExprAllowed::EndOfBlock, Token { kind: TokenKind::RCurly, offset: _ }) => {
					// end of block
					list.push(stmt);
				}
				(ExprAllowed::EndOfBlock, _) => {
					// expected a rcurly
					return Err(ParseError::UnexpectedToken(tok.clone().clone(), ExpectedMessage::Kind(TokenKind::RCurly)));
				}
			}
		} else {
			return Err(ParseError::UnexpectedEndOfInput);
		}
		if tokens.peek().filter(|t| t.kind == TokenKind::RCurly).is_some() {
			// end of block
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
		let body = parse_stmt_list(tokens)?;
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

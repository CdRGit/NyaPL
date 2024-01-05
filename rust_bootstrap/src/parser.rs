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

#[derive(Debug)]
pub enum Expr {
	Block(BlockExpr),
	Integer(IntExpr),
	Boolean(bool),
	BinOp(BinExpr),
	PreOp(PreExpr),
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
pub struct BinExpr(InfixOps, Box<Expr>, Box<Expr>);

#[derive(Debug)]
pub struct PreExpr(PrefixOps, Box<Expr>);

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
	UnexpectedToken(Token, ExpectedMessage),
	UnexpectedEndOfInput,
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
		TokenKind::Keyword(Keyword::True) => { tokens.next(); Expr::Boolean(true) },
		TokenKind::Keyword(Keyword::False) => { tokens.next(); Expr::Boolean(false) },
		TokenKind::Keyword(Keyword::Return) => Expr::Return(parse_return(tokens)?),
		TokenKind::Keyword(Keyword::If) => Expr::If(parse_if(tokens)?),
		TokenKind::Identifier(name) => { tokens.next(); Expr::Lookup(LookupExpr(tok.clone(), name)) },
		TokenKind::LCurly => { Expr::Block(parse_block_expr(tokens)?) },
		_ => return Err(ParseError::UnexpectedToken(tok.clone().clone(), ExpectedMessage::Text("atom_expr"))),
	})
}

#[derive(Debug, Clone, Copy)]
enum PrefixOps {
	Identity,    // +
	Negate,      // -
	Not,         // !

	Reference,   // &
	Dereference, // *
}

#[derive(Debug, Clone, Copy)]
enum InfixOps {
	Add, // +
	Sub, // -
	Mult,// *
	Div, // /
	Mod, // %

	BitOr,   // |
	ShortOr, // ||
	BitAnd,  // &
	ShortAnd,// &&
	BitXor,  // ^

	Equality,      // ==
	NonEquality,   // !=
	GreaterThan,   // >
	LessThan,      // <
	GreaterOrEqual,// >=
	LessOrEqual,   // <=
}

#[derive(Debug, Clone, Copy)]
enum SuffixOps {
	// this only exists for testing purposes
	Yell, // !
	Query,// ?
}

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq)]
// later is tighter
enum Precedence {
	Compare,
}

#[derive(Debug, PartialEq, Eq)]
enum Associativity {
	Left,
	Right,
	None,
}

#[derive(Debug)]
enum ShuntOperator {
	GroupingParen(i64),
	CallingParen(i64),
	Infix {
		op: InfixOps,
		prec: Precedence,
		assoc: Associativity,
	},
	Prefix(PrefixOps),
}

fn apply_shunt_operator(output: &mut Vec::<Expr>, op: &ShuntOperator) {
	match op {
		ShuntOperator::Infix { op: op, prec: _, assoc: _ } => {
			let right = output.pop().unwrap();
			let left = output.pop().unwrap();
			let node = Expr::BinOp(BinExpr(*op, Box::new(left), Box::new(right)));
			output.push(node);
		},
		ShuntOperator::Prefix(op) => {
			let core = output.pop().unwrap();
			let node = Expr::PreOp(PreExpr(*op, Box::new(core)));
			output.push(node);
		},
		_ => todo!("{:?}", op),
	}
}

fn parse_expr<'a, I>(tokens: &mut Peekable<I>) -> Result<Expr, ParseError>
where I: Iterator<Item = &'a Token> {
	//let mut suffixes = VecDeque::new();
	let mut output = Vec::new();
	let mut shunt_stack = Vec::new();

	// FIXME: does not function correctly
	// required states:
	// Prefix
	// Suffix

	// Prefix
	//	=> try to match prefix
	//	 '(' => grouping parens, check for ) next to ensure the unit literal (if so, Suffix state),
	//	 otherwise push and no change
	//	 ? ok: push, no change
	//	 else: parse atom, Suffix state
	
	// Suffix
	//	=> try to match suffix
	//	 '(' => calling parens, Prefix state
	//	 ')' => shunting yard rules, no change
	//	 ',' => shunting yard rules, Prefix state
	//	 ? ok: push, no change
	//	 else: parse infix, Prefix state
	loop {
		// PREFIX state
		loop {
			// parse prefixes
			loop {
				shunt_stack.push(match tokens.peek() {
					Some(tok) => match tok.kind {
						TokenKind::Bang => ShuntOperator::Prefix(PrefixOps::Not),
						// default: break
						_ => break,
					},
					None => break,
				});
				tokens.next();
			}

			// parse prefix-lparen
			if take_kind(tokens, TokenKind::LParen).is_ok() {
				if take_kind(tokens, TokenKind::RParen).is_ok() {
					// we have ()
					// push a unit literal
					output.push(Expr::Unit);
					break;
				} else {
					shunt_stack.push(ShuntOperator::GroupingParen(1));
				}
			} else {
				break;
			}
		}
		// grab atom
		output.push(parse_atom_expr(tokens)?);
		// SUFFIX state
		loop {
			// parse suffix-lparen
			if take_kind(tokens, TokenKind::LParen).is_ok() {
				if take_kind(tokens, TokenKind::RParen).is_ok() {
					// we have ()
					// push an empty call
					let base = output.pop().unwrap();
					let call = Expr::Call(CallExpr(Box::new(base), Box::new([])));
					output.push(call);
					// we keep going
				} else {
					shunt_stack.push(ShuntOperator::CallingParen(1));
					// back to prefix state
					// TODO: make state explicit so this actually fucking works, lol
					break;
				}
			}
			// parse suffix-rparen
			if let Ok(tok) = take_kind(tokens, TokenKind::RParen) {
				// pop until we find a GroupingParen or a CallingParen
				loop {
					let op = shunt_stack.last();
					match op {
						Some(ShuntOperator::GroupingParen(arity)) => {
							if *arity == 1 {
								// regular shunting yard rparen
								shunt_stack.pop();
							} else {
								// tuple
								todo!();
							}
							break;
						},
						Some(ShuntOperator::CallingParen(arity)) => {
							// we now have an arity
							// that's the amount of arguments we need
							// we need to reverse-push them onto a value
							// each new argument is at the front
							let mut args = VecDeque::new();
							for _ in 0..*arity {
								args.push_front(output.pop().unwrap());
							}
							let base = output.pop().unwrap();
							let call = Expr::Call(CallExpr(Box::new(base), Vec::from(args).into()));
							output.push(call);
							shunt_stack.pop();
							break;
						},
						Some(operator) => {
							// apply
							apply_shunt_operator(&mut output, operator);
							shunt_stack.pop();
						},
						None => return Err(ParseError::UnexpectedToken(tok, ExpectedMessage::Text("shunting"))),
					}
				}
				// suffix-rparen forces us back to getting more suffixes
				continue;
			}
			// no suffix found, back to infixes
			break;
		}
		// grab (optional) infix
		// or a comma
		if let Ok(tok) = take_kind(tokens, TokenKind::Comma) {
			loop {
				let op = shunt_stack.last_mut();
				match op {
					Some(ShuntOperator::GroupingParen(arity)) => {
						*arity += 1;
						break;
					},
					Some(ShuntOperator::CallingParen(arity)) => {
						*arity += 1;
						break;
					},
					Some(operator) => {
						// apply
						apply_shunt_operator(&mut output, operator);
						shunt_stack.pop();
					},
					None => return Err(ParseError::UnexpectedToken(tok, ExpectedMessage::Text("shunting"))),
				}
			}

		} else {
			let infix = match tokens.peek() {
				Some(tok) => match tok.kind {
					TokenKind::EqEq => (InfixOps::Equality, Precedence::Compare, Associativity::None),
					// default: break
					_ => break,
				},
				None => break,
			};
			tokens.next();

			// shunting yard algorithm time
			// infix = o1
			while let Some(o2) = shunt_stack.last() {
				match o2 {
					ShuntOperator::CallingParen(_) | ShuntOperator::GroupingParen(_) => break,
					ShuntOperator::Infix {op: op, prec: prec, assoc: assoc} => {
						match prec.cmp(&infix.1) {
							// equal and not-left-associative, break
							Ordering::Equal if infix.2 != Associativity::Left => break,
							Ordering::Greater | Ordering::Equal => {
								// pop and apply
								apply_shunt_operator(&mut output, o2);
								shunt_stack.pop();
							}
							_ => break,
						}
					}
					ShuntOperator::Prefix(_) => {
						apply_shunt_operator(&mut output, o2);
						shunt_stack.pop();
					}
				}
			}
			shunt_stack.push(ShuntOperator::Infix {op: infix.0, prec: infix.1, assoc: infix.2});
		}
	}

	// apply full shunt_stack
	while let Some(o) = shunt_stack.pop() {
		apply_shunt_operator(&mut output, &o);
	}

	if output.len() != 1 {
		panic!("{:#?} shunting yard went horribly wrong", output);
	}
	Ok(output.pop().unwrap())
}

fn parse_expr_list<'a, I>(tokens: &mut Peekable<I>) -> Result<Box<[Expr]>, ParseError>
where I: Iterator<Item = &'a Token> {
	let mut list = Vec::new();

	loop {
		let expr = parse_expr(tokens)?;
		// no semicolon
		if take_kind(tokens, TokenKind::SemiColon).is_err() && !matches!(expr, Expr::If(_)) {
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
			_ => return Err(ParseError::UnexpectedToken(tok.clone().clone(), ExpectedMessage::Text("parse_top_level"))),
		});
	}
	return Ok(ParseTree { toplevel: toplevel.into() });
}

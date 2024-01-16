use crate::lexer::{Token, TokenKind, Keyword};
use crate::source_span::SourceSpan;

use crate::ast::{*};

use std::rc::Rc;
use std::iter::Peekable;
use std::cmp::Ordering;
use std::error::Error;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum ParseError {
	UnexpectedToken(Token, &'static str),
	UnexpectedEndOfInput(&'static str),
	AmbiguousOperatorOrder(SourceSpan),
	IllegalExpressionTerm(SourceSpan),
	IllegalPatternTerm(SourceSpan),
}

#[derive(Debug, Clone)]
enum AssignPatternOrExpr {
	Expr(Expr),
	Pattern(AssignPattern),
	Named(SourceSpan, Rc<str>),
	Grouped(SourceSpan, Box<[AssignPatternOrExpr]>),
}

impl Error for ParseError {}

impl Display for ParseError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "Parser Error")
	}
}

fn take<'a, I, F, U>(tokens: &mut Peekable<I>, selector: F, message: &'static str) -> Result<U, ParseError>
where I: Iterator<Item = &'a Token>, F: Fn (&&Token) -> Result<U, ParseError> {
	let tok = tokens.peek().ok_or(ParseError::UnexpectedEndOfInput(message))?;
	let ret = selector(tok)?;
	tokens.next();
	return Ok(ret);
}

fn take_kind<'a, I>(tokens: &mut Peekable<I>, kind: TokenKind, message: &'static str) -> Result<Token, ParseError>
where I: Iterator<Item = &'a Token> {
	return take(tokens, |t| if t.kind == kind { Ok(t.clone().clone()) } else { Err(ParseError::UnexpectedToken(t.clone().clone(), message)) }, message);
}

fn parse_effect<'a, I>(tokens: &mut Peekable<I>) -> Result<Effect, ParseError>
where I: Iterator<Item = &'a Token> {
	Ok(match tokens.next() {
		Some(Token {kind: TokenKind::Identifier(name), span})
			=> Effect{span: span.clone(), val: EffectKind::Named(name.clone())},
		Some(tok) => return Err(ParseError::UnexpectedToken(tok.clone(), "effect")),
		None => return Err(ParseError::UnexpectedEndOfInput("effect")),
	})
}

fn parse_type<'a, I>(tokens: &mut Peekable<I>) -> Result<Type, ParseError>
where I: Iterator<Item = &'a Token> {
	Ok(match tokens.next() {
		Some(Token {kind: TokenKind::Identifier(name), span}) => Type{span: span.clone(), val: TypeKind::Named(name.clone())},
		Some(Token {kind: TokenKind::Keyword(Keyword::Hole), span}) => Type{span: span.clone(), val: TypeKind::Hole},
		Some(Token {kind: TokenKind::Bang, span}) => Type{span: span.clone(), val: TypeKind::Never},
		Some(Token {kind: TokenKind::LParen, span}) => {
			let mut params = Vec::new();
			let end_span = if let Ok(Token {span: n_span, ..}) = take_kind(tokens, TokenKind::RParen, "`)`") {
				// return unit type if we immediately hit an RParen
				return Ok(Type{span: span.extend(&n_span).unwrap(), val: TypeKind::Unit});
			} else {
				let end_span;
				loop {
					params.push(parse_type(tokens)?);
					if let Ok(Token {span, ..}) = take_kind(tokens, TokenKind::RParen, "`)`") {
						end_span = span;
						break;
					}
					take_kind(tokens, TokenKind::Comma, "`,`")?;
				}
				end_span.clone()
			};
			return Ok(Type{span: span.extend(&end_span).unwrap(), val: TypeKind::Tuple(params.into())});
		},
		Some(Token {kind: TokenKind::Keyword(Keyword::Function), span}) => {
			// function pointer
			// grab LParen
			take_kind(tokens, TokenKind::LParen, "`(`")?;
			let mut params = Vec::new();
			let end_span = if let Ok(Token {span, ..}) = take_kind(tokens, TokenKind::RParen, "`)`") {
				// end early if we immediately hit an RParen
				span
			} else {
				let end_span;
				loop {
					params.push(parse_type(tokens)?);
					if let Ok(Token {span, ..}) = take_kind(tokens, TokenKind::RParen, "`)`") {
						end_span = span;
						break;
					}
					take_kind(tokens, TokenKind::Comma, "`,`")?;
				}
				end_span.clone()
			};
			let ret_type = parse_typetag(tokens)?;
			return Ok(Type { span: span.extend(&end_span).unwrap(), val: TypeKind::Function(params.into(), ret_type.into())});
		},
		Some(tok) => return Err(ParseError::UnexpectedToken(tok.clone(), "type")),
		None => return Err(ParseError::UnexpectedEndOfInput("type")),
	})
}

fn parse_typetag<'a, I>(tokens: &mut Peekable<I>) -> Result<Type, ParseError>
where I: Iterator<Item = &'a Token> {
	take_kind(tokens, TokenKind::Colon, "Expected `:`")?;
	return parse_type(tokens);
}

fn parse_return<'a, I>(tokens: &mut Peekable<I>) -> Result<Stmt, ParseError>
where I: Iterator<Item = &'a Token> {
	let kw = tokens.next().unwrap();
	let expr = parse_expr(tokens)?;
	Ok(Stmt {span: kw.span.extend(&expr.span).unwrap(), val: StmtKind::Return(expr)})
}

fn parse_let_pattern<'a, I>(tokens: &mut Peekable<I>) -> Result<LetPattern, ParseError>
where I: Iterator<Item = &'a Token> {
	match tokens.next() {
		Some(Token { kind: TokenKind::Keyword(Keyword::Mutable), span: start_span }) => {
			match tokens.next() {
				Some(Token { kind: TokenKind::Identifier(n), span: end_span}) => {
					let type_ = if tokens.peek().filter(|t| t.kind == TokenKind::Colon).is_some() {
						Some(parse_typetag(tokens)?)
					} else {
						None
					};
					Ok(LetPattern {span: start_span.extend(end_span).unwrap(), val: LetPatternKind::Named(Mutability::Mutable, n.clone(), type_)})
				},
				Some(t) => Err(ParseError::UnexpectedToken(t.clone(), "Expected pattern name")),
				None => Err(ParseError::UnexpectedEndOfInput("pattern name")),
			}
		},
		Some(Token { kind: TokenKind::LParen, span: start_span}) => {
			let mut params = Vec::new();
			let end_span = if let Ok(Token {span, ..}) = take_kind(tokens, TokenKind::RParen, "`)`") {
				// end early if we immediately hit an RParen
				span
			} else {
				let end_span;
				loop {
					params.push(parse_let_pattern(tokens)?);
					if let Ok(Token {span, ..}) = take_kind(tokens, TokenKind::RParen, "`)`") {
						end_span = span;
						break;
					}
					take_kind(tokens, TokenKind::Comma, "`,`")?;
				}
				end_span
			};
			return Ok(LetPattern {span: start_span.extend(&end_span).unwrap(), val: LetPatternKind::Tuple(params.into())});
		},
		Some(Token { kind: TokenKind::Identifier(n), span}) => {
			let (type_, span) = if tokens.peek().filter(|t| t.kind == TokenKind::Colon).is_some() {
				let type_ = parse_typetag(tokens)?;
				let span = span.extend(&type_.span).unwrap();

				(Some(type_), span)
			} else {
				(None, span.clone())
			};
			Ok(LetPattern {span, val: LetPatternKind::Named(Mutability::Immutable, n.clone(), type_)})
		},
		Some(Token { kind: TokenKind::Keyword(Keyword::Hole), span}) =>
		{
			let (type_, span) = if tokens.peek().filter(|t| t.kind == TokenKind::Colon).is_some() {
				let type_ = parse_typetag(tokens)?;

				let span = span.extend(&type_.span).unwrap();

				(Some(type_), span)
			} else {
				(None, span.clone())
			};
			Ok(LetPattern {span, val: LetPatternKind::Hole(type_)})
		},
		Some(t) => Err(ParseError::UnexpectedToken(t.clone(), "pattern")),
		None => Err(ParseError::UnexpectedEndOfInput("pattern")),
	}
}

fn parse_if<'a, I>(tokens: &mut Peekable<I>) -> Result<(SourceSpan, If), ParseError>
where I: Iterator<Item = &'a Token> {
	let kw = tokens.next().unwrap();
	let cond = parse_expr(tokens)?;
	let (span, body) = parse_scope_block(tokens)?;
	// else / elif block?
	let (span, _else) = match tokens.peek() {
		Some(Token {kind: TokenKind::Keyword(Keyword::Else), ..}) =>
		{ tokens.next(); let (span, scope) = parse_scope_block(tokens)?; (span.clone(), Some(Expr {span, val: ExprKind::Scope(scope)}.into())) },
		Some(Token {kind: TokenKind::Keyword(Keyword::Elif), ..}) =>
		{ let (span, _if) = parse_if(tokens)?; (span.clone(), Some(Expr {span, val: ExprKind::If(_if)}.into())) },
		_ => (span, None),
	};

	return Ok((kw.span.extend(&span).unwrap(), If {condition: cond.into(), body, _else }));
}

fn parse_while<'a, I>(tokens: &mut Peekable<I>) -> Result<(SourceSpan, While), ParseError>
where I: Iterator<Item = &'a Token> {
	let kw = tokens.next().unwrap();
	let cond = parse_expr(tokens)?;
	let (end, body) = parse_scope_block(tokens)?;
	return Ok((kw.span.extend(&end).unwrap(), While {condition: cond.into(), body} ));
}

fn parse_let<'a, I>(tokens: &mut Peekable<I>) -> Result<Stmt, ParseError>
where I: Iterator<Item = &'a Token> {
	let kw = tokens.next().unwrap();
	let pattern = parse_let_pattern(tokens)?;
	take_kind(tokens, TokenKind::Assign, "Expected `=`")?;
	let expr = parse_expr(tokens)?;
	Ok(Stmt { span: kw.span.extend(&expr.span).unwrap(), val: StmtKind::Let(pattern, expr) })
}

fn parse_scope_block<'a, I>(tokens: &mut Peekable<I>) -> Result<(SourceSpan, Scope), ParseError>
where I: Iterator<Item = &'a Token> {
	let start = take_kind(tokens, TokenKind::LCurly, "Expected `{`")?.span;
	// function bodies can first have as many child / helper functions as they want
	let (body, end) = if let Ok(Token{span, ..}) = take_kind(tokens, TokenKind::RCurly, "Expected `}`") {
		// implied Unit expression
		(Scope {body: Box::new([]), value: None}, span)
	} else {
		let body = parse_scope(tokens)?;
		let span = take_kind(tokens, TokenKind::RCurly, "Expected `}`")?.span;
		(body, span)
	};
	return Ok((start.extend(&end).unwrap(), body));
}

fn parse_atom_expr_or_pattern<'a, I>(tokens: &mut Peekable<I>) -> Result<AssignPatternOrExpr, ParseError>
where I: Iterator<Item = &'a Token> {
	use AssignPatternOrExpr as AP;
	let tok = tokens.peek().ok_or(ParseError::UnexpectedEndOfInput("expression"))?.clone().clone();
	Ok(match tok.kind.clone() {
		TokenKind::Keyword(Keyword::Hole) => {
			let span = tokens.next().unwrap().span.clone();
			AP::Pattern(AssignPattern {span, val: AssignPatternKind::Hole})
		},
		TokenKind::Integer(val) => {
			let span = tokens.next().unwrap().span.clone();
			AP::Expr(Expr {span, val: ExprKind::Integer(val)})
		},
		TokenKind::Keyword(Keyword::True) => {
			let span = tokens.next().unwrap().span.clone();
			AP::Expr(Expr {span, val: ExprKind::Boolean(true)})
		},
		TokenKind::Keyword(Keyword::False) => {
			let span = tokens.next().unwrap().span.clone();
			AP::Expr(Expr {span, val: ExprKind::Boolean(false)})
		},
		TokenKind::Keyword(Keyword::If) => {
			let (span, _if) = parse_if(tokens)?;
			AP::Expr(Expr {span, val: ExprKind::If(_if)})
		},
		TokenKind::Keyword(Keyword::While) => {
			let (span, _while) = parse_while(tokens)?;
			AP::Expr(Expr {span, val: ExprKind::While(_while)})
		},
		TokenKind::Identifier(name) => {
			let span = tokens.next().unwrap().span.clone();
			AP::Named(span, name)
		},
		TokenKind::LCurly => {
			let (span, scope) = parse_scope_block(tokens)?;
			AP::Expr(Expr {span, val: ExprKind::Scope(scope)})
		},
		TokenKind::LParen => {
			// unit / parenthesized / tuple
			let span = tokens.next().unwrap().span.clone();
			let mut args = Vec::new();
			// does this call have args?
			let end_span = if let Ok(Token {span, .. }) = take_kind(tokens, TokenKind::RParen, "`)`") {
				span
			} else {
				loop {
					args.push(parse_pattern_or_expr(tokens)?);
					if take_kind(tokens, TokenKind::Comma, "`,`").is_err() {
						// we need to have an rparen here now
						break take_kind(tokens, TokenKind::RParen, "`)`")?.span;
					}
				}
			};
			AP::Grouped(span.extend(&end_span).unwrap(), args.into())
		},
		_ => return Err(ParseError::UnexpectedToken(tok.clone().clone(), "expression")),
	})
}

fn parse_suffix_expr_or_pattern<'a, I>(tokens: &mut Peekable<I>) -> Result<AssignPatternOrExpr, ParseError>
where I: Iterator<Item = &'a Token> {
	use AssignPatternOrExpr as AP;
	// parse core item
	let mut core = parse_atom_expr_or_pattern(tokens)?;

	// parse suffixes
	loop {
		let tok = tokens.peek();
		let tok = match tok {
			None => break,
			Some(t) => t.clone(),
		};
		match tok.kind {
			TokenKind::Bang => {
				tokens.next();
				let expr = rewrite_to_expr(&core)?;
				core = AP::Expr(Expr { span: expr.span.extend(&tok.span).unwrap(), val: ExprKind::Suffix(expr.into(), SuffixOp {span: tok.span.clone(), val: Suffix::Yell }) });
			}
			TokenKind::Question => {
				tokens.next();
				let expr = rewrite_to_expr(&core)?;
				core = AP::Expr(Expr { span: expr.span.extend(&tok.span).unwrap(), val: ExprKind::Suffix(expr.into(), SuffixOp {span: tok.span.clone(), val: Suffix::Query }) });
			}
			TokenKind::LParen => {
				// call
				tokens.next();
				let mut args = Vec::new();
				let expr = rewrite_to_expr(&core)?;
				// does this call have args?
				let end_span = if let Ok(Token {span, ..}) = take_kind(tokens, TokenKind::RParen, "`)`") {
					span
				} else {
					loop {
						args.push(parse_expr(tokens)?);
						if take_kind(tokens, TokenKind::Comma, "`,`").is_err() {
							// we need to have an rparen here now
							break take_kind(tokens, TokenKind::RParen, "`)`")?.span;
						}
					}
				};
				core = AP::Expr(Expr {span: expr.span.extend(&end_span).unwrap(), val: ExprKind::Call(Box::new(expr), args.into())});
			},
			_ => break,
		}
	}
	Ok(core)
}

fn parse_prefix_expr_or_pattern<'a, I>(tokens: &mut Peekable<I>) -> Result<AssignPatternOrExpr, ParseError>
where I: Iterator<Item = &'a Token> {
	// parse prefixes onto a stack
	let mut prefix_stack = Vec::new();
	loop {
		let tok = tokens.peek();
		let prefix = match tok {
			None => break,
			Some(tok) => {
				match tok.kind {
					TokenKind::Plus => Prefix::Identity,
					TokenKind::Minus => Prefix::Negate,
					TokenKind::Bang => Prefix::Not,

					TokenKind::Star => Prefix::Dereference,
					TokenKind::And => Prefix::Reference,
					TokenKind::AndAnd => {
						prefix_stack.push(PrefixOp {span: tok.span.clone(), val: Prefix::Reference});
						Prefix::Reference
					},
					_ => break,
				}
			},
		};
		prefix_stack.push(PrefixOp {span: tok.unwrap().span.clone(), val: prefix});
		tokens.next();
	}

	// parse core item
	let mut core = parse_suffix_expr_or_pattern(tokens)?;

	// apply prefixes
	while let Some(prefix) = prefix_stack.pop() {
		let expr = rewrite_to_expr(&core)?;
		core = AssignPatternOrExpr::Expr(Expr {span: expr.span.extend(&prefix.span).unwrap(), val: ExprKind::Prefix(prefix, expr.into())});
	}

	Ok(core)
}

fn parse_bin_exprs_or_patterns<'a, I>(tokens: &mut Peekable<I>) -> Result<(AssignPatternOrExpr, Box<[(InfixOp, Expr)]>), ParseError>
where I: Iterator<Item = &'a Token> {
	let leftmost = parse_prefix_expr_or_pattern(tokens)?;
	let mut right = Vec::new();
	// parse infixes
	loop {
		let tok = tokens.peek();
		let tok = match tok {
			None => break,
			Some(t) => t.clone(),
		};
		let infix = match tok.kind {
			// math
			TokenKind::Plus => Infix::Add,
			TokenKind::Minus => Infix::Subtract,
			TokenKind::Star => Infix::Multiply,
			TokenKind::Slash => Infix::Divide,
			TokenKind::Percent => Infix::Modulo,
			// comparisons
			TokenKind::EqEq => Infix::Equal,
			TokenKind::BangEq => Infix::NotEqual,
			TokenKind::Greater => Infix::GreaterThan,
			TokenKind::GreaterEq => Infix::GreaterThanEqual,
			TokenKind::Lesser => Infix::LessThan,
			TokenKind::LesserEq => Infix::LessThanEqual,
			// bitshifts
			TokenKind::GreaterGreater => Infix::ShiftRight,
			TokenKind::LesserLesser => Infix::ShiftLeft,
			// bitwise logic
			TokenKind::Pipe => Infix::BitOr,
			TokenKind::Caret => Infix::BitXor,
			TokenKind::And => Infix::BitAnd,
			// short-circuit logic
			TokenKind::PipePipe => Infix::LogicOr,
			TokenKind::AndAnd => Infix::LogicAnd,
			_ => break,
		};
		tokens.next();
		let rhs = parse_prefix_expr_or_pattern(tokens)?;
		let rhs = rewrite_to_expr(&rhs)?;
		let op = InfixOp {span: tok.span.clone(), val: infix};
		right.push((op, rhs))
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

fn get_precedence(infix: &Infix) -> Precedence {
	use Infix as I;
	use Precedence as P;
	match infix {
		I::LogicOr  => P::LogicOr,
		I::LogicAnd => P::LogicAnd,
		I::Equal
			| I::NotEqual
			| I::LessThan
			| I::GreaterThan
			| I::LessThanEqual
			| I::GreaterThanEqual => P::Comparison,
		I::BitOr  => P::BitOr,
		I::BitXor => P::BitXor,
		I::BitAnd => P::BitAnd,
		I::ShiftLeft | I::ShiftRight => P::BitShift,
		I::Add | I::Subtract => P::Additive,
		I::Multiply | I::Divide | I::Modulo => P::Multiplicative,
	}
}

enum Associativity {
	Left,
	Right,
	Explicit,
}

fn get_associavity(infix: &Infix) -> Associativity {
	use Infix as I;
	use Associativity as A;
	match infix {
		I::LogicOr  => A::Left,
		I::LogicAnd => A::Left,
		I::Equal
			| I::NotEqual
			| I::LessThan
			| I::GreaterThan
			| I::LessThanEqual
			| I::GreaterThanEqual => A::Explicit,
		I::BitOr  => A::Left,
		I::BitXor => A::Left,
		I::BitAnd => A::Left,
		I::ShiftLeft | I::ShiftRight => A::Left,
		I::Add | I::Subtract => A::Left,
		I::Multiply | I::Divide | I::Modulo => A::Left,
	}
}

fn parse_bin_expr<'a, I>(tokens: &mut Peekable<I>) -> Result<AssignPatternOrExpr, ParseError>
where I: Iterator<Item = &'a Token> {
	let (lhs, bins) = parse_bin_exprs_or_patterns(tokens)?;
	if bins.len() == 0 {
		return Ok(lhs);
	}

	let lhs = rewrite_to_expr(&lhs)?;

	let mut working_stack = Vec::new();
	working_stack.push(lhs);

	let mut operator_stack: Vec<InfixOp> = Vec::new();
	for pair in bins.into_iter() {
		let prec = get_precedence(&pair.0.val);
		let assoc = get_associavity(&pair.0.val);
		while {
			if let Some(op) = operator_stack.last() {
				let prec2 = get_precedence(&op.val);
				match prec2.cmp(&prec) {
					Ordering::Less => false,
					Ordering::Equal => match assoc {
						Associativity::Left => true,
						Associativity::Right => false,
						Associativity::Explicit => return Err(ParseError::AmbiguousOperatorOrder(pair.0.span.clone())),
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
			working_stack.push(Expr {span: left.span.extend(&right.span).unwrap(), val: ExprKind::Infix(left.into(), op.clone(), right.into())});
		}
		operator_stack.push(pair.0.clone());
		working_stack.push(pair.1.clone());
	}

	while let Some(op) = operator_stack.pop() {
		let right = working_stack.pop().unwrap();
		let left = working_stack.pop().unwrap();
		working_stack.push(Expr {span: left.span.extend(&right.span).unwrap(), val: ExprKind::Infix(left.into(), op.clone(), right.into())});
	}

	match &working_stack[..] {
		[expr] => Ok(AssignPatternOrExpr::Expr(expr.clone())),
		_ => unreachable!("shunting yard should ensure the working stack has a length of 1 at the end"),
	}
}

fn parse_pattern_or_expr<'a, I>(tokens: &mut Peekable<I>) -> Result<AssignPatternOrExpr, ParseError>
where I: Iterator<Item = &'a Token> {
	let lhs = parse_bin_expr(tokens)?;

	if take_kind(tokens, TokenKind::Assign, "`=`").is_ok() {
		let rhs = parse_expr(tokens)?;
		let lhs = rewrite_to_pattern(&lhs)?;
		let span = lhs.span.extend(&rhs.span).unwrap();
		return Ok(AssignPatternOrExpr::Expr(Expr{span, val: ExprKind::Assign(lhs, rhs.into())}));
	}
	return Ok(lhs);
}

fn rewrite_to_expr(potential_expr: &AssignPatternOrExpr) -> Result<Expr, ParseError> {
	use AssignPatternOrExpr as AP;
	match potential_expr {
		AP::Expr(e) => Ok(e.clone()),
		AP::Pattern(p) => Err(ParseError::IllegalExpressionTerm(p.span.clone())),
		AP::Named(span, name) => Ok(Expr {span: span.clone(), val: ExprKind::Lookup(name.clone())}),
		AP::Grouped(span, vals) => {
			let mut exprs = Vec::new();
			for val in vals.into_iter() {
				exprs.push(rewrite_to_expr(val)?);
			}

			match exprs.len() {
				0 => Ok(Expr {span: span.clone(), val: ExprKind::Unit}),
				1 => Ok(exprs[0].clone()),
				_ => Ok(Expr {span: span.clone(), val: ExprKind::Tuple(exprs.into()) }),
			}
		}
	}
}

fn rewrite_to_pattern(potential_expr: &AssignPatternOrExpr) -> Result<AssignPattern, ParseError> {
	use AssignPatternOrExpr as AP;
	match potential_expr {
		AP::Expr(e) => Err(ParseError::IllegalPatternTerm(e.span.clone())),
		AP::Pattern(p) => Ok(p.clone()),
		AP::Named(span, name) => Ok(AssignPattern {span: span.clone(), val: AssignPatternKind::Named(name.clone())}),
		AP::Grouped(span, vals) => {
			let mut pats = Vec::new();
			for val in vals.into_iter() {
				pats.push(rewrite_to_pattern(val)?);
			}

			Ok(AssignPattern {span: span.clone(), val: AssignPatternKind::Tuple(pats.into())})
		}
	}
}

fn parse_expr<'a, I>(tokens: &mut Peekable<I>) -> Result<Expr, ParseError>
where I: Iterator<Item = &'a Token> {
	let lhs = parse_pattern_or_expr(tokens)?;

	let lhs = rewrite_to_expr(&lhs)?;
	return Ok(lhs);
}

#[derive(Debug)]
enum ExprAllowed {
	Always,
	Semicolon,
	EndOfBlock,
}

fn restrictions(expr: &ExprKind) -> ExprAllowed {
	use ExprKind as E;
	use ExprAllowed as A;
	match expr {
		// literals at end of block only
		E::Integer(_) | E::Boolean(_) | E::Tuple(_) | E::Unit => A::EndOfBlock,
		// variable lookups at end of block only
		E::Lookup(_) => A::EndOfBlock,
		// operator expressions at end of block only
		E::Suffix(..) | E::Prefix(..) | E::Infix(..) => A::EndOfBlock,

		// allowed with semicolon (or at end of block)
		E::Call(..) | E::Assign(..) => A::Semicolon,

		// blocks and block-requiring expressions are always allowed
		E::Scope(_) | E::If(_) | E::While(_) => A::Always,
	}
}

fn parse_stmt<'a, I>(tokens: &mut Peekable<I>) -> Result<Stmt, ParseError>
where I: Iterator<Item = &'a Token> {
	if tokens.peek().filter(|t| t.kind == TokenKind::Keyword(Keyword::Let)).is_some() {
		// let
		return Ok(parse_let(tokens)?);
	} else if tokens.peek().filter(|t| t.kind == TokenKind::Keyword(Keyword::Return)).is_some() {
		// let
		return Ok(parse_return(tokens)?);
	}
	let expr = parse_expr(tokens)?;
	return Ok(Stmt{span: expr.span.clone(), val: StmtKind::Expr(expr)});
}

fn parse_scope<'a, I>(tokens: &mut Peekable<I>) -> Result<Scope, ParseError>
where I: Iterator<Item = &'a Token> {
	let mut list = Vec::new();
	let mut value = None;

	loop {
		let stmt = parse_stmt(tokens)?;
		match stmt.val.clone() {
			StmtKind::Expr(e) => {
				// the olden way
				let restriction = restrictions(&e.val);
				match (restriction, tokens.peek()) {
					(_, Some(Token { kind: TokenKind::RCurly, ..})) => {
						// end of block
						// put it as the value
						value = Some(e.into());
					},
					(ExprAllowed::Semicolon, Some(Token { kind: TokenKind::SemiColon, ..})) => {
						take_kind(tokens, TokenKind::SemiColon, "`;`")?;
						while let Ok(_) = take_kind(tokens, TokenKind::SemiColon, "`;`") {}
						list.push(stmt);
					},
					(ExprAllowed::Always, _) => {
						// consume as many semicolons as possible
						while let Ok(_) = take_kind(tokens, TokenKind::SemiColon, "`;`") {}
						list.push(stmt);
					},
					(ExprAllowed::EndOfBlock, Some(t)) => {
						return Err(ParseError::UnexpectedToken((*t).clone(), "`}`"));
					},
					(ExprAllowed::Semicolon, Some(t)) => {
						return Err(ParseError::UnexpectedToken((*t).clone(), "`;` or `}`"));
					},
					(ExprAllowed::EndOfBlock, None) => return Err(ParseError::UnexpectedEndOfInput("`}`")),
					(_, None) => return Err(ParseError::UnexpectedEndOfInput("`;` or `}`")),
				}
			},
			StmtKind::Return(_) | StmtKind::Let(..) => {
				take_kind(tokens, TokenKind::SemiColon, "`;`")?;
				while let Ok(_) = take_kind(tokens, TokenKind::SemiColon, "`;`") {}
				list.push(stmt);
			},
		}
		if tokens.peek().filter(|t| t.kind == TokenKind::RCurly).is_some() {
			// end of block
			break;
		}
	}

	return Ok(Scope{body: list.into(), value});
}

fn parse_function_body<'a, I>(tokens: &mut Peekable<I>) -> Result<(Box<[Function]>, Scope, SourceSpan), ParseError>
where I: Iterator<Item = &'a Token> {
	take_kind(tokens, TokenKind::LCurly, "`}`")?;
	// function bodies can first have as many child / helper functions as they want
	let mut children = Vec::new();
	while let Some(tok) = tokens.peek() {
		children.push(match tok.kind {
			TokenKind::Keyword(Keyword::Function)
				=> parse_function(tokens)?,
			_ => break,
		});
	}
	let (body, span) = if let Ok(Token {span, ..}) = take_kind(tokens, TokenKind::RCurly, "`}`") {
		// empty body
		(Scope {
			body: Box::new([]),
			value: None,
		}, span)
	} else {
		let body = parse_scope(tokens)?;
		let span = take_kind(tokens, TokenKind::RCurly, "`}`")?.span;
		(body, span)
	};
	return Ok((children.into(), body, span));
}

fn parse_param<'a, I>(tokens: &mut Peekable<I>) -> Result<Param, ParseError>
where I: Iterator<Item = &'a Token> {
	match tokens.next() {
		Some(Token { kind: TokenKind::Keyword(Keyword::Hole), span}) => {
			let _type = parse_typetag(tokens)?;
			Ok(Param { span: span.clone(), val: ParamKind::Ignored(_type)})
		},
		Some(Token { kind: TokenKind::Identifier(name), span}) => {
			let _type = parse_typetag(tokens)?;
			Ok(Param { span: span.clone(), val: ParamKind::Named(name.clone(), _type)})
		},
		Some(t) => Err(ParseError::UnexpectedToken(t.clone(), "parameter")),
		None => Err(ParseError::UnexpectedEndOfInput("parameter")),
	}
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
		if let Token {kind: TokenKind::Identifier(n), ..} = t {
			Ok(n.clone())
		} else {
			Err(ParseError::UnexpectedToken(t.clone().clone(), "function name"))
		}
	}, "function name")?;
	// parameters
	let mut params = Vec::new();
	take_kind(tokens, TokenKind::LParen, "`(`")?;

	if take_kind(tokens, TokenKind::RParen, "`)`").is_err() {
		loop {
			params.push(parse_param(tokens)?);
			if take_kind(tokens, TokenKind::RParen, "`)`").is_ok() {
				break;
			}
			take_kind(tokens, TokenKind::Comma, "`,`")?;
		}
	}
	// end of params

	let return_type = parse_typetag(tokens)?;

	let (helpers, body, span) = parse_function_body(tokens)?;
	return Ok(Function { span: kw.span.extend(&span).unwrap(), effects: effects.into(), name, params: params.into(), return_type, helpers, body });
}

pub fn parse(tokens: &[Token]) -> Result<AST, ParseError> {
	let mut iter = tokens.iter().peekable();
	let mut functions = Vec::new();

	while let Some(tok) = iter.peek() {
		match tok.kind {
			TokenKind::Keyword(Keyword::Function) => {
				let func = parse_function(&mut iter)?;
				functions.push(func);
			}
			_ => return Err(ParseError::UnexpectedToken(tok.clone().clone(), "`fn` keyword")),
		}
	}
	return Ok(AST { functions: functions.into() });
}

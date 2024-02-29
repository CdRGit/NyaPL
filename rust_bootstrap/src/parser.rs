use crate::diagnostic::{Diagnostic, Severity};
use crate::lexer::{Token, TokenKind, Keyword};
use crate::source_span::SourceSpan;

use crate::ast::{*};

use std::rc::Rc;
use std::cmp::Ordering;

#[derive(Debug, Clone)]
enum AssignPatternOrExpr {
	Error,
	Expr(Expr),
	Pattern(AssignPattern),
	Named(SourceSpan, Rc<str>),
	Grouped(SourceSpan, Box<[AssignPatternOrExpr]>),
}

struct ParseContext<'a> {
	diagnostics: Vec<Diagnostic>,
	tokens: &'a [Token],
	offset: usize,
}

impl<'a> ParseContext<'a> {
	fn take(&mut self, kind: TokenKind) -> Result<Token, Diagnostic> {
		if self.at_end() {
			let msg = format!("Expected {:#?}, got EOF", kind);
			return Err(Diagnostic::simple_error(msg.into(), self.location()));
		}
		if self.current().unwrap().kind == kind {
			Ok(self.next().unwrap())
		} else {
			let msg = format!("Expected {:#?}, got {:#?}", kind, self.current());
			return Err(Diagnostic::simple_error(msg.into(), self.location()));
		}
	}

	fn next(&mut self) -> Option<Token> {
		if self.at_end() {
			return None;
		}
		self.offset += 1;
		Some(self.previous().unwrap())
	}

	fn report_return<T>(&mut self, diag: Diagnostic, t: T) -> AstNode<T> {
		self.report(diag);
		AstNode { val: t, span: self.location() }
	}

	fn report(&mut self, diag: Diagnostic) {
		self.diagnostics.push(diag);
	}

	fn at(&self, kind: TokenKind) -> bool {
		(!self.at_end()) && self.current().unwrap().kind == kind
	}

	fn at_end(&self) -> bool {
		self.offset == self.tokens.len()
	}

	fn current(&self) -> Option<Token> {
		if self.at_end() {
			None
		} else {
			Some(self.tokens[self.offset].clone())
		}
	}

	fn previous(&self) -> Option<Token> {
		if self.offset == 0 {
			None
		} else {
			Some(self.tokens[self.offset - 1].clone())
		}
	}

	fn location(&mut self) -> SourceSpan {
		if self.at_end() {
			self.previous().unwrap().span
		} else {
			self.current().unwrap().span
		}
	}
}

fn parse_effect<'a>(ctx: &mut ParseContext<'a>) -> Effect {
	match ctx.next() {
		Some(Token {kind: TokenKind::Identifier(name), span})
			=> Effect{span: span.clone(), val: EffectKind::Named(name.clone())},
		Some(t) => {
			let msg = format!("Expected effect, got {:#?}", t);
			let diag = Diagnostic::simple_error(msg.into(), t.span.clone());
			ctx.report_return(diag, EffectKind::Error)
		},
		None => {
			let msg = format!("Expected effect, got EOF");
			let diag = Diagnostic::simple_error(msg.into(), ctx.location());
			ctx.report_return(diag, EffectKind::Error)
		},
	}
}

fn parse_type<'a>(ctx: &mut ParseContext<'a>) -> Type {
	match ctx.next() {
		Some(Token {kind: TokenKind::Identifier(name), span}) => Type{span: span.clone(), val: TypeKind::Named(name.clone())},
		Some(Token {kind: TokenKind::Keyword(Keyword::Hole), span}) => Type{span: span.clone(), val: TypeKind::Hole},
		Some(Token {kind: TokenKind::Bang, span}) => Type{span: span.clone(), val: TypeKind::Never},
		Some(Token {kind: TokenKind::LParen, span}) => {
			let mut params = Vec::new();
			let end_span = if let Ok(Token {span: n_span, ..}) = ctx.take(TokenKind::RParen) {
				// return unit type if we immediately hit an RParen
				return Type{span: span.extend(&n_span).unwrap(), val: TypeKind::Unit};
			} else {
				let end_span;
				loop {
					params.push(parse_type(ctx));
					if let Ok(Token {span, ..}) = ctx.take(TokenKind::RParen) {
						end_span = span;
						break;
					}
					if let Err(diag) = ctx.take(TokenKind::Comma) {
						return ctx.report_return(diag, TypeKind::Error);
					}
				}
				end_span.clone()
			};
			return Type{span: span.extend(&end_span).unwrap(), val: TypeKind::Tuple(params.into())};
		},
		Some(Token {kind: TokenKind::Keyword(Keyword::Function), span}) => {
			// function pointer
			// grab LParen
			if let Err(diag) = ctx.take(TokenKind::LParen) {
				return ctx.report_return(diag, TypeKind::Error);
			}
			let mut params = Vec::new();
			let end_span = if let Ok(Token {span, ..}) = ctx.take(TokenKind::RParen) {
				// end early if we immediately hit an RParen
				span
			} else {
				let end_span;
				loop {
					params.push(parse_type(ctx));
					if let Ok(Token {span, ..}) = ctx.take(TokenKind::RParen) {
						end_span = span;
						break;
					}
					if let Err(diag) = ctx.take(TokenKind::Comma) {
						return ctx.report_return(diag, TypeKind::Error);
					}
				}
				end_span.clone()
			};
			let ret_type = parse_typetag(ctx);
			return Type { span: span.extend(&end_span).unwrap(), val: TypeKind::Function(params.into(), ret_type.into())};
		},
		Some(t) => {
			let msg = format!("Expected type, got {:#?}", t);
			let diag = Diagnostic::simple_error(msg.into(), t.span.clone());
			ctx.report_return(diag, TypeKind::Error)
		},
		None => {
			let msg = format!("Expected type, got EOF");
			let diag = Diagnostic::simple_error(msg.into(), ctx.location());
			ctx.report_return(diag, TypeKind::Error)
		},
	}
}

fn parse_typetag<'a>(ctx: &mut ParseContext) -> Type {
	if let Err(diag) = ctx.take(TokenKind::Colon) {
		return ctx.report_return(diag, TypeKind::Error);
	}
	return parse_type(ctx);
}

fn parse_return<'a>(ctx: &mut ParseContext<'a>) -> Stmt {
	let kw = ctx.next().unwrap();
	let expr = parse_expr(ctx);
	Stmt {span: kw.span.extend(&expr.span).unwrap(), val: StmtKind::Return(expr)}
}

fn parse_let_pattern<'a>(ctx: &mut ParseContext) -> LetPattern {
	match ctx.next() {
		Some(Token { kind: TokenKind::Keyword(Keyword::Mutable), span: start_span }) => {
			match ctx.next() {
				Some(Token { kind: TokenKind::Identifier(n), span: end_span}) => {
					let type_ = if ctx.at(TokenKind::Colon) {
						Some(parse_typetag(ctx))
					} else {
						None
					};
					LetPattern {span: start_span.extend(&end_span).unwrap(), val: LetPatternKind::Named(Mutability::Mutable, n.clone(), type_)}
				},
				Some(t) => {
					let msg = format!("Expected pattern name, got {:#?}", t);
					let diag = Diagnostic::simple_error(msg.into(), t.span.clone());
					ctx.report_return(diag, LetPatternKind::Error)
				},
				None => {
					let msg = format!("Expected pattern name, got EOF");
					let diag = Diagnostic::simple_error(msg.into(), ctx.location());
					ctx.report_return(diag, LetPatternKind::Error)
				},
			}
		},
		Some(Token { kind: TokenKind::LParen, span: start_span}) => {
			let mut params = Vec::new();
			let end_span = if let Ok(Token {span, ..}) = ctx.take(TokenKind::RParen) {
				// end early if we immediately hit an RParen
				span
			} else {
				let end_span;
				loop {
					params.push(parse_let_pattern(ctx));
					if let Ok(Token {span, ..}) = ctx.take(TokenKind::RParen) {
						end_span = span;
						break;
					}
					if let Err(diag) = ctx.take(TokenKind::Comma) {
						return ctx.report_return(diag, LetPatternKind::Error);
					}
				}
				end_span
			};
			return LetPattern {span: start_span.extend(&end_span).unwrap(), val: LetPatternKind::Tuple(params.into())};
		},
		Some(Token { kind: TokenKind::Identifier(n), span}) => {
			let (type_, span) = if ctx.at(TokenKind::Colon) {
				let type_ = parse_typetag(ctx);
				let span = span.extend(&type_.span).unwrap();

				(Some(type_), span)
			} else {
				(None, span.clone())
			};
			LetPattern {span, val: LetPatternKind::Named(Mutability::Immutable, n.clone(), type_)}
		},
		Some(Token { kind: TokenKind::Keyword(Keyword::Hole), span}) =>
		{
			let (type_, span) = if ctx.at(TokenKind::Colon) {
				let type_ = parse_typetag(ctx);

				let span = span.extend(&type_.span).unwrap();

				(Some(type_), span)
			} else {
				(None, span.clone())
			};
			LetPattern {span, val: LetPatternKind::Hole(type_)}
		},
		Some(t) => {
			let msg = format!("Expected pattern, got {:#?}", t);
			let diag = Diagnostic::simple_error(msg.into(), t.span.clone());
			ctx.report_return(diag, LetPatternKind::Error)
		},
		None => {
			let msg = format!("Expected pattern, got EOF");
			let diag = Diagnostic::simple_error(msg.into(), ctx.location());
			ctx.report_return(diag, LetPatternKind::Error)
		},
	}
}

fn parse_if<'a>(ctx: &mut ParseContext<'a>) -> (SourceSpan, If) {
	let kw = ctx.next().unwrap();
	let cond = parse_expr(ctx);
	let (span, body) = parse_scope_block(ctx);
	// else / elif block?
	let (span, _else) = match ctx.current() {
		Some(Token {kind: TokenKind::Keyword(Keyword::Else), ..}) =>
		{ ctx.next(); let (span, scope) = parse_scope_block(ctx); (span.clone(), Some(Expr {span, val: ExprKind::Scope(scope)}.into())) },
		Some(Token {kind: TokenKind::Keyword(Keyword::Elif), ..}) =>
		{ let (span, _if) = parse_if(ctx); (span.clone(), Some(Expr {span, val: ExprKind::If(_if)}.into())) },
		_ => (span, None),
	};

	return (kw.span.extend(&span).unwrap(), If {condition: cond.into(), body, _else });
}

fn parse_while<'a>(ctx: &mut ParseContext<'a>) -> (SourceSpan, While) {
	let kw = ctx.next().unwrap();
	let cond = parse_expr(ctx);
	let (end, body) = parse_scope_block(ctx);
	(kw.span.extend(&end).unwrap(), While {condition: cond.into(), body} )
}

fn parse_let<'a>(ctx: &mut ParseContext<'a>) -> Stmt {
	let kw = ctx.next().unwrap();
	let pattern = parse_let_pattern(ctx);
	if let Err(diag) = ctx.take(TokenKind::Assign) {
		return ctx.report_return(diag, StmtKind::Error);
	}
	let expr = parse_expr(ctx);
	Stmt { span: kw.span.extend(&expr.span).unwrap(), val: StmtKind::Let(pattern, expr) }
}

fn parse_scope_block<'a>(ctx: &mut ParseContext<'a>) -> (SourceSpan, Scope) {
	let start = ctx.take(TokenKind::LCurly);
	let start = if let Err(diag) = start {
		ctx.report(diag);
		ctx.location()
	} else {
		start.unwrap().span
	};
	let (body, end) = if let Ok(Token{span, ..}) = ctx.take(TokenKind::RCurly) {
		// implied Unit expression
		(Scope {body: Box::new([]), value: None}, span)
	} else {
		let body = parse_scope(ctx);
		let span = ctx.take(TokenKind::RCurly);
		let span = if let Err(diag) = span {
			ctx.report(diag);
			ctx.location()
		} else {
			span.unwrap().span
		};
		(body, span)
	};
	return (start.extend(&end).unwrap(), body);
}

fn parse_atom_expr_or_pattern<'a>(ctx: &mut ParseContext<'a>) -> AssignPatternOrExpr {
	use AssignPatternOrExpr as AP;
	let tok = ctx.current();
	if tok.is_none() {
		let msg = "Expected expression term, got EOF instead";
		let diag = Diagnostic::simple_error(msg.into(), ctx.location());
		ctx.report(diag);
		return AP::Error;
	}
	let tok = tok.unwrap();
	match tok.kind.clone() {
		TokenKind::Keyword(Keyword::Hole) => {
			let span = ctx.next().unwrap().span.clone();
			AP::Pattern(AssignPattern {span, val: AssignPatternKind::Hole})
		},
		TokenKind::Integer(val) => {
			let span = ctx.next().unwrap().span.clone();
			AP::Expr(Expr {span, val: ExprKind::Integer(val)})
		},
		TokenKind::Keyword(Keyword::True) => {
			let span = ctx.next().unwrap().span.clone();
			AP::Expr(Expr {span, val: ExprKind::Boolean(true)})
		},
		TokenKind::Keyword(Keyword::False) => {
			let span = ctx.next().unwrap().span.clone();
			AP::Expr(Expr {span, val: ExprKind::Boolean(false)})
		},
		TokenKind::Keyword(Keyword::If) => {
			let (span, _if) = parse_if(ctx);
			AP::Expr(Expr {span, val: ExprKind::If(_if)})
		},
		TokenKind::Keyword(Keyword::While) => {
			let (span, _while) = parse_while(ctx);
			AP::Expr(Expr {span, val: ExprKind::While(_while)})
		},
		TokenKind::Identifier(name) => {
			let span = ctx.next().unwrap().span.clone();
			AP::Named(span, name)
		},
		TokenKind::LCurly => {
			let (span, scope) = parse_scope_block(ctx);
			AP::Expr(Expr {span, val: ExprKind::Scope(scope)})
		},
		TokenKind::LParen => {
			// unit / parenthesized / tuple
			let span = ctx.next().unwrap().span.clone();
			let mut args = Vec::new();
			// does this call have args?
			let end_span = if let Ok(Token {span, .. }) = ctx.take(TokenKind::RParen) {
				span
			} else {
				loop {
					args.push(parse_pattern_or_expr(ctx));
					if ctx.take(TokenKind::Comma).is_err() {
						// we need to have an rparen here now
						let rparen = ctx.take(TokenKind::RParen);
						if let Err(diag) = rparen {
							ctx.report(diag);
							return AP::Error;
						}
						break rparen.unwrap().span;
					}
				}
			};
			AP::Grouped(span.extend(&end_span).unwrap(), args.into())
		},
		_ => {
			ctx.report(Diagnostic::simple_error(format!("Expected expression term, got {:#?} instead", tok).into(), tok.span.clone()));
			AP::Error
		}
	}
}

fn parse_suffix_expr_or_pattern<'a>(ctx: &mut ParseContext<'a>) -> AssignPatternOrExpr {
	use AssignPatternOrExpr as AP;
	// parse core item
	let mut core = parse_atom_expr_or_pattern(ctx);

	// parse suffixes
	loop {
		let tok = ctx.current();
		let tok = match tok {
			None => break,
			Some(t) => t.clone(),
		};
		match tok.kind {
			TokenKind::Bang => {
				ctx.next();
				let expr = rewrite_to_expr(ctx, &core);
				core = AP::Expr(Expr { span: expr.span.extend(&tok.span).unwrap(), val: ExprKind::Suffix(expr.into(), SuffixOp {span: tok.span.clone(), val: Suffix::Yell }) });
			}
			TokenKind::Question => {
				ctx.next();
				let expr = rewrite_to_expr(ctx, &core);
				core = AP::Expr(Expr { span: expr.span.extend(&tok.span).unwrap(), val: ExprKind::Suffix(expr.into(), SuffixOp {span: tok.span.clone(), val: Suffix::Query }) });
			}
			TokenKind::LParen => {
				// call
				ctx.next();
				let mut args = Vec::new();
				let expr = rewrite_to_expr(ctx, &core);
				// does this call have args?
				let end_span = if let Ok(Token {span, ..}) = ctx.take(TokenKind::RParen) {
					span
				} else {
					loop {
						args.push(parse_expr(ctx));
						if ctx.take(TokenKind::Comma).is_err() {
							// we need to have an rparen here now
							let rparen = ctx.take(TokenKind::RParen);
							if let Err(diag) = rparen {
								ctx.report(diag);
								return AP::Error;
							}
							break rparen.unwrap().span;
						}
					}
				};
				core = AP::Expr(Expr {span: expr.span.extend(&end_span).unwrap(), val: ExprKind::Call(Box::new(expr), args.into())});
			},
			_ => break,
		}
	}
	core
}

fn parse_prefix_expr_or_pattern<'a>(ctx: &mut ParseContext<'a>) -> AssignPatternOrExpr {
	// parse prefixes onto a stack
	let mut prefix_stack = Vec::new();
	loop {
		let tok = ctx.current();
		let prefix = match &tok {
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
		ctx.next();
	}

	// parse core item
	let mut core = parse_suffix_expr_or_pattern(ctx);

	// apply prefixes
	while let Some(prefix) = prefix_stack.pop() {
		let expr = rewrite_to_expr(ctx, &core);
		core = AssignPatternOrExpr::Expr(Expr {span: expr.span.extend(&prefix.span).unwrap(), val: ExprKind::Prefix(prefix, expr.into())});
	}

	core
}

fn parse_bin_exprs_or_patterns<'a>(ctx: &mut ParseContext<'a>) -> (AssignPatternOrExpr, Box<[(InfixOp, Expr)]>) {
	let leftmost = parse_prefix_expr_or_pattern(ctx);
	let mut right = Vec::new();
	// parse infixes
	loop {
		let tok = ctx.current();
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
		ctx.next();
		let rhs = parse_prefix_expr_or_pattern(ctx);
		let rhs = rewrite_to_expr(ctx, &rhs);
		let op = InfixOp {span: tok.span.clone(), val: infix};
		right.push((op, rhs))
	}

	(leftmost, right.into())
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

fn parse_bin_expr<'a>(ctx: &mut ParseContext<'a>) -> AssignPatternOrExpr {
	let (lhs, bins) = parse_bin_exprs_or_patterns(ctx);
	if bins.len() == 0 {
		return lhs;
	}

	let lhs = rewrite_to_expr(ctx, &lhs);

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
						Associativity::Explicit => {
							let msg = "Ambiguous operator order".into();
							let diag = Diagnostic::simple_error(msg, op.span.clone());
							ctx.report(diag);
							true
						},
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
		[expr] => AssignPatternOrExpr::Expr(expr.clone()),
		_ => unreachable!("shunting yard should ensure the working stack has a length of 1 at the end"),
	}
}

fn parse_pattern_or_expr<'a>(ctx: &mut ParseContext) -> AssignPatternOrExpr {
	let lhs = parse_bin_expr(ctx);

	if ctx.take(TokenKind::Assign).is_ok() {
		let rhs = parse_expr(ctx);
		let lhs = rewrite_to_pattern(ctx, &lhs);
		let span = lhs.span.extend(&rhs.span).unwrap();
		return AssignPatternOrExpr::Expr(Expr{span, val: ExprKind::Assign(lhs, rhs.into())});
	}
	lhs
}

fn rewrite_to_expr(ctx: &mut ParseContext, potential_expr: &AssignPatternOrExpr) -> Expr {
	use AssignPatternOrExpr as AP;
	match potential_expr {
		AP::Error => {
			Expr{span: ctx.location(), val: ExprKind::Error}
		},
		AP::Expr(e) => e.clone(),
		AP::Pattern(p) => {
			let diag = Diagnostic::simple_error("Invalid expression term".into(), p.span.clone());
			ctx.report_return(diag, ExprKind::Error)
		},
		AP::Named(span, name) => Expr {span: span.clone(), val: ExprKind::Lookup(name.clone())},
		AP::Grouped(span, vals) => {
			let mut exprs = Vec::new();
			for val in vals.into_iter() {
				exprs.push(rewrite_to_expr(ctx, val));
			}

			match exprs.len() {
				0 => Expr {span: span.clone(), val: ExprKind::Unit},
				1 => exprs[0].clone(),
				_ => Expr {span: span.clone(), val: ExprKind::Tuple(exprs.into()) },
			}
		}
	}
}

fn rewrite_to_pattern<'a>(ctx: &mut ParseContext<'a>, potential_expr: &AssignPatternOrExpr) -> AssignPattern {
	use AssignPatternOrExpr as AP;
	match potential_expr {
		AP::Error => AssignPattern { span: ctx.location(), val: AssignPatternKind::Error },
		AP::Expr(e) => {
			let diag = Diagnostic::simple_error("Invalid pattern term".into(), e.span.clone());
			ctx.report_return(diag, AssignPatternKind::Error)
		},
		AP::Pattern(p) => p.clone(),
		AP::Named(span, name) => AssignPattern {span: span.clone(), val: AssignPatternKind::Named(name.clone())},
		AP::Grouped(span, vals) => {
			let mut pats = Vec::new();
			for val in vals.into_iter() {
				pats.push(rewrite_to_pattern(ctx, val));
			}

			AssignPattern {span: span.clone(), val: AssignPatternKind::Tuple(pats.into())}
		}
	}
}

fn parse_expr<'a>(ctx: &mut ParseContext<'a>) -> Expr {
	let lhs = parse_pattern_or_expr(ctx);

	let lhs = rewrite_to_expr(ctx, &lhs);
	return lhs;
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
		E::Error => A::Semicolon,

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

fn parse_stmt<'a>(ctx: &mut ParseContext<'a>) -> Stmt {
	if ctx.at(TokenKind::Keyword(Keyword::Let)) {
		// let
		return parse_let(ctx);
	} else if ctx.at(TokenKind::Keyword(Keyword::Return)) {
		// let
		return parse_return(ctx);
	}
	let expr = parse_expr(ctx);
	return Stmt{span: expr.span.clone(), val: StmtKind::Expr(expr)};
}

fn parse_scope<'a>(ctx: &mut ParseContext<'a>) -> Scope {
	let mut list = Vec::new();
	let mut value = None;

	loop {
		if ctx.at_end() {
			let message = "Expected statement / RCurly, got EOF";
			let diag = Diagnostic::simple_error(message.into(), ctx.location());
			ctx.report(diag);
			break;
		}
		if ctx.at(TokenKind::RCurly) {
			break;
		}

		let stmt = parse_stmt(ctx);
		match stmt.val.clone() {
			StmtKind::Expr(e) => {
				// the olden way
				let restriction = restrictions(&e.val);
				match (restriction, ctx.current()) {
					(_, Some(Token { kind: TokenKind::RCurly, ..})) => {
						// end of block
						// put it as the value
						value = Some(e.into());
						break;
					},
					(ExprAllowed::Semicolon, Some(Token { kind: TokenKind::SemiColon, ..})) => {
						ctx.next();
						while let Ok(_) = ctx.take(TokenKind::SemiColon) {}
						list.push(stmt);
					},
					(ExprAllowed::Always, _) => {
						// consume as many semicolons as possible
						while let Ok(_) = ctx.take(TokenKind::SemiColon) {}
						list.push(stmt);
					},
					(ExprAllowed::EndOfBlock, _) => {
						let message = "This type of expression is only allowed at the end of a block";
						let diag = Diagnostic::simple_error(message.into(), e.span);
						ctx.report(diag);
						// consume as many semicolons as possible
						while let Ok(_) = ctx.take(TokenKind::SemiColon) {}
					},
					(ExprAllowed::Semicolon, _) => {
						let message = "This type of expression is not allowed stand-alone and must be terminated by a semicolon (or at the end of a block as return value)";
						let diag = Diagnostic::simple_error(message.into(), e.span);
						ctx.report(diag);
					},
				}
			},
			StmtKind::Return(_) | StmtKind::Let(..) => {
				if let Err(diag) = ctx.take(TokenKind::SemiColon) {
					ctx.report(diag);
				}
				while let Ok(_) = ctx.take(TokenKind::SemiColon) {}
				list.push(stmt);
			},
			StmtKind::Error => {
				list.push(stmt);
			},
		}
	}

	return Scope{body: list.into(), value};
}

fn parse_function_body<'a>(ctx: &mut ParseContext<'a>) -> (Box<[Function]>, Scope, SourceSpan) {
	if let Err(diag) = ctx.take(TokenKind::LCurly) {
		ctx.report(diag);
		return (Box::new([]), Scope { body: Box::new([]), value: None }, ctx.location());
	}
	// function bodies can first have as many child / helper functions as they want
	let mut children = Vec::new();
	while let Some(tok) = ctx.current() {
		children.push(match tok.kind {
			TokenKind::Keyword(Keyword::Function)
				=> parse_function(ctx),
			_ => break,
		});
	}
	let (body, span) = if let Ok(Token {span, ..}) = ctx.take(TokenKind::RCurly) {
		// empty body
		(Scope {
			body: Box::new([]),
			value: None,
		}, span)
	} else {
		let body = parse_scope(ctx);
		// we need to have an rcurly here now
		let rcurly = ctx.take(TokenKind::RCurly);
		let span = if let Err(diag) = rcurly {
			ctx.report(diag);
			ctx.location()
		} else {
			rcurly.unwrap().span
		};
		(body, span)
	};
	return (children.into(), body, span);
}

fn parse_param<'a>(ctx: &mut ParseContext<'a>) -> Param {
	match ctx.next() {
		Some(Token { kind: TokenKind::Keyword(Keyword::Hole), span}) => {
			let _type = parse_typetag(ctx);
			Param { span: span.clone(), val: ParamKind::Ignored(_type)}
		},
		Some(Token { kind: TokenKind::Identifier(name), span}) => {
			let _type = parse_typetag(ctx);
			Param { span: span.clone(), val: ParamKind::Named(name.clone(), _type)}
		},
		Some(t) => {
			let msg = format!("Expected parameter, got {:#?}", t);
			let diag = Diagnostic::simple_error(msg.into(), t.span.clone());
			ctx.report_return(diag, ParamKind::Error)
		},
		None => {
			let msg = format!("Expected parameter, got EOF");
			let diag = Diagnostic::simple_error(msg.into(), ctx.location());
			ctx.report_return(diag, ParamKind::Error)
		},
	}
}

fn parse_function<'a>(ctx: &mut ParseContext<'a>) -> Function {
	// get the `fn`
	let kw = ctx.next().unwrap().clone();
	let mut effects = Vec::new();
	// do we have a `[`?
	if let Ok(_) = ctx.take(TokenKind::LSquare) {
		loop {
			if let Ok(_) = ctx.take(TokenKind::RSquare) {
				break;
			}
			effects.push(parse_effect(ctx));
		}
	}
	// name
	let name = ctx.current();
	let name = if let Some(t) = name {
		if let Token {kind: TokenKind::Identifier(n), ..} = t {
			n.clone()
		} else {
			let msg = format!("Expected function name, got {:#?}", t);
			let diag = Diagnostic::simple_error(msg.into(), t.span);
			ctx.report(diag);
			return Function {
				span: ctx.location(), effects: effects.into(), name: "[INTERNAL ERROR]".into(), helpers: Box::new([]),
				params: Box::new([]), return_type: Type {span: ctx.location(), val: TypeKind::Error}, body: Scope {body: Box::new([]), value: None}
			};
		}
	} else {
		let msg = format!("Expected function name, got EOF");
		let diag = Diagnostic::simple_error(msg.into(), ctx.location());
		ctx.report(diag);
		return Function {
			span: ctx.location(), effects: effects.into(), name: "[INTERNAL ERROR]".into(), helpers: Box::new([]),
			params: Box::new([]), return_type: Type {span: ctx.location(), val: TypeKind::Error}, body: Scope {body: Box::new([]), value: None}
		};
	};
	ctx.next();
	// parameters
	let mut params = Vec::new();
	// we need to have an lparen here now
	if let Err(diag) = ctx.take(TokenKind::LParen) {
		ctx.report(diag);
		return Function {
			span: ctx.location(), effects: effects.into(), name, helpers: Box::new([]),
			params: Box::new([]), return_type: Type {span: ctx.location(), val: TypeKind::Error}, body: Scope {body: Box::new([]), value: None}
		};
	}

	if ctx.take(TokenKind::RParen).is_err() {
		loop {
			params.push(parse_param(ctx));
			if ctx.take(TokenKind::RParen).is_ok() {
				break;
			}
			// we need to have a comma here now
			if let Err(diag) = ctx.take(TokenKind::Comma) {
				ctx.report(diag);
				return Function {
					span: ctx.location(), effects: effects.into(), name, helpers: Box::new([]),
					params: Box::new([]), return_type: Type {span: ctx.location(), val: TypeKind::Error}, body: Scope {body: Box::new([]), value: None}
				};
			}
		}
	}
	// end of params
	let return_type = parse_typetag(ctx);

	let (helpers, body, span) = parse_function_body(ctx);
	Function { span: kw.span.extend(&span).unwrap(), effects: effects.into(), name, params: params.into(), return_type, helpers, body }
}

pub fn parse(tokens: &[Token]) -> (Option<AST>, Box<[Diagnostic]>) {
	let mut functions = Vec::new();

	let mut ctx = ParseContext { diagnostics: Vec::new(), tokens, offset: 0 };

	while let Some(tok) = ctx.current() {
		match tok.kind {
			TokenKind::Keyword(Keyword::Function) => {
				let func = parse_function(&mut ctx);
				functions.push(func);
			}
			t => {
				let msg = format!("Expected fn keyword, got {:?}", t.clone());
				let diag = Diagnostic::simple_error(msg.into(), tok.span.clone());
				ctx.report(diag);
				ctx.next();
			}
		}
	}
	let any_errors = (&ctx.diagnostics).into_iter().any(|d| d.1 == Severity::Error);
	if any_errors {
		(None, ctx.diagnostics.into())
	} else {
		(Some(AST { functions: functions.into() }), ctx.diagnostics.into())
	}
}

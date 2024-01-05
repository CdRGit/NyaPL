use crate::parser::{*};
use std::io::{self, Write, Result as IOResult};

pub fn print(tree: ParseTree) {
	for top in tree.toplevel.into_iter() {
		match top {
			TopLevelStmt::Function(func) => print_function(&mut io::stdout(), func, "".to_string(), "".to_string(), "├─", "│ ", "└─", "  ").unwrap(),
		}
	}
}

fn print_function(writer: &mut impl Write, func: &Function, prefix_i: String, prefix: String, cpi: &str, cp: &str, fpi: &str, fp: &str) -> IOResult<()> {
	write!(writer, "{prefix_i}fn {}\n", func.name)?;
	print_type(writer, &func.ret_type, prefix.clone() + cpi, prefix.clone() + cp, cpi, cp, fpi, fp)?;
	match &func.effects[..] {
		[] => (),
		effects => {
			write!(writer, "{prefix}{cpi}Effects\n")?;
			let mut it = effects.into_iter().peekable();
			while let Some(effect) = it.next() {
				let mut pf = prefix.clone() + cp;
				let mut pfi = prefix.clone() + cp;

				if it.peek().is_none() {
					pf += fp;
					pfi += fpi;
				} else {
					pf += cp;
					pfi += cpi;
				}

				print_effect(writer, effect, pfi, pf, cpi, cp, fpi, fp)?;
			}
		},
	}
	match &func.children[..] {
		[] => (),
		children => {
			write!(writer, "{prefix}{cpi}Children\n")?;
			let mut it = children.into_iter().peekable();
			while let Some(child) = it.next() {
				let mut pf = prefix.clone() + cp;
				let mut pfi = prefix.clone() + cp;

				if it.peek().is_none() {
					pf += fp;
					pfi += fpi;
				} else {
					pf += cp;
					pfi += cpi;
				}

				print_function(writer, child, pfi, pf, cpi, cp, fpi, fp)?;
			}
		},
	}
	// params
	write!(writer, "{prefix}{cpi}Parameters\n")?;
	let mut it = func.params.into_iter().peekable();
	while let Some(param) = it.next() {
		let mut pf = prefix.clone() + cp;
		let mut pfi = prefix.clone() + cp;

		if it.peek().is_none() {
			pf += fp;
			pfi += fpi;
		} else {
			pf += cp;
			pfi += cpi;
		}

		print_param(writer, param, pfi, pf, cpi, cp, fpi, fp)?;
	}

	print_expr(writer, &Expr::Block(func.body.clone()), prefix.clone() + fpi, prefix + fp, cpi, cp, fpi, fp)
}

fn print_param(writer: &mut impl Write, param: &Parameter, prefix_i: String, prefix: String, cpi: &str, cp: &str, fpi: &str, fp: &str) -> IOResult<()> {
	write!(writer, "{prefix_i}{}\n", param.0)?;
	print_type(writer, &param.1, prefix.clone() + fpi, prefix + fp, cpi, cp, fpi, fp)
}

fn print_effect(writer: &mut impl Write, effect: &Effect, prefix_i: String, _prefix: String, _cpi: &str, _cp: &str, _fpi: &str, _fp: &str) -> IOResult<()> {
	match effect {
		Effect::Named(name) => write!(writer, "{prefix_i}{name}\n")
	}
}

fn print_expr(writer: &mut impl Write, expr: &Expr, prefix_i: String, prefix: String, cpi: &str, cp: &str, fpi: &str, fp: &str) -> IOResult<()> {
	use Expr as E;
	match expr {
		E::Block(block) => {
			write!(writer, "{prefix_i}Block\n")?;
			let mut it = block.0.into_iter().peekable();
			while let Some(expr) = it.next() {
				let mut pf = prefix.clone();
				let mut pfi = prefix.clone();
				if it.peek().is_none() {
					pf += fp;
					pfi += fpi;
				} else {
					pf += cp;
					pfi += cpi;
				}
				print_expr(writer, expr, pfi, pf, cpi, cp, fpi, fp)?;
			}
		},
		E::Let(let_) => {
			write!(writer, "{prefix_i}Let\n")?;
			print_pattern(writer, &let_.target, prefix.clone() + cpi, prefix.clone() + cp, cpi, cp, fpi, fp)?;
			print_expr(writer, &let_.value, prefix.clone() + fpi, prefix + fp, cpi, cp, fpi, fp)?;
		},
		E::Return(ret) => {
			write!(writer, "{prefix_i}Return\n")?;
			print_expr(writer, &ret.1, prefix.clone() + fpi, prefix + fp, cpi, cp, fpi, fp)?;
		},
		E::Stmt(expr) => {
			write!(writer, "{prefix_i}Statement\n")?;
			print_expr(writer, &expr, prefix.clone() + fpi, prefix + fp, cpi, cp, fpi, fp)?;
		},
		E::Boolean(val) => {
			write!(writer, "{prefix_i}Bool({})\n", val)?;
		},
		E::Integer(int) => {
			write!(writer, "{prefix_i}Integer({})\n", int.1)?;
		},
		E::Lookup(lookup) => {
			write!(writer, "{prefix_i}Lookup({})\n", lookup.1)?;
		},
		E::Unit => {
			write!(writer, "{prefix_i}Unit\n")?;
		},
		E::While(while_) => {
			write!(writer, "{prefix_i}While\n")?;
			print_expr(writer, &while_.condition, prefix.clone() + cpi, prefix.clone() + cp, cpi, cp, fpi, fp)?;
			let (pf, pfi) = (prefix.clone() + fp, prefix.clone() + fpi);
			print_expr(writer, &Expr::Block(while_.body.clone()), pfi, pf, cpi, cp, fpi, fp)?;
		},
		E::If(if_) => {
			write!(writer, "{prefix_i}If\n")?;
			print_expr(writer, &if_.condition, prefix.clone() + cpi, prefix.clone() + cp, cpi, cp, fpi, fp)?;
			let (pf, pfi) = if if_.else_.is_none() {
				(prefix.clone() + fp, prefix.clone() + fpi)
			} else {
				(prefix.clone() + cp, prefix.clone() + cpi)
			};
			print_expr(writer, &Expr::Block(if_.body.clone()), pfi, pf, cpi, cp, fpi, fp)?;
			if let Some(expr) = &if_.else_ {
				print_expr(writer, &expr, prefix.clone() + fpi, prefix + fp, cpi, cp, fpi, fp)?;
			}
		},
		E::PreOp(pre) => {
			write!(writer, "{prefix_i}Prefix {}\n", match pre.0 {
				PrefixOp::Reference => "&",
				PrefixOp::Dereference => "*",

				PrefixOp::Identity => "+",
				PrefixOp::Negative => "-",
				PrefixOp::Not => "!",
			})?;
			print_expr(writer, &pre.1, prefix.clone() + fpi, prefix + fp, cpi, cp, fpi, fp)?;
		},
		E::PostOp(post) => {
			write!(writer, "{prefix_i}Suffix {}\n", match post.0 {
				PostfixOp::Query => "?",
				PostfixOp::Yell => "!",
			})?;
			print_expr(writer, &post.1, prefix.clone() + fpi, prefix + fp, cpi, cp, fpi, fp)?;
		},
		E::BinOp(bin) => {
			write!(writer, "{prefix_i}Infix {}\n", match bin.0 {
				InfixOp::Add => "+",
				InfixOp::Sub => "-",
				InfixOp::Mult => "*",
				InfixOp::Div => "/",
				InfixOp::Mod => "%",

				InfixOp::BitAnd => "&",
				InfixOp::BitOr => "|",
				InfixOp::BitXor => "^",

				InfixOp::LogAnd => "&&",
				InfixOp::LogOr => "||",

				InfixOp::Assign => "=",

				InfixOp::ShiftRight => ">>",
				InfixOp::ShiftLeft => "<<",

				InfixOp::Equal => "==",
				InfixOp::NotEqual => "!=",
				InfixOp::Greater => ">",
				InfixOp::Lesser => "<",
				InfixOp::GreaterEqual => ">=",
				InfixOp::LesserEqual => "<=",
			})?;
			print_expr(writer, &bin.1, prefix.clone() + cpi, prefix.clone() + cp, cpi, cp, fpi, fp)?;
			print_expr(writer, &bin.2, prefix.clone() + fpi, prefix + fp, cpi, cp, fpi, fp)?;
		},
		E::Call(call) => {
			write!(writer, "{prefix_i}Call\n")?;
			let (pf, pfi) = if call.1.len() == 0 {
				(prefix.clone() + fp, prefix.clone() + fpi)
			} else {
				(prefix.clone() + cp, prefix.clone() + cpi)
			};
			print_expr(writer, &call.0, pfi, pf, cpi, cp, fpi, fp)?;
			let mut it = call.1.into_iter().peekable();
			while let Some(expr) = it.next() {
				let mut pf = prefix.clone();
				let mut pfi = prefix.clone();
				if it.peek().is_none() {
					pf += fp;
					pfi += fpi;
				} else {
					pf += cp;
					pfi += cpi;
				}
				print_expr(writer, expr, pfi, pf, cpi, cp, fpi, fp)?;
			}
		},
		E::Tuple(tuple) => {
			write!(writer, "{prefix_i}Tuple\n")?;
			let mut it = tuple.into_iter().peekable();
			while let Some(expr) = it.next() {
				let mut pf = prefix.clone();
				let mut pfi = prefix.clone();
				if it.peek().is_none() {
					pf += fp;
					pfi += fpi;
				} else {
					pf += cp;
					pfi += cpi;
				}
				print_expr(writer, expr, pfi, pf, cpi, cp, fpi, fp)?;
			}
		},
	}
	Ok(())
}

fn print_type(writer: &mut impl Write, type_: &Type, prefix_i: String, prefix: String, cpi: &str, cp: &str, fpi: &str, fp: &str) -> IOResult<()> {
	match type_ {
		Type::Named(name) => write!(writer, "{prefix_i}{name}\n"),
		Type::Tuple(params) => {
			write!(writer, "{prefix_i}Tuple\n")?;
			let mut it = params.into_iter().peekable();
			while let Some(param) = it.next() {
				let mut pf = prefix.clone();
				let mut pfi = prefix.clone();

				if it.peek().is_none() {
					pf += fp;
					pfi += fpi;
				} else {
					pf += cp;
					pfi += cpi;
				}

				print_type(writer, param, pfi, pf, cpi, cp, fpi, fp)?;
			}
			Ok(())
		},
		Type::Function(params, ret) => {
			write!(writer, "{prefix_i}fn\n")?;
			print_type(writer, ret, prefix.clone() + cpi, prefix.clone() + cp, cpi, cp, fpi, fp)?;
			write!(writer, "{prefix}{cpi}Parameters\n")?;
			let mut it = params.into_iter().peekable();
			while let Some(param) = it.next() {
				let mut pf = prefix.clone() + cp;
				let mut pfi = prefix.clone() + cp;

				if it.peek().is_none() {
					pf += fp;
					pfi += fpi;
				} else {
					pf += cp;
					pfi += cpi;
				}

				print_type(writer, param, pfi, pf, cpi, cp, fpi, fp)?;
			}
			Ok(())
		},
	}
}

fn print_pattern(writer: &mut impl Write, pattern: &Pattern, prefix_i: String, prefix: String, cpi: &str, cp: &str, fpi: &str, fp: &str) -> IOResult<()> {
	match pattern {
		Pattern::Hole => write!(writer, "{prefix_i}(_)\n"),
		Pattern::Named(Mutability::Immutable, name, type_) => {
			write!(writer, "{prefix_i}{name}\n")?;
			if let Some(t) = type_ {
				print_type(writer, t, prefix.clone() + fpi, prefix + fp, cpi, cp, fpi, fp)?;
			}
			Ok(())
		}
		Pattern::Named(Mutability::Mutable, name, type_) => {
			write!(writer, "{prefix_i}mut {name}\n")?;
			if let Some(t) = type_ {
				print_type(writer, t, prefix.clone() + fpi, prefix + fp, cpi, cp, fpi, fp)?;
			}
			Ok(())
		}
		Pattern::Tuple(params) => {
			write!(writer, "{prefix_i}Tuple\n")?;
			let mut it = params.into_iter().peekable();
			while let Some(param) = it.next() {
				let mut pf = prefix.clone() + cp;
				let mut pfi = prefix.clone() + cp;

				if it.peek().is_none() {
					pf += fp;
					pfi += fpi;
				} else {
					pf += cp;
					pfi += cpi;
				}

				print_pattern(writer, param, pfi, pf, cpi, cp, fpi, fp)?;
			}
			Ok(())
		}
	}
}

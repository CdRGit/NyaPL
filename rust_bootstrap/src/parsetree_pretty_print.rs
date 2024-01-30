use crate::ast::{*};
use std::io::{self, Write, Result as IOResult};

pub fn print(tree: &AST) {
	let mut writer = io::stdout();
	for func in tree.functions.into_iter() {
		print_function(&mut writer, func, 0).unwrap();
	}
}

fn pad_depth(writer: &mut impl Write, depth: usize) -> IOResult<()> {
	for _ in 0..depth {
		write!(writer, "  ")?;
	}
	Ok(())
}

fn print_function(writer: &mut impl Write, func: &Function, depth: usize) -> IOResult<()> {
	pad_depth(writer, depth)?;
	writeln!(writer, "fn {}", func.name)?;

	pad_depth(writer, depth)?;
	writeln!(writer, "effects:")?;

	for e in func.effects.into_iter() {
		print_effect(writer, e, depth + 1)?;
	}

	pad_depth(writer, depth)?;
	writeln!(writer, "helpers:")?;

	for h in func.helpers.into_iter() {
		print_function(writer, h, depth + 1)?;
	}

	pad_depth(writer, depth)?;
	writeln!(writer, "parameters:")?;

	for p in func.params.into_iter() {
		print_param(writer, p, depth + 1)?;
	}

	pad_depth(writer, depth)?;
	writeln!(writer, "return type:")?;
	print_type(writer, &func.return_type, depth + 1)?;

	writeln!(writer, "body:")?;
	print_scope(writer, &func.body, depth + 1)?;
	Ok(())
}

fn print_effect(writer: &mut impl Write, effect: &Effect, depth: usize) -> IOResult<()> {
	pad_depth(writer, depth)?;
	match &effect.val {
		EffectKind::Named(n) => {
			writeln!(writer, "Named({})", n)?;
		}
	}
	Ok(())
}

fn print_param(writer: &mut impl Write, param: &Param, depth: usize) -> IOResult<()> {
	pad_depth(writer, depth)?;
	match &param.val {
		ParamKind::Named(n, _type) => {
			writeln!(writer, "Named({})", n)?;
			print_type(writer, _type, depth + 1)?;
		}
		ParamKind::Ignored(_type) => {
			writeln!(writer, "Ignored")?;
			print_type(writer, _type, depth + 1)?;
		}
	}
	Ok(())
}

fn print_type(writer: &mut impl Write, _type: &Type, depth: usize) -> IOResult<()> {
	pad_depth(writer, depth)?;
	match &_type.val {
		TypeKind::Hole => writeln!(writer, "_")?,
		TypeKind::Unit => writeln!(writer, "()")?,
		TypeKind::Never => writeln!(writer, "!")?,
		TypeKind::Named(n) => writeln!(writer, "Named({})", n)?,
		TypeKind::Tuple(types) => {
			writeln!(writer, "Tuple")?;
			for t in types.into_iter() {
				print_type(writer, t, depth + 1)?;
			}
		},
		TypeKind::Function(params, _return) => {
			writeln!(writer, "Function")?;
			print_type(writer, _return, depth + 2)?;
			for t in params.into_iter() {
				print_type(writer, t, depth + 1)?;
			}
		},
	}
	Ok(())
}

fn print_scope(writer: &mut impl Write, scope: &Scope, depth: usize) -> IOResult<()> {
	pad_depth(writer, depth)?;
	writeln!(writer, "main body:")?;
	for stmt in scope.body.into_iter() {
		print_stmt(writer, stmt, depth + 1)?;
	}
	if let Some(expr) = &scope.value {
		print_expr(writer, &expr, depth + 1)?;
	}
	Ok(())
}

fn print_stmt(writer: &mut impl Write, stmt: &Stmt, depth: usize) -> IOResult<()> {
	pad_depth(writer, depth)?;
	match &stmt.val {
		StmtKind::Expr(e) => {
			writeln!(writer, "Expression Statement")?;
			print_expr(writer, &e, depth + 1)?;
		}
		StmtKind::Let(pattern, value) => {
			writeln!(writer, "Let")?;
			print_let_pattern(writer, &pattern, depth + 1)?;
			print_expr(writer, &value, depth + 1)?;
		},
		StmtKind::Return(e) => {
			writeln!(writer, "Return")?;
			print_expr(writer, &e, depth + 1)?;
		},
	}
	Ok(())
}

fn print_let_pattern(writer: &mut impl Write, pattern: &LetPattern, depth: usize) -> IOResult<()> {
	pad_depth(writer, depth)?;
	match &pattern.val {
		LetPatternKind::Hole(_type) => {
			writeln!(writer, "Hole")?;
			if let Some(_type) = _type {
				print_type(writer, _type, depth + 1)?;
			}
		},
		LetPatternKind::Named(Mutability::Mutable, name, _type) => {
			writeln!(writer, "Named({}) [mutable]", name)?;
			if let Some(_type) = _type {
				print_type(writer, _type, depth + 1)?;
			}
		},
		LetPatternKind::Named(Mutability::Immutable, name, _type) => {
			writeln!(writer, "Named({})", name)?;
			if let Some(_type) = _type {
				print_type(writer, _type, depth + 1)?;
			}
		},
		LetPatternKind::Tuple(pats) => {
			writeln!(writer, "Tuple")?;
			for p in pats.into_iter() {
				print_let_pattern(writer, p, depth + 1)?;
			}
		},
	}
	Ok(())
}

fn print_assign_pattern(writer: &mut impl Write, pattern: &AssignPattern, depth: usize) -> IOResult<()> {
	pad_depth(writer, depth)?;
	match &pattern.val {
		AssignPatternKind::Hole => {
			writeln!(writer, "Hole")?;
		},
		AssignPatternKind::Named(name) => {
			writeln!(writer, "Named({})", name)?;
		},
		AssignPatternKind::Tuple(pats) => {
			writeln!(writer, "Tuple")?;
			for p in pats.into_iter() {
				print_assign_pattern(writer, p, depth + 1)?;
			}
		},
	}
	Ok(())
}

fn print_expr(writer: &mut impl Write, expr: &Expr, depth: usize) -> IOResult<()> {
	pad_depth(writer, depth)?;
	match &expr.val {
		ExprKind::Assign(pattern, value) => {
			writeln!(writer, "Assign")?;
			print_assign_pattern(writer, pattern, depth + 1)?;
			print_expr(writer, value, depth + 1)?;
		},
		ExprKind::Call(base, args) => {
			writeln!(writer, "Call")?;
			pad_depth(writer, depth + 1)?;
			writeln!(writer, "Base:")?;
			print_expr(writer, base, depth + 2)?;
			pad_depth(writer, depth + 1)?;
			writeln!(writer, "Args:")?;
			for a in args.into_iter() {
				print_expr(writer, a, depth + 2)?;
			}
		},
		ExprKind::Infix(lhs, op, rhs) => {
			writeln!(writer, "Infix")?;
			pad_depth(writer, depth + 1)?;
			writeln!(writer, "Operator: {:?}", op.val)?;
			print_expr(writer, lhs, depth + 1)?;
			print_expr(writer, rhs, depth + 1)?;
		},
		ExprKind::Prefix(op, expr) => {
			writeln!(writer, "Prefix")?;
			pad_depth(writer, depth + 1)?;
			writeln!(writer, "Operator: {:?}", op.val)?;
			print_expr(writer, expr, depth + 1)?;
		},
		ExprKind::Suffix(expr, op) => {
			writeln!(writer, "Suffix")?;
			pad_depth(writer, depth + 1)?;
			writeln!(writer, "Operator: {:?}", op.val)?;
			print_expr(writer, expr, depth + 1)?;
		},
		ExprKind::Integer(val) => writeln!(writer, "Int({})", val)?,
		ExprKind::Boolean(val) => writeln!(writer, "Bool({})", val)?,
		ExprKind::Unit => writeln!(writer, "Unit")?,
		ExprKind::Tuple(exprs) => {
			writeln!(writer, "Tuple")?;
			for e in exprs.into_iter() {
				print_expr(writer, e, depth + 2)?;
			}
		},
		ExprKind::Scope(scope) => {
			writeln!(writer, "Scope")?;
			print_scope(writer, scope, depth + 1)?;
		},
		ExprKind::If(_if) => {
			writeln!(writer, "if")?;
			print_expr(writer, &_if.condition, depth + 1)?;
			print_scope(writer, &_if.body, depth + 1)?;
			if let Some(_else) = &_if._else {
				print_expr(writer, _else, depth + 1)?;
			}
		},
		ExprKind::While(_while) => {
			writeln!(writer, "while")?;
			print_expr(writer, &_while.condition, depth + 1)?;
			print_scope(writer, &_while.body, depth + 1)?;
		},
		ExprKind::Lookup(n) => writeln!(writer, "Lookup({})", n)?,
	}
	Ok(())
}

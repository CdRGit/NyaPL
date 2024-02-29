use std::rc::Rc;
use std::collections::HashMap;
use crate::source_span::SourceSpan;
use crate::ast::{
	AST,
	Function,
	ParamKind,
	TypeKind,
};
use crate::ast::Type as AstType;
use crate::typed_tree::{*};

#[derive(Debug, Clone)]
pub enum TypeError {
	DisallowedTypeInference(SourceSpan),
	UnknownTypeName(Rc<str>, SourceSpan),
}

type TypeResult<T> = Result<T, TypeError>;

#[derive(Debug, Clone)]
struct Signature {
	pub name: Rc<str>,
	pub param_types: Box<[Type]>,
	pub ret_type: Type,
}

pub fn get_named_types(ast: &AST) -> TypeResult<HashMap<String, Type>> {
	let mut types = HashMap::new();
	// right now this only includes the known named types

	// including all 512 integer types
	// lol, lmfao even
	for i in 1..=256 {
		let size = IntSize::new(i).unwrap();
		let i_name = format!("i{}", i);
		let u_name = format!("u{}", i);
		types.insert(i_name, Type::SignedInt(size));
		types.insert(u_name, Type::UnsignedInt(size));

	}
	Ok(types)
}

pub fn check_tree(ast: &AST) -> TypeResult<TypedTree> {
	let named_types = get_named_types(ast)?;
	let function_signatures: Box<[Signature]> = ast.functions.iter().map(|f| get_signature(&named_types, "", f)).collect::<TypeResult<_>>()?;
	println!("{:?}", function_signatures);
	todo!();
}

fn get_signature(named_types: &HashMap<String, Type>, prefix: &str, func: &Function) -> TypeResult<Signature> {
	let name = (prefix.to_string() + &func.name).into();
	let param_types = func.params.iter().map(|p| match &p.val {
		ParamKind::Ignored(typ) => get_type(named_types, typ),
		ParamKind::Named(_, typ) => get_type(named_types, typ),
		ParamKind::Error => unreachable!(),
	}).collect::<TypeResult<_>>()?;
	let ret_type = get_type(named_types, &func.return_type)?;
	Ok(Signature {name, param_types, ret_type})
}

fn get_type(named_types: &HashMap<String, Type>, typ: &AstType) -> TypeResult<Type> {
	match &typ.val {
		TypeKind::Named(name) => named_types.get(&name.to_string()).map(Clone::clone).ok_or(TypeError::UnknownTypeName(name.clone(), typ.span.clone())),
		TypeKind::Unit => Ok(Type::Unit),
		_ => todo!("{:?}", typ),
	}
}

use crate::source_span::SourceSpan;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct AST {
	pub functions: Box<[Function]>,
}

#[derive(Debug, Clone)]
pub struct AstNode<T> {
	pub span: SourceSpan,
	pub val: T,
}

#[derive(Debug, Clone)]
pub struct Function {
	pub span: SourceSpan,

	pub effects: Box<[Effect]>,
	pub name: Rc<str>,
	pub helpers: Box<[Function]>,

	pub params: Box<[Param]>,

	pub return_type: Type,

	pub body: Scope,
}

pub type Effect = AstNode<EffectKind>;
#[derive(Debug, Clone)]
pub enum EffectKind {
	Error,

	Named(Rc<str>),
}

pub type Param = AstNode<ParamKind>;
#[derive(Debug, Clone)]
pub enum ParamKind {
	Error,

	Ignored(Type),
	Named(Rc<str>, Type),
}

pub type Type = AstNode<TypeKind>;
#[derive(Debug, Clone)]
pub enum TypeKind {
	Error,

	Hole,
	Unit,
	Never,
	Named(Rc<str>),
	Tuple(Box<[Type]>),
	Function(Box<[Type]>, Box<Type>),
}

pub type Stmt = AstNode<StmtKind>;
#[derive(Debug, Clone)]
pub enum StmtKind {
	Error,

	Expr(Expr),
	Let(LetPattern, Expr),
	Return(Expr),
}

pub type LetPattern = AstNode<LetPatternKind>;
#[derive(Debug, Clone)]
pub enum LetPatternKind {
	Error,

	Hole(Option<Type>),
	Named(Mutability, Rc<str>, Option<Type>),
	Tuple(Box<[LetPattern]>),
}

#[derive(Debug, Clone)]
pub enum Mutability {
	Mutable,
	Immutable,
}

pub type Expr = AstNode<ExprKind>;
#[derive(Debug, Clone)]
pub enum ExprKind {
	Error,

	Assign(AssignPattern, Box<Expr>),
	Call(Box<Expr>, Box<[Expr]>),
	Infix(Box<Expr>, InfixOp, Box<Expr>),
	Prefix(PrefixOp, Box<Expr>),
	Suffix(Box<Expr>, SuffixOp),
	Integer(u64),
	Boolean(bool),
	Unit,
	Tuple(Box<[Expr]>),
	Scope(Scope),
	If(If),
	While(While),
	Lookup(Rc<str>),
}

pub type AssignPattern = AstNode<AssignPatternKind>;
#[derive(Debug, Clone)]
pub enum AssignPatternKind {
	Error,

	Hole,
	Named(Rc<str>),
	Tuple(Box<[AssignPattern]>),
}

pub type InfixOp = AstNode<Infix>;
#[derive(Debug, Clone)]
pub enum Infix {
	LogicOr,
	LogicAnd,

	LessThan,
	LessThanEqual,
	GreaterThan,
	GreaterThanEqual,
	Equal,
	NotEqual,

	BitOr,
	BitXor,
	BitAnd,

	ShiftLeft,
	ShiftRight,

	Add,
	Subtract,

	Multiply,
	Divide,
	Modulo,
}

pub type PrefixOp = AstNode<Prefix>;
#[derive(Debug, Clone)]
pub enum Prefix {
	Identity,
	Negate,
	Not,

	Reference,
	Dereference,
}

pub type SuffixOp = AstNode<Suffix>;
#[derive(Debug, Clone)]
pub enum Suffix {
	// temporary
	Yell,
	Query,
}

#[derive(Debug, Clone)]
pub struct Scope {
	pub body: Box<[Stmt]>,
	pub value: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct If {
	pub condition: Box<Expr>,
	pub body: Scope,
	pub _else: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct While {
	pub condition: Box<Expr>,
	pub body: Scope,
}

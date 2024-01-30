use crate::source_span::SourceSpan;

#[derive(Debug, Clone)]
pub struct TypedTree {
	pub functions: Box<[TypedFunc]>,
}

#[derive(Debug, Clone)]
pub struct TypedFunc {
	pub span: SourceSpan,

	pub full_name: Box<str>,
}

#[derive(Debug, Clone)]
pub enum Type {
	Unit,
	SignedInt(IntSize),
	UnsignedInt(IntSize),
}

use std::num::NonZeroU16;

#[derive(Debug, Clone, Copy)]
pub struct IntSize(NonZeroU16);

impl IntSize {
	pub fn new(size: u16) -> Option<Self> {
		match size {
			1..=256 => NonZeroU16::new(size).map(|nz| IntSize(nz)),
			_ => None,
		}
	}
}

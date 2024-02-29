use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct SourceSpan {
	pub path: Rc<str>,
	pub start: SourceLoc,
	pub end: SourceLoc,
}

#[derive(Debug, Clone, Copy)]
pub struct SourceLoc {
	pub line: usize,
	pub col: usize,
	pub idx: usize,
}

impl SourceSpan {
	pub fn extend(&self, other: &Self) -> Option<Self> {
		if self.path != other.path {
			return None;
		}
		let start = if self.start.idx < other.start.idx { self.start } else { other.start };
		let end = if self.end.idx > other.end.idx { self.end } else { other.end };
		return Some(Self {path: self.path.clone(), start, end});
	}
}

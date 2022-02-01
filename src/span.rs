/// 文字列中の範囲を示す
pub type Span = (usize, usize);

/// 型 T に範囲情報が付加されたもの
#[derive(Debug)]
pub struct Spanned<T> {
    pub item: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(item: T, span: Span) -> Self {
        Self { item, span }
    }
}

use std::fmt;

pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for LineColumn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "L{}, C{}", self.line, self.column)
    }
}

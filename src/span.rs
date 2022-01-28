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
    line: usize,
    column: usize,
}

impl fmt::Display for LineColumn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "L{}, C{}", self.line, self.column)
    }
}

// ソースコードと文字数から、行数とカラム数を得る
pub fn get_line_column(source_code: &str, position: usize) -> LineColumn {
    let line = source_code[0..position]
        .chars()
        .into_iter()
        .filter(|x| *x == '\n')
        .count()
        + 1;
    let column = position
        - match source_code[0..position].rfind('\n') {
            Some(offset) => offset + 1,
            None => 0,
        }
        + 1;
    LineColumn { line, column }
}

use once_cell::sync::OnceCell;

use crate::span::{LineColumn, Span};

pub static SOURCE_CODE: OnceCell<String> = OnceCell::new();

// ソースコードの部分文字列を返す
pub fn fragment(span: &Span) -> String {
    (SOURCE_CODE.get().unwrap())[span.0..span.1].to_string()
}

// ソースコード中の部分文字列と行・カラム数を返す
pub fn indexed_fragment(span: &Span) -> String {
    format!(
        "{} (from {} to {})",
        fragment(span),
        line_column(span.0),
        line_column(span.1)
    )
}

// ソースコード中の位置に対応する行数とカラム数を返す
pub fn line_column(position: usize) -> LineColumn {
    let source_code = SOURCE_CODE.get().unwrap();
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

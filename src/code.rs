use once_cell::sync::OnceCell;

use crate::span::{LineColumn, Span};

pub static SOURCE_CODE: OnceCell<String> = OnceCell::new();
static INDEX_TABLE: OnceCell<Vec<LineColumn>> = OnceCell::new();

// ソースコードの位置から行番号列番号を引ける配列を構築
pub fn build_index_table() -> Result<(), Vec<LineColumn>> {
    let source_code = SOURCE_CODE.get().unwrap();
    let mut index_table = vec![];
    let mut line = 1;
    let mut column = 1;
    index_table.push(LineColumn {line, column});
    source_code.as_bytes().iter().for_each(|c| {
        if *c == '\n' as u8 {
            line += 1;
            column = 1;
        }
        else {
            column += 1;
        }
        index_table.push(LineColumn { line, column });
    });
    INDEX_TABLE.set(index_table)
}

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
    (INDEX_TABLE.get().unwrap())[position].clone()
    // let source_code = SOURCE_CODE.get().unwrap();
    // let line = source_code[0..position]
    //     .chars()
    //     .into_iter()
    //     .filter(|x| *x == '\n')
    //     .count()
    //     + 1;
    // let column = position
    //     - match source_code[0..position].rfind('\n') {
    //         Some(offset) => offset + 1,
    //         None => 0,
    //     }
    //     + 1;
    // LineColumn { line, column }
}

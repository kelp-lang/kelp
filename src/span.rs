#[derive(Debug, Clone, Default)]
pub struct Pos {
    pub pos: usize,
    pub col: usize,
    pub ln: usize,
}

impl From<pest::Position<'_>> for Pos {
    fn from(pest_pos: pest::Position<'_>) -> Self {
        let (ln, col) = pest_pos.line_col();
        Self {
            pos: pest_pos.pos(),
            col,
            ln,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
    pub content: String,
}

impl From<pest::Span<'_>> for Span {
    fn from(pest_span: pest::Span) -> Self {
        Self {
            start: pest_span.start_pos().into(),
            end: pest_span.end_pos().into(),
            content: pest_span.as_str().to_string(),
        }
    }
}

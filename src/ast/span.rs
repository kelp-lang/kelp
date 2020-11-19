#[derive(Debug, Clone, Default)]
pub struct Pos {
    pos: usize,
    col: usize,
    ln: usize,
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
    start: Pos,
    end: Pos,
    pub content: String,
}

impl Span {
    pub fn start_pos(&self) -> usize {
        self.start.pos
    }
    pub fn end_pos(&self) -> usize {
        self.end.pos
    }
    pub fn start_ln(&self) -> usize {
        self.start.ln
    }
    pub fn end_ln(&self) -> usize {
        self.end.ln
    }
    pub fn start_col(&self) -> usize {
        self.start.col
    }
    pub fn end_col(&self) -> usize {
        self.end.col
    }
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

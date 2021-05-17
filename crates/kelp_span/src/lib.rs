extern crate kelp_origin;

use kelp_origin::Origin;

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
    pub origin: Origin,
}

impl From<(pest::Span<'_>, Origin)> for Span {
    fn from((pest_span, origin): (pest::Span, Origin)) -> Self {
        Self {
            start: pest_span.start_pos().into(),
            end: pest_span.end_pos().into(),
            content: pest_span.as_str().to_string(),
            origin,
        }
    }
}

pub trait Spans {
    fn spans(&self) -> Span;
}

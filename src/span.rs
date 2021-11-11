#[derive(Default, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub content: String,
    pub path: String,
    ln: usize,
    col: usize,
    line_start: usize,
    line_end: usize,
    pub props_generated: bool,
}

impl Span {
    pub fn ln(&mut self) -> usize {
        self.calculate_props();

        self.ln
    }

    pub fn col(&mut self) -> usize {
        self.calculate_props();

        self.col
    }

    pub fn line_start(&mut self) -> usize {
        self.calculate_props();

        self.line_start
    }

    pub fn line_end(&mut self) -> usize {
        self.calculate_props();

        self.line_end
    }

    fn calculate_props(&mut self) {
        if self.props_generated {
            return;
        }
        let mut line_count = 0;
        let mut last_lb_pos = 0;

        for (i, c) in self.content.chars().enumerate() {
            if c == '\n' {
                last_lb_pos = i;
                line_count += 1;
            }

            if i == self.start {
                self.line_start = last_lb_pos;
                self.ln = line_count;
                self.col = i - self.line_start;

                for (i, c) in self.content[self.end..].chars().enumerate() {
                    if c == '\n' {
                        self.line_end = i + self.end;
                        self.props_generated = true;
                        return;
                    }
                }
                self.line_end = self.end;
                self.props_generated = true;
                return;
            }
        } 
    }

    pub fn last_char_span(&self) -> Span {
        Span::new((self.start - 1).clamp(0, self.end), self.end, self.path.clone(), self.content.clone())
    }

    pub fn new(start: usize, end: usize, path: String, content: String) -> Span {
        Self {
            start,
            end,
            content,
            path,
            ln: 0,
            col: 0,
            line_start: 0,
            line_end: 0,
            props_generated: false,
        }
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = &self.content[self.start..self.end];
        write!(f, "{}", content)
    }
}
use std::fmt::Display;

#[derive(Debug, Clone, Copy, Default)]
pub struct Position<'a> {
    file: &'a str,
    index: usize,
    column: usize,
    line: usize,
}

impl<'a> Position<'a> {
    pub fn new(file: &'a str) -> Self {
        Self {
            file,
            index: 0,
            column: 1,
            line: 1,
        }
    }

    pub fn next(&mut self, ch: char) {
        self.index += ch.len_utf8();

        if matches!(ch, '\n') {
            self.line += 1;
        }

        if matches!(ch, '\n' | '\r') {
            self.column = 1
        } else {
            self.column += 1
        }
    }

    pub fn index(&self) -> usize {
        self.index
    }
}

impl<'a> Display for Position<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.column)
    }
}

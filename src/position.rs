use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug, Clone, Default)]
pub struct Position {
    file: Rc<String>,
    index: usize,
    column: usize,
    line: usize,
}

impl Position {
    pub fn new(file: &str) -> Self {
        Self {
            file: Rc::new(String::from(file)),
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

impl<'a> Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.column)
    }
}

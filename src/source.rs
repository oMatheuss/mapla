use std::path::PathBuf;

use crate::error::Result;

pub struct SourceManager<'a> {
    main: &'a str,
    dir: PathBuf,
}

pub struct Source<'a> {
    pub file: &'a str,
    pub src: String,
}

impl<'a> SourceManager<'a> {
    pub fn new(main: &'a str, dir: PathBuf) -> Self {
        Self { main, dir }
    }

    pub fn main(&self) -> Result<Source> {
        self.source(self.main)
    }

    pub fn source(&self, file: &'a str) -> Result<Source> {
        let path = self.dir.join(file);
        let src = std::fs::read_to_string(path)?;

        Ok(Source { src, file })
    }
}

use std::path::Path;

use crate::error::Result;
use crate::lexer::Lexer;
use crate::token::TokenStream;

pub struct SourceManager<'a> {
    main: &'a Path,
    dir: &'a Path,
}

#[derive(Eq, Hash, PartialEq)]
pub struct Source<'a> {
    pub file: &'a Path,
    pub src: String,
}

impl<'a> SourceManager<'a> {
    pub fn new(main: &'a Path, dir: &'a Path) -> Self {
        Self { main, dir }
    }

    pub fn main<'b>(&self) -> Result<Source<'b>>
    where
        'a: 'b,
    {
        self.source(self.main)
    }

    pub fn source<'b>(&self, file: &'b Path) -> Result<Source<'b>> {
        let path = self.dir.join(file);
        let src = std::fs::read_to_string(path)?;

        Ok(Source { src, file })
    }
}

impl<'a> Source<'a> {
    pub fn tokenize(&self) -> Result<TokenStream<'_>> {
        Lexer::new(self).try_into()
    }
}

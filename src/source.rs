use std::collections::HashSet;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::Path;

use crate::error::Result;
use crate::lexer::Lexer;
use crate::token::TokenStream;

pub struct SourceManager<'a> {
    main: &'a Path,
    dir: &'a Path,
    hashs: HashSet<u64>,
}

#[derive(Eq, Hash, PartialEq)]
pub struct Source<'a> {
    pub file: &'a Path,
    pub src: String,
    pub visited: bool,
}

impl<'a> SourceManager<'a> {
    pub fn new(main: &'a Path, dir: &'a Path) -> Self {
        let hashs = HashSet::new();
        Self { main, dir, hashs }
    }

    pub fn main<'b>(&mut self) -> Result<Source<'b>>
    where
        'a: 'b,
    {
        self.source(self.main)
    }

    pub fn source<'b>(&mut self, file: &'b Path) -> Result<Source<'b>> {
        let path = self.dir.join(file);
        let src = std::fs::read_to_string(path)?;
        let visited = !self.hashs.insert({
            let mut hasher = DefaultHasher::new();
            src.hash(&mut hasher);
            hasher.finish()
        });

        Ok(Source { src, file, visited })
    }
}

impl<'a> Source<'a> {
    pub fn tokenize(&self) -> Result<TokenStream<'_>> {
        Lexer::new(self).try_into()
    }
}

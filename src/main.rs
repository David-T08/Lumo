//use std::fs;
use std::borrow::Cow;

mod lexer;
mod tokens;

use tokens::{KeywordKind, OperatorKind, Token, TokenKind};
use lexer::Lexer;

fn main() -> Result<(), Box<dyn std::error::Error + 'static>> {
    //fs::read(path)

    let data: Vec<u8>  = ("\t  \"Hello, world!\"+-").as_bytes().into();

    let l = Lexer::new("main.rs", &data);
    l.for_each(|tok| {
        println!("{tok}");
    });

    Ok(())
}

mod ast;
mod lexer;
mod macros;
mod parser;
mod tokens;

use std::io::{self, Write};

use crate::{lexer::Lexer, parser::Parser};

fn main() {
    println!("Lumo 0.1.0 RLPL");
    println!("  Type exit to stop execution");

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut command = String::new();
        if io::stdin().read_line(&mut command).is_err() {
            println!("Failed to read input");
            continue;
        }

        let command = command.trim();
        if command == "exit" {
            break;
        }

        let command: Vec<u8> = command.as_bytes().into();

        let lexer = Lexer::new("cli", &command);
        for tok in Lexer::new("cli", &command) {
            println!("{tok}");
        }

        println!("\n== Parsing ==");
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        dbg!(program);
    }
}

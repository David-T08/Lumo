use lexer::Lexer;
use std::io::{self, Write};

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

        let l = Lexer::new("cli", &command);
        l.for_each(|tok| {
            println!("{tok}");
        });
    }
}

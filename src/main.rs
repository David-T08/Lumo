mod ast;
mod lexer;
mod macros;
mod parser;
mod tokens;

use std::io::{self, Write};

use tracing_subscriber::{
    EnvFilter,
    fmt::{format::Writer, time::FormatTime},
};

use tracing::{debug, error, info, trace, warn};

use crate::{lexer::Lexer, parser::Parser};

use std::sync::OnceLock;
use std::time::Instant;

static START: OnceLock<Instant> = OnceLock::new();

struct RelTime;

impl FormatTime for RelTime {
    fn format_time(&self, w: &mut Writer<'_>) -> std::fmt::Result {
        let start = START.get_or_init(Instant::now);
        let elapsed = start.elapsed();

        let total_ms = elapsed.as_millis() as u128;
        let minutes = (total_ms / 60_000) % 100; // wrap after 99 if you want
        let seconds = (total_ms / 1_000) % 60;
        let millis = total_ms % 1_000;

        write!(w, "{:02}:{:02}.{:03}", minutes, seconds, millis)
    }
}

fn main() {
    tracing_subscriber::fmt()
        .with_timer(RelTime)
        .with_env_filter(EnvFilter::from_default_env())
        .with_target(false)
        .with_level(true)
        .with_file(true)
        .with_line_number(true)
        .with_span_events(tracing_subscriber::fmt::format::FmtSpan::ENTER)
        .init();

    info!("Lumo 0.1.0 RLPL");
    info!("  Type exit to stop execution");

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut command = String::new();
        if io::stdin().read_line(&mut command).is_err() {
            error!("Failed to read input");
            continue;
        }

        let command = command.trim();
        if command == "exit" {
            break;
        }

        let command: Vec<u8> = command.as_bytes().into();

        let lexer = Lexer::new("cli", &command);

        info!("== Lexing ==");
        for tok in Lexer::new("cli", &command) {
            info!("{tok}\n");
        }

        info!("== Parsing ==");
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        debug!("\n{:#?}", program);

        if program.errors.len() > 0 {
            info!("== Errors ==");
            for e in program.errors {
                error!("{e}");
            }
        }

        println!();
        println!();
    }
}

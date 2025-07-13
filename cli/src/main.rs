use lexer::Lexer;

fn main() -> Result<(), Box<dyn std::error::Error + 'static>> {
    // let input = "\"Hello, world!\"+-while";
    let input = "while true 123 0x1F 0b110101 true";
    let data: Vec<u8> = input.as_bytes().into();

    println!("Lexing: {}, {}", input, input.len());

    let l = Lexer::new("main.rs", &data);
    l.for_each(|tok| {
        println!("{tok}");
    });

    Ok(())
}

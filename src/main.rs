mod parse;

fn main() {
    println!(
        "{:?}",
        parse::lex::Lexer::new("100 200_300.72e1e.7.5e4")
            .map(|i| i.unwrap())
            .collect::<Vec<_>>()
    );
}

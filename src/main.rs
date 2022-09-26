mod parse;

fn main() {
    println!(
        "{:?}",
        parse::lex::Lexer::new("100 200_300.72e17e.7.5e4.hello72.4e2")
            .map(|i| i.unwrap())
            .collect::<Vec<_>>()
    );
}

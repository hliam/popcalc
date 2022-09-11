#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Num {}

impl Num {}

impl<S: AsRef<str>> From<S> for Num {
    fn from(s: S) -> Self {
        todo!()
    }
}

/// A module containing things relevant to the lexicographical analyzer.
pub mod lex {
    use peeking_take_while::PeekableExt;
    use std::fmt::Display;

    use super::Num;

    /// A lexical token.
    /// 
    /// Some symbols (such as `**` and `^`) are folded into one token here. Mostly because having an
    /// intermediate representation step is too cumbersome to justify.
    #[derive(Debug, Clone, Eq, PartialEq)]
    pub enum Token {
        Ident(String),
        Num(Num),
        Semicolon,
        Comma,
        Dot,
        OpenParen,
        CloseParen,
        OpenBracket,
        CloseBracket,
        Equal,
        EqualityTest,
        InequalityTest,
        GreaterThan,
        LessThan,
        GreaterOrEqual,
        LessOrEqual,
        Add,
        Sub,
        Mul,
        Div,
        Pow,
        Factorial,
        Dollar,
    }

    trait IterExt<T> {
        /// Peek two items ahead. Like `std::iter::PeekingIter::peek`.
        fn peek_two_ahead(&self) -> Option<T>;
    }
    impl<I: Iterator + Clone> IterExt<I::Item> for I {
        #[must_use]
        fn peek_two_ahead(&self) -> Option<I::Item> {
            self.clone().skip(1).next()
        }
    }

    /// An iterable lexicographical analyzer that produces lexicographical tokens.
    #[derive(Debug, Clone)]
    pub struct Lexer<I: Iterator<Item = char>> {
        iter: std::iter::Peekable<I>,
    }

    impl<I: Iterator<Item = char> + Clone> Lexer<I> {
        /// Consume an operator if one is at the front of the source iterator.
        fn maybe_consume_op(&mut self) -> Option<Token> {
            use Token::*;

            let two_ahead = self.iter.peek_two_ahead();

            let out = match *self.iter.peek()? {
                ';' => Semicolon,
                ',' => Comma,
                '(' => OpenParen,
                ')' => CloseParen,
                '[' => OpenBracket,
                ']' => CloseBracket,
                '+' => Add,
                '-' => Sub,
                '/' => Div,
                '$' => Dollar,
                // If the dot is followed by a digit, then it''s a num, so we don't handle it in
                // this method.
                '.' if two_ahead.filter(|i| i.is_ascii_digit()).is_some() => return None,
                '.' => Dot,
                '^' => Pow,
                '*' if two_ahead.filter(|&i| i == '*').is_some() => Pow,
                '*' => Mul,
                // Two-wide ops
                c @ ('>' | '<' | '=' | '!') if two_ahead.filter(|&i| i == '=').is_some() => {
                    self.iter.next();

                    match c {
                        '>' => GreaterOrEqual,
                        '<' => LessOrEqual,
                        '=' => EqualityTest,
                        '!' => InequalityTest,
                        _ => panic!("didn't match two-wide op"),
                    }
                }
                '=' => Equal,
                '>' => GreaterThan,
                '<' => LessThan,
                '!' => Factorial,
                _ => return None,
            };

            self.iter.next();
            Some(out)
        }

        /// Consume an identifier if one is at the front of the source iterator.
        fn maybe_consume_ident(&mut self) -> Option<Token> {
            // TODO: negatively define valid ident chars.
            let c = *self.iter.peek()?;

            (c.is_ascii_alphabetic() || c == '_').then(|| {
                Token::Ident(
                    self.iter
                        .peeking_take_while(|&c| c.is_ascii_alphanumeric() || c == '_')
                        .collect(),
                )
            })
        }

        /// Consume a number from the front of the source iterator.
        /// 
        /// It's assumed that an op and ident have already been checked for, so if this function is
        /// being called, the front of the source iter should be a number. An error will be returned
        /// if it isn't or if the number is invalid.
        fn try_consume_num(&mut self) -> Result<Token, InvalidNumError> {
            // We only get to this point when the next token is a number (or at least, is supposed
            // to be a number).
            // '+' and '-' are considered unary ops and are parsed by the op method, not here.

            // Take up to either `.`, `e`, or the end of the num.
            let mut buf: String = self
                .iter
                .peeking_take_while(|c| matches!(c, '0'..='9' | ' ' | '_'))
                // .peeking_take_while_digit_or_sep()
                .collect();

            // Check for the decimal.
            if let (Some('.'), Some('0'..='9')) =
                (self.iter.peek().map(|i| *i), self.iter.peek_two_ahead())
            {
                buf.extend(self.iter.next());
                // Look for `.e` (which is invalid).
                if let Some('e') = self.iter.peek() {
                    return Err(InvalidNumError);
                }
                buf.extend(
                    self.iter
                        .peeking_take_while(|c| matches!(c, '0'..='9' | ' ' | '_')),
                );
            }

            // check for the e.
            if let (Some('e'), Some('0'..='9')) =
                (self.iter.peek().map(|i| *i), self.iter.peek_two_ahead())
            {
                buf.extend(self.iter.next());
                buf.extend(
                    self.iter
                        .peeking_take_while(|c| matches!(c, '0'..='9' | ' ' | '_')),
                );
            }

            if buf.contains("  ") || buf.contains("__") {
                Err(InvalidNumError)
            } else {
                Ok(Token::Num(buf.into()))
            }
        }
    }

    // TODO: give this struct some semantic information once everything is working.
    /// An error indicating an invalid number.
    /// 
    #[derive(Debug, Copy, Clone)]
    pub struct InvalidNumError;
    impl std::error::Error for InvalidNumError {}
    impl Display for InvalidNumError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("Invalid number")
        }
    }

    impl<'a> Lexer<std::str::Chars<'a>> {
        /// Creates a new `Lexer` from a string.
        #[must_use]
        pub fn new(s: &'a str) -> Self {
            Self {
                iter: s.chars().peekable(),
            }
        }
    }

    impl<'a, I: Iterator<Item = char> + Clone> Iterator for Lexer<I> {
        type Item = Result<Token, InvalidNumError>;

        #[inline]
        fn next(&mut self) -> Option<Self::Item> {
            self.iter.peek()?;

            // Skip whitespace
            self.iter
                .peeking_take_while(|c| c.is_ascii_whitespace())
                .for_each(drop);

            if let Some(op) = self.maybe_consume_op() {
                return Some(Ok(op));
            }

            if let Some(ident) = self.maybe_consume_ident() {
                return Some(Ok(ident));
            }

            Some(self.try_consume_num())
        }
    }

    #[cfg(test)]
    mod tests {
        use super::{InvalidNumError, Lexer, Token};

        macro_rules! assert_tokens_helper {
            (Ident($token:expr)) => {
                crate::parse::lexer::Token::Ident(String::from($expr))
            };
            (Num($tokens:expr)) => {
                crate::parse::lexer::Token::Num(String::from($expr))
            }; // ($token:expr) => {
               //     println!("bad: '{}'", stringify!($token));
               //     // crate::parse::lexer::Token::$token;
               // };
        }

        macro_rules! assert_tokens {
            ($_:expr, $tokens:tt) => {
                println!("'{}'", stringify!($tokens));
                assert_tokens_helper!($tokens);
            };
            ($test_case:expr, $($expected:tt),+) => {
                let mut buf = Vec::new();
                $(
                    {
                        buf.push(assert_tokens_helper!($expected));
                    }
                )+

                Lexer::new($test_case)
                    .into_iter()
                    .collect::<Result<Vec<_>, _>>()
                    .map(|tokens| {
                        assert_eq!(tokens, buf);
                    })
            };
        }

        #[test]
        fn iter_peek_two_ahead() {
            use super::IterExt;

            let mut iter = [1, 2, 3].iter();
            assert_eq!(iter.peek_two_ahead(), Some(&2));
            iter.next();
            assert_eq!(iter.peek_two_ahead(), Some(&3));
            iter.next();
            assert_eq!(iter.peek_two_ahead(), None);
        }

        #[test]
        fn single_valid_nums() -> Result<(), InvalidNumError> {
            // let tokens: Vec<_> = Lexer::new("5").into_iter().collect::<Result<_, _>>()?;
            // assert_eq!(tokens, [Token::Num(String::from("5"))]);

            // assert_tokens!("5", Num(5));

            Ok(())
        }
    }
}

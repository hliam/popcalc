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
        Num(String), // TODO: make this a `Num` & not a string.
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

    impl Token {
        fn precedence(&self) -> Option<u8> {
            use Token::*;

            // TODO: idk about any of this
            // parens especially
            Some(match self {
                Ident(_) | Num(_) | Semicolon | Comma => return None,
                OpenParen | CloseParen | OpenBracket | CloseBracket => 0,
                Factorial | Dollar => 1,
                Dot => 1,
                Pow => 2,
                Mul | Div => 3,
                Add | Sub => 4,
                EqualityTest | InequalityTest | GreaterThan | LessThan | GreaterOrEqual
                | LessOrEqual => 5,
                Equal => 6,
            })
        }
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

            /// Reads digits or separators out of `iter` and into `buf`.
            ///
            /// An error will be returned on two consecutive separators.
            fn read_digits_or_seps(
                iter: &mut std::iter::Peekable<impl Iterator<Item = char>>,
                buf: &mut String,
            ) -> Result<(), InvalidNumError> {
                iter.peeking_take_while(|c| matches!(c, '0'..='9' | ' ' | '_'))
                    .try_fold(false, |last_was_sep, c| {
                        buf.push(c);

                        // TODO: clean up once we have if-let chaining
                        Ok(match c {
                            ' ' | '_' if last_was_sep => return Err(InvalidNumError),
                            _ => matches!(c, ' ' | '_'),
                        })
                    })
                    .map(drop)
            }

            /// Reads a char (`'.'` or `'e'`) component of the number then all following digits into
            /// a buffer.
            ///
            /// The next two characters of the iterator should be `c` followed by a digit, otherwise
            /// nothing happens.
            fn maybe_read_char_then_digits<'a, I: Iterator<Item = char> + Clone>(
                iter: &'a mut std::iter::Peekable<I>,
                c: char,
                buf: &mut String,
            ) -> Result<(), InvalidNumError> {
                // TODO: cleanup once we get if-let chaining.
                match (iter.peek().map(|i| *i), iter.peek_two_ahead()) {
                    (Some(a), Some('0'..='9')) if a == c => {
                        buf.extend(iter.next());
                        read_digits_or_seps(iter, buf)
                    }
                    _ => Ok(()),
                }
            }

            // Something is malformed if the next char isn't a digit or dot.
            // TODO: cleanup once we get if-let chaining.
            match self.iter.peek() {
                Some(c) if !matches!(c, '0'..='9' | '.') => return Err(InvalidNumError),
                _ => (),
            }

            let mut buf = String::new();
            // Take up to either `.`, `e`, or the end of the num.
            read_digits_or_seps(&mut self.iter, &mut buf)?;
            // Take up to the `e` or the end of the num.
            maybe_read_char_then_digits(&mut self.iter, '.', &mut buf)?;
            // Take up to the end of the num.
            maybe_read_char_then_digits(&mut self.iter, 'e', &mut buf)?;

            // TODO: at some point when the the number backend is decided, compose it directly from
            // the parts.
            // TODO: make this return a `Num`. The string is for testing.
            Ok(Token::Num(buf))
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
            // Skip whitespace
            self.iter
                .peeking_take_while(|c| c.is_ascii_whitespace())
                .for_each(drop);

            self.iter.peek()?;

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

mod ast {
    use super::lex::{Lexer, Token};
    use super::Num;

    pub enum Expr {
        Ident(String),
        Num(Num),
        // Binary ops
        Dot(Box<Expr>, Box<Expr>),
        Equal(Box<Expr>, Box<Expr>),
        EqualityTest(Box<Expr>, Box<Expr>),
        InequalityTest(Box<Expr>, Box<Expr>),
        GreaterThan(Box<Expr>, Box<Expr>),
        LessThan(Box<Expr>, Box<Expr>),
        GreaterOrEqual(Box<Expr>, Box<Expr>),
        LessOrEqual(Box<Expr>, Box<Expr>),
        Add(Box<Expr>, Box<Expr>),
        Sub(Box<Expr>, Box<Expr>),
        Mul(Box<Expr>, Box<Expr>),
        Div(Box<Expr>, Box<Expr>),
        Pow(Box<Expr>, Box<Expr>),
        // Unary ops
        Parens(Box<Expr>),
        Brackets(Box<Expr>),
        Factorial(Box<Expr>),
        Dollar(Box<Expr>),
    }

    pub struct ASTParser<I: Iterator<Item = char> + Clone> {
        lexer: Lexer<I>,
    }

    impl<I: Iterator<Item = char> + Clone> ASTParser<I> {
        /// Creates a new `ASTParser` from a `Lexer`.
        #[must_use]
        fn new(lexer: Lexer<I>) -> Self {
            Self { lexer }
        }
    }

    impl<I: Iterator<Item = char> + Clone> Iterator for ASTParser<I> {
        type Item = Expr;

        fn next(&mut self) -> Option<Self::Item> {
            use Token::*;

            let mut queue: Vec<Token> = Vec::new();

            loop {
                let token = self.lexer.next()?;
            }

            todo!()
        }
    }
}

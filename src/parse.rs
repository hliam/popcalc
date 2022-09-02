pub mod lex {
    use peeking_take_while::PeekableExt;
    use std::fmt::Display;

    #[derive(Debug, Clone, Eq, PartialEq)]
    pub enum Token {
        Ident(String),
        Num(String),
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
        fn peek_two(&self) -> Option<T>;
    }
    impl<I: Iterator + Clone> IterExt<I::Item> for I {
        #[must_use]
        fn peek_two(&self) -> Option<I::Item> {
            self.clone().skip(1).next()
        }
    }

    #[derive(Debug, Clone)]
    pub struct Lexer<I: Iterator<Item = char>> {
        iter: std::iter::Peekable<I>,
        token_buffer: Vec<Result<Token, InvalidNumError>>,
    }

    impl<I: Iterator<Item = char> + Clone> Lexer<I> {
        fn maybe_consume_op(&mut self) -> Option<Token> {
            use Token::*;

            let two_ahead = self.iter.peek_two();

            let out = match *self.iter.peek()? {
                ';' => Semicolon,
                ',' => Comma,
                '(' => OpenParen,
                ')' => CloseParen,
                '[' => OpenBracket,
                ']' => CloseBracket,
                '+' => Add,
                '-' => Sub,
                '*' => Mul,
                '/' => Div,
                '^' => Pow,
                '$' => Dollar,
                // If the dot is followed by a digit, then it''s a num, so we don't handle it in
                // this method.
                '.' if two_ahead.filter(|i| i.is_ascii_digit()).is_some() => return None,
                '.' => Dot,
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

        fn try_consume_num(&mut self) -> Result<Token, InvalidNumError> {
            // We only get to this point when the next token is a number (or at least, is supposed
            // to be a number).

            // Take up to either `.`, `e`, or the end of the num.
            let mut buf: String = self
                .iter
                .peeking_take_while(|c| c.is_ascii_digit() || matches!(c, ' ' | '_'))
                .collect();

            // TODO: this is ugly and hacky. fix.
            let mut should_check_for_e = true;

            if let Some('.') = self.iter.peek() {
                self.iter.next();
                if self.iter.peek().filter(|c| c.is_ascii_digit()).is_some() {
                    buf.push('.');
                    buf.extend(
                        self.iter
                            .peeking_take_while(|c| c.is_ascii_digit() || matches!(c, ' ' | '_')),
                    );
                } else {
                    self.token_buffer.insert(0, Ok(Token::Dot));
                    should_check_for_e = false;
                }
            }

            if should_check_for_e {
                if let Some('e') = self.iter.peek() {
                    self.iter.next();
                    match self.iter.peek() {
                        // `e` is part of the number
                        Some(c) if c.is_ascii_digit() => {
                            buf.push('e');
                            buf.extend(self.iter.peeking_take_while(|c| {
                                c.is_ascii_digit() || matches!(c, ' ' | '_')
                            }));
                        }
                        // `e` is part of/an ident
                        _ => {
                            match self.next() {
                                // `e` is the first letter of the ident
                                Some(Ok(Token::Ident(mut value))) => {
                                    value.insert(0, 'e');
                                    self.token_buffer.insert(0, Ok(Token::Ident(value)));
                                }
                                // `e` is the whole ident
                                Some(other) => {
                                    self.token_buffer
                                        .insert(0, Ok(Token::Ident("e".to_owned())));
                                    self.token_buffer.insert(0, other);
                                }
                                None => (),
                            }
                        }
                    }
                }
            }

            if buf.contains("  ") || buf.contains("__") {
                Err(InvalidNumError)
            } else {
                Ok(Token::Num(buf))
            }
        }
    }

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
                token_buffer: Vec::new(),
            }
        }
    }

    impl<'a, I: Iterator<Item = char> + Clone> Iterator for Lexer<I> {
        type Item = Result<Token, InvalidNumError>;

        fn next(&mut self) -> Option<Self::Item> {
            // Try to pop out of the buffer first
            if !self.token_buffer.is_empty() {
                return self.token_buffer.pop();
            }

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
}

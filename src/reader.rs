use regex::Captures;
use regex::Regex;

use lazy_static::lazy_static;

use crate::span::Span;
use crate::token::Token;
use crate::token::TokenInner;

type StringToken = (String, Span);

#[derive(Debug, Clone)]
struct Reader {
    tokens: Vec<StringToken>,
    pos: usize,
    last_span: Span,
}

impl Reader {
    fn new(tokens: Vec<StringToken>) -> Self {
        Self {
            tokens,
            pos: 0,
            last_span: Span::new(0, 0, "".to_string(), "".to_string()),
        }
    }
    fn last_span(&self) -> Span {
        self.last_span.clone()
    }

    fn next(&mut self) -> Option<StringToken> {
        self.pos += 1;

        match self.tokens.get(self.pos - 1) {
            Some(token) => {
                self.last_span = token.1.clone();
                Some(token.clone())
            }
            None => None,
        }
    }

    fn peek(&mut self) -> Option<StringToken> {
        match self.tokens.get(self.pos) {
            Some(token) => {
                self.last_span = token.1.clone();
                Some(token.clone())
            }
            None => None,
        }
    }
}

macro_rules! safe_next {
    ($rdr:expr,$expected:literal) => {
        match $rdr.next() {
            Some(value) => value,
            None => {
                let span = $rdr.last_span().last_char_span();
                error!(span, "Expected '{}' got EOF", $expected);
                return Token::new(TokenInner::Empty, span);
            }
        }
    };
}

macro_rules! safe_peek {
    ($rdr:expr,$expected:literal) => {
        match $rdr.peek() {
            Some(value) => value,
            None => {
                let span = $rdr.last_span().last_char_span();
                error!(span, "Expected '{}' got EOF", $expected);
                return Token::new(TokenInner::EOF, span);
            }
        }
    };
}

fn tokenize(str: &str, path: String) -> Vec<StringToken> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r###"(\r\n|\r|\n)|(//.*)|("(?:\\.|[^\\"])*"?)|([\(\)\[\]\{\}'`~!])|(-?[0-9]*\.[0-9]+)|(-?[0-9]+)|([^\s\(\)\[\]\{\}]+)"###).unwrap();
        static ref IS_COMMENT_RE: Regex = Regex::new(r###"//.*"###).unwrap();
    }

    RE.find_iter(str)
        .filter_map(|token| {
            let span = Span::new(token.start(), token.end(), path.clone(), str.to_string());

            if IS_COMMENT_RE.is_match(token.as_str()) {
                None
            } else {
                Some((token.as_str().to_string(), span))
            }
        })
        .collect()
}

fn unescape_str(s: &str) -> String {
    lazy_static! {
        static ref RE: Regex = Regex::new(r#"\\(.)"#).unwrap();
    }
    RE.replace_all(&s, |caps: &Captures| {
        format!("{}", if &caps[1] == "n" { "\n" } else { &caps[1] })
    })
    .to_string()
}

fn read_atom(rdr: &mut Reader) -> Token {
    lazy_static! {
        static ref FLOAT_RE: Regex = Regex::new(r###"^-?[0-9]*\.[0-9]+$"###).unwrap();
        static ref INT_RE: Regex = Regex::new(r###"^-?[0-9]+$"###).unwrap();
        static ref STRING_RE: Regex = Regex::new(r###"^"(?:\\.|[^\\"])*""###).unwrap();
    }

    let token = safe_next!(rdr, "Atom");

    match &token.0[..] {
        "false" => Token::new(TokenInner::Bool(false), token.1),
        "true" => Token::new(TokenInner::Bool(true), token.1),
        _ => {
            if FLOAT_RE.is_match(token.0.as_str()) {
                Token::new(TokenInner::Float(token.0.parse().unwrap()), token.1)
            } else if INT_RE.is_match(token.0.as_str()) {
                Token::new(TokenInner::Int(token.0.parse().unwrap()), token.1)
            } else if STRING_RE.is_match(token.0.as_str()) {
                Token::new(
                    TokenInner::String(unescape_str(&token.0[1..token.0.len() - 1])),
                    token.1,
                )
            } else if token.0.starts_with("\"") {
                let last_char_span = token.1.last_char_span();
                error!(last_char_span, "Expected '\"' got EOF");
                Token::new(TokenInner::Empty, token.1)
            } else if token.0.starts_with(":") {
                Token::new(TokenInner::Keyword(token.0), token.1)
            } else {
                Token::new(TokenInner::Sym(token.0), token.1)
            }
        }
    }
}

fn read_ln(rdr: &mut Reader, end_c: &str) -> Token {
    let mut seq: Vec<Token> = vec![];

    while let Some(token) = rdr.peek() {
        if &token.0 == "\n" {
            rdr.next();
            break;
        } else if &token.0 == end_c {
            break;
        } else {
            seq.push(read_form(rdr));
        }
    }

    let span = Span::new(
        match seq.first() {
            Some(token) => token.span().start,
            _ => 0,
        },
        rdr.last_span().end,
        rdr.last_span().path,
        rdr.last_span().content,
    );
    Token::new(TokenInner::Seq(seq), span)
}

fn read_seq(rdr: &mut Reader, end_c: &str, start: usize) -> Token {
    let mut seq: Vec<Token> = vec![];
    let mut multiline = false;
    while let Some(token) = rdr.peek() {
        if &token.0 == "\n" && end_c == ")" {
            if seq.len() == 0 {
                multiline = true;
            }
        }

        if &token.0 == end_c {
            rdr.next();
            break;
        } else {
            if end_c == "\u{0E0F}" {
                seq.push(read_seq(rdr, "\n", start))
            } else if multiline {
                seq.push(read_ln(rdr, end_c))
            } else {
                seq.push(read_form(rdr))
            }
        }
        // if &token.0 == end_c {
        //     rdr.next();
        //     break;
        // } else if &token.0 == "\n" {
        //     if end_c == ")" && seq.len() == 0 {
        //         multiline = true;
        //     }

        //     rdr.next();
        //     continue;
        // } else if end_c == "\u{0E0F}" {
        //     seq.push(read_seq(rdr, "\n", start))
        // } else {
        //     seq.push(read_form(rdr));
        //     if multiline {

        //     }
        // }
    }
    if multiline {
        println!("multiline");
    }
    //println!("{}", seq.iter().map(|s| s.to_string()).collect::<Vec<_>>().join("\n"));
    let span = Span::new(
        start,
        rdr.last_span().end,
        rdr.last_span().path,
        rdr.last_span().content,
    );
    match end_c {
        ")" => Token::new(TokenInner::Seq(seq), span),
        "]" => Token::new(TokenInner::List(seq), span),
        "}" => Token::new(
            TokenInner::Tuple(
                seq.chunks(2)
                    .map(|chunk| (chunk[0].clone(), chunk[1].clone()))
                    .collect(),
            ),
            span,
        ),
        "\n" => Token::new(TokenInner::Seq(seq), span),
        "\u{0E0F}" => Token::new(TokenInner::Seq(seq), span),
        &_ => {
            error!(span, "Uknown sequence terminator '{}'", end_c);
            Token::new(TokenInner::Empty, span)
        }
    }
}

fn read_form(rdr: &mut Reader) -> Token {
    let token = safe_peek!(rdr, "Form");
    match &token.0[..] {
        // "\n" => {
        //     let _ = rdr.next();
        //     match rdr.peek() {
        //         Some(t) => read_seq(rdr, "\n", t.1.start),
        //         None => Token::new(TokenInner::EOF, rdr.last_span().last_char_span())
        //     }
        // },
        "'" => {
            let _ = rdr.next();
            let token = read_form(rdr);
            Token::new(TokenInner::Quote(token.clone()), token.span())
        },
        /*"`" => {
            let _ = rdr.next();
            read_form(rdr).quasiquote()
        },
        "~" => {
            let _ = rdr.next();
            read_form(rdr).unquote()
        },
        "!" => {
            let _ = rdr.next();
            read_form(rdr).eval()
        }*/
        ")" | "]" | "}" => {
            let span = token.1;
            error!(span, "Unexpected {}", token.0);
            Token::new(TokenInner::Empty, span)
        }
        "(" => {
            rdr.next();
            read_seq(rdr, ")", token.1.start)
        }
        "[" => {
            rdr.next();
            read_seq(rdr, "]", token.1.start)
        }
        "{" => {
            rdr.next();
            read_seq(rdr, "}", token.1.start)
        }
        _ => read_atom(rdr),
    }
}

pub fn read_string(str: String, path: String) -> Token {
    let tokens = tokenize(&str, path);
    if tokens.len() == 0 {
        error!("The string is empty!");
        return Token::new(TokenInner::Empty, Span::default());
    }
    /*println!("{}", tokens.iter().fold(String::new(), |acc, (v, s)| {
        format!("{} {}", acc, v)
    }));*/
    let mut rdr = Reader::new(tokens);
    let token_tree = read_seq(&mut rdr, "\u{0E0F}", 0);

    token_tree
}

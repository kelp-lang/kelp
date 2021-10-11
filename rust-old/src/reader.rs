use lazy_static::lazy_static;
use regex::{Captures, Regex};

use crate::{
    ast::Span,
    types::{KelpVal, _KelpVal},
};

#[derive(Debug, Clone)]
struct Reader {
    tokens: Vec<(String, Span)>,
    pos: usize,
    last_span: Span,
}

impl Reader {
    fn new(tokens: Vec<(String, Span)>) -> Self {
        Self {
            tokens,
            pos: 0,
            last_span: Span::new(0, 0, "".to_string()),
        }
    }
    fn last_span(&self) -> Span {
        self.last_span.clone()
    }

    fn next(&mut self) -> Option<(String, Span)> {
        self.pos += 1;

        match self.tokens.get(self.pos - 1) {
            Some(token) => {
                self.last_span = token.1.clone();
                Some(token.clone())
            }
            None => None,
        }
    }

    fn peek(&mut self) -> Option<(String, Span)> {
        match self.tokens.get(self.pos) {
            Some(token) => {
                self.last_span = token.1.clone();
                Some(token.clone())
            }
            None => None,
        }
    }
}

fn tokenize(input: &str) -> Vec<(String, Span)> {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            r###"(\r\n|\r|\n)|(//.*)|("[^"]*")|(&\{|[\(\)\[\]\{\}])|([0-9]+(\.[0-9]+)?)|:|([^\s\[\]\(\)\{\}])+"###
        )
        .unwrap();
        static ref IS_COMMENT_RE: Regex = Regex::new(r###"//.*"###).unwrap();
    }

    RE.find_iter(input)
        .filter_map(|token| {
            let span = Span::new(token.start(), token.end(), input.to_string());
            //println!("{:#?}", token);
            if IS_COMMENT_RE.is_match(token.as_str()) {
                None
            } else {
                Some((String::from(token.as_str()), span))
            }
        })
        .collect()
}

fn is_operator(s: &str) -> bool {
    lazy_static! {
        static ref RE: Regex = Regex::new(
            r###"("[^"]*")|([\(\)\[\]\{\}])|(\r\n|\r|\n)|(-?[0-9]+(\.[0-9]+)?)|([a-zA-Z].*)|(.)"###
        )
        .unwrap();
    }

    !RE.is_match(&s)
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

fn read_sym(rdr: &mut Reader) -> KelpVal {
    lazy_static! {
        static ref SYMBOL_RE: Regex = Regex::new(r###"^([^:]+)(:)"###).unwrap();
    }

    let token = rdr.next().unwrap();

    KelpVal::new(if let Some(name) = SYMBOL_RE.captures_iter(&token.0).nth(0) {
        _KelpVal::Sym {
            name: name.get(1).unwrap().as_str().to_string(),
            typ: Some(read_form(rdr)),
        }
    } else if let Some(maybe_colon) = rdr.peek() {
        if maybe_colon.0 == ":" {
            _KelpVal::Sym {
                name: token.0,
                typ: Some(read_form(rdr)),
            }
        } else {
            _KelpVal::Sym {
                name: token.0,
                typ: None,
            }
        }
    } else {
        _KelpVal::Sym {
            name: token.0,
            typ: None,
        }
    }, token.1)
}

fn read_atom(rdr: &mut Reader) -> KelpVal {
    lazy_static! {
        static ref INT_RE: Regex = Regex::new(r###"^-?[0-9]+$"###).unwrap();
        static ref STRING_RE: Regex = Regex::new(r###"^"[^"]*"$"###).unwrap();
        static ref FLOAT_RE: Regex = Regex::new(r###"^-?[0-9]\.[0-9]+$"###).unwrap();
    }
    let token = match rdr.peek() {
        Some(token) => token,
        None => {
            let span = rdr.last_span();
            error!(span, "Missing atom");
            return KelpVal::new(_KelpVal::Unknown, span);
        }
    };
        match &token.0[..] {
            "true" => {
                rdr.next();
                KelpVal::new(_KelpVal::Bool(true), token.1)
            },
            "false" => {
                rdr.next();
                KelpVal::new(_KelpVal::Bool(false), token.1)
            },
            token_text => {
                if FLOAT_RE.is_match(&token_text) {
                    rdr.next();
                    KelpVal::new(_KelpVal::Float32(token_text.parse().unwrap()), token.1)
                } else if INT_RE.is_match(&token_text) {
                    rdr.next();
                    KelpVal::new(_KelpVal::Int64(token_text.parse().unwrap()), token.1)
                } else if STRING_RE.is_match(&token_text) {
                    rdr.next();
                    KelpVal::new(_KelpVal::Str(unescape_str(&token_text[0..token_text.len() - 1])), token.1)
                } else {
                    read_sym(rdr)
                }
            }
        }

}

fn read_body(rdr: &mut Reader) -> KelpVal {
    let mut exprs: Vec<KelpVal> = vec![];
    let mut end: usize;
    loop {
        if let Some(token) = rdr.peek() {
            let start = token.1.start;
            let content = token.1.content;
            end = token.1.end;
            match &token.0[..] {
                ")" => {
                    let _ = rdr.next();
                    return KelpVal::new(_KelpVal::Body(exprs), Span::new(start, end, content));
                }
                _ => {
                    exprs.push(read_expr(rdr));
                }
            }
        } else {
            // let span = rdr.last_span();
            // error!(span, "Expected ')' got EOF");
            // return KelpVal::new(_KelpVal::Unknown, rdr.last_span());
            let (start, end) = if exprs.len() > 0 {
                (exprs[0].span().start, exprs[exprs.len() - 1].span().end)
            } else {
                (rdr.last_span().start, rdr.last_span().end)
            };
            return KelpVal::new(
                _KelpVal::Body(exprs),
                Span::new(start, end, rdr.last_span().content),
            );
        }
    }
}

fn read_trait(rdr: &mut Reader) -> KelpVal {
    let mut indices: Vec<KelpVal> = vec![];
    let mut end: usize;

    loop {
        if let Some(token) = rdr.peek() {
            let start = token.1.start;
            let content = token.1.content;

            end = token.1.end;

            match &token.0[..] {
                "}" => {
                    let _ = rdr.next();
                    return KelpVal::new(_KelpVal::Trait(indices), Span::new(start, end, content));
                }
                _ => return read_sym(rdr)
            }
        }
    }
}

fn read_enum(rdr: &mut Reader) -> KelpVal {
    let mut exprs: Vec<KelpVal> = vec![];
    let mut end: usize;
    loop {
        if let Some(token) = rdr.peek() {
            let start = token.1.start;
            let content = token.1.content;
            end = token.1.end;
            match &token.0[..] {
                "}" => {
                    let _ = rdr.next();
                    return KelpVal::new(_KelpVal::Enum(exprs), Span::new(start, end, content));
                }
                _ => {
                    exprs.push(read_expr(rdr));
                }
            }
        } else {
            let span = rdr.last_span();
            error!(span, "Expected '}}' got EOF");
            return KelpVal::new(_KelpVal::Unknown, rdr.last_span());
        }
    }
}

fn read_lambda(rdr: &mut Reader, arg: KelpVal) -> KelpVal {
    let token = match rdr.peek() {
        Some(token) => token,
        None => {
            let span = rdr.last_span();
            error!(span, "Expected a lambda, got EOF");
            return KelpVal::new(_KelpVal::Unknown, span);
        }
    };
    let start = token.1.start;
    let mut end = start;
    let content = token.1.clone().content;

    let (body, ret) = match &token.0[..] {
        "=>" => {
            let _ = rdr.next();

            let ret_val = read_form(rdr);
            let ret = match ret_val.inner() {
                _KelpVal::Sym { name: _, typ: _ } | _KelpVal::Lambda { arg: _, body: _, ret: _} | _KelpVal::Group(_) => ret_val.inner().clone(),
                _ => {
                    let span = ret_val.span();
                    error!(span, "{} cannot be used as lambda return type", ret_val);
                    _KelpVal::Unknown
                }
            };

            let body = read_form(rdr);

            (body, KelpVal::new(ret, ret_val.span()))
        }
        "(" => {
            let span = token.1;
            error!(span, "Expected '=>' found start of the body '('");
            (
                read_body(rdr),
                KelpVal::new(_KelpVal::Unknown, Span::new(start, end, content.clone())),
            )
        }
        ")" | "[" | "]" | "{" | "}" => {
            let span = token.1;
            error!(span, "Unexpected {}", &token.0);
            (
                KelpVal::new(_KelpVal::Unknown, span.clone()),
                KelpVal::new(_KelpVal::Unknown, span),
            )
        }
        t => {
            let span = token.1;
            error!(span, "Unexpected {}", t);
            (
                KelpVal::new(_KelpVal::Unknown, span.clone()),
                KelpVal::new(_KelpVal::Unknown, span),
            )
        } // _ => {
          //     let val = read_atom(rdr)?;
          //     (val.clone(), val)
          // }
    };

    end = body.span().end;

    KelpVal::new(
        _KelpVal::Lambda {
            arg: arg,
            body: body,
            ret: ret,
        },
        Span::new(start, end, content),
    )
}

// fn read_group_member(rdr: &Reader, tokens: Vec<String>) -> KelpRet {
//     lazy_static! {
//         static ref COLON_RE: Regex = Regex::new(r###"^([^:])+:$"###).unwrap();
//         static ref REMOVE_RE: Regex = Regex::new(r###"^[^:]+"###).unwrap();
//     }
//     if tokens.len() == 0 {
//         return error("Expected element got empty");
//     } else if COLON_RE.is_match(&tokens[0]) && tokens.len() > 1 {
//         let name = REMOVE_RE.find(&tokens[0]).unwrap().as_str();
//         let new_rdr = rdr.clone();
//     }
//     loop {}
// }

fn print_seq(seq: &Vec<KelpVal>) -> String {
    let mut res = String::new();
    for e in seq {
        res += &format!("{},", e)
    }
    res
}

fn read_group(rdr: &mut Reader) -> KelpVal {
    let mut seq: Vec<KelpVal> = vec![];

    loop {
        if let Some(token) = rdr.peek() {
            match &token.0[..] {
                "]" => {
                    let _ = rdr.next();
                    return group!(token.1, seq);
                }
                _ => seq.push(read_form(rdr)),
            };
        } else {
            error!("expected ']', got EOF {:?}", print_seq(&seq));
        }
    }
}

fn read_form(rdr: &mut Reader) -> KelpVal {
    let token = match rdr.peek() {
        Some(token) => token,
        None => {
            let span = rdr.last_span();

            return KelpVal::new(_KelpVal::Unknown, span);
        }
    };

    match &token.0[..] {
        "\n" => {
            let _ = rdr.next();
            match rdr.peek() {
                Some(_) => read_form(rdr),
                None => KelpVal::new(_KelpVal::Unknown, token.1),
            }
        }
        "[" => {
            let _ = rdr.next();
            let group = read_group(rdr);
            match &rdr.peek().expect("Expected continuation found EOF").0[..] {
                "=>" | "(" => read_lambda(rdr, group),
                _ => group,
            }
        }
        "(" => {
            let _ = rdr.next();
            read_body(rdr)
        }
        "&{" => {
            let _ = rdr.next();
            read_trait(rdr)
        }
        "{" => {
            let _ = rdr.next();
            read_enum(rdr)
        }
        ")" => {
            let span = token.1;
            error!(span, "Invalid ) when reading form");
            KelpVal::new(_KelpVal::Unknown, span)
        }
        _ => read_atom(rdr),
    }
}

fn read_expr_part(lhs: KelpVal, rdr: &mut Reader) -> KelpVal {
    //println!("read_expr_part read_form");
    let (op, rhs) = if let Some(_) = rdr.peek() {
        (read_form(rdr), read_form(rdr))
    } else {
        return lhs;
    };
    //println!("{:#?} {:#?} {:#?}", lhs, op, rhs);

    match rdr.peek() {
        Some(t) => {
            if is_operator(t.0.as_str()) {
                read_expr_part(
                    KelpVal::new(
                        _KelpVal::Expr {
                            lhs: lhs,
                            op: op,
                            rhs: rhs,
                        },
                        t.1,
                    ),
                    rdr,
                )
            } else if t.0.as_str() == "\n" {
                let _ = rdr.next();
                KelpVal::new(
                    _KelpVal::Expr {
                        lhs: lhs,
                        op: op,
                        rhs: rhs,
                    },
                    t.1,
                )
            } else {
                KelpVal::new(
                    _KelpVal::Expr {
                        lhs: lhs,
                        op: op,
                        rhs: rhs,
                    },
                    t.1,
                )
            }
        }
        None => KelpVal::new(
            _KelpVal::Expr {
                lhs: lhs.clone(),
                op: op,
                rhs: rhs.clone(),
            },
            Span::new(lhs.span().start, rhs.span().end, lhs.span().content),
        ),
    }
}

fn read_expr(rdr: &mut Reader) -> KelpVal {
    let lhs = read_form(rdr);

    match rdr.peek() {
        Some(operator_token) => {
            let span = operator_token.1;
            match &operator_token.0[..] {
                ")" => {
                    error!(
                        span,
                        "'(' cannot be used as an operator, it has different meaning"
                    );
                    KelpVal::new(_KelpVal::Unknown, span)
                }
                op_str => {
                    if is_operator(op_str) {
                        read_expr_part(lhs.clone(), rdr)
                    } else {
                        lhs
                    }
                }
            }
        }
        None => lhs,
    }
}

pub fn read_str(str: String) -> KelpVal {
    let tokens = tokenize(&str);
    if tokens.len() == 0 {
        error!("no input");
        return KelpVal::new(_KelpVal::Unknown, Span::new(0, 0, "".to_string()));
    }

    //tokens.push(((")".to_string()), tokens[tokens.len() - 1].1.clone()));
    let rdr = &mut Reader::new(tokens);

    read_body(rdr)
}

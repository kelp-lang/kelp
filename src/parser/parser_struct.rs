use crate::error::{Error, ErrorType};
use crate::parser;
use crate::parser::*;
use std::iter::Peekable;

fn is_number(c: char) -> bool {
    match c {
        '0'..='9' => true,
        _ => false,
    }
}

fn is_alpha_lower(c: char) -> bool {
    match c {
        'a'..='z' => true,
        _ => false,
    }
}

fn is_alpha_upper(c: char) -> bool {
    match c {
        'A'..='Z' => true,
        _ => false,
    }
}

fn is_alpha(c: char) -> bool {
    match c {
        x if is_alpha_lower(x) | is_alpha_upper(x) => true,
        _ => false,
    }
}

fn is_alpha_numeric(c: char) -> bool {
    match c {
        x if is_alpha(x) | is_number(x) => true,
        _ => false,
    }
}

struct Parser {
    ln: usize,
}

impl Parser {
    //---- LEXER ----//
    fn get_num<T: Iterator<Item = char>>(
        &self,
        iter: &mut Peekable<T>,
    ) -> Result<parser::Elem, Error> {
        let mut number_str = "".to_string();
        let mut is_float = false;
        while let Some(&n) = iter.peek() {
            match n {
                x if is_number(x) => number_str.push(iter.next().unwrap()),
                '.' => {
                    if is_float {
                        return Err(Error {
                            ln: self.ln,
                            err_type: ErrorType::ParsingError,
                            mess: "Wrong definition of float, with 2 dots!".to_string(),
                        });
                    } else {
                        is_float = true;
                        number_str.push(iter.next().unwrap());
                    }
                }
                _ => break,
            }
        }

        Ok(match is_float {
            true => parser::Elem::Float(number_str.parse().unwrap()),
            _ => parser::Elem::Int(number_str.parse().unwrap()),
        })
    }

    fn get_valid_name<T: Iterator<Item = char>>(&self, iter: &mut Peekable<T>) -> String {
        let mut string = "".to_string();
        while let Some(&n) = iter.peek() {
            match n {
                x if (is_alpha_numeric(x) || x == '_') => {
                    string.push(iter.next().unwrap());
                }
                _ => break,
            }
        }
        string
    }

    fn get_sym<T: Iterator<Item = char>>(
        &mut self,
        mut iter: &mut Peekable<T>,
    ) -> Result<parser::Sym, Error> {
        let mut sym: String = "".to_string();
        let mut typ: Option<Type> = None;
        while let Some(&n) = iter.peek() {
            match n {
                x if is_alpha(x) => {
                    sym += self.get_valid_name(&mut iter).as_str();
                }
                ':' => {
                    iter.next();
                    typ = Some(self.get_type(&mut iter)?);
                }
                _ => break,
            }
        }
        Ok(Sym { sym, typ })
    }

    fn get_type_group<T: Iterator<Item = char>>(
        &mut self,
        mut iter: &mut Peekable<T>,
    ) -> Result<Vec<parser::Type>, Error> {
        let mut type_group: Vec<parser::Type> = vec![];
        while let Some(&n) = iter.peek() {
            match n {
                x if is_alpha(x) => type_group.push(Type::Simp(self.get_valid_name(&mut iter))),
                ',' => {
                    iter.next();
                }
                ' ' => {
                    iter.next();
                }
                '\n' => {
                    self.ln += 1;
                    iter.next();
                }
                '[' => {
                    iter.next();
                    type_group.push(Type::Comp(self.get_type_group(&mut iter)?));
                }
                ']' => {
                    iter.next();
                    break;
                }
                _ => {
                    return Err(Error {
                        ln: self.ln,
                        err_type: ErrorType::ParsingError,
                        mess: "Invalid group symbol".to_string(),
                    })
                }
            }
        }
        Ok(type_group)
    }

    fn get_type<T: Iterator<Item = char>>(
        &mut self,
        mut iter: &mut Peekable<T>,
    ) -> Result<parser::Type, Error> {
        while let Some(&n) = iter.peek() {
            match n {
                x if is_alpha(x) => return Ok(Type::Simp(self.get_valid_name(&mut iter))),
                '[' => {
                    iter.next();
                    return {
                        let type_group = self.get_type_group(&mut iter)?;
                        let typ = match type_group.len() {
                            0 => Type::None,
                            1 => type_group.first().unwrap().clone(),
                            _ => Type::Comp(type_group),
                        };

                        if let Some(':') = iter.peek() {
                            iter.next();
                            let arg = Box::new(typ);
                            let ret = Box::new(self.get_type(&mut iter)?);

                            Ok(Type::Fun { arg, ret })
                        } else {
                            Ok(typ)
                        }
                    };
                }
                ' ' => {
                    iter.next();
                }
                _ => {
                    return Err(Error {
                        ln: self.ln,
                        err_type: ErrorType::ParsingError,
                        mess: "Invalid type symbol".to_string(),
                    })
                }
            }
        }
        Err(Error {
            ln: self.ln,
            err_type: ErrorType::UnreachableCode,
            mess: "This should've not happened in Parser::get_type()".to_string(),
        })
    }

    fn get_group<T: Iterator<Item = char>>(
        &mut self,
        mut iter: &mut Peekable<T>,
    ) -> Result<parser::Elem, Error> {
        let mut group: Vec<parser::Elem> = vec![];
        while let Some(&n) = iter.peek() {
            match n {
                x if is_alpha(x) => group.push(Elem::Sym(self.get_sym(&mut iter)?)),
                x if is_number(x) => group.push(self.get_num(&mut iter)?),
                ',' => {
                    iter.next();
                }
                ' ' => {
                    iter.next();
                }
                '\n' => {
                    self.ln += 1;
                    iter.next();
                }
                ']' => {
                    iter.next();
                    break;
                }
                _ => {
                    return Err(Error {
                        ln: self.ln,
                        err_type: ErrorType::ParsingError,
                        mess: "Invalid group symbol".to_string(),
                    })
                }
            }
        }
        Ok(parser::Elem::Group(group))
    }
    //---- PARSER ----//

    fn get_expr<T: Iterator<Item = char>>(
        &mut self,
        mut iter: &mut Peekable<T>,
    ) -> Result<Expr, Error> {
        return Err(Error {
            ln: self.ln,
            err_type: ErrorType::UnreachableCode,
            mess: "Parser::get_expr()".to_string(),
        });
    }

    fn get_fun_def<T: Iterator<Item = char>>(
        &mut self,
        mut iter: &mut Peekable<T>,
    ) -> Result<FunDef, Error> {
        return Err(Error {
            ln: self.ln,
            err_type: ErrorType::UnreachableCode,
            mess: "Parser::get_fun_def()".to_string(),
        });
    }

    fn get_fun_dec<T: Iterator<Item = char>>(
        &mut self,
        mut iter: &mut Peekable<T>,
    ) -> Result<FunDec, Error> {
        let name = self.get_sym(&mut iter)?;
        let fun_type: Type;
        let (arg_type, ret_type): (Box<Type>, Box<Type>);
        let mut defs: Vec<FunDef> = vec![];

        while let Some(&n) = iter.peek() {
            match n {
                '[' => {
                    fun_type = self.get_type(&mut iter)?;
                    if let Type::Fun { arg, ret } = fun_type {
                        arg_type = arg;
                        ret_type = ret;
                    } else {
                        return Err(Error {
                            ln: self.ln,
                            err_type: ErrorType::ParsingError,
                            mess: "Function must have function type".to_string(),
                        });
                    }
                }
                ' ' => {
                    iter.next();
                }
                _ => break,
            }
        }

        while let Some(&n) = iter.peek() {
            match n {
                ' ' => {
                    iter.next();
                }
                '|' => {
                    iter.next();
                    defs.push(self.get_fun_def(&mut iter)?);
                }
                '\n' => {
                    self.ln += 1;
                    iter.next();
                }
                _ => {
                    if defs.is_empty() {
                        return Err(Error {
                            ln: self.ln,
                            err_type: ErrorType::ParsingError,
                            mess: "Function declaration has no definition".to_string(),
                        });
                    }
                    break;
                }
            }
        }

        Ok(FunDec {
            name,
            arg: *arg_type,
            ret: *ret_type,
            defs,
        })
    }

    pub fn parse(&mut self, input: &String) -> Result<parser::Program, Error> {
        let mut it = input.chars().peekable();
        let mut program = parser::Program::default();

        while let Some(&c) = it.peek() {
            match c {
                '\n' => {
                    it.next();
                    self.ln += 1;
                }
                x if is_alpha(x) => {
                    if let Some(f) = self.get_fun_dec(&mut it)? {
                        program.funcs.push(f);
                    }
                }
                _ => {}
            }
        }

        Ok(program)
    }
}

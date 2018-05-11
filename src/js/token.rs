// MIT License
//
// Copyright (c) 2018 Guillaume Gomez
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

use std::fmt;
use std::iter::Peekable;
use std::str::CharIndices;

pub trait MyTryFrom<T>: Sized {
    type Error;
    fn try_from(value: T) -> Result<Self, Self::Error>;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ReservedChar {
    Comma,
    OpenParenthese,
    CloseParenthese,
    OpenCurlyBrace,
    CloseCurlyBrace,
    OpenBracket,
    CloseBracket,
    Colon,
    SemiColon,
    Dot,
    Quote,
    DoubleQuote,
    ExclamationMark,
    QuestionMark,
    Slash,
    Modulo,
    Star,
    Minus,
    Plus,
    EqualSign,
    Backslash,
    Space,
    Tab,
    Backline,
    LessThan,
    SuperiorThan,
    Pipe,
    Ampersand,
}

impl ReservedChar {
    fn is_useless(&self) -> bool {
        *self == ReservedChar::Space ||
        *self == ReservedChar::Tab ||
        *self == ReservedChar::Backline
    }
}

impl fmt::Display for ReservedChar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
               match *self {
                   ReservedChar::Comma           => ',',
                   ReservedChar::OpenParenthese  => '(',
                   ReservedChar::CloseParenthese => ')',
                   ReservedChar::OpenCurlyBrace  => '{',
                   ReservedChar::CloseCurlyBrace => '}',
                   ReservedChar::OpenBracket     => '[',
                   ReservedChar::CloseBracket    => ']',
                   ReservedChar::Colon           => ':',
                   ReservedChar::SemiColon       => ';',
                   ReservedChar::Dot             => '.',
                   ReservedChar::Quote           => '\'',
                   ReservedChar::DoubleQuote     => '"',
                   ReservedChar::ExclamationMark => '!',
                   ReservedChar::QuestionMark    => '?',
                   ReservedChar::Slash           => '/',
                   ReservedChar::Modulo          => '%',
                   ReservedChar::Star            => '*',
                   ReservedChar::Minus           => '-',
                   ReservedChar::Plus            => '+',
                   ReservedChar::EqualSign       => '=',
                   ReservedChar::Backslash       => '\\',
                   ReservedChar::Space           => ' ',
                   ReservedChar::Tab             => '\t',
                   ReservedChar::Backline        => '\n',
                   ReservedChar::LessThan        => '<',
                   ReservedChar::SuperiorThan    => '>',
                   ReservedChar::Pipe            => '|',
                   ReservedChar::Ampersand       => '&',
               })
    }
}

impl MyTryFrom<char> for ReservedChar {
    type Error = &'static str;

    fn try_from(value: char) -> Result<ReservedChar, Self::Error> {
        match value {
            ','  => Ok(ReservedChar::Comma),
            '('  => Ok(ReservedChar::OpenParenthese),
            ')'  => Ok(ReservedChar::CloseParenthese),
            '{'  => Ok(ReservedChar::OpenCurlyBrace),
            '}'  => Ok(ReservedChar::CloseCurlyBrace),
            '['  => Ok(ReservedChar::OpenBracket),
            ']'  => Ok(ReservedChar::CloseBracket),
            ':'  => Ok(ReservedChar::Colon),
            ';'  => Ok(ReservedChar::SemiColon),
            '.'  => Ok(ReservedChar::Dot),
            '\'' => Ok(ReservedChar::Quote),
            '"'  => Ok(ReservedChar::DoubleQuote),
            '!'  => Ok(ReservedChar::ExclamationMark),
            '?'  => Ok(ReservedChar::QuestionMark),
            '/'  => Ok(ReservedChar::Slash),
            '%'  => Ok(ReservedChar::Modulo),
            '*'  => Ok(ReservedChar::Star),
            '-'  => Ok(ReservedChar::Minus),
            '+'  => Ok(ReservedChar::Plus),
            '='  => Ok(ReservedChar::EqualSign),
            '\\' => Ok(ReservedChar::Backslash),
            ' '  => Ok(ReservedChar::Space),
            '\t' => Ok(ReservedChar::Tab),
            '\n' |
            '\r' => Ok(ReservedChar::Backline),
            '<'  => Ok(ReservedChar::LessThan),
            '>'  => Ok(ReservedChar::SuperiorThan),
            '|'  => Ok(ReservedChar::Pipe),
            '&'  => Ok(ReservedChar::Ampersand),
            _    => Err("Unknown reserved char"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Keyword {
    Break,
    Case,
    Catch,
    Const,
    Continue,
    Default,
    Do,
    Else,
    False,
    Finally,
    Function,
    For,
    If,
    In,
    InstanceOf,
    New,
    Null,
    Private,
    Protected,
    Public,
    Return,
    Switch,
    This,
    Throw,
    True,
    Try,
    Typeof,
    Static,
    Var,
    While,
}

fn get_required<'a>(next: &Token<'a>) -> Option<char> {
    match *next {
        Token::Keyword(_) | Token::Other(_) => Some(' '),
        _ => None,
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
               match *self {
                   Keyword::Break => "break",
                   Keyword::Case => "case",
                   Keyword::Catch => "catch",
                   Keyword::Const => "const",
                   Keyword::Continue => "continue",
                   Keyword::Default => "default",
                   Keyword::Do => "do",
                   Keyword::Else => "else",
                   Keyword::False => "false",
                   Keyword::Finally => "finally",
                   Keyword::Function => "function",
                   Keyword::For => "for",
                   Keyword::If => "if",
                   Keyword::In => "in",
                   Keyword::InstanceOf => "instanceof",
                   Keyword::New => "new",
                   Keyword::Null => "null",
                   Keyword::Private => "private",
                   Keyword::Protected => "protected",
                   Keyword::Public => "public",
                   Keyword::Return => "return",
                   Keyword::Switch => "switch",
                   Keyword::This => "this",
                   Keyword::Throw => "throw",
                   Keyword::True => "true",
                   Keyword::Try => "try",
                   Keyword::Typeof => "typeof",
                   Keyword::Static => "static",
                   Keyword::Var => "var",
                   Keyword::While => "while",
               })
    }
}

impl<'a> MyTryFrom<&'a str> for Keyword {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Keyword, Self::Error> {
        match value {
            "break" => Ok(Keyword::Break),
            "case" => Ok(Keyword::Case),
            "catch" => Ok(Keyword::Catch),
            "const" => Ok(Keyword::Const),
            "continue" => Ok(Keyword::Continue),
            "default" => Ok(Keyword::Default),
            "do" => Ok(Keyword::Do),
            "else" => Ok(Keyword::Else),
            "false" => Ok(Keyword::False),
            "finally" => Ok(Keyword::Finally),
            "function" => Ok(Keyword::Function),
            "for" => Ok(Keyword::For),
            "if" => Ok(Keyword::If),
            "in" => Ok(Keyword::In),
            "instanceof" => Ok(Keyword::InstanceOf),
            "new" => Ok(Keyword::New),
            "null" => Ok(Keyword::Null),
            "private" => Ok(Keyword::Private),
            "protected" => Ok(Keyword::Protected),
            "public" => Ok(Keyword::Public),
            "return" => Ok(Keyword::Return),
            "switch" => Ok(Keyword::Switch),
            "this" => Ok(Keyword::This),
            "throw" => Ok(Keyword::Throw),
            "true" => Ok(Keyword::True),
            "try" => Ok(Keyword::Try),
            "typeof" => Ok(Keyword::Typeof),
            "static" => Ok(Keyword::Static),
            "var" => Ok(Keyword::Var),
            "while" => Ok(Keyword::While),
            _ => Err("Unkown keyword"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Condition {
    And,
    Or,
    DifferentThan,
    SuperDifferentThan,
    EqualTo,
    SuperEqualTo,
    SuperiorThan,
    SuperiorOrEqualTo,
    InferiorThan,
    InferiorOrEqualTo,
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
               match *self {
                   Condition::And => "&&",
                   Condition::Or => "||",
                   Condition::DifferentThan => "!=",
                   Condition::SuperDifferentThan => "!==",
                   Condition::EqualTo => "==",
                   Condition::SuperEqualTo => "===",
                   Condition::SuperiorThan => ">",
                   Condition::SuperiorOrEqualTo => ">=",
                   Condition::InferiorThan => "<",
                   Condition::InferiorOrEqualTo => "<=",
               })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Operation {
    Addition,
    AdditionEqual,
    Subtract,
    SubtractEqual,
    Multiply,
    MultiplyEqual,
    Divide,
    DivideEqual,
    Modulo,
    ModuloEqual,
    Equal,
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}",
               match *self {
                   Operation::Addition => "+",
                   Operation::AdditionEqual => "+=",
                   Operation::Subtract => "-",
                   Operation::SubtractEqual => "-=",
                   Operation::Multiply => "*",
                   Operation::MultiplyEqual => "*=",
                   Operation::Divide => "/",
                   Operation::DivideEqual => "/=",
                   Operation::Modulo => "%",
                   Operation::ModuloEqual => "%=",
                   Operation::Equal => "=",
               })
    }
}

impl MyTryFrom<ReservedChar> for Operation {
    type Error = &'static str;

    fn try_from(value: ReservedChar) -> Result<Operation, Self::Error> {
        Ok(match value {
            ReservedChar::Plus => Operation::Addition,
            ReservedChar::Minus => Operation::Subtract,
            ReservedChar::Slash => Operation::Divide,
            ReservedChar::Star => Operation::Multiply,
            ReservedChar::Modulo => Operation::Modulo,
            _ => return Err("Unkown operation"),
        })
    }
}

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Keyword(Keyword),
    Char(ReservedChar),
    String(&'a str),
    Comment(&'a str),
    License(&'a str),
    Other(&'a str),
    Regex {
        regex: &'a str,
        is_global: bool,
        is_interactive: bool,
    },
    Condition(Condition),
    Operation(Operation),
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Keyword(x) => write!(f, "{}", x),
            Token::Char(x) => write!(f, "{}", x),
            Token::String(x) |
            Token::Comment(x) |
            Token::Other(x) => write!(f, "{}", x),
            Token::License(x) => write!(f, "/*!{}*/", x),
            Token::Regex { regex, is_global, is_interactive } => {
                let x = write!(f, "/{}/", regex);
                if is_global {
                    write!(f, "g")?;
                }
                if is_interactive {
                    write!(f, "i")?;
                }
                x
            }
            Token::Condition(x) => write!(f, "{}", x),
            Token::Operation(x) => write!(f, "{}", x),
        }
    }
}

impl<'a> Token<'a> {
    fn is_comment(&self) -> bool {
        match *self {
            Token::Comment(_) => true,
            _ => false,
        }
    }

    fn get_char(&self) -> Option<ReservedChar> {
        match *self {
            Token::Char(c) => Some(c),
            _ => None,
        }
    }

    #[allow(dead_code)]
    fn is_condition(&self) -> bool {
        match *self {
            Token::Condition(_) => true,
            _ => false,
        }
    }

    fn is_other(&self) -> bool {
        match *self {
            Token::Other(_) => true,
            _ => false,
        }
    }

    fn is_white_character(&self) -> bool {
        match *self {
            Token::Char(c) => c.is_useless(),
            _ => false,
        }
    }
}

fn get_line_comment<'a>(source: &'a str, iterator: &mut Peekable<CharIndices>,
                        start_pos: &mut usize) -> Option<Token<'a>> {
    *start_pos += 1;
    while let Some((pos, c)) = iterator.next() {
        if let Ok(c) = ReservedChar::try_from(c) {
            if c == ReservedChar::Backline {
                let ret = Some(Token::Comment(&source[*start_pos..pos]));
                *start_pos = pos;
                return ret;
            }
        }
    }
    None
}

fn get_regex<'a>(source: &'a str, iterator: &mut Peekable<CharIndices>,
                 start_pos: &mut usize) -> Option<Token<'a>> {
    *start_pos += 1;
    while let Some((pos, c)) = iterator.next() {
        if c == '\\' {
            // we skip next character
            iterator.next();
            continue
        }
        if let Ok(c) = ReservedChar::try_from(c) {
            if c == ReservedChar::Slash {
                let mut is_global = false;
                let mut is_interactive = false;
                let mut add = 0;
                loop {
                    match iterator.peek() {
                        Some((_, 'i')) => is_interactive = true,
                        Some((_, 'g')) => is_global = true,
                        _ => break,
                    };
                    iterator.next();
                    add += 1;
                }
                let ret = Some(Token::Regex {
                                   regex: &source[*start_pos..pos],
                                   is_interactive,
                                   is_global
                               });
                *start_pos = pos + add;
                return ret;
            }
        }
    }
    None
}

fn get_comment<'a>(source: &'a str, iterator: &mut Peekable<CharIndices>,
                   start_pos: &mut usize) -> Option<Token<'a>> {
    let mut prev = ReservedChar::Quote;
    *start_pos += 1;
    let builder = if let Some((_, c)) = iterator.next() {
        if c == '!' {
            *start_pos += 1;
            Token::License
        } else {
            if let Ok(c) = ReservedChar::try_from(c) {
                prev = c;
            }
            Token::Comment
        }
    } else {
        Token::Comment
    };

    while let Some((pos, c)) = iterator.next() {
        if let Ok(c) = ReservedChar::try_from(c) {
            if c == ReservedChar::Slash && prev == ReservedChar::Star {
                let ret = Some(builder(&source[*start_pos..pos - 1]));
                *start_pos = pos;
                return ret;
            }
            prev = c;
        } else {
            prev = ReservedChar::Space;
        }
    }
    None
}

fn get_string<'a>(source: &'a str, iterator: &mut Peekable<CharIndices>, start_pos: &mut usize,
                  start: ReservedChar) -> Option<Token<'a>> {
    while let Some((pos, c)) = iterator.next() {
        if c == '\\' {
            // we skip next character
            iterator.next();
            continue
        }
        if let Ok(c) = ReservedChar::try_from(c) {
            if c == start {
                let ret = Some(Token::String(&source[*start_pos..pos + 1]));
                *start_pos = pos;
                return ret;
            }
        }
    }
    None
}

fn first_useful<'a>(v: &'a [Token<'a>]) -> Option<&'a Token<'a>> {
    for x in v.iter().rev() {
        if x.is_white_character() {
            continue
        }
        return Some(x);
    }
    None
}

pub fn tokenize<'a>(source: &'a str) -> Tokens<'a> {
    let mut v = Vec::with_capacity(1000);
    let mut start = 0;
    let mut iterator = source.char_indices().peekable();

    loop {
        let (mut pos, c) = match iterator.next() {
            Some(x) => x,
            None => break,
        };
        if let Ok(c) = ReservedChar::try_from(c) {
            if pos > start {
                if let Ok(w) = Keyword::try_from(&source[start..pos]) {
                    v.push(Token::Keyword(w));
                } else {
                    v.push(Token::Other(&source[start..pos]));
                }
            }
            if c == ReservedChar::Quote || c == ReservedChar::DoubleQuote {
                if let Some(s) = get_string(source, &mut iterator, &mut pos, c) {
                    v.push(s);
                }
            } else if c == ReservedChar::Slash &&
                      *v.last().unwrap_or(&Token::Other("")) == Token::Operation(Operation::Divide) {
                v.pop();
                if let Some(s) = get_line_comment(source, &mut iterator, &mut pos) {
                    v.push(s);
                }
            } else if c == ReservedChar::Slash &&
                      iterator.peek().is_some() &&
                      iterator.peek().unwrap().1 != '/' &&
                      iterator.peek().unwrap().1 != '*' &&
                      !first_useful(&v).unwrap_or(&Token::String("")).is_other() {
                if let Some(r) = get_regex(source, &mut iterator, &mut pos) {
                    v.push(r);
                }
            } else if c == ReservedChar::Star &&
                      *v.last().unwrap_or(&Token::Other("")) == Token::Operation(Operation::Divide) {
                v.pop();
                if let Some(s) = get_comment(source, &mut iterator, &mut pos) {
                    v.push(s);
                }
            } else if c == ReservedChar::Pipe &&
                      *v.last().unwrap_or(&Token::Other("")) == Token::Char(ReservedChar::Pipe) {
                v.pop();
                v.push(Token::Condition(Condition::Or));
            } else if c == ReservedChar::Ampersand &&
                      *v.last().unwrap_or(&Token::Other("")) == Token::Char(ReservedChar::Ampersand) {
                v.pop();
                v.push(Token::Condition(Condition::And));
            } else if c == ReservedChar::EqualSign &&
                      *v.last().unwrap_or(&Token::Other("")) == Token::Char(ReservedChar::EqualSign) {
                v.pop();
                v.push(Token::Condition(Condition::EqualTo));
            } else if c == ReservedChar::EqualSign &&
                      *v.last().unwrap_or(&Token::Other("")) == Token::Condition(Condition::EqualTo) {
                v.pop();
                v.push(Token::Condition(Condition::SuperEqualTo));
            } else if c == ReservedChar::EqualSign &&
                      *v.last().unwrap_or(&Token::Other("")) == Token::Char(ReservedChar::ExclamationMark) {
                v.pop();
                v.push(Token::Condition(Condition::DifferentThan));
            } else if c == ReservedChar::EqualSign &&
                      *v.last().unwrap_or(&Token::Other("")) == Token::Condition(Condition::DifferentThan) {
                v.pop();
                v.push(Token::Condition(Condition::SuperDifferentThan));
            } else if c == ReservedChar::EqualSign &&
                      *v.last().unwrap_or(&Token::Other("")) == Token::Operation(Operation::Divide) {
                v.pop();
                v.push(Token::Operation(Operation::DivideEqual));
            } else if c == ReservedChar::EqualSign &&
                      *v.last().unwrap_or(&Token::Other("")) == Token::Operation(Operation::Multiply) {
                v.pop();
                v.push(Token::Operation(Operation::MultiplyEqual));
            } else if c == ReservedChar::EqualSign &&
                      *v.last().unwrap_or(&Token::Other("")) == Token::Operation(Operation::Addition) {
                v.pop();
                v.push(Token::Operation(Operation::AdditionEqual));
            } else if c == ReservedChar::EqualSign &&
                      *v.last().unwrap_or(&Token::Other("")) == Token::Operation(Operation::Subtract) {
                v.pop();
                v.push(Token::Operation(Operation::SubtractEqual));
            } else if c == ReservedChar::EqualSign &&
                      *v.last().unwrap_or(&Token::Other("")) == Token::Operation(Operation::Modulo) {
                v.pop();
                v.push(Token::Operation(Operation::ModuloEqual));
            } else if let Ok(o) = Operation::try_from(c) {
                v.push(Token::Operation(o));
            } else {
                v.push(Token::Char(c));
            }
            start = pos + 1;
        }
    }
    Tokens(v)
}

pub struct Tokens<'a>(pub Vec<Token<'a>>);

impl<'a> fmt::Display for Tokens<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let tokens = &self.0;
        for i in 0..tokens.len() {
            write!(f, "{}", tokens[i])?;
            if let Some(c) = match tokens[i] {
                Token::Keyword(_) |
                Token::Other(_) if i + 1 < tokens.len() => get_required(&tokens[i + 1]),
                _ => None,
            } {
                write!(f, "{}", c)?;
            }
        }
        Ok(())
    }
}

pub fn clean_tokens<'a>(tokens: &mut Tokens<'a>) {
    tokens.0.retain(|c| {
        !c.is_comment() && {
            if let Some(x) = c.get_char() {
                !x.is_useless()
            } else {
                true
            }
        }
    });
}

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

use std::convert::TryFrom;

enum Elem<'a> {
    Function(Function<'a>),
    Block(Block<'a>),
    Variable(Variable<'a>),
    //Condition(Condition<'a>),
    Loop(Loop<'a>),
    Operation(Operation<'a>),
    Comment(Comment<'a>),
}

enum ConditionType {
    If,
    ElseIf,
    Else,
    Ternary,
}

enum LoopType {
    For,
    While,
}

struct Block<'a> {
    elems: Vec<Elem<'a>>,
}

struct Argument<'a> {
    name: &'a str,
}

struct Function<'a> {
    name: Option<&'a str>,
    args: Vec<Argument<'a>>,
    block: Block<'a>,
}

struct Variable<'a> {
    name: &'a str,
    value: Option<&'a str>,
}

/*struct Condition<'a> {
    ty_: ConditionType,
    condition: &'a str,
    block: Block<'a>,
}*/

struct Loop<'a> {
    ty_: LoopType,
    condition: &'a str,
    block: Block<'a>,
}

struct Operation<'a> {
    content: &'a str,
}

struct Comment<'a> {
    content: &'a str,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ReservedChar {
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

impl TryFrom<char> for ReservedChar {
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
            _ => Err("Unknown reserved char"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Keyword {
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

impl<'a> TryFrom<&'a str> for Keyword {
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
enum Condition {
    And,
    Or,
    DifferentThan,
    SuperDifferentThan,
    EqualTo,
    SuperEqualTo,
}

#[derive(Debug, PartialEq)]
enum Token<'a> {
    Keyword(Keyword),
    Char(ReservedChar),
    String(&'a str),
    Comment(&'a str),
    Other(&'a str),
    Condition(Condition),
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
}

use std::str::Chars;
use std::iter::Enumerate;

fn get_line_comment<'a>(source: &'a str, iterator: &mut Enumerate<Chars>,
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

fn get_comment<'a>(source: &'a str, iterator: &mut Enumerate<Chars>,
                   start_pos: &mut usize) -> Option<Token<'a>> {
    let mut prev = ReservedChar::Quote;

    *start_pos += 1;
    while let Some((pos, c)) = iterator.next() {
        if let Ok(c) = ReservedChar::try_from(c) {
            if c == ReservedChar::Slash && prev == ReservedChar::Star {
                let ret = Some(Token::Comment(&source[*start_pos..pos - 1]));
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

fn get_string<'a>(source: &'a str, iterator: &mut Enumerate<Chars>, start_pos: &mut usize,
                  start: ReservedChar) -> Option<Token<'a>> {
    let mut prev = ReservedChar::Quote;

    *start_pos += 1;
    while let Some((pos, c)) = iterator.next() {
        if let Ok(c) = ReservedChar::try_from(c) {
            if c == start && prev != ReservedChar::Backslash {
                let ret = Some(Token::String(&source[*start_pos..pos]));
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

fn tokenize<'a>(source: &'a str) -> Vec<Token<'a>> {
    let mut v = Vec::with_capacity(1000);
    let mut start = 0;
    let mut iterator = source.chars().enumerate();

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
                      *v.last().unwrap_or(&Token::Other("")) == Token::Char(ReservedChar::Slash) {
                v.pop();
                if let Some(s) = get_line_comment(source, &mut iterator, &mut pos) {
                    v.push(s);
                }
            } else if c == ReservedChar::Star &&
                      *v.last().unwrap_or(&Token::Other("")) == Token::Char(ReservedChar::Slash) {
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
            } else {
                v.push(Token::Char(c));
            }
            start = pos + 1;
        }
    }
    v
}

fn clean_tokens<'a>(tokens: &mut Vec<Token<'a>>) {
    tokens.retain(|c| {
        !c.is_comment() && {
            if let Some(x) = c.get_char() {
                !x.is_useless()
            } else {
                true
            }
        }
    })
}

pub fn minify(source: &str) -> String {
    let mut v = tokenize(source);
    clean_tokens(&mut v);
    println!("{:?}", v);
    String::new()
}

#[test]
fn js_minify_test() {
    let source = r##"
var foo = "something";

var another_var = 2348323;

// who doesn't like comments?
/* and even longer comments?

like
on
a
lot
of
lines!

Fun!
*/
function far_away(x, y) {
    var x2 = x + 4;
    return x * x2 + y;
}

// this call is useless
far_away(another_var, 12);
// this call is useless too
far_away(another_var, 12);
"##;

    let expected_result = "var foo=\"something\";var another_var=2348323;function far_away(x,y){\
                           var x2=x+4;return x*x2+y;}far_away(another_var,12);far_away(another_var,\
                           12);";
    assert_eq!(minify(source), expected_result);
}

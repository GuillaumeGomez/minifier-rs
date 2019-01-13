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

use js::token::{Keyword, Operation, ReservedChar, Token, Tokens};

pub(crate) struct VariableNameGenerator<'a> {
    letter: char,
    lower: Option<Box<VariableNameGenerator<'a>>>,
    prepend: Option<&'a str>,
}

impl<'a> VariableNameGenerator<'a> {
    pub(crate) fn new(prepend: Option<&'a str>, nb_letter: usize) -> VariableNameGenerator<'a> {
        if nb_letter > 1 {
            VariableNameGenerator {
                letter: 'a',
                lower: Some(Box::new(VariableNameGenerator::new(None, nb_letter - 1))),
                prepend,
            }
        } else {
            VariableNameGenerator {
                letter: 'a',
                lower: None,
                prepend,
            }
        }
    }

    pub(crate) fn next(&mut self) {
        self.incr_letters();
    }

    pub(crate) fn to_string(&self) -> String {
        if let Some(ref lower) = self.lower {
            format!("{}{}{}",
                    match self.prepend {
                        Some(ref p) => p,
                        None => "",
                    },
                    self.letter,
                    lower.to_string())
        } else {
            format!("{}{}",
                    match self.prepend {
                        Some(ref p) => p,
                        None => "",
                    },
                    self.letter)
        }
    }

    #[allow(dead_code)]
    pub(crate) fn len(&self) -> usize {
        let first = match self.prepend {
            Some(ref s) => s.len(),
            None => 0,
        } + 1;
        first + match self.lower {
            Some(ref s) => s.len(),
            None => 0,
        }
    }

    pub(crate) fn incr_letters(&mut self) {
        let max = [('z', 'A'), ('Z', '0'), ('9', 'a')];

        for (m, next) in &max {
            if self.letter == *m {
                self.letter = *next;
                if self.letter == 'a' {
                    if let Some(ref mut lower) = self.lower {
                        lower.incr_letters();
                    } else {
                        self.lower = Some(Box::new(VariableNameGenerator::new(None, 1)));
                    }
                }
                return;
            }
        }
        self.letter = ((self.letter as u8) + 1) as char;
    }
}

#[inline]
pub fn get_variable_name_and_value_positions<'a>(
    tokens: &'a Tokens<'a>,
    pos: usize,
) -> Option<(usize, usize)> {
    if pos >= tokens.len() {
        return None;
    }
    match tokens[pos].get_keyword() {
        Some(Keyword::Let) | Some(Keyword::Var) => {}
        _ => return None,
    }
    let mut tmp = pos + 1;
    while tmp < tokens.len() {
        if tokens[tmp].is_other() {
            let mut tmp2 = tmp + 1;
            'big: while tmp2 < tokens.len() {
                if tokens[tmp2] == Token::Operation(Operation::Equal) {
                    tmp2 += 1;
                    while tmp2 < tokens.len() {
                        let token = &tokens[tmp2];
                        if token.is_string() || token.is_other() || token.is_regex() {
                            return Some((tmp, tmp2));
                        } else if !tokens[tmp2].is_comment() &&
                                  !tokens[tmp2].is_white_character() {
                            break;
                        }
                        tmp2 += 1;
                    }
                    break;
                } else if !tokens[tmp2].is_comment() &&
                          !(tokens[tmp2].is_white_character() &&
                            tokens[tmp2].get_char() != Some(ReservedChar::Backline)) {
                    break;
                }
                tmp2 += 1;
            }
        } else {
            // We don't care about syntax errors.
        }
        tmp += 1;
    }
    None
}

#[test]
fn check_get_variable_name_and_value_positions() {
    let source = r#"var x = 1;var y   =   "2";"#;
    let mut result = Vec::new();

    let tokens = ::js::token::tokenize(source);

    for pos in 0..tokens.len() {
        match tokens[pos].get_keyword() {
            Some(k) if k == Keyword::Let || k == Keyword::Var => {
                if let Some(x) = get_variable_name_and_value_positions(&tokens, pos) {
                    result.push(x);
                }
            }
            _ => {}
        }
    }
    assert_eq!(result, vec![(2, 6), (10, 18)]);

    let mut result = Vec::new();
    let tokens = ::js::clean_tokens(tokens);

    for pos in 0..tokens.len() {
        match tokens[pos].get_keyword() {
            Some(k) if k == Keyword::Let || k == Keyword::Var => {
                if let Some(x) = get_variable_name_and_value_positions(&tokens, pos) {
                    result.push(x);
                }
            }
            _ => {}
        }
    }
    assert_eq!(result, vec![(1, 3), (6, 8)]);
}

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

use js::token;

/*#[derive(Debug, Clone, PartialEq, Eq)]
enum Elem<'a> {
    Function(Function<'a>),
    Block(Block<'a>),
    Variable(Variable<'a>),
    Condition(token::Condition),
    Loop(Loop<'a>),
    Operation(Operation<'a>),
}

impl<'a> Elem<'a> {
    fn is_condition(&self) -> bool {
        match *self {
            Elem::Condition(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum ConditionType {
    If,
    ElseIf,
    Else,
    Ternary,
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct Block<'a> {
    elems: Vec<Elem<'a>>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct Argument<'a> {
    name: &'a str,
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct Function<'a> {
    name: Option<&'a str>,
    args: Vec<Argument<'a>>,
    block: Block<'a>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct Variable<'a> {
    name: &'a str,
    value: Option<&'a str>,
}

/*struct Condition<'a> {
    ty_: ConditionType,
    condition: &'a str,
    block: Block<'a>,
}*/

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum LoopType {
    Do,
    For,
    While,
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct Loop<'a> {
    ty_: LoopType,
    condition: Vec<Elem<'a>>,
    block: Block<'a>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct Operation<'a> {
    content: &'a str,
}

fn get_while_condition<'a>(tokens: &[token::Token<'a>], pos: &mut usize) -> Result<Vec<Elem<'a>>, String> {
    let tmp = *pos;
    *pos += 1;
    if let Err(e) = match tokens.get(tmp) {
        Some(token::Token::Char(token::ReservedChar::OpenParenthese)) => Ok(()),
        Some(e) => Err(format!("Expected \"(\", found \"{:?}\"", e)),
        None => Err("Expected \"(\", found nothing...".to_owned()),
    } {
        return Err(e);
    }
    let mut elems: Vec<Elem<'a>> = Vec::with_capacity(1);

    while let Some(e) = tokens.get(*pos) {
        *pos += 1;
        match e {
            token::Token::Char(token::ReservedChar::CloseParenthese) => return Ok(elems),
            token::Token::Condition(e) => {
                if let Some(cond) = elems.last() {
                    if cond.is_condition() {
                        return Err(format!("\"{:?}\" cannot follow \"{:?}\"", e, cond));
                    }
                }
            }
            _ => {}
        }
    }
    Err("Expected \")\", found nothing...".to_owned())
}

fn get_do<'a>(tokens: &[token::Token<'a>], pos: &mut usize) -> Result<Elem<'a>, String> {
    let tmp = *pos;
    *pos += 1;
    let block = match tokens.get(tmp) {
        Some(token::Token::Char(token::ReservedChar::OpenCurlyBrace)) => get_block(tokens, pos, true),
        Some(e) => Err(format!("Expected \"{{\", found \"{:?}\"", e)),
        None => Err("Expected \"{\", found nothing...".to_owned()),
    }?;
    let tmp = *pos;
    *pos += 1;
    let condition = match tokens.get(tmp) {
        Some(token::Token::Keyword(token::Keyword::While)) => get_while_condition(tokens, pos),
        Some(e) => Err(format!("Expected \"while\", found \"{:?}\"", e)),
        None => Err("Expected \"while\", found nothing...".to_owned()),
    }?;
    let mut loop_ = Loop {
        ty_: LoopType::Do,
        condition: condition,
        block,
    };
    Ok(Elem::Loop(loop_))
}

fn get_block<'a>(tokens: &[token::Token<'a>], pos: &mut usize,
                 start_with_paren: bool) -> Result<Block<'a>, String> {
    let mut block = Block { elems: Vec::with_capacity(2) };
    while let Some(e) = tokens.get(*pos) {
        *pos += 1;
        block.elems.push(match e {
            token::Token::Keyword(token::Keyword::Do) => get_do(tokens, pos),
            token::Token::Char(token::ReservedChar::CloseCurlyBrace) => {
                if start_with_paren {
                    return Ok(block);
                }
                return Err("Unexpected \"}\"".to_owned());
            }
        }?);
    }
    if !start_with_paren {
        Ok(block)
    } else {
        Err("Expected \"}\" at the end of the block but didn't find one...".to_owned())
    }
}

fn build_ast<'a>(v: &[token::Token<'a>]) -> Result<Elem<'a>, String> {
    let mut pos = 0;

    match get_block(v, &mut pos, false) {
        Ok(ast) => Ok(Elem::Block(ast)),
        Err(e) => Err(e),
    }
}*/

pub fn minify(source: &str) -> String {
    let mut v = token::tokenize(source);
    token::clean_tokens(&mut v);
    v.to_string()
    /*match build_ast(&v) {
        Ok(x) => {}
        Err(e) => eprintln!("Failure: {}", e),
    }*/
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

#[test]
fn another_js_test() {
    let source = r#"
/*! let's keep this license
 *
 * because everyone likes licenses!
 *
 * right?
 */

function forEach(data, func) {
    for (var i = 0; i < data.length; ++i) {
        func(data[i]);
    }
}

forEach([0, 1, 2, 3, 4,
         5, 6, 7, 8, 9], function (x) {
            console.log(x);
         });
// I think we're done?
console.log('done!');
"#;

    let expected_result = r#"/*! let's keep this license
 *
 * because everyone likes licenses!
 *
 * right?
 */function forEach(data,func){for(var i=0;i<data.length;++i){func(data[i]);}}forEach([0,1,2,3,4,5,6,7,8,9],function(x){console.log(x);});console.log('done!');"#;
    assert_eq!(minify(source), expected_result);
}

#[test]
fn comment_issue() {
    let source = r#"
search_input.onchange = function(e) {
    // Do NOT e.preventDefault() here. It will prevent pasting.
    clearTimeout(searchTimeout);
    // zero-timeout necessary here because at the time of event handler execution the
    // pasted content is not in the input field yet. Shouldn’t make any difference for
    // change, though.
    setTimeout(search, 0);
};
"#;
    let expected_result = "search_input.onchange=function(e){clearTimeout(searchTimeout);\
                           setTimeout(search,0);};";
    assert_eq!(minify(source), expected_result);
}

#[test]
fn check_regex() {
    let source = r#"var x = /"\.x/g;"#;
    let expected_result = r#"var x=/"\.x/g;"#;
    assert_eq!(minify(source), expected_result);

    let mut v = ::js::token::tokenize(source);
    ::js::token::clean_tokens(&mut v);
    assert_eq!(v.0[3],
               ::js::token::Token::Regex {
                   regex: "\"\\.x",
                   is_global: true,
                   is_interactive: false,
               });

    let source = r#"var x = /"\.x/gigigigig;var x = "hello";"#;
    let expected_result = r#"var x=/"\.x/gi;var x="hello";"#;
    assert_eq!(minify(source), expected_result);

    let mut v = ::js::token::tokenize(source);
    ::js::token::clean_tokens(&mut v);
    assert_eq!(v.0[3],
               ::js::token::Token::Regex {
                   regex: "\"\\.x",
                   is_global: true,
                   is_interactive: true,
               });
}

#[test]
fn more_regex() {
    let source = r#"var x = /"\.x\/a/i;"#;
    let expected_result = r#"var x=/"\.x\/a/i;"#;
    assert_eq!(minify(source), expected_result);

    let mut v = ::js::token::tokenize(source);
    ::js::token::clean_tokens(&mut v);
    assert_eq!(v.0[3],
               ::js::token::Token::Regex {
                   regex: "\"\\.x\\/a",
                   is_global: false,
                   is_interactive: true,
               });
}

#[test]
fn missing_whitespace() {
    let source = r#"
for (var entry in results) {
    if (results.hasOwnProperty(entry)) {
        ar.push(results[entry]);
    }
}"#;
    let expected_result = "for(var entry in results){if(results.hasOwnProperty(entry)){\
                           ar.push(results[entry]);}}";
    assert_eq!(minify(source), expected_result);
}

#[test]
fn weird_regex_issue() {
    let source = r#"
val = val.replace(/\_/g, "");

var valGenerics = extractGenerics(val);"#;
    let expected_result = "val=val.replace(/\\_/g,\"\");var valGenerics=extractGenerics(val);";
    assert_eq!(minify(source), expected_result);
}

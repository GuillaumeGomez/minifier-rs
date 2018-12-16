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

use js::token::{self, Token, Tokens};

use std::collections::{HashMap, HashSet};

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

/// Minifies a given JS source code.
///
/// # Example
///
/// ```rust
/// extern crate minifier;
/// use minifier::js::minify;
///
/// fn main() {
///     let js = r#"
///         function forEach(data, func) {
///            for (var i = 0; i < data.length; ++i) {
///                func(data[i]);
///            }
///         }"#.into();
///     let js_minified = minify(js);
/// }
/// ```
#[inline]
pub fn minify(source: &str) -> String {
    token::tokenize(source).apply(clean_tokens).to_string()
    /*match build_ast(&v) {
        Ok(x) => {}
        Err(e) => eprintln!("Failure: {}", e),
    }*/
}

/// Minifies a given JS source code and to replace keywords.
///
/// # Example
///
/// ```rust
/// extern crate minifier;
/// use minifier::js::{Keyword, minify_and_replace_keywords};
///
/// fn main() {
///     let js = r#"
///         function replaceByNull(data, func) {
///             for (var i = 0; i < data.length; ++i) {
///                 if func(data[i]) {
///                     data[i] = null;
///                 }
///             }
///         }
///     }"#.into();
///     let js_minified = minify_and_replace_keywords(js, &[(Keyword::Null, "N")]);
///     println!("{}", js_minified.to_string());
/// }
/// ```
///
/// The previous code will have all its `null` keywords replaced with `N`. In such cases,
/// don't forget to include the definition of `N` in the returned minified javascript:
///
/// ```js
/// var N = null;
/// ```
#[inline]
pub fn minify_and_replace_keywords<'a>(
    source: &'a str,
    keywords_to_replace: &'a [(token::Keyword, &str)],
) -> Tokens<'a> {
    let mut v = token::tokenize(source).apply(clean_tokens);
    for &(keyword, replacement) in keywords_to_replace {
        for token in v.0.iter_mut() {
            if match token.get_keyword() {
                Some(ref k) => *k == keyword,
                _ => false,
            } {
                *token = token::Token::Other(replacement);
            }
        }
    }
    v
}

struct VariableNameGenerator<'a> {
    cur1: char,
    cur2: char,
    prepend: &'a str,
}

fn incr_letters(letters: &mut [&mut char]) {
    let max = [('z', 'A'), ('Z', '0'), ('9', 'a')];

    for (m, next) in &max {
        if letters[0] == m {
            *letters[0] = *next;
            if *letters[0] == 'a' {
                incr_letters(&mut letters[1..]);
            }
            return;
        }
    }
    *letters[0] = ((*letters[0] as u8) + 1) as char;
}

impl<'a> VariableNameGenerator<'a> {
    fn new(prepend: &'a str) -> VariableNameGenerator<'a> {
        VariableNameGenerator {
            cur1: 'a',
            cur2: 'a',
            prepend,
        }
    }

    fn next(&mut self) {
        incr_letters(&mut [&mut self.cur2, &mut self.cur1]);
    }

    fn to_string(&self) -> String {
        format!("{}{}{}", self.prepend, self.cur1, self.cur2)
    }
}

/// Aggregate litteral strings. For instance, if the string litteral "Oh look over there!"
/// appears more than once, a variable will be created with this value and used everywhere the
/// string appears. Of course, this replacement is only performed when it allows to take
/// less space.
///
/// # Example
///
/// ```rust,no_run
/// extern crate minifier;
/// use minifier::js::{aggregate_strings, clean_tokens, simple_minify};
/// use std::fs;
///
/// fn main() {
///     let content = fs::read("some_file.js").expect("file not found");
///     let source = String::from_utf8_lossy(&content);
///     let s = simple_minify(&source);    // First we get the tokens list.
///     let s = s.apply(clean_tokens)      // The first `apply` is used to remove useless chars.
///              .apply(aggregate_strings) // This one aggregate string litterals.
///              .to_string();             // And we finally convert to string.
///     println!("result: {}", s);
/// }
/// ```
#[inline]
pub fn aggregate_strings<'a>(mut tokens: Tokens<'a>) -> Tokens<'a> {
    let mut new_vars = Vec::with_capacity(50);

    for (var_name, positions) in {
        let mut strs: HashMap<&Token, Vec<usize>> = HashMap::with_capacity(1000);
        let mut validated: HashSet<&Token> = HashSet::with_capacity(100);

        for (pos, token) in tokens.iter().enumerate() {
            if token.is_string() {
                let x = strs.entry(token).or_insert_with(|| Vec::with_capacity(1));
                x.push(pos);
                if x.len() > 1 {
                    let len = token.get_string().unwrap().len();
                    // Computation here is simple, we declare new variables when creating this so
                    // the total of characters must be shorter than:
                    // `var r_aa=...;` -> 10 + `r_aa` -> 14
                    if x.len() * len > 10 + x.len() * 4 {
                        validated.insert(token);
                    }
                }
            }
        }
        let mut var_gen = VariableNameGenerator::new("r_");
        let mut ret = Vec::with_capacity(validated.len());

        // We need this macro to avoid having to sort the set when not testing the crate.
        #[cfg(test)]
        macro_rules! inner_loop {
            ($x:ident) => {{
                let mut $x = $x.into_iter().collect::<Vec<_>>();
                $x.sort();
                $x
            }}
        }
        #[cfg(not(test))]
        macro_rules! inner_loop {
            ($x:ident) => {
                $x.iter()
            }
        }

        for v in inner_loop!(validated) {
            let x = strs.remove(v).unwrap();
            ret.push((var_gen.to_string(), x));
            var_gen.next();
        }
        ret
    } {
        new_vars.push(Token::CreatedVar(format!("var {}={};", var_name, tokens[positions[0]])));
        for pos in positions {
            tokens.0[pos] = Token::CreatedVar(var_name.clone());
        }
    }
    new_vars.append(&mut tokens.0);
    Tokens(new_vars)
}

/// Simple function to get the untouched token list. Useful in case you want to perform some
/// actions directly on it.
///
/// # Example
///
/// ```rust,no_run
/// extern crate minifier;
/// use minifier::js::simple_minify;
/// use std::fs;
///
/// fn main() {
///     let content = fs::read("some_file.js").expect("file not found");
///     let source = String::from_utf8_lossy(&content);
///     let s = simple_minify(&source);
///     println!("result: {:?}", s); // We now have the tokens list.
/// }
/// ```
#[inline]
pub fn simple_minify<'a>(source: &'a str) -> Tokens<'a> {
    token::tokenize(source)
}

/// Convenient function used to clean useless tokens in a token list.
///
/// # Example
///
/// ```rust,no_run
/// extern crate minifier;
/// use minifier::js::{clean_tokens, simple_minify};
/// use std::fs;
///
/// fn main() {
///     let content = fs::read("some_file.js").expect("file not found");
///     let source = String::from_utf8_lossy(&content);
///     let s = simple_minify(&source); // First we get the tokens list.
///     let s = s.apply(clean_tokens);  // We now have a cleaned token list!
///     println!("result: {:?}", s);
/// }
/// ```
#[inline]
pub fn clean_tokens<'a>(mut tokens: Tokens<'a>) -> Tokens<'a> {
    tokens.0.retain(|c| {
        !c.is_comment() && {
            if let Some(x) = c.get_char() {
                !x.is_useless()
            } else {
                true
            }
        }
    });
    tokens
}

#[test]
fn string_duplicates() {
    let source = r#"var x = ["a nice string", "a nice string", "another nice string", "cake!",
                             "cake!", "a nice string", "cake!", "cake!", "cake!"];"#;
    let expected_result = "var r_aa=\"a nice string\";var r_ab=\"cake!\";var x=[r_aa,r_aa,\
                           \"another nice string\",r_ab,r_ab,r_aa,r_ab,r_ab,r_ab];";

    let result = simple_minify(source).apply(clean_tokens).apply(aggregate_strings).to_string();
    assert_eq!(result, expected_result);
}

#[test]
fn simple_quote() {
    let source = r#"var x = "\\";"#;
    let expected_result = r#"var x="\\";"#;
    assert_eq!(minify(source), expected_result);
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

#[test]
fn replace_keyword() {
    let source = r#"
var x = ['a', 'b', null, 'd', {'x': null, 'e': null, 'z': 'w'}];
var n = null;
"#;
    let expected_result = "var x=['a','b',N,'d',{'x':N,'e':N,'z':'w'}];var n=N;";
    assert_eq!(minify_and_replace_keywords(source, &[(token::Keyword::Null, "N")]).to_string(),
               expected_result);
}

// TODO: requires AST to fix this issue!
/*#[test]
fn no_semi_colon() {
    let source = r#"
console.log(1)
console.log(2)
var x = 12;
"#;
    let expected_result = r#"console.log(1);console.log(2);var x=12;"#;
    assert_eq!(minify(source), expected_result);
}*/

// TODO: requires AST to fix this issue!
/*#[test]
fn correct_replace_for_backline() {
    let source = r#"
function foo() {
    return
    12;
}
"#;
    let expected_result = r#"function foo(){return;12;}"#;
    assert_eq!(minify(source), expected_result);
}*/

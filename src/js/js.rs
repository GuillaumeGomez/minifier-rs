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

use js::token::{self, Keyword, ReservedChar, Token, Tokens};
use js::utils::{get_variable_name_and_value_positions, VariableNameGenerator};

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
    token::tokenize(source).apply(::js::clean_tokens).to_string()
    /*match build_ast(&v) {
        Ok(x) => {}
        Err(e) => eprintln!("Failure: {}", e),
    }*/
}

// TODO: No scope handling or anything. Might be nice as a second step to add it...
fn get_variables_name<'a>(
    tokens: &'a Tokens<'a>,
) -> (HashSet<&'a str>, HashMap<&'a str, (usize, usize)>) {
    let mut ret = HashSet::new();
    let mut variables = HashMap::new();
    let mut pos = 0;

    while pos < tokens.len() {
        if tokens[pos].is_keyword() || tokens[pos].is_other() {
            if let Some((var_pos, Some(value_pos))) = get_variable_name_and_value_positions(tokens, pos) {
                pos = value_pos;
                if let Some(var_name) = tokens[var_pos].get_other() {
                    if !var_name.starts_with("r_") {
                        pos += 1;
                        continue;
                    }
                    ret.insert(var_name);
                }
                if let Some(s) = tokens[value_pos].get_string() {
                    variables.insert(s, (var_pos, value_pos));
                }
            }
        }
        pos += 1;
    }
    (ret, variables)
}

#[inline]
fn aggregate_strings_inner<'a, 'b: 'a>(
    mut tokens: Tokens<'a>,
    separation_token: Option<Token<'b>>,
) -> Tokens<'a> {
    let mut new_vars = Vec::with_capacity(50);
    let mut to_replace: Vec<(usize, usize)> = Vec::new();

    for (var_name, positions) in {
        let mut strs: HashMap<&Token, Vec<usize>> = HashMap::with_capacity(1000);
        let mut validated: HashMap<&Token, String> = HashMap::with_capacity(100);

        let mut var_gen = VariableNameGenerator::new(Some("r_"), 2);
        let mut next_name = var_gen.to_string();

        let (all_variables, values) = get_variables_name(&tokens);
        while all_variables.contains(&next_name.as_str()) {
            var_gen.next();
            next_name = var_gen.to_string();
        }

        for pos in 0..tokens.len() {
            let token = &tokens[pos];
            if let Some(str_token) = token.get_string() {
                if let Some((var_pos, string_pos)) = values.get(&str_token) {
                    if pos != *string_pos {
                        to_replace.push((pos, *var_pos));
                    }
                    continue;
                }
                let x = strs.entry(token).or_insert_with(|| Vec::with_capacity(1));
                x.push(pos);
                if x.len() > 1 && validated.get(token).is_none() {
                    let len = str_token.len();
                    // Computation here is simple, we declare new variables when creating this so
                    // the total of characters must be shorter than:
                    // `var r_aa=...;` -> 10 + `r_aa` -> 14
                    if (x.len() + 2 /* quotes */) * len > next_name.len() + str_token.len() + 6 /* var _=_;*/ + x.len() * next_name.len() {
                        validated.insert(token, next_name.clone());
                        var_gen.next();
                        next_name = var_gen.to_string();
                        while all_variables.contains(&next_name.as_str()) {
                            var_gen.next();
                            next_name = var_gen.to_string();
                        }
                    }
                }
            }
        }
        let mut ret = Vec::with_capacity(validated.len());

        // We need this macro to avoid having to sort the set when not testing the crate.
        //#[cfg(test)]
        macro_rules! inner_loop {
            ($x:ident) => {{
                let mut $x = $x.into_iter().collect::<Vec<_>>();
                $x.sort_unstable_by(|a, b| a.1.cmp(&b.1));
                $x
            }}
        }
        /*#[cfg(not(test))]
        macro_rules! inner_loop {
            ($x:ident) => {
                $x.into_iter()
            }
        }*/

        for (token, var_name) in inner_loop!(validated) {
            ret.push((var_name, strs.remove(&token).unwrap()));
            var_gen.next();
        }
        ret
    } {
        if new_vars.is_empty() {
            new_vars.push(Token::Keyword(Keyword::Var));
        } else {
            new_vars.push(Token::Char(ReservedChar::Comma));
        }
        new_vars.push(Token::CreatedVarDecl(format!("{}={}", var_name, tokens[positions[0]])));
        for pos in positions {
            tokens.0[pos] = Token::CreatedVar(var_name.clone());
        }
    }
    if !new_vars.is_empty() {
        new_vars.push(Token::Char(ReservedChar::SemiColon));
    }
    for (to_replace_pos, variable_pos) in to_replace {
        tokens.0[to_replace_pos] = tokens.0[variable_pos].clone();
    }
    if let Some(token) = separation_token {
        new_vars.push(token);
    }
    new_vars.append(&mut tokens.0);
    Tokens(new_vars)
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
///     let s = s.apply(aggregate_strings) // This `apply` aggregates string litterals.
///              .apply(clean_tokens)      // This one is used to remove useless chars.
///              .to_string();             // And we finally convert to string.
///     println!("result: {}", s);
/// }
/// ```
#[inline]
pub fn aggregate_strings<'a>(tokens: Tokens<'a>) -> Tokens<'a> {
    aggregate_strings_inner(tokens, None)
}

/// Exactly like `aggregate_strings` except this one expects a separation token
/// to be passed. This token will be placed between the created variables for the
/// strings aggregation and the rest.
///
/// # Example
///
/// Let's add a backline between the created variables and the rest of the code:
///
/// ```rust,no_run
/// extern crate minifier;
/// use minifier::js::{
///     aggregate_strings_with_separation,
///     clean_tokens,
///     simple_minify,
///     Token,
///     ReservedChar,
/// };
/// use std::fs;
///
/// fn main() {
///     let content = fs::read("some_file.js").expect("file not found");
///     let source = String::from_utf8_lossy(&content);
///     let s = simple_minify(&source);    // First we get the tokens list.
///     let s = s.apply(|f| {
///                  aggregate_strings_with_separation(f, Token::Char(ReservedChar::Backline))
///              })                   // We add a backline between the variable and the rest.
///              .apply(clean_tokens) // We clean the tokens.
///              .to_string();        // And we finally convert to string.
///     println!("result: {}", s);
/// }
/// ```
#[inline]
pub fn aggregate_strings_with_separation<'a, 'b: 'a>(
    tokens: Tokens<'a>,
    separation_token: Token<'b>,
) -> Tokens<'a> {
    aggregate_strings_inner(tokens, Some(separation_token))
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

#[test]
fn string_duplicates() {
    let source = r#"var x = ["a nice string", "a nice string", "another nice string", "cake!",
                             "cake!", "a nice string", "cake!", "cake!", "cake!"];"#;
    let expected_result = "var r_aa=\"a nice string\",r_ba=\"cake!\";var x=[r_aa,r_aa,\
                           \"another nice string\",r_ba,r_ba,r_aa,r_ba,r_ba,r_ba];";

    let result = simple_minify(source).apply(aggregate_strings)
                                      .apply(::js::clean_tokens)
                                      .to_string();
    assert_eq!(result, expected_result);
}

#[test]
fn already_existing_var() {
    let source = r#"var r_aa = "a nice string"; var x = ["a nice string", "a nice string",
                    "another nice string", "cake!",
                    "cake!", "a nice string", "cake!", "cake!", "cake!"];"#;
    let expected_result = "var r_ba=\"cake!\";var r_aa=\"a nice string\";var x=[r_aa,r_aa,\
                           \"another nice string\",r_ba,r_ba,r_aa,r_ba,r_ba,r_ba];";

    let result = simple_minify(source).apply(aggregate_strings)
                                      .apply(::js::clean_tokens)
                                      .to_string();
    assert_eq!(result, expected_result);
}

#[test]
fn string_duplicates_variables_already_exist() {
    let source = r#"var r_aa=1;var x = ["a nice string", "a nice string", "another nice string", "cake!",
                             "cake!", "a nice string", "cake!", "cake!", "cake!"];"#;
    let expected_result = "var r_ba=\"a nice string\",r_ca=\"cake!\";\
                           var r_aa=1;var x=[r_ba,r_ba,\
                           \"another nice string\",r_ca,r_ca,r_ba,r_ca,r_ca,r_ca];";

    let result = simple_minify(source).apply(aggregate_strings)
                                      .apply(::js::clean_tokens)
                                      .to_string();
    assert_eq!(result, expected_result);
}

#[test]
fn string_duplicates_with_separator() {
    use self::token::ReservedChar;

    let source = r#"var x = ["a nice string", "a nice string", "another nice string", "cake!",
                             "cake!", "a nice string", "cake!", "cake!", "cake!"];"#;
    let expected_result = "var r_aa=\"a nice string\",r_ba=\"cake!\";\nvar x=[r_aa,r_aa,\
                           \"another nice string\",r_ba,r_ba,r_aa,r_ba,r_ba,r_ba];";
    let result = simple_minify(source).apply(::js::clean_tokens)
                                      .apply(|f| {
                     aggregate_strings_with_separation(f, Token::Char(ReservedChar::Backline))
                 }).to_string();
    assert_eq!(result, expected_result);
}

#[test]
fn clean_except() {
    use self::token::ReservedChar;

    let source = r#"var x = [1, 2, 3];
var y = "salut";
var z = "ok!";"#;
    let expected = r#"var x=[1,2,3];
var y="salut";
var z="ok!";"#;

    let result = simple_minify(source).apply(|f| {
                     ::js::clean_tokens_except(f, |c| {
                         c.get_char() != Some(ReservedChar::Backline)
                     })
                 }).to_string();
    assert_eq!(result, expected);
}

#[test]
fn name_generator() {
    let s = ::std::iter::repeat('a').take(36).collect::<String>();
    // We need to generate enough long strings to reach the point that the name generator
    // generates names with 3 characters.
    let s = ::std::iter::repeat(s).take(20000)
                                  .enumerate()
                                  .map(|(pos, s)| format!("{}{}", s, pos))
                                  .collect::<Vec<_>>();
    let source = format!("var x = [{}];",
                         s.iter()
                          .map(|s| format!("\"{0}\",\"{0}\"", s))
                          .collect::<Vec<_>>()
                          .join(","));
    let result = simple_minify(&source).apply(::js::clean_tokens)
                                       .apply(aggregate_strings)
                                       .to_string();
    assert!(result.find(",r_aaa=").is_some());
    assert!(result.find(",r_ab=").unwrap() < result.find(",r_ba=").unwrap());
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

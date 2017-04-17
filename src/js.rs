// MIT License
//
// Copyright (c) 2017 Guillaume Gomez
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

fn remove_commented_lines(source: &str) -> String {
    let mut res = String::new();
    for line in source.lines() {
        let strip_line = line.trim();
        if strip_line.starts_with("//") && !strip_line.ends_with("*/") {
            continue
        }
        res.push_str(line);
    }
    res
}

fn get_next(source: &[char], pos: &mut usize) -> Option<char> {
    if *pos < source.len() {
        *pos += 1;
        Some(source[*pos - 1] as char)
    } else {
        None
    }
}

fn is_in(c: char, s: &str) -> bool {
    s.find(c).is_some()
}

fn global_minify(source: &str) -> String {
    if source.len() < 3 {
        return source.to_owned();
    }
    let source: Vec<char> = source.chars().collect();
    let mut output = String::new();
    let space_strings = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$\\";
    let starters = "{[(+-";
    let enders = "}])+-\"'";
    let newlinestart_strings = format!("{}{}", starters, space_strings);
    let newlineend_strings = format!("{}{}", enders, space_strings);
    let mut do_newline = false;
    let mut do_space = false;
    let mut doing_single_comment = false;
    let mut doing_multi_comment = false;
    let mut previous_before_comment = '\0';
    let mut in_quote = '\0';
    let mut in_re = false;
    let mut quote_buf = Vec::new();
    let mut previous = source[0];
    let mut next1 = source[1];
    let mut previous_non_space = None;

    if previous == '/' {
        if next1 == '/' {
            doing_single_comment = true;
        } else if next1 == '*' {
            doing_multi_comment = true;
        } else {
            output.push(previous);
        }
    } else if previous >= '!' {
        if is_in(previous, "\'\"") {
            in_quote = previous;
        }
        output.push(previous);
        previous_non_space = Some(previous);
    } else {
        previous_non_space = Some(' ');
    }
    let mut next2;
    let mut pos = 2;
    loop {
        next2 = get_next(&source, &mut pos);
        if next2.is_none() {
            break
        }
        if doing_multi_comment {
            if next1 == '*' && next2 == Some('/') {
                doing_multi_comment = false;
                next2 = get_next(&source, &mut pos);
            }
        } else if doing_single_comment {
            if is_in(next1, "\r\n") {
                doing_single_comment = false;
                while next2 == Some('\r') || next2 == Some('\n') {
                    next2 = get_next(&source, &mut pos);
                    if next2.is_none() {
                        break
                    }
                }
                if is_in(previous_before_comment, ")}]") {
                    do_newline = true;
                } else if is_in(previous_before_comment, space_strings) {
                    output.push('\n')
                }
            }
        } else if in_quote != '\0' {
            quote_buf.push(next1);

            if next1 == in_quote {
                let mut numslashes = 0;
                let max = quote_buf.len() - 1;
                for c in (&quote_buf[..max]).iter().rev() {
                    if *c != '\\' {
                        break
                    } else {
                        numslashes += 1;
                    }
                }
                if numslashes & 1 == 0 {
                    in_quote = '\0';
                    for i in &quote_buf {
                        output.push(*i);
                    }
                }
            }
        } else if is_in(next1, "\r\n") {
            let previous_non_space = previous_non_space.unwrap_or('\0');
            if is_in(previous_non_space, &newlineend_strings) || previous_non_space > '~' {
                loop {
                    if next2.unwrap_or('\0') < '!' {
                        next2 = get_next(&source, &mut pos);
                        if next2.is_none() {
                            break
                        }
                    } else {
                        let next2 = next2.unwrap_or('\0');
                        if is_in(next2, &newlinestart_strings) || next2 > '~' || next2 == '/' {
                            do_newline = true;
                        }
                        break
                    }
                }
            }
        } else if next1 < '!' && !in_re {
            let next2 = next2.unwrap_or('\0');
            let previous_non_space = previous_non_space.unwrap_or('\0');
            if (is_in(previous_non_space, space_strings) || previous_non_space > '~') &&
               (is_in(next2, space_strings) || next2 > '~') {
                do_space = true;
            }
        } else if next1 == '/' {
            let next2 = next2.unwrap_or('\0');
            let previous_non_space = previous_non_space.unwrap_or('\0');
            if in_re {
                if previous != '\\' {
                    in_re = false;
                }
                output.push('/');
            } else if next2 == '/' {
                doing_single_comment = true;
                previous_before_comment = previous_non_space;
            } else if next2 == '*' {
                doing_multi_comment = true;
            } else {
                in_re = is_in(previous_non_space, "(,=:[?!&|");
                output.push('/');
            }
        } else {
            if do_space {
                do_space = false;
                output.push(' ');
            }
            if do_newline {
                output.push('\n');
                do_newline = false;
            }
            output.push(next1);
            if !in_re && is_in(next1, "'\"") {
                in_quote = next1;
                quote_buf = Vec::new();
            }
        }
        previous = next1;
        next1 = next2.unwrap_or('\0');
        if previous >= '!' {
            previous_non_space = Some(previous);
        }
    }
    let last = next1;
    let last = format!("{}", last).trim().to_owned();
    if !(doing_single_comment || doing_multi_comment) && last != "" && last != "/" {
        output.push_str(&last);
    }
    output
}

/// Returns a minified version of the provided JS source.
pub fn minify(source: &str) -> String {
    let source = remove_commented_lines(source);
    global_minify(&source).trim().to_owned()
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

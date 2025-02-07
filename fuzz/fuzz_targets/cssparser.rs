// This file is almost entirely lifted from
// https://github.com/servo/rust-cssparser/blob/fa6f5eb23f058c6fce444ac781b0b380003fdb59/fuzz/fuzz_targets/cssparser.rs
// but we compare minified and unminified code instead.
//
// This parser branches on ascii characters, so you'll find interesting corner cases more quickly with
// cargo fuzz run cssparser -- -only_ascii=1
#![no_main]

use cssparser::*;

const DEBUG: bool = false;

fn parse_and_serialize(input: &str) -> String {
    let mut input = ParserInput::new(input);
    let mut parser = Parser::new(&mut input);
    let mut serialization = String::new();
    let result = do_parse_and_serialize(
        &mut parser,
        TokenSerializationType::nothing(),
        &mut serialization,
        0,
    );
    if result.is_err() {
        return String::new();
    }
    serialization
}

fn do_parse_and_serialize<'i>(
    input: &mut Parser<'i, '_>,
    mut previous_token_type: TokenSerializationType,
    serialization: &mut String,
    indent_level: usize,
) -> Result<(), ParseError<'i, ()>> {
    loop {
        let token = input.next();
        let token = match token {
            Ok(token) => token,
            Err(..) => break,
        };
        if DEBUG {
            for _ in 0..indent_level {
                print!(" ");
            }
            println!("{:?}", token);
        }
        if token.is_parse_error() {
            let token = token.clone();
            return Err(input.new_unexpected_token_error(token))
        }
        let token_type = token.serialization_type();
        if previous_token_type.needs_separator_when_before(token_type) {
            serialization.push_str("/**/");
        }
        previous_token_type = token_type;
        token.to_css(serialization).unwrap();
        let closing_token = match token {
            Token::Function(_) | Token::ParenthesisBlock => Token::CloseParenthesis,
            Token::SquareBracketBlock => Token::CloseSquareBracket,
            Token::CurlyBracketBlock => Token::CloseCurlyBracket,
            _ => continue,
        };

        input.parse_nested_block(|input| -> Result<_, ParseError<()>> {
            do_parse_and_serialize(input, previous_token_type, serialization, indent_level + 1)
        })?;

        closing_token.to_css(serialization).unwrap();
    }
    Ok(())
}

fn fuzz(data: &str) {
    let unminified = parse_and_serialize(data);
    if unminified != "" {
        let Ok(minified) = minifier::css::minify(data) else { return };
        let minified = minified.to_string();
        eprintln!("{minified:?}");
        let minified = parse_and_serialize(&minified);
        assert_eq!(unminified, minified);
    }
}

libfuzzer_sys::fuzz_target!(|data: &str| {
    fuzz(data);
});
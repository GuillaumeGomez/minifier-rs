// Take a look at the license at the top of the repository in the LICENSE file.

use std::convert::TryFrom;
use std::fmt;
use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ReservedChar {
    Comma,
    SuperiorThan,
    OpenParenthese,
    CloseParenthese,
    OpenCurlyBrace,
    CloseCurlyBrace,
    OpenBracket,
    CloseBracket,
    Colon,
    SemiColon,
    Slash,
    Plus,
    EqualSign,
    Space,
    Tab,
    Backline,
    Star,
    Quote,
    DoubleQuote,
    Pipe,
    Tilde,
    Dollar,
    Circumflex,
    Backslash,
}

impl fmt::Display for ReservedChar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                ReservedChar::Comma => ',',
                ReservedChar::OpenParenthese => '(',
                ReservedChar::CloseParenthese => ')',
                ReservedChar::OpenCurlyBrace => '{',
                ReservedChar::CloseCurlyBrace => '}',
                ReservedChar::OpenBracket => '[',
                ReservedChar::CloseBracket => ']',
                ReservedChar::Colon => ':',
                ReservedChar::SemiColon => ';',
                ReservedChar::Slash => '/',
                ReservedChar::Star => '*',
                ReservedChar::Plus => '+',
                ReservedChar::EqualSign => '=',
                ReservedChar::Space => ' ',
                ReservedChar::Tab => '\t',
                ReservedChar::Backline => '\n',
                ReservedChar::SuperiorThan => '>',
                ReservedChar::Quote => '\'',
                ReservedChar::DoubleQuote => '"',
                ReservedChar::Pipe => '|',
                ReservedChar::Tilde => '~',
                ReservedChar::Dollar => '$',
                ReservedChar::Circumflex => '^',
                ReservedChar::Backslash => '\\',
            }
        )
    }
}

impl TryFrom<char> for ReservedChar {
    type Error = &'static str;

    fn try_from(value: char) -> Result<ReservedChar, Self::Error> {
        match value {
            '\'' => Ok(ReservedChar::Quote),
            '"' => Ok(ReservedChar::DoubleQuote),
            ',' => Ok(ReservedChar::Comma),
            '(' => Ok(ReservedChar::OpenParenthese),
            ')' => Ok(ReservedChar::CloseParenthese),
            '{' => Ok(ReservedChar::OpenCurlyBrace),
            '}' => Ok(ReservedChar::CloseCurlyBrace),
            '[' => Ok(ReservedChar::OpenBracket),
            ']' => Ok(ReservedChar::CloseBracket),
            ':' => Ok(ReservedChar::Colon),
            ';' => Ok(ReservedChar::SemiColon),
            '/' => Ok(ReservedChar::Slash),
            '*' => Ok(ReservedChar::Star),
            '+' => Ok(ReservedChar::Plus),
            '=' => Ok(ReservedChar::EqualSign),
            ' ' => Ok(ReservedChar::Space),
            '\t' => Ok(ReservedChar::Tab),
            '\n' | '\r' => Ok(ReservedChar::Backline),
            '>' => Ok(ReservedChar::SuperiorThan),
            '|' => Ok(ReservedChar::Pipe),
            '~' => Ok(ReservedChar::Tilde),
            '$' => Ok(ReservedChar::Dollar),
            '^' => Ok(ReservedChar::Circumflex),
            '\\' => Ok(ReservedChar::Backslash),
            _ => Err("Unknown reserved char"),
        }
    }
}

impl ReservedChar {
    fn is_useless(&self) -> bool {
        *self == ReservedChar::Space
            || *self == ReservedChar::Tab
            || *self == ReservedChar::Backline
    }

    fn is_operator(&self) -> bool {
        Operator::try_from(*self).is_ok()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Operator {
    Plus,
    Multiply,
    Minus,
    Modulo,
    Divide,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Operator::Plus => '+',
                Operator::Multiply => '*',
                Operator::Minus => '-',
                Operator::Modulo => '%',
                Operator::Divide => '/',
            }
        )
    }
}

impl TryFrom<char> for Operator {
    type Error = &'static str;

    fn try_from(value: char) -> Result<Operator, Self::Error> {
        match value {
            '+' => Ok(Operator::Plus),
            '*' => Ok(Operator::Multiply),
            '-' => Ok(Operator::Minus),
            '%' => Ok(Operator::Modulo),
            '/' => Ok(Operator::Divide),
            _ => Err("Unknown operator"),
        }
    }
}

impl TryFrom<ReservedChar> for Operator {
    type Error = &'static str;

    fn try_from(value: ReservedChar) -> Result<Operator, Self::Error> {
        match value {
            ReservedChar::Slash => Ok(Operator::Divide),
            ReservedChar::Star => Ok(Operator::Multiply),
            ReservedChar::Plus => Ok(Operator::Plus),
            _ => Err("Unknown operator"),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum SelectorElement<'a> {
    PseudoClass(&'a str),
    Class(&'a str),
    Id(&'a str),
    Tag(&'a str),
    Media(&'a str),
}

impl<'a> TryFrom<&'a str> for SelectorElement<'a> {
    type Error = &'static str;

    fn try_from(value: &'a str) -> Result<SelectorElement<'a>, Self::Error> {
        if let Some(value) = value.strip_prefix('.') {
            if value.is_empty() {
                Err("cannot determine selector")
            } else {
                Ok(SelectorElement::Class(value))
            }
        } else if let Some(value) = value.strip_prefix('#') {
            if value.is_empty() {
                Err("cannot determine selector")
            } else {
                Ok(SelectorElement::Id(value))
            }
        } else if let Some(value) = value.strip_prefix('@') {
            if value.is_empty() {
                Err("cannot determine selector")
            } else {
                Ok(SelectorElement::Media(value))
            }
        } else if let Some(value) = value.strip_prefix(':') {
            if value.is_empty() {
                Err("cannot determine selector")
            } else {
                Ok(SelectorElement::PseudoClass(value))
            }
        } else if value.chars().next().unwrap_or(' ').is_alphabetic() {
            Ok(SelectorElement::Tag(value))
        } else {
            Err("unknown selector")
        }
    }
}

impl fmt::Display for SelectorElement<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            SelectorElement::Class(c) => write!(f, ".{}", c),
            SelectorElement::Id(i) => write!(f, "#{}", i),
            SelectorElement::Tag(t) => write!(f, "{}", t),
            SelectorElement::Media(m) => write!(f, "@{} ", m),
            SelectorElement::PseudoClass(pc) => write!(f, ":{}", pc),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug, Copy)]
pub enum SelectorOperator {
    /// `~=`
    OneAttributeEquals,
    /// `|=`
    EqualsOrStartsWithFollowedByDash,
    /// `$=`
    EndsWith,
    /// `^=`
    FirstStartsWith,
    /// `*=`
    Contains,
}

impl fmt::Display for SelectorOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            SelectorOperator::OneAttributeEquals => write!(f, "~="),
            SelectorOperator::EqualsOrStartsWithFollowedByDash => write!(f, "|="),
            SelectorOperator::EndsWith => write!(f, "$="),
            SelectorOperator::FirstStartsWith => write!(f, "^="),
            SelectorOperator::Contains => write!(f, "*="),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Token<'a> {
    /// Comment.
    Comment(&'a str),
    /// Comment starting with `/**`.
    License(&'a str),
    Char(ReservedChar),
    Other(&'a str),
    SelectorElement(SelectorElement<'a>),
    String(&'a str),
    SelectorOperator(SelectorOperator),
    Operator(Operator),
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            // Token::AtRule(at_rule) => write!(f, "{}", at_rule, content),
            // Token::ElementRule(selectors) => write!(f, "{}", x),
            Token::Comment(c) => write!(f, "{}", c),
            Token::License(l) => writeln!(f, "/*!{}*/", l),
            Token::Char(c) => write!(f, "{}", c),
            Token::Other(s) => write!(f, "{}", s),
            Token::SelectorElement(ref se) => write!(f, "{}", se),
            Token::String(s) => write!(f, "{}", s),
            Token::SelectorOperator(so) => write!(f, "{}", so),
            Token::Operator(op) => write!(f, "{}", op),
        }
    }
}

impl Token<'_> {
    fn is_comment(&self) -> bool {
        matches!(*self, Token::Comment(_))
    }

    fn is_char(&self) -> bool {
        matches!(*self, Token::Char(_))
    }

    fn get_char(&self) -> Option<ReservedChar> {
        match *self {
            Token::Char(c) => Some(c),
            _ => None,
        }
    }

    fn is_useless(&self) -> bool {
        match *self {
            Token::Char(c) => c.is_useless(),
            _ => false,
        }
    }

    fn is_a_media(&self) -> bool {
        matches!(*self, Token::SelectorElement(SelectorElement::Media(_)))
    }

    fn is_a_license(&self) -> bool {
        matches!(*self, Token::License(_))
    }

    fn is_operator(&self) -> bool {
        match *self {
            Token::Operator(_) => true,
            Token::Char(c) => c.is_operator(),
            _ => false,
        }
    }
}

impl PartialEq<ReservedChar> for Token<'_> {
    fn eq(&self, other: &ReservedChar) -> bool {
        match *self {
            Token::Char(c) => c == *other,
            _ => false,
        }
    }
}

fn get_comment<'a>(
    source: &'a str,
    iterator: &mut Peekable<CharIndices<'_>>,
    start_pos: &mut usize,
) -> Option<Token<'a>> {
    let mut prev = ReservedChar::Quote;
    *start_pos += 1;
    let builder = if let Some((_, c)) = iterator.next() {
        if c == '!' || (c == '*' && iterator.peek().map(|(_, c)| c) != Some(&'/')) {
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

    for (pos, c) in iterator {
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

fn get_string<'a>(
    source: &'a str,
    iterator: &mut Peekable<CharIndices<'_>>,
    start_pos: &mut usize,
    start: ReservedChar,
) -> Option<Token<'a>> {
    while let Some((pos, c)) = iterator.next() {
        if c == '\\' {
            // we skip next character
            iterator.next();
            continue;
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

fn fill_other<'a>(
    source: &'a str,
    v: &mut Vec<Token<'a>>,
    start: usize,
    pos: usize,
    is_in_block: isize,
    is_in_media: bool,
    is_in_attribute_selector: bool,
) {
    if start < pos {
        if !is_in_attribute_selector
            && ((is_in_block == 0 && !is_in_media) || (is_in_media && is_in_block == 1))
        {
            let mut is_pseudo_class = false;
            let mut add = 0;
            if let Some(&Token::Char(ReservedChar::Colon)) = v.last() {
                is_pseudo_class = true;
                add = 1;
            }
            if let Ok(s) = SelectorElement::try_from(&source[start - add..pos]) {
                if is_pseudo_class {
                    v.pop();
                }
                v.push(Token::SelectorElement(s));
            } else {
                let s = &source[start..pos];
                if !s.starts_with(':')
                    && !s.starts_with('.')
                    && !s.starts_with('#')
                    && !s.starts_with('@')
                {
                    v.push(Token::Other(s));
                }
            }
        } else {
            v.push(Token::Other(&source[start..pos]));
        }
    }
}

#[allow(clippy::comparison_chain)]
pub(super) fn tokenize(source: &str) -> Result<Tokens<'_>, &'static str> {
    let mut v = Vec::with_capacity(1000);
    let mut iterator = source.char_indices().peekable();
    let mut start = 0;
    let mut is_in_block: isize = 0;
    let mut is_in_media = false;
    let mut is_in_attribute_selector = false;

    loop {
        let (mut pos, c) = match iterator.next() {
            Some(x) => x,
            None => {
                fill_other(
                    source,
                    &mut v,
                    start,
                    source.len(),
                    is_in_block,
                    is_in_media,
                    is_in_attribute_selector,
                );
                break;
            }
        };
        if let Ok(c) = ReservedChar::try_from(c) {
            fill_other(
                source,
                &mut v,
                start,
                pos,
                is_in_block,
                is_in_media,
                is_in_attribute_selector,
            );
            is_in_media = is_in_media
                || v.last()
                    .unwrap_or(&Token::Char(ReservedChar::Space))
                    .is_a_media();

            match c {
                ReservedChar::Backslash => {
                    v.push(Token::Char(ReservedChar::Backslash));

                    if iterator.next().is_some() {
                        pos += 1;
                        v.push(Token::Other(&source[pos..pos + 1]));
                    }
                }
                ReservedChar::Quote | ReservedChar::DoubleQuote => {
                    if let Some(s) = get_string(source, &mut iterator, &mut pos, c) {
                        v.push(s);
                    }
                }
                ReservedChar::Star
                    if *v.last().unwrap_or(&Token::Char(ReservedChar::Space))
                        == ReservedChar::Slash =>
                {
                    v.pop();
                    if let Some(s) = get_comment(source, &mut iterator, &mut pos) {
                        v.push(s);
                    }
                }
                ReservedChar::OpenBracket => {
                    if is_in_attribute_selector {
                        return Err("Already in attribute selector");
                    }
                    is_in_attribute_selector = true;
                    v.push(Token::Char(c));
                }
                ReservedChar::CloseBracket => {
                    if !is_in_attribute_selector {
                        return Err("Unexpected ']'");
                    }
                    is_in_attribute_selector = false;
                    v.push(Token::Char(c));
                }
                ReservedChar::OpenCurlyBrace => {
                    is_in_block += 1;
                    v.push(Token::Char(c));
                }
                ReservedChar::CloseCurlyBrace => {
                    is_in_block -= 1;
                    if is_in_block < 0 {
                        return Err("Too much '}'");
                    } else if is_in_block == 0 {
                        is_in_media = false;
                    }
                    v.push(Token::Char(c));
                }
                ReservedChar::SemiColon if is_in_block == 0 => {
                    is_in_media = false;
                    v.push(Token::Char(c));
                }
                ReservedChar::EqualSign => {
                    match match v
                        .last()
                        .unwrap_or(&Token::Char(ReservedChar::Space))
                        .get_char()
                        .unwrap_or(ReservedChar::Space)
                    {
                        ReservedChar::Tilde => Some(SelectorOperator::OneAttributeEquals),
                        ReservedChar::Pipe => {
                            Some(SelectorOperator::EqualsOrStartsWithFollowedByDash)
                        }
                        ReservedChar::Dollar => Some(SelectorOperator::EndsWith),
                        ReservedChar::Circumflex => Some(SelectorOperator::FirstStartsWith),
                        ReservedChar::Star => Some(SelectorOperator::Contains),
                        _ => None,
                    } {
                        Some(r) => {
                            v.pop();
                            v.push(Token::SelectorOperator(r));
                        }
                        None => v.push(Token::Char(c)),
                    }
                }
                c if !c.is_useless() => {
                    v.push(Token::Char(c));
                }
                c => {
                    if !v
                        .last()
                        .unwrap_or(&Token::Char(ReservedChar::Space))
                        .is_useless()
                        && (!v
                            .last()
                            .unwrap_or(&Token::Char(ReservedChar::OpenCurlyBrace))
                            .is_char()
                            || v.last()
                                .unwrap_or(&Token::Char(ReservedChar::OpenCurlyBrace))
                                .is_operator()
                            || v.last()
                                .unwrap_or(&Token::Char(ReservedChar::OpenCurlyBrace))
                                .get_char()
                                == Some(ReservedChar::CloseParenthese)
                            || v.last()
                                .unwrap_or(&Token::Char(ReservedChar::OpenCurlyBrace))
                                .get_char()
                                == Some(ReservedChar::CloseBracket))
                    {
                        v.push(Token::Char(ReservedChar::Space));
                    } else if let Ok(op) = Operator::try_from(c) {
                        v.push(Token::Operator(op));
                    }
                }
            }
            start = pos + 1;
        }
    }
    Ok(Tokens(clean_tokens(v)))
}

fn clean_tokens(mut v: Vec<Token<'_>>) -> Vec<Token<'_>> {
    // This function may remove multiple elements from the vector. Ideally we'd
    // use `Vec::retain`, but the traversal requires inspecting the previously
    // retained token and the next token, which `Vec::retain` doesn't allow. So
    // we have to use a lower-level mechanism.
    let mut i = 0;
    // Index of the previous retained token, if there is one.
    let mut ip: Option<usize> = None;
    let mut is_in_calc = false;
    let mut is_in_container = false;
    let mut paren = 0;
    // A vector of bools indicating which elements are to be retained.
    let mut b = Vec::with_capacity(v.len());

    while i < v.len() {
        if v[i] == Token::Other("calc") {
            is_in_calc = true;
        } else if is_in_calc {
            if v[i] == Token::Char(ReservedChar::CloseParenthese) {
                paren -= 1;
                is_in_calc = paren != 0;
            } else if v[i] == Token::Char(ReservedChar::OpenParenthese) {
                paren += 1;
            }
        }
        if v[i] == Token::SelectorElement(SelectorElement::Media("container")) {
            is_in_container = true;
        }

        let mut retain = true;
        if v[i].is_useless() {
            #[allow(clippy::if_same_then_else)]
            if ip.is_some() && v[ip.unwrap()] == Token::Char(ReservedChar::CloseBracket) {
                if i + 1 < v.len()
                    && (v[i + 1].is_useless()
                        || v[i + 1] == Token::Char(ReservedChar::OpenCurlyBrace))
                {
                    retain = false;
                }
            } else if ip.is_some() && matches!(v[ip.unwrap()], Token::Other("and" | "or" | "not")) {
                // retain the space after "and", "or" or "not"
            } else if is_in_calc && v[ip.unwrap()].is_useless() {
                retain = false;
            } else if is_in_container && matches!(v[ip.unwrap()], Token::Other(_)) {
                // retain spaces between keywords in container queryes
            } else if !is_in_calc
                && ((ip.is_some() && {
                    let prev = &v[ip.unwrap()];
                    (prev.is_char() && prev != &Token::Char(ReservedChar::CloseParenthese))
                        || prev.is_a_media()
                        || prev.is_a_license()
                }) || (i < v.len() - 1 && v[i + 1].is_char()))
            {
                retain = false;
            }
        } else if v[i].is_comment() {
            retain = false;
        }
        if retain {
            ip = Some(i);
        }
        b.push(retain);
        i += 1;
    }
    assert_eq!(v.len(), b.len());
    let mut b = b.into_iter();
    v.retain(|_| b.next().unwrap());
    v
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(super) struct Tokens<'a>(Vec<Token<'a>>);

impl Tokens<'_> {
    pub(super) fn write<W: std::io::Write>(self, mut w: W) -> std::io::Result<()> {
        for token in self.0.iter() {
            write!(w, "{}", token)?;
        }
        Ok(())
    }
}

impl fmt::Display for Tokens<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for token in self.0.iter() {
            write!(f, "{}", token)?;
        }
        Ok(())
    }
}

#[test]
fn css_basic() {
    let s = r#"
/*! just some license */
.foo > #bar p:hover {
    color: blue;
    background: "blue";
}

/* a comment! */
@media screen and (max-width: 640px) {
    .block:hover {
        display:    block;
    }
}"#;
    let expected = vec![
        Token::License(" just some license "),
        Token::SelectorElement(SelectorElement::Class("foo")),
        Token::Char(ReservedChar::SuperiorThan),
        Token::SelectorElement(SelectorElement::Id("bar")),
        Token::Char(ReservedChar::Space),
        Token::SelectorElement(SelectorElement::Tag("p")),
        Token::SelectorElement(SelectorElement::PseudoClass("hover")),
        Token::Char(ReservedChar::OpenCurlyBrace),
        Token::Other("color"),
        Token::Char(ReservedChar::Colon),
        Token::Other("blue"),
        Token::Char(ReservedChar::SemiColon),
        Token::Other("background"),
        Token::Char(ReservedChar::Colon),
        Token::String("\"blue\""),
        Token::Char(ReservedChar::SemiColon),
        Token::Char(ReservedChar::CloseCurlyBrace),
        Token::SelectorElement(SelectorElement::Media("media")),
        Token::Other("screen"),
        Token::Char(ReservedChar::Space),
        Token::Other("and"),
        Token::Char(ReservedChar::Space),
        Token::Char(ReservedChar::OpenParenthese),
        Token::Other("max-width"),
        Token::Char(ReservedChar::Colon),
        Token::Other("640px"),
        Token::Char(ReservedChar::CloseParenthese),
        Token::Char(ReservedChar::OpenCurlyBrace),
        Token::SelectorElement(SelectorElement::Class("block")),
        Token::SelectorElement(SelectorElement::PseudoClass("hover")),
        Token::Char(ReservedChar::OpenCurlyBrace),
        Token::Other("display"),
        Token::Char(ReservedChar::Colon),
        Token::Other("block"),
        Token::Char(ReservedChar::SemiColon),
        Token::Char(ReservedChar::CloseCurlyBrace),
        Token::Char(ReservedChar::CloseCurlyBrace),
    ];
    assert_eq!(tokenize(s), Ok(Tokens(expected)));
}

#[test]
fn elem_selector() {
    let s = r#"
/** just some license */
a[href*="example"] {
    background: yellow;
}
a[href$=".org"] {
  font-style: italic;
}
span[lang|="zh"] {
    color: red;
}
a[href^="/"] {
    background-color: gold;
}
div[value~="test"] {
    border-width: 1px;
}
span[lang="pt"] {
    font-size: 12em; /* I love big fonts */
}
"#;
    let expected = vec![
        Token::License(" just some license "),
        Token::SelectorElement(SelectorElement::Tag("a")),
        Token::Char(ReservedChar::OpenBracket),
        Token::Other("href"),
        Token::SelectorOperator(SelectorOperator::Contains),
        Token::String("\"example\""),
        Token::Char(ReservedChar::CloseBracket),
        Token::Char(ReservedChar::OpenCurlyBrace),
        Token::Other("background"),
        Token::Char(ReservedChar::Colon),
        Token::Other("yellow"),
        Token::Char(ReservedChar::SemiColon),
        Token::Char(ReservedChar::CloseCurlyBrace),
        Token::SelectorElement(SelectorElement::Tag("a")),
        Token::Char(ReservedChar::OpenBracket),
        Token::Other("href"),
        Token::SelectorOperator(SelectorOperator::EndsWith),
        Token::String("\".org\""),
        Token::Char(ReservedChar::CloseBracket),
        Token::Char(ReservedChar::OpenCurlyBrace),
        Token::Other("font-style"),
        Token::Char(ReservedChar::Colon),
        Token::Other("italic"),
        Token::Char(ReservedChar::SemiColon),
        Token::Char(ReservedChar::CloseCurlyBrace),
        Token::SelectorElement(SelectorElement::Tag("span")),
        Token::Char(ReservedChar::OpenBracket),
        Token::Other("lang"),
        Token::SelectorOperator(SelectorOperator::EqualsOrStartsWithFollowedByDash),
        Token::String("\"zh\""),
        Token::Char(ReservedChar::CloseBracket),
        Token::Char(ReservedChar::OpenCurlyBrace),
        Token::Other("color"),
        Token::Char(ReservedChar::Colon),
        Token::Other("red"),
        Token::Char(ReservedChar::SemiColon),
        Token::Char(ReservedChar::CloseCurlyBrace),
        Token::SelectorElement(SelectorElement::Tag("a")),
        Token::Char(ReservedChar::OpenBracket),
        Token::Other("href"),
        Token::SelectorOperator(SelectorOperator::FirstStartsWith),
        Token::String("\"/\""),
        Token::Char(ReservedChar::CloseBracket),
        Token::Char(ReservedChar::OpenCurlyBrace),
        Token::Other("background-color"),
        Token::Char(ReservedChar::Colon),
        Token::Other("gold"),
        Token::Char(ReservedChar::SemiColon),
        Token::Char(ReservedChar::CloseCurlyBrace),
        Token::SelectorElement(SelectorElement::Tag("div")),
        Token::Char(ReservedChar::OpenBracket),
        Token::Other("value"),
        Token::SelectorOperator(SelectorOperator::OneAttributeEquals),
        Token::String("\"test\""),
        Token::Char(ReservedChar::CloseBracket),
        Token::Char(ReservedChar::OpenCurlyBrace),
        Token::Other("border-width"),
        Token::Char(ReservedChar::Colon),
        Token::Other("1px"),
        Token::Char(ReservedChar::SemiColon),
        Token::Char(ReservedChar::CloseCurlyBrace),
        Token::SelectorElement(SelectorElement::Tag("span")),
        Token::Char(ReservedChar::OpenBracket),
        Token::Other("lang"),
        Token::Char(ReservedChar::EqualSign),
        Token::String("\"pt\""),
        Token::Char(ReservedChar::CloseBracket),
        Token::Char(ReservedChar::OpenCurlyBrace),
        Token::Other("font-size"),
        Token::Char(ReservedChar::Colon),
        Token::Other("12em"),
        Token::Char(ReservedChar::SemiColon),
        Token::Char(ReservedChar::CloseCurlyBrace),
    ];
    assert_eq!(tokenize(s), Ok(Tokens(expected)));
}

#[test]
fn check_media() {
    let s = "@media (max-width: 700px) { color: red; }";

    let expected = vec![
        Token::SelectorElement(SelectorElement::Media("media")),
        Token::Char(ReservedChar::OpenParenthese),
        Token::Other("max-width"),
        Token::Char(ReservedChar::Colon),
        Token::Other("700px"),
        Token::Char(ReservedChar::CloseParenthese),
        Token::Char(ReservedChar::OpenCurlyBrace),
        Token::SelectorElement(SelectorElement::Tag("color")),
        Token::Char(ReservedChar::Colon),
        Token::Other("red"),
        Token::Char(ReservedChar::SemiColon),
        Token::Char(ReservedChar::CloseCurlyBrace),
    ];

    assert_eq!(tokenize(s), Ok(Tokens(expected)));
}

#[test]
fn check_supports() {
    let s = "@supports not (display: grid) { div { float: right; } }";

    let expected = vec![
        Token::SelectorElement(SelectorElement::Media("supports")),
        Token::Other("not"),
        Token::Char(ReservedChar::Space),
        Token::Char(ReservedChar::OpenParenthese),
        Token::Other("display"),
        Token::Char(ReservedChar::Colon),
        Token::Other("grid"),
        Token::Char(ReservedChar::CloseParenthese),
        Token::Char(ReservedChar::OpenCurlyBrace),
        Token::SelectorElement(SelectorElement::Tag("div")),
        Token::Char(ReservedChar::OpenCurlyBrace),
        Token::Other("float"),
        Token::Char(ReservedChar::Colon),
        Token::Other("right"),
        Token::Char(ReservedChar::SemiColon),
        Token::Char(ReservedChar::CloseCurlyBrace),
        Token::Char(ReservedChar::CloseCurlyBrace),
    ];

    assert_eq!(tokenize(s), Ok(Tokens(expected)));
}

#[test]
fn check_calc() {
    let s = ".foo { width: calc(100% - 34px); }";

    let expected = vec![
        Token::SelectorElement(SelectorElement::Class("foo")),
        Token::Char(ReservedChar::OpenCurlyBrace),
        Token::Other("width"),
        Token::Char(ReservedChar::Colon),
        Token::Other("calc"),
        Token::Char(ReservedChar::OpenParenthese),
        Token::Other("100%"),
        Token::Char(ReservedChar::Space),
        Token::Other("-"),
        Token::Char(ReservedChar::Space),
        Token::Other("34px"),
        Token::Char(ReservedChar::CloseParenthese),
        Token::Char(ReservedChar::SemiColon),
        Token::Char(ReservedChar::CloseCurlyBrace),
    ];
    assert_eq!(tokenize(s), Ok(Tokens(expected)));
}

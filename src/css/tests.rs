// Take a look at the license at the top of the repository in the LICENSE file.

use crate::css::minify;

/*enum Element {
    /// Rule starting with `@`:
    ///
    /// * charset
    /// * font-face
    /// * import
    /// * keyframes
    /// * media
    AtRule(AtRule<'a>),
    /// Any "normal" CSS rule block.
    ///
    /// Contains the selector(s) and its content.
    ElementRule(Vec<&'a str>, Vec<Property<'a>>),
}

fn get_property<'a>(source: &'a str, iterator: &mut Peekable<CharIndices>,
                    start_pos: &mut usize) -> Option<Property<'a>> {
    let mut end_pos = None;
    // First we get the property name.
    while let Some((pos, c)) = iterator.next() {
        if let Ok(c) = ReservedChar::try_from(c) {
            if c.is_useless() {
                continue
            } else if c == ReservedChar::OpenCurlyBrace {
                return None
            } else if c == ReservedChar::Colon {
                end_pos = Some(pos);
                break
            } else { // Invalid character.
                return None;
            }
        } else if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '-' {
            // everything's fine for now...
        } else {
            return None; // invalid character
        }
    }
    if end_pos.is_none() || end_pos == Some(*start_pos + 1) {
        return None;
    }
    while let Some((pos, c)) = iterator.next() {
        if let Ok(c) = ReservedChar::try_from(c) {
            if c == ReservedChar::DoubleQuote || c == ReservedChar::Quote {
                get_string(source, iterator, &mut 0, c)
            } else if c == ReservedChar::SemiColon {
                // we reached the end!
                let end_pos = end_pos.unwrap();
                *start_pos = pos;
                return Property {
                    name: &source[start_pos..end_pos],
                    value: &source[end_pos..pos],
                }
            }
        }
    }
    None
}

enum Selector<'a> {
    Tag(&'a str),
    /// '.'
    Class(&'a str),
    /// '#'
    Id(&'a str),
    /// '<', '>', '(', ')', '+', ' ', '[', ']'
    Operator(char),
}

struct ElementRule<'a> {
    selectors: Vec<Selector<'a>>,
    properties: Vec<Property<'a>>,
}

fn get_element_rule<'a>(source: &'a str, iterator: &mut Peekable<CharIndices>,
                        c: char) -> Option<Token<'a>> {
    let mut selectors = Vec::with_capacity(2);

    while let Some(s) = get_next_selector(source, iterator, c) {
        if !selectors.is_empty() || !s.empty_operator() {
        }
        selectors.push(s);
    }
}

fn get_media_query<'a>(source: &'a str, iterator: &mut Peekable<CharIndices>,
                       start_pos: &mut usize) -> Option<Token<'a>> {
    while let Some((pos, c)) = iterator.next() {
        if c == '{' {
            ;
        }
    }
    None // An error occurred, sad life...
}


fn get_properties<'a>(source: &'a str, iterator: &mut Peekable<CharIndices>,
                      start_pos: &mut usize) -> Vec<Property> {
    let mut ret = Vec::with_capacity(2);
    while let Some(property) = get_property(source, iterator, start_pos) {
        ret.push(property);
    }
    ret
}

pub struct Property<'a> {
    name: &'a str,
    value: &'a str,
}

pub enum AtRule<'a> {
    /// Contains the charset. Supposed to be the first rule in the style sheet and be present
    /// only once.
    Charset(&'a str),
    /// font-face rule.
    FontFace(Vec<Property<'a>>),
    /// Contains the import.
    Import(&'a str),
    /// Contains the rule and the block.
    Keyframes(&'a str, Tokens<'a>),
    /// Contains the rules and the block.
    Media(Vec<&'a str>, Tokens<'a>),
}

impl fmt::Display for AtRule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "@{}", &match *self {
            AtRule::Charset(c) => format!("charset {};", c),
            AtRule::FontFace(t) => format!("font-face {{{}}};", t),
            AtRule::Import(i) => format!("import {};", i),
            AtRule::Keyframes(r, t) => format!("keyframes {} {{{}}}", r, t),
            AtRule::Media(r, t) => format!("media {} {{{}}}", r.join(" ").collect::<String>(), t),
        })
    }
}*/

#[test]
fn check_minification() {
    let s = r#"
/** Baguette! */
.b > p + div:hover {
    background: #fff;
}

a[target = "_blank"] {
    /* I like weird tests. */
    border: 1px solid yellow   ;
}
"#;
    let expected = r#"/*! Baguette! */
.b>p+div:hover{background:#fff;}a[target="_blank"]{border:1px solid yellow;}"#;
    assert_eq!(minify(s).expect("minify failed").to_string(), expected);
}

#[test]
fn check_minification2() {
    let s = r#"
h2, h3:not(.impl):not(.method):not(.type) {
    background-color: #0a042f !important;
}

:target { background: #494a3d; }

.table-display tr td:first-child {
    float: right;
}

/* just some
 * long
 *
 * very
 * long
 * comment :)
 */
@media (max-width: 700px) {
    .theme-picker {
        left: 10px;
        top: 54px;
        z-index: 1;
        background-color: rgba(0,  0  ,  0  ,  0);
        font: 15px "SFMono-Regular", Consolas, "Liberation Mono", Menlo, Courier, monospace;
    }
}"#;
    let expected = "h2,h3:not(.impl):not(.method):not(.type){background-color:#0a042f !important;}\
                    :target{background:#494a3d;}.table-display tr td:first-child{float:right;}\
                    @media (max-width:700px){.theme-picker{left:10px;top:54px;z-index:1;\
                    background-color:rgba(0,0,0,0);font:15px \"SFMono-Regular\",Consolas,\
                    \"Liberation Mono\",Menlo,Courier,monospace;}}";
    assert_eq!(minify(s).expect("minify failed").to_string(), expected);
}

#[test]
fn check_calc() {
    let s = ".foo { width: calc(100% - 34px); }";
    let expected = ".foo{width:calc(100% - 34px);}";
    assert_eq!(minify(s).expect("minify failed").to_string(), expected);
}

#[test]
fn check_container() {
    let s = "@container rustdoc (min-width: 1250px) { .foo { width: 100px; } }";
    let expected = "@container rustdoc (min-width:1250px){.foo{width:100px;}}";
    assert_eq!(minify(s).expect("minify failed").to_string(), expected);
}

#[test]
fn check_spaces() {
    let s = ".line-numbers .line-highlighted { color: #0a042f !important; }";
    let expected = ".line-numbers .line-highlighted{color:#0a042f !important;}";
    assert_eq!(minify(s).expect("minify failed").to_string(), expected);
}

#[test]
fn check_space_after_paren() {
    let s = ".docblock:not(.type-decl) a:not(.srclink) {}";
    let expected = ".docblock:not(.type-decl) a:not(.srclink){}";
    assert_eq!(minify(s).expect("minify failed").to_string(), expected);
}

#[test]
fn check_space_after_and() {
    let s = "@media only screen and (max-width : 600px) {}";
    let expected = "@media only screen and (max-width:600px){}";
    assert_eq!(minify(s).expect("minify failed").to_string(), expected);
}

#[test]
fn check_space_after_or_not() {
    let s = "@supports not ((text-align-last: justify) or (-moz-text-align-last: justify)) {}";
    let expected = "@supports not ((text-align-last:justify) or (-moz-text-align-last:justify)){}";
    assert_eq!(minify(s).expect("minify failed").to_string(), expected);
}

#[test]
fn check_space_after_brackets() {
    let s = "#main[data-behavior = \"1\"] {}";
    let expected = "#main[data-behavior=\"1\"]{}";
    assert_eq!(minify(s).expect("minify failed").to_string(), expected);

    let s = "#main[data-behavior = \"1\"] .aclass";
    let expected = "#main[data-behavior=\"1\"] .aclass";
    assert_eq!(minify(s).expect("minify failed").to_string(), expected);

    let s = "#main[data-behavior = \"1\"] ul.aclass";
    let expected = "#main[data-behavior=\"1\"] ul.aclass";
    assert_eq!(minify(s).expect("minify failed").to_string(), expected);
}

#[test]
fn check_whitespaces_in_calc() {
    let s = ".foo { width: calc(130px     + 10%); }";
    let expected = ".foo{width:calc(130px + 10%);}";
    assert_eq!(minify(s).expect("minify failed").to_string(), expected);

    let s = ".foo { width: calc(130px + (45% - 10% +   (12   *   2px))); }";
    let expected = ".foo{width:calc(130px + (45% - 10% + (12 * 2px)));}";
    assert_eq!(minify(s).expect("minify failed").to_string(), expected);
}

#[test]
fn check_weird_comments() {
    let s = ".test1 {
    font-weight: 30em;
}/**/
.test2 {
    font-weight: 30em;
}/**/
.test3 {
    font-weight: 30em;
}/**/";
    let expected = ".test1{font-weight:30em;}.test2{font-weight:30em;}.test3{font-weight:30em;}";
    assert_eq!(minify(s).expect("minify failed").to_string(), expected);
}

#[test]
fn check_slash_slash() {
    let s = "body {
    background-image: url(data:image/webp;base64,c//S4KP//ZZ/19Uj/UA==);
}";
    let expected = "body{background-image:url(data:image/webp;base64,c//S4KP//ZZ/19Uj/UA==);}";
    assert_eq!(minify(s).expect("minify failed").to_string(), expected);
}

#[test]
fn check_escaped_characters() {
    let s = r#".before\:prose-headings\:content-\[\'\#\'\] :is(:where(h1,h2,h3,h4,h5,h6,th):not(:where([class~="not-prose"] *)))::before{
  --en-content: '#';
  content: var(--en-content);
}"#;
    let expected = r#".before\:prose-headings\:content-\[\'\#\'\] :is(:where(h1,h2,h3,h4,h5,h6,th):not(:where([class~="not-prose"] *)))::before{--en-content:'#';content:var(--en-content);}"#;
    assert_eq!(minify(s).expect("minify failed").to_string(), expected);
}

#[test]
fn issue_80() {
    assert_eq!(
        minify("@import 'i';t{x: #fff;}").unwrap().to_string(),
        "@import 'i';t{x:#fff;}",
    );
}

#[test]
fn check_attribute() {
    assert_eq!(minify("x [y] {}").unwrap().to_string(), "x [y]{}",);
}

#[test]
fn check_unicode_edge_cases() {
    let edge_cases = [";-/**\u{651}p", "\\\u{59c}"];
    for edge_case in edge_cases {
        let _ = minify(edge_case);
    }
}

#[test]
fn check_weird_input() {
    assert!(minify(r##""}"##).is_err());
    assert!(minify(r##"/*}"##).is_err());
    assert_eq!(minify(".").unwrap().to_string(), ".");
    assert_eq!(minify("//**/*").unwrap().to_string(), "/ *");
    assert_eq!(minify("1/**/H").unwrap().to_string(), "1 H");
    assert_eq!(minify("*/**/=").unwrap().to_string(), "* =");
    assert_eq!(minify(r#"\-1"#).unwrap().to_string(), r#"\-1"#);
}

#[test]
fn check_space_after_star() {
    assert_eq!(minify("* .original").unwrap().to_string(), "* .original");
    assert_eq!(
        minify(".a * .original").unwrap().to_string(),
        ".a * .original"
    );
}

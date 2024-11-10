// Take a look at the license at the top of the repository in the LICENSE file.

use std::{fmt, io};

mod token;

/// Minifies a given CSS source code.
///
/// # Example
///
/// ```rust
/// use minifier::css::minify;
///
/// let css = r#"
///     .foo > p {
///         color: red;
///     }"#.into();
/// let css_minified = minify(css).expect("minification failed");
/// assert_eq!(&css_minified.to_string(), ".foo>p{color:red;}");
/// ```
pub fn minify(content: &str) -> Result<Minified<'_>, &'static str> {
    token::tokenize(content).map(Minified)
}

pub struct Minified<'a>(token::Tokens<'a>);

impl Minified<'_> {
    pub fn write<W: io::Write>(self, w: W) -> io::Result<()> {
        self.0.write(w)
    }
}

impl fmt::Display for Minified<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[cfg(test)]
mod tests;

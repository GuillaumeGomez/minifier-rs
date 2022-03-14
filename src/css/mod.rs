// Take a look at the license at the top of the repository in the LICENSE file.

use std::{fmt, io};

mod token;

/// Minifies a given CSS source code.
///
/// # Example
///
/// ```rust
/// extern crate minifier;
/// use minifier::css::minify;
///
/// fn main() {
///     let css = r#"
///         .foo > p {
///             color: red;
///         }"#.into();
///     let css_minified = minify(css);
/// }
/// ```
pub fn minify<'a>(content: &'a str) -> Result<Minified<'a>, &'static str> {
    token::tokenize(content).map(Minified)
}

pub struct Minified<'a>(token::Tokens<'a>);

impl<'a> Minified<'a> {
    pub fn write<W: io::Write>(self, w: W) -> io::Result<()> {
        self.0.write(w)
    }
}

impl<'a> fmt::Display for Minified<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[cfg(test)]
mod tests;

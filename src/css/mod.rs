// Take a look at the license at the top of the repository in the LICENSE file.

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
pub fn minify<'a>(content: &'a str) -> Result<String, &'static str> {
    token::tokenize(content).map(|t| format!("{}", t))
}

#[cfg(test)]
mod tests;

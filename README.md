[![Build Status](https://api.travis-ci.org/GuillaumeGomez/minifier-rs.png?branch=master)](https://travis-ci.org/GuillaumeGomez/minifier-rs) [![Build status](https://ci.appveyor.com/api/projects/status/5bj86vqsah7927tc?svg=true)](https://ci.appveyor.com/project/GuillaumeGomez/minifier-rs)

# minifier-rs

Minifier tool/lib for JS/CSS/JSON files.

This crate provides both a library and binary, depending on your needs.

## Usage

To use the binary, just run like this:

```
> cargo run test.js
```

To use the library, add it into your `Cargo.toml` file like this:

```toml
[dependencies]
minifier = "^0.0.1"
```

Then import it into your code like this:

```rust
extern crate minifier;
```

## WARNING!!

Please be aware that this is still at a very early stage of development so you shouldn't rely on it too much!

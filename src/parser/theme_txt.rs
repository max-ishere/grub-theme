//! Everything for parsing GRUB's theme.txt file.
//!
//! To parse the file, call [`Document::parse`]. The document contains a list of
//! statements, each is either a [`Component`] or a [`GlobalProperty`].
//!
//! ```rust
//! # use theme_parser::parser::theme_txt::*;
//! let theme_txt = r#"# your average theme file
//!
//! terminal-font: "Monospace"
//!
//! + label {
//!     text = Hello
//! }
//! "#;
//!
//! let doc = Document::parse(theme_txt).unwrap().1;
//!
//! assert_eq!(doc, Document(vec![
//!     Statement::GlobalProperty(GlobalProperty {
//!         name: "terminal-font".to_string(),
//!         value: "Monospace".to_string(),
//!     }),
//!     Statement::Component(Component {
//!         name: "label".to_string(),
//!         properties: vec![ComponentProperty {
//!             name: "text".to_string(),
//!             value: "Hello".to_string(),
//!         }],
//!     }),
//! ]));
//! ```

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while, take_while1},
    character::complete::{char, newline, not_line_ending},
    combinator::{opt, recognize, value},
    multi::many0,
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    IResult, InputLength,
};

/// Represents the entire theme.txt file.
#[derive(Debug, PartialEq, Eq)]
pub struct Document(pub Vec<Statement>);

/// A single statement in the document.
#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    GlobalProperty(GlobalProperty),
    Component(Component),
}

/// A key-value pair, separated with `:`.
#[derive(Debug, PartialEq, Eq)]
pub struct GlobalProperty {
    pub name: String,
    pub value: String,
}

/// A UI component defined as `+ name { `[`Vec<ComponentProperty>`]` }`.
#[derive(Debug, PartialEq, Eq)]
pub struct Component {
    pub name: String,
    pub properties: Vec<ComponentProperty>,
}

/// A key-value pair, separated with `=` that modifies how a [`Component`] looks.
#[derive(Debug, PartialEq, Eq)]
pub struct ComponentProperty {
    pub name: String,
    pub value: String,
}

impl Document {
    pub fn parse(input: &str) -> IResult<&str, Self> {
        let (input, list) = many0(Statement::parse)(input)?;

        Ok((input, Self(list)))
    }
}

impl Statement {
    pub fn parse(input: &str) -> IResult<&str, Self> {
        let (cleaned, ()) = preceded_empty(input)?;
        alt((
            |input| {
                let (input, gp) = GlobalProperty::parse(input)?;
                Ok((input, Self::GlobalProperty(gp)))
            },
            |input| {
                let (input, c) = Component::parse(input)?;
                Ok((input, Self::Component(c)))
            },
        ))(cleaned)
    }
}

impl GlobalProperty {
    fn parse(input: &str) -> IResult<&str, Self> {
        let (input, (name, value)) = separated_pair(
            identifier,
            delimited(whitespace0, tag(":"), whitespace0),
            expression,
        )(input)?;

        Ok((
            input,
            Self {
                name: name.to_string(),
                value: value.to_string(),
            },
        ))
    }
}

impl Component {
    fn parse(input: &str) -> IResult<&str, Self> {
        let (input, ()) = preceded_empty(input)?;
        let (input, (name, props)) = preceded(
            terminated(tag("+"), whitespace0),
            pair(
                identifier,
                delimited(
                    preceded(whitespace0, tag("{")),
                    many0(component_property),
                    preceded(preceded_empty, tag("}")),
                ),
            ),
        )(input)?;

        Ok((
            input,
            Self {
                name: name.to_string(),
                properties: props
                    .iter()
                    .map(|(name, value)| ComponentProperty {
                        name: name.to_string(),
                        value: value.to_string(),
                    })
                    .collect(),
            },
        ))
    }
}

/// Parses the `key = value` component properties.
fn component_property(input: &str) -> IResult<&str, (&str, &str)> {
    let (input, ()) = preceded_empty(input)?;

    separated_pair(
        identifier,
        delimited(whitespace0, char('='), whitespace0),
        expression,
    )(input)
}

/// Parses a identifier such as component proprety name or a global property name
fn identifier(input: &str) -> IResult<&str, &str> {
    preceded(
        whitespace0,
        take_while1(
            |c: char| c.is_alphanumeric() || c == '-' || c == '_', // GRUB allows _ in source code
        ),
    )(input)
}

/// Returns either a `quoted string`, a `(tuple with arbitrary content)` or a single `word`.
fn expression(input: &str) -> IResult<&str, &str> {
    let (input, _) = whitespace0(input)?;

    match input.chars().next() {
        Some('"') => delimited(tag("\""), take_until("\""), tag("\""))(input),
        Some('(') => recognize(delimited(tag("("), take_until(")"), tag(")")))(input),
        Some(_) => take_while(|c: char| !c.is_whitespace())(input),
        None => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Eof,
        ))),
    }
}

/// Returned input has no preceding spaces, newlines or comments.
///
/// ```ignore
/// let with_comments = r" # some comments
///
///  #newlines
/// oh, look, not a comment";
///
/// let (stripped, ()) = preceded_empty(with_comments).unwrap();
///
/// assert_eq!(stripped, "oh, look, not a comment");
/// ```
fn preceded_empty(input: &str) -> IResult<&str, ()> {
    value((), many0(alt((comment, whitespace1))))(input)
}

fn whitespace1(input: &str) -> IResult<&str, ()> {
    let (i1, ()) = whitespace0(input)?;

    if i1.input_len() == input.input_len() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Space,
        )));
    }

    Ok((i1, ()))
}

fn whitespace0(input: &str) -> IResult<&str, ()> {
    value((), take_while(char::is_whitespace))(input)
}

fn comment(input: &str) -> IResult<&str, ()> {
    value((), delimited(char('#'), not_line_ending, opt(newline)))(input)
}

#[cfg(test)]
mod tests {
    #![allow(non_snake_case)] // Tests of class methods are inside same-name modules

    use nom::IResult;

    #[test_case("identifier" => Ok(("", "identifier")); "alha")]
    #[test_case("two_words" => Ok(("", "two_words")); "with underscore")]
    #[test_case("two-words" => Ok(("", "two-words")); "with dash")]
    #[test_case("123-alpha_test" => Ok(("", "123-alpha_test")); "all allowed character types")]
    fn identifier(input: &str) -> IResult<&str, &str> {
        super::identifier(input)
    }

    #[test_case(r#""quoted string""# => Ok(("", "quoted string")); "quoted")]
    #[test_case("(tuple , thing)" => Ok(("", "(tuple , thing)")); "tuple")]
    #[test_case("().." => Ok(("..", "()")); "tuple with trailing characters")]
    #[test_case("word" => Ok(("", "word")); "word")]
    fn expression(input: &str) -> IResult<&str, &str> {
        super::expression(input)
    }

    #[test_case("# doc\nkey = value # doc2" => Ok((" # doc2", ("key", "value"))); "key eq value with docs around")]
    fn component_property(input: &str) -> IResult<&str, (&str, &str)> {
        super::component_property(input)
    }

    #[test_case("" => Ok(("", ())); "empty input")]
    #[test_case("\n\n\n   \t\t \n" => Ok(("", ())); "some spaces")]
    #[test_case("  a" => Ok(("a", ())); "space, then any character")]
    #[test_case("# comment" => Ok(("", ())); "single comment")]
    #[test_case("#" => Ok(("", ())); "just the hash symbol")]
    #[test_case(r"# some comment

    # and empty lines
    
    finally stuff" => Ok(("finally stuff", ())); "whitespace and comments")]
    #[test_case("should not change anything\n# dont touch this later comment" 
    => Ok(("should not change anything\n# dont touch this later comment", ()));
     "unchanged")]
    fn preceded_empty(input: &str) -> IResult<&str, ()> {
        super::preceded_empty(input)
    }

    #[test_case("# some comment
finally stuff" => Ok(("finally stuff", ())); "a comment with content on next line")]
    pub fn comment(input: &str) -> IResult<&str, ()> {
        super::comment(input)
    }

    mod Statement {
        use super::super::{Component, ComponentProperty, GlobalProperty, Statement};

        use nom::IResult;

        #[test_case(r"# docs
        name: value" => Ok(("", Statement::GlobalProperty(GlobalProperty{
            name: "name".to_string(),
            value: "value".to_string()
        }))); "global property with doc comment above")]
        #[test_case(r"# docs
        + label { #
            # doc
            prop = value # doc
        }" => Ok(("", Statement::Component(Component{
            name: "label".to_string(),
            properties: vec![ComponentProperty{
                name: "prop".to_string(),
                value: "value".to_string(),
            }]
        }))); "component with many comments")]
        fn parse(input: &str) -> IResult<&str, Statement> {
            Statement::parse(input)
        }
    }
}

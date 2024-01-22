use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until, take_while, take_while1},
    character::complete::char,
    combinator::{recognize, value},
    error::{Error, ErrorKind, ParseError},
    multi::many0,
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    Err, IResult, Parser,
};

#[cfg(test)]
#[macro_use]
extern crate test_case;

#[macro_use]
extern crate log;

#[derive(Debug, PartialEq, Eq)]
pub struct Document(pub Vec<Statement>);

impl Document {
    /// Creates self from a text definition of the component
    /// ```rust
    /// # use theme_parser::*;
    /// #
    /// let text = r#"
    /// + label { thing = foo }
    ///
    /// + label {
    ///   foo = thing
    /// }
    /// "#;
    ///
    /// let comp = Document::parse(text).unwrap().1;
    ///
    /// println!("{comp:?}");
    /// panic!();
    /// ```
    pub fn parse(input: &str) -> IResult<&str, Self> {
        let (input, list) = many0(Statement::parse)(input)?;

        Ok((input, Self(list)))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    GlobalProperty(GlobalProperty),
    Component(Component),
}

impl Statement {
    pub fn parse(input: &str) -> IResult<&str, Self> {
        either(vec![
            |input| {
                let (input, gp) = GlobalProperty::parse(input)?;
                debug!("Parsing global property");
                Ok((input, Self::GlobalProperty(gp)))
            },
            |input| {
                let (input, c) = Component::parse(input)?;
                debug!("Parsing component");
                Ok((input, Self::Component(c)))
            },
        ])(input)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct GlobalProperty {
    pub name: String,
    pub value: String,
}

impl GlobalProperty {
    pub fn parse(input: &str) -> IResult<&str, Self> {
        let (input, (name, value)) = global_property(input)?;
        Ok((
            input,
            Self {
                name: name.to_string(),
                value: value.to_string(),
            },
        ))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Component {
    pub name: String,
    pub properties: Vec<ComponentProperty>,
}

impl Component {
    /// Creates self from a text definition of the component
    /// ```rust
    /// # use theme_parser::ComponentProperty;
    /// # use theme_parser::Component;
    /// #
    /// let text = r#"
    /// + label {
    ///   text = "Hello world"
    /// }"#;
    ///
    /// let comp = Component::parse(text).unwrap().1;
    ///
    /// assert_eq!(comp, Component {
    ///     name: "label".to_string(),
    ///     properties: vec![
    ///         ComponentProperty{
    ///             name: "text".to_string(),
    ///             value: "Hello world".to_string()
    ///     }],
    /// });
    /// ```
    pub fn parse(input: &str) -> IResult<&str, Self> {
        let (input, (name, values)) = component(input)?;
        Ok((
            input,
            Self {
                name: name.to_string(),
                properties: values
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

#[derive(Debug, PartialEq, Eq)]
pub struct ComponentProperty {
    pub name: String,
    pub value: String,
}

pub fn global_property(input: &str) -> IResult<&str, (&str, &str)> {
    let (input, ()) = preceding_empty(input)?;

    separated_pair(
        identifier,
        delimited(whitespace0, tag(":"), whitespace0),
        expression,
    )(input)
}

type ParsedComponent<'a> = (&'a str, Vec<(&'a str, &'a str)>);

pub fn component(input: &str) -> IResult<&str, ParsedComponent> {
    let (input, ()) = preceding_empty(input)?;

    preceded(
        terminated(tag("+"), whitespace0),
        pair(identifier, component_property_list),
    )(input)
}

pub fn comment(input: &str) -> IResult<&str, ()> {
    value(
        (), // Output is thrown away.
        preceded(whitespace0, pair(char('#'), is_not("\n"))),
    )(input)
}

fn component_property(input: &str) -> IResult<&str, (&str, &str)> {
    separated_pair(
        identifier,
        delimited(whitespace0, tag("="), whitespace0),
        expression,
    )(input)
}

fn component_property_list(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
    delimited(
        preceded(whitespace0, tag("{")),
        many0(preceded(preceding_empty, component_property)),
        preceded(preceding_empty, tag("}")),
    )(input)
}

/// Returns an identifier composed of alphanumeric characters and `-`, `_`.
fn identifier(input: &str) -> IResult<&str, &str> {
    preceded(
        whitespace0,
        take_while1(
            |c: char| c.is_alphanumeric() || c == '-' || c == '_', // GRUB allows _ in source code
        ),
    )(input)
}

/// Returns either a `"quoted string"`, a `(tuple with arbitrary content)` or a single `word`.
/// There should be no spaces in the beginning of `input`.
fn expression(input: &str) -> IResult<&str, &str> {
    let (input, _) = whitespace0(input)?;

    match input.chars().next() {
        Some('"') => delimited(tag("\""), take_until("\""), tag("\""))(input),
        Some('(') => recognize(delimited(tag("("), take_until(")"), tag(")")))(input),
        Some(_) => take_while(|c: char| !c.is_whitespace())(input),
        None => Err(Err::Error(Error::new(input, ErrorKind::Eof))),
    }
}

/// Replacement for `space0` function from nom.
/// This function uses char::is_whitespace instead of just matching space and tabs.
fn whitespace0(input: &str) -> IResult<&str, ()> {
    value((), take_while(char::is_whitespace))(input)
}

fn preceding_empty(input: &str) -> IResult<&str, ()> {
    let res = value((), many0(either(vec![whitespace0, comment])))(input);
    let a = alt((whitespace0,));
    // If neither of parsers captured anything its ok
    if let Err(nom::Err::Error(Error {
        input: _,
        code: ErrorKind::Many0,
    })) = res
    {
        Ok((input, ()))
    } else {
        res
    }
}

pub fn either<I, O, E>(mut parsers: Vec<impl Parser<I, O, E>>) -> impl FnMut(I) -> IResult<I, O, E>
where
    I: Clone,
    E: ParseError<I>,
{
    move |input| {
        let mut parsers = parsers.iter_mut();

        let mut res = parsers
            .next()
            .expect("The list of parsers should never be empty.")
            .parse(input.clone());

        for p in parsers {
            if res.is_ok() {
                break;
            }

            res = p.parse(input.clone());
        }

        res
    }
}

#[cfg(test)]
mod tests {
    use nom::IResult;

    #[test_case("size: 30" => Ok(("", ("size", "30"))); "number value")]
    #[test_case("size: \"30\"" => Ok(("", ("size", "30"))); "quoted value")]
    #[test_case("position: center" => Ok(("", ("position", "center"))); "word value")]
    #[test_case("size: (10 10)" => Ok(("", ("size", "(10 10)"))); "tuple value")]
    #[test_case("size: trailing word" => Ok((" word", ("size", "trailing"))); "trailing word not captured")]
    fn global_property(input: &str) -> IResult<&str, (&str, &str)> {
        super::global_property(input)
    }

    #[test_case(
r#"+ label {
    size = 30
}"# => Ok(("", ("label", vec![
        ("size", "30")
    ]))); "simple label")]
    fn component(input: &str) -> IResult<&str, (&str, Vec<(&str, &str)>)> {
        super::component(input)
    }

    #[test_case("# test" => Ok(("", ())); "at end of document")]
    // Should preserve \n because all that should happen is removing the comment text
    #[test_case("# test\nsomething" => Ok(("\nsomething", ())); "has content later")]
    fn comment(input: &str) -> IResult<&str, ()> {
        super::comment(input)
    }

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

    #[test_case("prop = value" => Ok(("", ("prop", "value"))); "prop is value")]
    fn component_property(input: &str) -> IResult<&str, (&str, &str)> {
        super::component_property(input)
    }

    #[test_case("{ prop = value prop2 = value }" => Ok(("", vec![("prop", "value"), ("prop2", "value")])); "one liner")]
    #[test_case(r#"{
        prop = value
        prop2 = value
    }"# => Ok(("", vec![("prop", "value"), ("prop2", "value")])); "multi line")]
    fn component_property_list(input: &str) -> IResult<&str, Vec<(&str, &str)>> {
        super::component_property_list(input)
    }
}

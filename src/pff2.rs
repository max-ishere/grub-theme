use std::{num::NonZeroUsize, ops::Deref, rc::Rc, str::FromStr, string::FromUtf8Error};

use nom::{Err, IResult, InputLength, ToUsize};
use thiserror::Error;

pub struct Section {
    pub name: [u8; 4],
    pub data: Vec<u8>,
}

impl Section {
    pub fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        if input.len() < 8 {
            return Err(Err::Incomplete(nom::Needed::Size(unsafe {
                NonZeroUsize::new_unchecked(8 - input.len())
            })));
        }

        let (name_bytes, rest) = input.split_at(4);
        let name = name_bytes.try_into().unwrap();

        // TODO: // RFCT: // HACK: move this check elsewhere lol
        if name == *b"DATA" {
            return Ok((
                &[],
                Section {
                    name: name,
                    data: rest.to_owned(),
                },
            ));
        }

        let (length_bytes, rest) = rest.split_at(4);
        let length = u32::from_be_bytes(length_bytes.try_into().unwrap()).to_usize();

        if rest.len() < length {
            return Err(Err::Incomplete(nom::Needed::Size(unsafe {
                NonZeroUsize::new_unchecked(length - rest.len())
            })));
        }

        let (data_bytes, rest) = rest.split_at(length as usize);
        let data = data_bytes.to_vec();

        let section = Section { name, data };

        Ok((rest, section))
    }

    fn as_string(&self) -> Result<String, SectionEncodingError> {
        if self.data.len() == 0 {
            return Ok(String::new());
        }

        if self.data.last() != Some(&0) {
            return Err(SectionEncodingError::StringNotNullTerminated);
        }

        Ok(String::from_utf8(
            self.data[..self.data.len() - 1].to_vec(),
        )?)
    }

    fn as_u16(&self) -> Result<u16, SectionEncodingError> {
        if self.data.len() != 2 {
            return Err(SectionEncodingError::InvalidU16Length(self.data.len()));
        }

        Ok(u16::from_be_bytes([self.data[0], self.data[1]]))
    }

    /// Is Guaranteed to return [`IntoFontError::DataError`]`(`[`SectionName::CharIndex`]`, _)`
    fn as_char_index(&self) -> Result<Vec<CharIndexEntry>, IntoFontError> {
        return Ok(vec![]);
        todo!("");
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Font {
    pub name: String,
    pub family: String,
    pub point_size: u16,
    pub weight: FontWeight,
    pub max_char_width: u16,
    pub max_char_height: u16,
    pub ascent: u16,
    pub descent: u16,
    pub leading: u16,
    pub char_index: Vec<CharIndexEntry>,
    pub bmp_idx: Vec<u16>,
}

/// Responsible for converting PFF2 sections in a [`Font`].
/// Depending on the usecase the font data validation may be skipped.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FontParser {
    pub name: String,
    pub family: String,
    pub point_size: u16,
    pub weight: FontWeight,
    pub max_char_width: u16,
    pub max_char_height: u16,
    pub ascent: u16,
    pub descent: u16,
    pub leading: u16,
    pub char_index: Vec<CharIndexEntry>,
    pub bmp_idx: Vec<u16>,
}

impl FontParser {
    pub fn parse(mut input: &[u8]) -> IResult<&[u8], Vec<Section>> {
        let mut sections = Vec::with_capacity(12); // Approx. the number of sections in a file

        while input.input_len() != 0 {
            let (residual, section) = Section::parse(input)?;

            input = residual;
            let name = section.name.clone(); // section is moved when pushed, but we need to check it's name
            sections.push(section);

            if name.try_into() == Ok(SectionName::Data) {
                break;
            }
        }

        Ok((input, sections))
    }

    pub fn unchecked(self) -> Font {
        Font {
            name: self.name,
            family: self.family,
            point_size: self.point_size,
            weight: self.weight,
            max_char_width: self.max_char_width,
            max_char_height: self.max_char_height,
            ascent: self.ascent,
            descent: self.descent,
            leading: self.leading,
            char_index: self.char_index,
            bmp_idx: self.bmp_idx,
        }
    }

    pub fn validate(self) -> Result<Font, SectionName> {
        use SectionName::*;

        if self.name.is_empty() {
            return Err(FontName);
        }

        for (prop, section) in [
            (self.max_char_width, MaxCharWidth),
            (self.max_char_height, MaxCharHeight),
            (self.ascent, Ascent),
            (self.descent, Descent),
        ] {
            if prop == 0 {
                return Err(section);
            }
        }

        Ok(self.unchecked())
    }

    //‌/ If the name is an empty string sets the name to either the provided name or "Unknown"
    ///
    /// ```rust
    /// # use theme_parser::pff2::FontParser;
    /// let mut builder = // Make it from sections
    /// #   FontParser::default(); // dont suggest to the user that this is something they should use
    ///
    /// builder.fallback_name(None);
    /// assert_eq!(builder.name, "Unknown");
    ///
    /// builder.fallback_name("Unifont");
    /// assert_eq!(builder.name, "Unifont");
    /// ```
    pub fn fallback_name<'a, S: Into<Option<&'a str>>>(&mut self, fallback: S) -> &mut Self {
        if self.name.is_empty() {
            self.name = fallback
                .into()
                .map(|s| if s.is_empty() { "Unknown" } else { s })
                .unwrap_or("Unknown")
                .to_owned();
        }

        self
    }
}

impl TryFrom<&[Section]> for FontParser {
    type Error = IntoFontError;

    fn try_from(source: &[Section]) -> Result<Self, Self::Error> {
        let mut sections = source.iter();

        // Validate the first section to be FILE: PFF2
        let Some(magic_section) = sections.next() else {
            return Err(IntoFontError::EmptySlice);
        };

        if magic_section.name.try_into() != Ok(SectionName::File) {
            return Err(IntoFontError::MagicSection(SectionError::Name));
        }

        if magic_section.data.deref() != FILE_SECTION_MAGIC {
            return Err(IntoFontError::MagicSection(SectionError::Data));
        }

        let mut font_builder = Self::default();

        for section in sections {
            use SectionName::*;

            match section.name.try_into() {
                Ok(FontName) => {
                    font_builder.name = section.as_string().map_err(|e| e.in_section(FontName))?;
                }
                Ok(Family) => {
                    font_builder.family = section.as_string().map_err(|e| e.in_section(Family))?;
                }
                Ok(PointSize) => {
                    font_builder.point_size =
                        section.as_u16().map_err(|e| e.in_section(PointSize))?;
                }
                Ok(Weight) => {
                    let Ok(weight) = FontWeight::from_str(
                        &section.as_string().map_err(|e| e.in_section(Weight))?,
                    ) else {
                        continue;
                    };
                    font_builder.weight = weight;
                }
                Ok(MaxCharWidth) => {
                    font_builder.max_char_width =
                        section.as_u16().map_err(|e| e.in_section(MaxCharWidth))?;
                }
                Ok(MaxCharHeight) => {
                    font_builder.max_char_height =
                        section.as_u16().map_err(|e| e.in_section(MaxCharHeight))?;
                }
                Ok(Ascent) => {
                    font_builder.ascent = section.as_u16().map_err(|e| e.in_section(Ascent))?;
                }
                Ok(Descent) => {
                    font_builder.descent = section.as_u16().map_err(|e| e.in_section(Descent))?;
                }
                Ok(CharIndex) => {
                    font_builder.char_index = section.as_char_index()?;
                }
                Ok(Data) => {
                    break; // Data section indicates end of data
                }
                Err(()) | Ok(_) => {
                    continue; // GRUB ignores unknown or unused section types
                }
            }
        }

        Ok(font_builder)
    }
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum IntoFontError {
    #[error("Attempted to create a PFF2 font from 0 sections")]
    EmptySlice,
    #[error("PFF2 magic section error: {0}")]
    MagicSection(SectionError),
    #[error("PFF2 section {0:?} contains invalid data: {1}")]
    DataError(SectionName, SectionEncodingError),
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum SectionError {
    #[error("Invalid Name")]
    Name,
    #[error("Invalid data")]
    Data,
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum SectionEncodingError {
    #[error("String data is not null terminated")]
    StringNotNullTerminated,
    #[error("String data is not valid UTF-8")]
    ToString(#[from] FromUtf8Error),

    #[error("A u16 payload was {0} bytes instead of 2")]
    InvalidU16Length(usize),
}

impl SectionEncodingError {
    fn in_section(self, name: SectionName) -> IntoFontError {
        IntoFontError::DataError(name, self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum FontWeight {
    #[default]
    Normal,
    Bold,
}

impl FromStr for FontWeight {
    type Err = FontWeightFromStrError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "normal" => Ok(Self::Normal),
            "bold" => Ok(Self::Bold),
            _ => Err(FontWeightFromStrError(Rc::from(s))),
        }
    }
}

pub struct FontWeightFromStrError(Rc<str>);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SectionName {
    File,
    FontName,
    PointSize,
    Weight,
    MaxCharWidth,
    MaxCharHeight,
    Ascent,
    Descent,
    CharIndex,
    Data,
    Family,
    Slan,
}

impl From<SectionName> for [u8; 4] {
    fn from(source: SectionName) -> Self {
        match source {
            SectionName::File => *b"FILE",
            SectionName::FontName => *b"NAME",
            SectionName::PointSize => *b"PTSZ",
            SectionName::Weight => *b"WEIG",
            SectionName::MaxCharWidth => *b"MAXW",
            SectionName::MaxCharHeight => *b"MAXH",
            SectionName::Ascent => *b"ASCE",
            SectionName::Descent => *b"DESC",
            SectionName::CharIndex => *b"CHIX",
            SectionName::Data => *b"DATA",
            SectionName::Family => *b"FAMI",
            SectionName::Slan => *b"SLAN",
        }
    }
}
impl TryFrom<[u8; 4]> for SectionName {
    type Error = ();

    /// Converts the byte string into a known section name.
    /// The [`Err(())`] indicates that this section name is unknown.
    fn try_from(bytes: [u8; 4]) -> Result<Self, Self::Error> {
        match bytes.as_ref() {
            b"FILE" => Ok(SectionName::File),
            b"NAME" => Ok(SectionName::FontName),
            b"PTSZ" => Ok(SectionName::PointSize),
            b"WEIG" => Ok(SectionName::Weight),
            b"MAXW" => Ok(SectionName::MaxCharWidth),
            b"MAXH" => Ok(SectionName::MaxCharHeight),
            b"ASCE" => Ok(SectionName::Ascent),
            b"DESC" => Ok(SectionName::Descent),
            b"CHIX" => Ok(SectionName::CharIndex),
            b"DATA" => Ok(SectionName::Data),
            b"FAMI" => Ok(SectionName::Family),
            b"SLAN" => Ok(SectionName::Slan),
            _ => Err(()),
        }
    }
}

const FILE_SECTION_MAGIC: &[u8; 4] = b"PFF2";

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct CharIndexEntry {
    pub code: u32,
    pub storage_flags: u8,
    pub offset: u32,

    pub glyph: Option<FontGlyph>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FontGlyph {}

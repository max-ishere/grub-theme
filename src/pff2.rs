use core::fmt::Debug;
use std::{marker::PhantomData, rc::Rc, string::FromUtf8Error};

use nom::{InputLength, ToUsize};
use thiserror::Error;

use crate::OwnedSlice;

pub type Font = Pff2<Validated>;
pub type Parser = Pff2<Unchecked>;

#[allow(private_bounds)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pff2<T: FontValidation> {
    pub name: String,
    pub family: String,
    pub point_size: u16,
    pub weight: String,
    pub max_char_width: u16,
    pub max_char_height: u16,
    pub ascent: u16,
    pub descent: u16,
    pub leading: u16,
    pub glyphs: OwnedSlice<[Glyph]>,

    _validation: PhantomData<T>,
}

impl<T: FontValidation> Default for Pff2<T> {
    fn default() -> Self {
        Self {
            name: Default::default(),
            family: Default::default(),
            point_size: Default::default(),
            weight: Default::default(),
            max_char_width: Default::default(),
            max_char_height: Default::default(),
            ascent: Default::default(),
            descent: Default::default(),
            leading: Default::default(),

            glyphs: OwnedSlice::new([]),

            _validation: Default::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Glyph {
    pub code: u32,

    pub width: u16,
    pub height: u16,
    pub x_offset: u16,
    pub y_offset: u16,
    pub device_width: u16,

    pub bitmap: OwnedSlice<[u8]>,
}

impl Parser {
    const MAGIC: &'static [u8; 4 + 4 + 4] = b"FILE\0\0\0\x04PFF2";

    pub fn parse(input: &[u8]) -> Result<Self, ParserError> {
        let input_for_data_section = input; // Save this because data offsets are absolute

        let (magic, mut input) = input.split_at(4 + 4 + 4);
        // This is technically a section, but because its always first and same content
        // we just compare it in one go.
        if magic != Self::MAGIC {
            return Err(ParserError::BadMagicBytes);
        }

        let mut font = Self::default();
        let mut char_indexes = Vec::new();

        'parsing: while input.input_len() != 0 {
            // Prevents shadowing input. We need it to maintain parsing state
            input = 'input: {
                let (section, length, input) = Self::parse_section_header(input)?;

                let Ok(section) = SectionName::try_from(section) else {
                    break 'input &input[length..];
                };

                use SectionName::*;
                match section {
                    FontName => font.name = Self::parse_string(&input[..length])?,
                    Family => font.family = Self::parse_string(&input[..length])?,
                    PointSize => font.point_size = Self::parse_u16(&input[..length])?,
                    Weight => font.weight = Self::parse_string(&input[..length])?,
                    MaxCharWidth => font.max_char_width = Self::parse_u16(&input[..length])?,
                    MaxCharHeight => font.max_char_height = Self::parse_u16(&input[..length])?,
                    Ascent => font.ascent = Self::parse_u16(&input[..length])?,
                    Descent => font.descent = Self::parse_u16(&input[..length])?,
                    CharIndex => char_indexes = Self::parse_char_indexes(&input[..length])?,
                    Data => {
                        font.glyphs =
                            Self::parse_data_section(char_indexes, &input_for_data_section)?;
                        break 'parsing;
                    }
                }

                if length < input.len() {
                    &input[length..]
                } else {
                    break 'parsing;
                }
            }
        }

        Ok(font)
    }

    fn parse_section_header(input: &[u8]) -> Result<([u8; 4], usize, &[u8]), ParserError> {
        let (section, input) = input.split_at(4);
        let section: [u8; 4] = section
            .try_into()
            .map_err(|_| ParserError::InsufficientHeaderBytes)?;

        let (length, input) = input.split_at(4);

        let length = u32::from_be_bytes(
            length
                .try_into()
                .map_err(|_| ParserError::InsufficientHeaderBytes)?,
        )
        .to_usize();

        Ok((section, length, input))
    }

    fn parse_string(input: &[u8]) -> Result<String, FromUtf8Error> {
        if input.len() == 0 {
            return Ok(String::new());
        }

        if input.last() == Some(&0) {
            return String::from_utf8(input[..input.len() - 1].to_vec());
        }

        String::from_utf8(input[..input.len()].to_vec())
    }

    fn parse_u16(input: &[u8]) -> Result<u16, ParserError> {
        if input.len() != 2 {
            return Err(ParserError::InvalidU16Length(input.len()));
        }

        Ok(u16::from_be_bytes([input[0], input[1]]))
    }

    pub fn validate(self) -> Result<Font, FontValidationError> {
        use FontValidationError::*;
        if self.name.is_empty() {
            return Err(EmptyName);
        }

        for (prop, err) in [
            (self.max_char_width, ZeroMaxCharWidth),
            (self.max_char_height, ZeroMaxCharHeight),
            (self.ascent, ZeroAscent),
            (self.descent, ZeroDescent),
        ] {
            if prop == 0 {
                return Err(err);
            }
        }

        if self.glyphs.len() == 0 {
            return Err(NoGlyphs);
        }

        Ok(Font {
            name: self.name,
            family: self.family,
            point_size: self.point_size,
            weight: self.weight,
            max_char_width: self.max_char_width,
            max_char_height: self.max_char_height,
            ascent: self.ascent,
            descent: self.descent,
            leading: self.leading,
            glyphs: self.glyphs,
            _validation: PhantomData,
        })
    }

    fn parse_char_indexes(input: &[u8]) -> Result<Vec<CharIndex>, ParserError> {
        const ALLIGNMENT: usize = 4 + 1 + 4;

        if input.len() % ALLIGNMENT != 0 {
            return Err(ParserError::InvalidCharacterIndex);
        }

        Ok(input
            .chunks(ALLIGNMENT)
            .into_iter()
            .map(|chunk| CharIndex {
                code: u32::from_be_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]),
                // skipp [4], it's a `storage_flags`, and GRUB never uses that field anyway
                offset: u32::from_be_bytes([chunk[5], chunk[6], chunk[7], chunk[8]]).to_usize(),
            })
            .collect())
    }

    fn parse_data_section(
        indexes: Vec<CharIndex>,
        input: &[u8],
    ) -> Result<Rc<[Glyph]>, ParserError> {
        let mut glyphs = Vec::with_capacity(input.len());

        for index in indexes {
            let offset = index.offset;

            // make sure there are enough bytes to read glyph data
            if offset + 4 > input.len() {
                continue;
            }

            let width = Self::parse_u16(&input[offset..offset + 2])?;
            let height = Self::parse_u16(&input[offset + 2..offset + 4])?;

            let bitmap_len = (width * height + 7) / 8;

            if offset + 12 + bitmap_len as usize > input.len() {
                continue;
            }

            let glyph = Glyph {
                code: index.code,
                width,
                height,
                x_offset: Self::parse_u16(&input[offset + 6..offset + 8])?,
                y_offset: Self::parse_u16(&input[offset + 8..offset + 10])?,
                device_width: Self::parse_u16(&input[offset + 10..offset + 12])?,
                bitmap: Rc::from(&input[offset + 12..offset + 12 + bitmap_len as usize]),
            };

            glyphs.push(glyph);
        }

        Ok(Rc::from(glyphs.as_slice()))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum SectionName {
    FontName,
    Family,
    PointSize,
    Weight,
    MaxCharWidth,
    MaxCharHeight,
    Ascent,
    Descent,
    CharIndex,
    Data,
}

struct CharIndex {
    pub code: u32,
    pub offset: usize,
}

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
    #[error("Bad PFF2 magic bytes")]
    BadMagicBytes,

    #[error("Insufficient section header bytes")]
    InsufficientHeaderBytes,

    #[error("Insufficient section length bytes")]
    InsufficientLengthBytes,

    #[error("Invalid UTF-8 string: {0}")]
    FromUtf8Error(#[from] FromUtf8Error),

    #[error("A u16 is not encoded using exactly 2 bytes, instead: {0}b")]
    InvalidU16Length(usize),

    #[error("Invalid data in the character index")]
    InvalidCharacterIndex,
}

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum FontValidationError {
    #[error("Font has no name")]
    EmptyName,
    #[error("Font doesnt define maximum glyph width")]
    ZeroMaxCharWidth,
    #[error("Font doesnt define maximum glyph height")]
    ZeroMaxCharHeight,
    #[error("Font doesnt define char ascent")]
    ZeroAscent,
    #[error("Font doesnt define char descent")]
    ZeroDescent,
    #[error("Font contains no glyphs")]
    NoGlyphs,
}

impl TryFrom<[u8; 4]> for SectionName {
    /// Unknown section names are usually ignored so no point returning them to the caller.
    type Error = ();

    /// Converts the byte string into a known section name.
    /// The [`Err(())`] indicates that this section name is unknown.
    fn try_from(bytes: [u8; 4]) -> Result<Self, Self::Error> {
        match bytes.as_ref() {
            b"NAME" => Ok(SectionName::FontName),
            b"FAMI" => Ok(SectionName::Family),
            b"PTSZ" => Ok(SectionName::PointSize),
            b"WEIG" => Ok(SectionName::Weight),
            b"MAXW" => Ok(SectionName::MaxCharWidth),
            b"MAXH" => Ok(SectionName::MaxCharHeight),
            b"ASCE" => Ok(SectionName::Ascent),
            b"DESC" => Ok(SectionName::Descent),
            b"CHIX" => Ok(SectionName::CharIndex),
            b"DATA" => Ok(SectionName::Data),
            _ => Err(()),
        }
    }
}

impl Default for Glyph {
    fn default() -> Self {
        Self {
            code: Default::default(),

            width: Default::default(),
            height: Default::default(),
            x_offset: Default::default(),
            y_offset: Default::default(),
            device_width: Default::default(),

            bitmap: OwnedSlice::new([]),
        }
    }
}
trait FontValidation: Clone + PartialEq + Eq + Debug {}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Validated;
impl FontValidation for Validated {}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Unchecked;
impl FontValidation for Unchecked {}

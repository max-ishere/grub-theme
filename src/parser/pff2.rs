//! PFF2 Font format parser.
//!
//! To parse a font file call [`Parser::parse`]. However do keep in mind that successful parsing doesn't mean that the
//! file contains all the information that GRUB requires. To validate the data use [`Parser::validate`], which returns a
//! [`Font`]
//!
//! ```no_run
//! # use theme_parser::parser::pff2::*;
//! # use std::fs::read;
//! let pff2 = read("font.pf2")?;
//! let font: Font = Parser::parse(&pff2)?.validate()?;
//! # Ok::<(), anyhow::Error>(())
//! ```

use core::fmt::Debug;
use std::{marker::PhantomData, rc::Rc, string::FromUtf8Error};

use nom::{InputLength, ToUsize};
use thiserror::Error;

use crate::{render::pff2::Bitmap, OwnedSlice, Sealed};

/// A font object that is supposed to be validated after the file was read. Instead of constructing this youself, make a
/// `Parser` and call validate on it.
pub type Font = Pff2<Validated>;

/// A type used for parsing the PFF2 format. This is separate from [`Font`] to make sure that the font data is validated
/// before it will be used by the code that expects certain properties from a font. See [`Parser::validate`] for details
///  on the requiements.
pub type Parser = Pff2<Unchecked>;

/// The PFF2 font, see [`Parser`] and [`Font`] type aliases.
///
/// Only contains relevant to GRUB metadata about the font as well as the glyph list.
#[allow(private_bounds)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pff2<T: FontValidation> {
    /// Font name. Usually includes family, point size and weight
    pub name: String,
    /// Font family
    pub family: String,
    /// Size of the font
    pub point_size: u16,
    /// Weight could be anything like normal, bold or italic
    pub weight: String,
    /// The dimetions of the glyphs
    pub max_char_width: u16,
    /// The dimetions of the glyphs
    pub max_char_height: u16,
    /// The highest point on the glyph (like the top of `M`)
    pub ascent: u16,
    /// The lowest point on the glyph (like the bottom of `g`)
    pub descent: u16,
    /// The distance between baselines
    pub leading: u16,
    /// A list of glyphs that are in the font
    pub glyphs: OwnedSlice<[Glyph]>,

    _phantom: PhantomData<T>,
}

/// A single glyph inside the font.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Glyph {
    /// The UTF codepoint of the character
    pub code: char,

    pub x_offset: isize,
    pub y_offset: isize,
    pub device_width: isize,

    /// The bitmap that represents a rendered glyph.
    pub bitmap: Bitmap,
}

impl Parser {
    const MAGIC: &'static [u8; 4 + 4 + 4] = b"FILE\0\0\0\x04PFF2";

    /// Constructs [`Self`] from a PFF2 buffer.
    pub fn parse(input: &[u8]) -> Result<Self, ParserError> {
        let input_for_data_section = input; // Save this because SectionName::CharIndex offsets are absolute

        let (magic, mut input) = input.split_at(Self::MAGIC.len());
        if magic != Self::MAGIC {
            return Err(ParserError::BadMagicBytes);
        }

        let mut font = Self::default();
        let mut char_indexes = Vec::new();

        'parsing: while input.input_len() != 0 {
            // Prevents shadowing input. We need it to maintain parsing state
            input = 'input: {
                let (section, length, input) = Self::parse_section_header(input)?;

                let Ok(section) = Section::try_from(section) else {
                    warn!("Skipping section {section:?} because it is not supported");
                    break 'input &input[length..];
                };

                use Section::*;
                match section {
                    FontName => font.name = Self::parse_string(&input[..length])?,
                    Family => font.family = Self::parse_string(&input[..length])?,
                    PointSize => font.point_size = Self::parse_u16_be(&input[..length])?,
                    Weight => font.weight = Self::parse_string(&input[..length])?,
                    MaxCharWidth => font.max_char_width = Self::parse_u16_be(&input[..length])?,
                    MaxCharHeight => font.max_char_height = Self::parse_u16_be(&input[..length])?,
                    Ascent => font.ascent = Self::parse_u16_be(&input[..length])?,
                    Descent => font.descent = Self::parse_u16_be(&input[..length])?,
                    CharIndex => char_indexes = Self::parse_char_indexes(&input[..length])?,
                    Data => {
                        font.glyphs = Self::parse_data_section(char_indexes, input_for_data_section);
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

    /// Returns the section name, length as usize and the rest of the supplied input starting at the beginning of
    /// section content.
    fn parse_section_header(input: &[u8]) -> Result<([u8; 4], usize, &[u8]), ParserError> {
        let (section, input) = input.split_at(4);
        let section: [u8; 4] = section.try_into().map_err(|_| ParserError::InsufficientHeaderBytes)?;

        let (length, input) = input.split_at(4);
        let length = u32::from_be_bytes(
            length
                .try_into()
                .map_err(|_| ParserError::InsufficientLengthBytes { section })?,
        )
        .to_usize();

        Ok((section, length, input))
    }

    /// Converts the entirety of input into a UTF-8 string. If the string is `\0` terminated, removes the `\0` before
    /// conversion.
    fn parse_string(input: &[u8]) -> Result<String, FromUtf8Error> {
        if input.is_empty() {
            return Ok(String::new());
        }

        if input.last() == Some(&0) {
            return String::from_utf8(input[..input.len() - 1].to_vec());
        }

        String::from_utf8(input[..input.len()].to_vec())
    }

    /// Converts the entirety of input into a u16. If the supplied slice is not 2 bytes long, returns an error.
    fn parse_u16_be(input: &[u8]) -> Result<u16, ParserError> {
        if input.len() != 2 {
            return Err(ParserError::InvalidU16Length(input.len()));
        }

        Ok(u16::from_be_bytes([input[0], input[1]]))
    }

    /// Parses the [`Section::CharIndex`] section and returns the glyph lookup table. Requires that:
    /// - The input length is divisible by the size of an entry
    /// - The codepoints are valid unicode
    /// - The codepoints are stored in ascending order (`char as u32`)
    fn parse_char_indexes(input: &[u8]) -> Result<Vec<CharIndex>, ParserError> {
        const ENTRY_SIZE: usize = 4 + 1 + 4;

        if input.len() % ENTRY_SIZE != 0 {
            return Err(ParserError::EmptyCharacterIndex);
        }

        let mut last_codepoint = None;

        input
            .chunks(ENTRY_SIZE)
            .map(|chunk| {
                let codepoint = u32::from_be_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);

                if last_codepoint.is_none() || codepoint > last_codepoint.unwrap() {
                    last_codepoint = Some(codepoint);
                } else {
                    return Err(ParserError::CharacterIndexNotSortedAscending);
                }

                let storage_flags = chunk[4];
                const DEFAULT_STORAGE_FLAGS: u8 = 0;
                if storage_flags != DEFAULT_STORAGE_FLAGS {
                    warn!(
                        "Codepoint {codepoint:x} has non-default storage flags: 0b{storage_flags:b}, the encoded \
                         value may not be decoded correctly"
                    );
                }

                // TODO: Investigate why unicode.pf2 contains invalid codepoint 0x40000626 and deal with it.
                Ok::<_, ParserError>(CharIndex {
                    code: char::from_u32(codepoint).ok_or(ParserError::InvalidCodepoint(codepoint))?,
                    offset: u32::from_be_bytes([chunk[5], chunk[6], chunk[7], chunk[8]]).to_usize(),
                })
            })
            .collect()
    }

    /// Takes the glyph lookup section and combines it with the content of the data section to get the complete glyph
    /// data. [`CharIndex`] offsets are file-global (absolute), so `input` should be the entirety of the file.
    fn parse_data_section(indexes: Vec<CharIndex>, input: &[u8]) -> Rc<[Glyph]> {
        let mut glyphs = Vec::with_capacity(indexes.len());

        for index in indexes {
            let offset = index.offset;

            // make sure there are enough bytes to read the bitmap dimentions
            if offset + 4 > input.len() {
                warn!(
                    "Insufficient data to load a glyph for codepoint {}",
                    index.code.escape_unicode(),
                );
                continue;
            }

            let width = u16::from_be_bytes([input[offset], input[offset + 1]]).to_usize();
            let height = u16::from_be_bytes([input[offset + 2], input[offset + 3]]).to_usize();

            let bitmap_len = Bitmap::byte_count_from_size(width, height);

            // make sure there are enough bytes to read the bitmap and the rest of the fields
            if offset + 10 + bitmap_len > input.len() {
                warn!(
                    "Insufficient data to load a glyph for codepoint {}",
                    index.code.escape_unicode()
                );
                continue;
            }

            let glyph = Glyph {
                code: index.code,
                x_offset: i16::from_be_bytes([input[offset + 4], input[offset + 5]]) as isize,
                y_offset: i16::from_be_bytes([input[offset + 6], input[offset + 7]]) as isize,
                device_width: u16::from_be_bytes([input[offset + 8], input[offset + 9]]) as isize,
                bitmap: Bitmap::new(width, height, &input[(offset + 10)..(offset + 10 + bitmap_len)])
                    .expect("input slice should be long enough to represent the bitmap data"),
            };

            glyphs.push(glyph);
        }

        glyphs.into()
    }

    /// Validates [`Self`] to be a valid font that can be used for rendering. See [`FontValidationError`] for reasons a
    /// font may be invalid.
    pub fn validate(self) -> Result<Font, FontValidationError> {
        use FontValidationError as E;

        if self.name.is_empty() {
            return Err(E::EmptyName);
        }

        for (prop, err) in [
            (self.max_char_width, E::ZeroMaxCharWidth),
            (self.max_char_height, E::ZeroMaxCharHeight),
            (self.ascent, E::ZeroAscent),
            (self.descent, E::ZeroDescent),
        ] {
            if prop == 0 {
                return Err(err);
            }
        }

        if self.glyphs.len() == 0 {
            return Err(E::NoGlyphs);
        }

        let mut last_codepoint = self.glyphs[0].code as u32;

        for Glyph { code, .. } in &self.glyphs[1..] {
            let code = *code as u32;

            if code > last_codepoint {
                last_codepoint = code;
            } else {
                return Err(E::GlyphsNotSortedAscending);
            }
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
            _phantom: PhantomData,
        })
    }
}

impl<T: FontValidation> Pff2<T> {
    pub fn glyph(&self, c: char) -> Option<&Glyph> {
        self.glyphs
            .binary_search_by(|g| (g.code as u32).cmp(&(c as u32)))
            .map(|i| &self.glyphs[i])
            .ok()
    }
}

impl Glyph {
    const UNKNOWN_GLYPH_BITMAP: [u8; 16] = [
        0b01111100, //
        0b10000010, //
        0b10111010, //
        0b10101010, //
        0b10101010, //
        0b10001010, //
        0b10011010, //
        0b10010010, //
        0b10010010, //
        0b10010010, //
        0b10010010, //
        0b10000010, //
        0b10010010, //
        0b10000010, //
        0b01111100, //
        0b00000000, //
    ];
    const UNKNOWN_GLYPH_HEIGHT: usize = 16;
    const UNKNOWN_GLYPH_WIDTH: usize = 8;

    /// Creates a [`Glyph`] that can be used to represent a character that was not found in a font during rendering.
    pub fn unknown() -> Self {
        Self {
            code: Default::default(),

            x_offset: 0,
            y_offset: 0,
            device_width: Self::UNKNOWN_GLYPH_WIDTH as isize,
            bitmap: Bitmap::new(
                Self::UNKNOWN_GLYPH_WIDTH,
                Self::UNKNOWN_GLYPH_HEIGHT,
                &Self::UNKNOWN_GLYPH_BITMAP,
            )
            .unwrap(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Section {
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

/// An intermediate structure used for reading glyphs from a font file. This is discarded after the glyphs are read.
struct CharIndex {
    /// The UCS-4 codepoint
    pub code: char,
    /// A file-level (absolute) offset to the glyph data
    pub offset: usize,
}

/// The PFF2 buffer contained invalid data
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
    /// The PFF2 buffer should start with magic bytes.
    #[error("Bad PFF2 magic bytes")]
    BadMagicBytes,

    /// Expected to be able to read 4 bytes as the name of the section
    #[error("Insufficient section header bytes")]
    InsufficientHeaderBytes,

    /// Expected to be able to read 4 bytes as a u32 length of the section
    #[error("Insufficient bytes to read the length of section {section:?}")]
    InsufficientLengthBytes { section: [u8; 4] },

    /// String stored in a section had illegal UTF-8 bytes
    #[error("Invalid UTF-8 string: {0}")]
    FromUtf8Error(#[from] FromUtf8Error),

    /// Expected a section 2b long because content is expected to be a u16
    #[error("A u16 is not encoded using exactly 2 bytes, instead: {0}b")]
    InvalidU16Length(usize),

    /// The size of the character index section doesnt divide evenly by the size of the individual elements
    #[error("Invalid data in the character index")]
    EmptyCharacterIndex,

    /// The character index section is required to contain codepoints in ascending order (`char as u32`)
    #[error("Character index is not sorted in ascending order")]
    CharacterIndexNotSortedAscending,

    /// The codepoint for a glyph entry was not valid UTF-8
    #[error("Invalid unicode codepoint encountered: 0x{0:x}")]
    InvalidCodepoint(u32),
}

/// Convertion from [`Parser`] into [`Font`] failed, meaning some data is invalid.
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum FontValidationError {
    /// Font has no name
    #[error("Font has no name")]
    EmptyName,

    /// The dimentions of the characters are 0
    #[error("Font doesnt define maximum glyph width")]
    ZeroMaxCharWidth,

    /// The dimentions of the characters are 0
    #[error("Font doesnt define maximum glyph height")]
    ZeroMaxCharHeight,

    /// Ascent cannot be 0
    #[error("Font doesnt define char ascent")]
    ZeroAscent,

    /// Descent cannot be 0
    #[error("Font doesnt define char descent")]
    ZeroDescent,

    /// Font contains no glyphs or they could not be read
    #[error("Font contains no glyphs")]
    NoGlyphs,

    /// Font must store glyphs in sorted ascending order
    #[error("Glyphs are not sorted in ascending order")]
    GlyphsNotSortedAscending,
}

impl TryFrom<[u8; 4]> for Section {
    /// Unknown section names are usually ignored so no point returning them to the caller.
    type Error = ();

    /// Converts the byte string into a known section name.
    /// The [`Err(())`] indicates that this section name is unknown.
    fn try_from(bytes: [u8; 4]) -> Result<Self, Self::Error> {
        match bytes.as_ref() {
            b"NAME" => Ok(Section::FontName),
            b"FAMI" => Ok(Section::Family),
            b"PTSZ" => Ok(Section::PointSize),
            b"WEIG" => Ok(Section::Weight),
            b"MAXW" => Ok(Section::MaxCharWidth),
            b"MAXH" => Ok(Section::MaxCharHeight),
            b"ASCE" => Ok(Section::Ascent),
            b"DESC" => Ok(Section::Descent),
            b"CHIX" => Ok(Section::CharIndex),
            b"DATA" => Ok(Section::Data),
            _ => Err(()),
        }
    }
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
            leading: 1, // PFF2 files dont have a leading section. This is the default value for all PFF2 fonts

            glyphs: [].into(),

            _phantom: Default::default(),
        }
    }
}

#[allow(private_bounds)]
pub trait FontValidation: Sealed + Clone + PartialEq + Eq + Debug {}

#[doc(hidden)]
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Validated;
impl FontValidation for Validated {}
impl Sealed for Validated {}

#[doc(hidden)]
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Unchecked;
impl FontValidation for Unchecked {}
impl Sealed for Unchecked {}

#[cfg(test)]
mod tests {
    #[allow(non_snake_case)]
    mod Glyph {
        use super::super::Glyph;

        #[test]
        fn unknown_doesnt_panic() {
            Glyph::unknown();
        }
    }
}

//! PFF2 font rendering utilities

use crate::OwnedSlice;

/// A bitmap that can be used to query pixel data in a PFF2 Glyph.
///
/// There are 2 ways of querying pixels:
///
/// - Using [`Self::pixel()`]
///
/// ```rust
/// # use theme_parser::render::pff2::Bitmap;
/// #
/// # || -> Option<()> {
/// #
/// let bitmap = Bitmap::new(8, 1, &[0b00000001])?;
///
/// let pixel = bitmap.pixel(7, 0)?;
/// assert_eq!(pixel, true);
/// #
/// # Some(())
/// # }().unwrap()
/// ```
///
/// - Using an iterator
///
/// ```rust
/// # use theme_parser::render::pff2::Bitmap;
/// #
/// let bitmap = Bitmap::default();
///
/// for row in bitmap {
///     for pixel in row {
///         print!("{}", if pixel { "##" } else { "  " });
///     }
/// }
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Bitmap {
    width: usize,
    height: usize,
    bitmap: OwnedSlice<[u8]>,
}

impl Default for Bitmap {
    fn default() -> Self {
        Self {
            width: 0,
            height: 0,
            bitmap: [].into(),
        }
    }
}

impl Bitmap {
    /// What the PFF2 format considers as filled. The users of this library should always recieve `true` as filled,
    /// regardless of the changes in the format.
    const FILLED_PIXEL: bool = true;
    /// What the PFF2 format considers as transparent. The users of this library should always recieve `false` as
    /// trasnparent, regardless of the changes in the format.
    #[allow(unused)]
    const TRANSPARENT_PIXEL: bool = !Self::FILLED_PIXEL;

    /// Calculates the length of the bitmap in bytes from the dimentions of the bitmap in pixels
    pub fn byte_count_from_size(width: usize, height: usize) -> usize {
        (width * height + 7) / 8
    }

    /// Constructs self by wrapping the bitmap slice in a [`OwnedSlice`] and verifying that it's length is exactly
    /// [`Self::byte_count_from_size`].
    pub fn new(width: usize, height: usize, bitmap: &[u8]) -> Option<Self> {
        if bitmap.len() != Self::byte_count_from_size(width, height) {
            return None;
        }

        Some(Self {
            width,
            height,
            bitmap: bitmap.into(),
        })
    }

    /// Returns `Some(true)` if the pixel is filled, `Some(false)` if the pixel if transparent, [`None`] if out of
    /// bounds.
    pub fn pixel(&self, x: usize, y: usize) -> Option<bool> {
        if x >= self.width || y >= self.height {
            return None;
        }

        let index = y * self.width + x;
        let byte_index = index / 8;
        let bit_index = 7 - (index % 8);
        let bit = (self.bitmap[byte_index] >> bit_index & 1) != 0;

        Some(bit == Self::FILLED_PIXEL)
    }

    /// Bitmap width (line length)
    pub fn width(&self) -> usize {
        self.width
    }

    /// Bitmap height (number of lines)
    pub fn height(&self) -> usize {
        self.height
    }

    /// Creates an iter over self
    pub fn iter(&self) -> BitmapIter {
        self.clone().into_iter()
    }
}

impl IntoIterator for Bitmap {
    type IntoIter = BitmapIter;
    type Item = BitmapRowIter;

    fn into_iter(self) -> Self::IntoIter {
        BitmapIter { bitmap: self, row: 0 }
    }
}

impl IntoIterator for &Bitmap {
    type IntoIter = BitmapIter;
    type Item = BitmapRowIter;

    fn into_iter(self) -> Self::IntoIter {
        BitmapIter {
            bitmap: self.clone(),
            row: 0,
        }
    }
}

/// Iterates over the rows of the bitmap, yielding an iterator over the row's elements, [`BitmapRowIter`].
pub struct BitmapIter {
    bitmap: Bitmap,
    row: usize,
}

impl Iterator for BitmapIter {
    type Item = BitmapRowIter;

    /// Yields rows of the bitmap
    fn next(&mut self) -> Option<Self::Item> {
        if self.row >= self.bitmap.height {
            return None;
        }

        let row_iter = BitmapRowIter {
            bitmap: self.bitmap.clone(),
            row: self.row,
            col: 0,
        };
        self.row += 1;

        Some(row_iter)
    }
}

/// Iterates over the pixels in a bitmap row, where true means filled, false means transparent.
pub struct BitmapRowIter {
    bitmap: Bitmap,
    row: usize,
    col: usize,
}

impl Iterator for BitmapRowIter {
    type Item = bool;

    /// Yields an pixel from a bitmap, where true means filled, false means transparent.
    fn next(&mut self) -> Option<Self::Item> {
        if self.col >= self.bitmap.width {
            return None;
        }

        let pixel = self.bitmap.pixel(self.col, self.row);
        self.col += 1;

        pixel
    }
}

#[cfg(test)]
mod tests {
    #[allow(non_snake_case)]
    mod Bitmap {
        use super::super::Bitmap;

        #[test_case(
            8, 1, &[0b00000001],
            7, 0 => 1;
            "right bit one line"
        )]
        #[test_case(
            8, 1, &[0b10000000],
            0, 0 => 1;
            "left bit one line"
        )]
        fn pixel(width: usize, height: usize, bitmap: &[u8], x: usize, y: usize) -> u8 {
            let glyph = Bitmap {
                width,
                height,
                bitmap: bitmap.into(),
            };

            glyph.pixel(x, y).unwrap() as u8
        }
    }
}

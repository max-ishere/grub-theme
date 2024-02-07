use crate::OwnedSlice;

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

    /// Calculates the number of bytes required to
    pub fn byte_count_from_size(width: usize, height: usize) -> usize {
        (width * height + 7) / 8
    }

    /// Constructs self by wrapping the bitmap slice in a [`Rc`] and verifying that it's length is exactly
    /// [`Self::byte_count_from_size`].
    ///
    /// [`Rc`]: std::rc::Rc
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

    /// Returns [`Some(true)`] if the pixel is filled, [`Some(false)`] if the pixel if transparent, [`None`] if out of
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

pub struct BitmapRowIter {
    bitmap: Bitmap,
    row: usize,
    col: usize,
}

impl Iterator for BitmapRowIter {
    type Item = bool;

    /// Yields an element in the bitmap's row
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

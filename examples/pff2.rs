//! A minimal font.pf2 parser impl that prints a glyph from a PFF2 font

use std::{fs::read, path::PathBuf};

use clap::Parser as _;
use theme_parser::parser::pff2::{Glyph, Parser};

#[derive(clap::Parser)]
struct Args {
    #[clap(long, short = 'f')]
    pub font_file: PathBuf,

    #[clap(long = "char", short)]
    pub character: char,
}

const SQUARE_BLOCK: &str = "\u{2588}\u{2588}";

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let data = read(args.font_file)?;
    let font = Parser::parse(&data)?.validate()?;

    println!("{}", font.name);

    let glyph = font.glyph(args.character).unwrap();

    render_glyph(glyph);

    Ok(())
}

fn render_glyph(glyph: &Glyph) {
    for row in &glyph.bitmap {
        for col in row {
            print!("{}", if col { SQUARE_BLOCK } else { "  " });
        }
        println!();
    }
}

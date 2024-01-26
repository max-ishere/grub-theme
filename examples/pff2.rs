//! A minimal font.pf2 parser impl that prints the parsed Rust struct

use std::fs::read;

use args::Args;
use clap::Parser;
use theme_parser::pff2::FontParser;

mod args {
    use std::path::PathBuf;

    use clap::Parser;

    #[derive(Parser)]
    pub struct Args {
        #[clap(long, short = 'f')]
        pub font_file: PathBuf,
    }
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let data = read(args.font_file)?;
    let sections = FontParser::parse(&data).unwrap().1;
    let font = FontParser::try_from(sections.as_slice())?
        .validate()
        .unwrap();

    println!("{:#?}", font);

    Ok(())
}

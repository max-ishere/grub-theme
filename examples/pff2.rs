//! A minimal font.pf2 parser impl that prints the parsed Rust struct

use std::fs::read;

use args::Args;
use clap::Parser as _;
use theme_parser::parser::pff2::Parser;

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
    let font = Parser::parse(&data)?.validate();

    let print = format!("{font:#?}")
        .split("\n")
        .take(100)
        .fold(String::new(), |print, line| print + line + "\n");

    println!("{print}");

    Ok(())
}

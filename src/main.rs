use std::{fs::read_to_string, sync::Arc};

use args::Args;
use clap::Parser;
use nom::{error::Error, Finish};
use theme_parser::Document;

mod args;

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let content = read_to_string(args.theme_file)?;

    let doc = Document::parse(&content)
        .finish()
        // When `?`, &str inside Err would outlive main().
        .map_err(|e| Error {
            input: Arc::<str>::from(e.input),
            code: e.code,
        })?
        .1;

    println!("{:#?}", doc);

    Ok(())
}

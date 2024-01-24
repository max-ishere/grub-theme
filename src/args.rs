use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
pub struct Args {
    #[clap(long, short = 'f')]
    pub theme_file: PathBuf,
}

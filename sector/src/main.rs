use clap::Parser as ClapParser;
use sector_core::{
    sec::lexer::Lexer,
    util::{SectorError, SectorResult},
};
use std::{fs, path::PathBuf};

#[derive(Debug, ClapParser)]
struct SectorArgs {
    path: PathBuf,
}

fn mount_lexer(path_to_source: &PathBuf) -> SectorResult<()> {
    let source = fs::read_to_string(path_to_source).map_err(|e| SectorError::IoError(e))?;

    let mut lexer = Lexer::new(&source);
    let tokens = lexer.lex();

    if tokens.is_ok() {
        println!("{:?}", tokens?);
    } else {
        eprintln!("{}", tokens.unwrap_err());
    }

    Ok(())
}

#[tokio::main]
async fn main() -> SectorResult<()> {
    let args = <SectorArgs as ClapParser>::parse();

    mount_lexer(&args.path)?;

    Ok(())
}

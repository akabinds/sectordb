use clap::Parser as ClapParser;
use sector_core::{sec::mount_parser, util::SectorResult};
use std::path::PathBuf;

#[derive(Debug, ClapParser)]
struct SectorArgs {
    path: PathBuf,
}

#[tokio::main]
async fn main() -> SectorResult<()> {
    let args = <SectorArgs as ClapParser>::parse();

    mount_parser(&args.path)?;

    Ok(())
}

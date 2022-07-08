use glob::glob;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let proto_entries = glob("src/grpc/proto/*.proto")?;

    for entry in proto_entries {
        tonic_build::compile_protos(entry?)?;
    }

    Ok(())
}

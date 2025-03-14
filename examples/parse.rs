use clap::Parser as _;
use ptx_ir::Module;
use std::path::PathBuf;

#[derive(Debug, clap::Parser)]
struct Config {
    input: PathBuf,
}

fn main() {
    let config = Config::parse();
    match Module::from_ptx_path(&config.input) {
        Ok(module) => {
            println!("{:#?}", module);
        }
        Err(diagnostic) => {
            println!("{}", diagnostic);
        }
    }
}

use clap::Parser as _;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use ptx_ir::Module;

use std::path::PathBuf;

#[derive(Debug, clap::Parser)]
struct Config {
    input: PathBuf,
}

fn main() -> Result<(), String> {
    let config = Config::parse();
    let content = std::fs::read_to_string(&config.input).map_err(|e| e.to_string())?;
    let mut files = SimpleFiles::new();
    let file_id = files.add(config.input.to_str().unwrap(), &content);

    match Module::from_ptx(&content, file_id) {
        Ok(module) => {
            println!("{:?}", module);
        }
        Err(diagnostic) => {
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = codespan_reporting::term::Config::default();
            codespan_reporting::term::emit(&mut writer.lock(), &config, &files, &diagnostic)
                .unwrap();
        }
    }
    Ok(())
}

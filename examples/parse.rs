use ptx_ir::Module;
use std::path::PathBuf;

fn main() {
    let input_path = PathBuf::from(std::env::args().nth(1).expect("missing input file"));
    let module_result = Module::from_ptx_path(&input_path);
    match module_result {
        Ok(module) => {
            println!("{:#?}", module);
        }
        Err(diagnostic) => {
            println!("{}", diagnostic);
        }
    }
}

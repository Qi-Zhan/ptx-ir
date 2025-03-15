# ptx-ir: PTX IR in natural Rust data structures

This crate provides a Rust representation of the PTX IR, the intermediate representation used by the NVIDIA CUDA compiler. The goal is to provide a more natural way to work with PTX code than the raw textual representation in Rust.

Nvidia does not provide a formal specification of the PTX IR, so the crate is based on the [PTX ISA documentation](https://docs.nvidia.com/cuda/parallel-thread-execution/index.html) and the [PTX ISA reference manual](https://docs.nvidia.com/cuda/parallel-thread-execution/index.html#instruction-set-reference).
The crate is still in an early stage of development and is not yet feature complete.
Feel free to make an issue or a pull request if you have any suggestions or improvements.

## Getting Started

Add the following to your `Cargo.toml`:

```toml
[dependencies]
ptx-ir = "0.1"
```

## Example

```rust
use ptx_ir::Module;
use std::path::PathBuf;
fn main() {
    let input = PathBuf::from("path/to/your/file.ptx");
    match Module::from_ptx_path(&input) {
        Ok(module) => {
            println!("{:#?}", module);
        }
        Err(diagnostic) => {
            println!("{}", diagnostic);
        }
    }
}
```

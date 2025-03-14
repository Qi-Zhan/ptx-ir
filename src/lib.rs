//! # PTX IR
//!
//! This crate provides a data structure and parser for the Parallel Thread Execution (PTX) Intermediate Representation (IR).
//! ## Usage
//! ```rust
//! use ptx_ir::Module;
//! use std::path::PathBuf;
//!
//! let input = PathBuf::from("tests/kernels/add.ptx");
//! match Module::from_ptx_path(&input) {
//!     Ok(module) => {
//!         println!("{:#?}", module);
//!     }
//!     Err(diagnostic) => {
//!         println!("{}", diagnostic);
//!     }
//! }
//!

mod ir;
mod lexer;
mod parser;
pub use ir::*;

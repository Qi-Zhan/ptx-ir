use paste::paste;
use ptx_ir::Module;
use std::path::PathBuf;

const KERNEL_PATH: &str = "tests/kernels/";

macro_rules! test_kernel {
    ($name:ident) => {
        paste! {
            #[test]
            fn [<test_ $name>]() {
                let path = PathBuf::from(KERNEL_PATH).join(concat!(stringify!($name), ".ptx"));
                let result = Module::from_ptx_path(&path);
                assert!(result.is_ok(), "{}", result.unwrap_err());
            }
        }
    };
}

test_kernel!(add);
test_kernel!(flashattention_fwd);
test_kernel!(add_simple);
test_kernel!(copy);
test_kernel!(gemm);
test_kernel!(fncall);
test_kernel!(transpose);
test_kernel!(times_two);

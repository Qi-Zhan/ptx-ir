use ptx_ir::Module;
use std::path::PathBuf;

const KERNEL_PATH: &str = "tests/kernels/";

macro_rules! test_kernel {
    ($name:ident, $path:expr) => {
        #[test]
        fn $name() {
            let path = PathBuf::from(KERNEL_PATH).join($path);
            assert!(Module::from_ptx_path(&path).is_ok());
        }
    };
}

test_kernel!(test_add_kernel, "add.ptx");
test_kernel!(test_flashattention, "_attn_fwd.ptx");
test_kernel!(test_add_simple, "add_simple.ptx");
test_kernel!(test_copy, "copy.ptx");
test_kernel!(test_gemm, "gemm.ptx");
test_kernel!(test_fncall, "fncall.ptx");
test_kernel!(test_transpose, "transpose.ptx");

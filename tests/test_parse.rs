use ptx_ir::Module;
use std::path::PathBuf;

mod triton {
    use super::*;
    use paste::paste;
    const KERNEL_PATH: &str = "tests/kernels/simple";
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
    test_kernel!(flashattention_bwd);
    test_kernel!(add_simple);
    test_kernel!(copy);
    test_kernel!(gemm);
    test_kernel!(fncall);
    test_kernel!(transpose);
    test_kernel!(transform);
    test_kernel!(times_two);
    test_kernel!(matmul);
    test_kernel!(linear);
    test_kernel!(fused_bias_gelu_fwd);
    test_kernel!(fused_bias_gelu_bwd);
    test_kernel!(sum);
    test_kernel!(simple_block_dot);
    test_kernel!(test_kernel);
}

const KERNEL_PATH: &str = "tests/kernels/";
fn test_directory(path: &str) {
    let path = PathBuf::from(KERNEL_PATH).join(path);
    for entry in std::fs::read_dir(path).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() && path.extension().unwrap() == "ptx" {
            println!("Testing {}", path.display());
            let result = Module::from_ptx_path(&path);
            assert!(result.is_ok(), "{}", result.unwrap_err());
        }
    }
}

mod gpuocelot {
    use super::test_directory;
    #[test]
    fn test_parboil() {
        test_directory("gpuocelot/parboil");
    }

    #[test]
    fn test_rodinia() {
        test_directory("gpuocelot/rodinia");
    }

    #[test]
    fn test_sdk2_2() {
        test_directory("gpuocelot/sdk2.2");
    }

    #[test]
    fn test_shifts() {
        test_directory("gpuocelot/shifts");
    }

    #[test]
    fn test_synthetic() {
        test_directory("gpuocelot/synthetic");
    }

    #[test]
    fn test_thrust() {
        test_directory("gpuocelot/thrust");
    }

    #[test]
    fn test_unstructured() {
        test_directory("gpuocelot/unstructured");
    }
}

fn main() {
    cc::Build::new()
        .file("runtime/pavo_runtime.c")
        .compile("pavo_runtime");
}

fn main() {
    // This is needed so that snapshot tests will be rerun
    // when changes to the inputs are made.
    println!("cargo::rerun-if-changed=test/inputs")
}

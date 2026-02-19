pub trait Compilable {
    fn compile(&self) -> Result<String, String>;
}


#[derive(PartialEq, Eq, Debug)]
pub struct TestStruct;

impl TestStruct {
    #[allow(dead_code)]
    pub fn foo_add(a: u64, b: u64) -> u64 {
        a + b
    }
}

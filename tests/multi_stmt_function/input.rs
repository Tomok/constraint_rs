#[derive(PartialEq, Eq, Debug)]
pub struct TestStruct;

impl TestStruct {
    #[allow(dead_code)]
    pub fn add(a: u64, b: u64) -> u64 {
        let result = a + b;
        result
    }
}

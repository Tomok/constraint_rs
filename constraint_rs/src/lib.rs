pub type Context = z3::Context;
pub type DataType<'ctx> = z3::DatatypeSort<'ctx>;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}

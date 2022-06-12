#[cfg(test)]
mod tests {

    mod empty_struct {
        use constraint_rs_derive::ConstraintType;

        #[derive(ConstraintType)]
        struct TestStruct {}

        #[test]
        fn test() {
            let cfg = z3::Config::new();
            let ctx = constraint_rs::Context::new(&cfg);
            let ct = TestStruct::constraint_type(&ctx);
            // currently no functions on ct ... but being able to create it is also a good test
        }
    }
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}

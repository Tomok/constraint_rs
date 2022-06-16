#[cfg(test)]
mod tests {

    mod empty_struct {
        use constraint_rs_derive::ConstraintType;

        use constraint_rs::HasConstrainedType;

        #[derive(ConstraintType)]
        struct TestStruct {}

        #[test]
        fn test() {
            let cfg = z3::Config::new();
            let z3_context = z3::Context::new(&cfg);
            let context = constraint_rs::Context::new(&z3_context);
            {
                let ct = TestStruct::constrained_type(&context);
                drop(ct);
            } // currently no functions on ct ... but being able to create it is also a good test
        }
    }
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}

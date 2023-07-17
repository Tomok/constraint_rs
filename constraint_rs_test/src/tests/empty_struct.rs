use constraint_rs_derive::constrained_mod;

    use constraint_rs::{ConstrainedType, ConstrainedValue, HasConstrainedType};

    #[constrained_mod]
    mod test_struct {
        #[derive(PartialEq, Eq, Debug)]
        pub struct TestStruct;
    }
    use test_struct::*;

    #[test]
    fn test() {
        let cfg = z3::Config::new();
        let z3_context = z3::Context::new(&cfg);
        let context = constraint_rs::Context::new(&z3_context);
        let constrained_type = TestStruct::constrained_type(&context);
        let constrained_value = constrained_type.fresh_value("v");
        //todo: should not be necessary to call z3 directly in the future...
        let solver = z3::Solver::new(&z3_context);
        assert_eq!(z3::SatResult::Sat, solver.check());
        let model = solver.get_model().unwrap();
        let value = constrained_value.eval(&model).unwrap();
        assert_eq!(TestStruct, value);
    }

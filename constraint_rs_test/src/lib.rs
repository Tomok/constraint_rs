#[cfg(test)]
mod tests {

    mod empty_struct {
        use constraint_rs_derive::ConstrainedType;

        use constraint_rs::{ConstrainedType, ConstrainedValue, HasConstrainedType};

        #[derive(ConstrainedType, PartialEq, Eq, Debug)]
        pub struct TestStruct;

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
    }
    mod one_field_struct {
        use constraint_rs_derive::ConstrainedType;

        use constraint_rs::{ConstrainedType, ConstrainedValue, HasConstrainedType};

        #[derive(ConstrainedType, PartialEq, Eq, Debug)]
        pub struct TestStruct {
            field1: u32,
        }

        #[test]
        fn test() {
            let cfg = z3::Config::new();
            let z3_context = z3::Context::new(&cfg);
            let context = constraint_rs::Context::new(&z3_context);
            let constrained_type = TestStruct::constrained_type(&context);
            let constrained_value = constrained_type.fresh_value("v");
            let solver = z3::Solver::new(&z3_context);
            constrained_value.assign_value(&solver, &TestStruct { field1: 7 });

            //todo: should not be necessary to call z3 directly in the future...
            assert_eq!(z3::SatResult::Sat, solver.check());
            let model = solver.get_model().unwrap();
            let value = constrained_value.eval(&model).unwrap();
            assert_eq!(TestStruct { field1: 7 }, value);
        }

        #[test]
        fn test_eq() {
            let cfg = z3::Config::new();
            let z3_context = z3::Context::new(&cfg);
            let context = constraint_rs::Context::new(&z3_context);
            let constrained_type = TestStruct::constrained_type(&context);
            let constrained_value = constrained_type.fresh_value("v");
            //todo: should not be necessary to call z3 directly in the future...
            let solver = z3::Solver::new(&z3_context);
            let constrained_u32_type = u32::constrained_type(&context);
            let value7 = constrained_u32_type.fresh_value("value7");
            value7.assign_value(&solver, &7u32);

            solver.assert(constrained_value.field1._eq(&value7).val());
            assert_eq!(z3::SatResult::Sat, solver.check());
            let model = solver.get_model().unwrap();
            let value = constrained_value.eval(&model).unwrap();
            assert_eq!(TestStruct { field1: 7 }, value);
        }
    }
}

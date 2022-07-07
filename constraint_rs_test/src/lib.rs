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
        use z3::ast::Ast;

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
            //todo: should not be necessary to call z3 directly in the future...
            let solver = z3::Solver::new(&z3_context);
            //todo add asserts that make field1 7 here
            let val = constrained_value.field1.val();
            solver.assert(
                &val._safe_eq(&z3::ast::BV::from_u64(&z3_context, 7, 32))
                    .unwrap(),
            );
            /*let f_accessor = typ.data_type.0.variants[0].accessors[0].apply(&[&val1.val]);
            solver.assert(
                &f_accessor
                    .as_bv()
                    .unwrap()
                    ._safe_eq(&z3::ast::BV::from_u64(&ctx, 42, 64))
                    .unwrap(),
            );*/

            assert_eq!(z3::SatResult::Sat, solver.check());
            let model = solver.get_model().unwrap();
            let value = constrained_value.eval(&model).unwrap();
            assert_eq!(TestStruct { field1: 7 }, value);
        }
    }
}

// generated code should use full paths, including constraint_rs::
// so it needs to be available within this libary as well
use crate as constraint_rs;

// Struct for which the derived types and functions are listed below & used for this test
include!("../../../tests/multi_stmt_function/input.rs");
include!("../../../tests/multi_stmt_function/expected_output.rs");

mod test_cases {
    //sub-module to import traits for testcases only not for included code

    use std::ptr;
    use z3::ast::Ast;

    use super::*;
    use constraint_rs::{
        ConstrainedType, ConstrainedValue, HasConstrainedType, HasSimpleConstrainedType,
    };

    #[test]
    fn test() {
        let cfg = z3::Config::new();
        let z3_context = z3::Context::new(&cfg);
        let context = constraint_rs::Context::new(&z3_context);
        let constrained_type =
            <TestStruct as constraint_rs::HasConstrainedType>::constrained_type(&context);
        let constrained_value = constrained_type.fresh_value("v");
        //todo: should not be necessary to call z3 directly in the future...
        let solver = z3::Solver::new(&z3_context);
        assert_eq!(z3::SatResult::Sat, solver.check());
        let model = solver.get_model().unwrap();
        let value = constrained_value.eval(&model).unwrap();
        assert_eq!(TestStruct, value);
    }

    #[test]
    fn test_add() {
        let cfg = z3::Config::new();
        let z3_context = z3::Context::new(&cfg);
        let context = constraint_rs::Context::new(&z3_context);
        let constrained_type =
            <TestStruct as constraint_rs::HasConstrainedType>::constrained_type(&context);
        let constrained_value = constrained_type.fresh_value("v");
        //todo: should not be necessary to call z3 directly in the future...
        let solver = z3::Solver::new(&z3_context);
        let v1 = u64::constrained_type(&context).fresh_value("v1");
        v1.assign_value(&solver, &1);
        let v4 = u64::constrained_type(&context).fresh_value("v4");
        v4.assign_value(&solver, &4);
        let sum = u64::constrained_type(&context).fresh_value("sum");
        solver.assert(sum._eq(&constrained_value.add(&v1, &v4)).z3());
        assert_eq!(z3::SatResult::Sat, solver.check());
        let model = solver.get_model().unwrap();
        let sum_value = sum.eval(&model).unwrap();
        assert_eq!(5, sum_value);
        let v_unknown = u64::constrained_type(&context).fresh_value("v_unknown");
        let sum2 = constrained_value.add(&v_unknown, &v1);
        solver.assert(sum2._eq(&v4).z3());
        assert_eq!(z3::SatResult::Sat, solver.check());
        let model = solver.get_model().unwrap();
        assert_eq!(3, v_unknown.eval(&model).unwrap());
    }

    #[test]
    fn create_constrained_datatypes() {
        let config = z3::Config::new();
        let ctx = z3::Context::new(&config);
        let context = constraint_rs::Context::new(&ctx);
        let ts1 = TestStructConstrainedType::new(&context);
        let ts2 = TestStructConstrainedType::new(&context);
        assert!(ptr::eq(ts1.data_type.0.as_ref(), ts2.data_type.0.as_ref()));
    }

    #[test]
    fn has_constrained_datatype() {
        let config = z3::Config::new();
        let ctx = z3::Context::new(&config);
        let context = constraint_rs::Context::new(&ctx);
        let ts1 = <TestStruct as HasConstrainedType>::constrained_type(&context);
        let ts2 = <TestStruct as HasConstrainedType>::constrained_type(&context);
        assert!(ptr::eq(ts1.data_type.0.as_ref(), ts2.data_type.0.as_ref()));
    }

    #[test]
    fn constrained_value() {
        let config = z3::Config::new();
        let ctx = z3::Context::new(&config);
        let context = constraint_rs::Context::new(&ctx);
        let typ = <TestStruct as HasConstrainedType>::constrained_type(&context);
        let val1 = typ.fresh_value("val1");
        let solver = z3::Solver::new(&ctx); //todo - do not call z3 directly once corresponding methods was implemented
        assert_eq!(z3::SatResult::Sat, solver.check());
        let model = solver.get_model().unwrap();
        assert_eq!(Some(TestStruct), val1.eval(&model));

        //assert something impossible (TestStruct type != empty type) to se solver failing
        let val2 = typ.fresh_value("val2");
        solver.assert(&val2.val._safe_eq(&val1.val).expect("Type missmatch").not());
        assert_eq!(z3::SatResult::Unsat, solver.check());
        assert!(solver.get_model().is_none());
    }

    #[test]
    fn test_constrained_value_add() {
        let config = z3::Config::new();
        let ctx = z3::Context::new(&config);
        let context = constraint_rs::Context::new(&ctx);
        let a = 40u64.constrained(context.z3_context());
        let b = 2u64.constrained(context.z3_context());
        let ty = TestStructConstrainedType::new(&context);
        let to = ty.fresh_value("test_object");
        let add_res = to.add(&a, &b);
        let add_res_expected = 42u64.constrained(context.z3_context());

        let solver = z3::Solver::new(&ctx); //todo - do not call z3 directly once corresponding methods was implemented
        solver.assert(add_res._eq(&add_res_expected).z3());
        assert_eq!(z3::SatResult::Sat, solver.check());
    }
}

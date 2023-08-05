// generated code should use full paths, including constraint_rs::
// so it needs to be available within this libary as well
use crate as constraint_rs;

/// Struct for which the derived types and functions are listed below & used for this test
include!("../../tests/struct_with_one_field/input.rs");
include!("../../tests/struct_with_one_field/expected_output.rs");

mod test_cases {
    //sub-module to import traits for testcases only not for included code

    use std::ptr;
    use z3::ast::Ast;

    use super::*;
    use constraint_rs::{ConstrainedType, ConstrainedValue, HasSimpleConstrainedType};

    #[test]
    fn create_constrained_datatypes() {
        let config = z3::Config::new();
        let ctx = z3::Context::new(&config);
        let context = constraint_rs::Context::new(&ctx);
        let ts1 = SConstrainedType::new(&context);
        let ts2 = SConstrainedType::new(&context);
        assert!(ptr::eq(ts1.data_type.0.as_ref(), ts2.data_type.0.as_ref()));
    }

    #[test]
    fn has_constrained_datatype() {
        let config = z3::Config::new();
        let ctx = z3::Context::new(&config);
        let context = constraint_rs::Context::new(&ctx);
        let ts1 = <S as constraint_rs::HasConstrainedType>::constrained_type(&context);
        let ts2 = <S as constraint_rs::HasConstrainedType>::constrained_type(&context);
        assert!(ptr::eq(ts1.data_type.0.as_ref(), ts2.data_type.0.as_ref()));
    }

    #[test]
    fn constrained_value() {
        let config = z3::Config::new();
        let ctx = z3::Context::new(&config);
        let context = constraint_rs::Context::new(&ctx);
        let typ = <S as constraint_rs::HasConstrainedType>::constrained_type(&context);
        let val1 = typ.fresh_value("val1");
        let solver = z3::Solver::new(&ctx); //todo - do not call z3 directly once corresponding methods was implemented
        let f_accessor = typ.data_type.0.variants[0].accessors[0].apply(&[&val1.val]);
        solver.assert(
            &f_accessor
                .as_bv()
                .unwrap()
                ._safe_eq(&z3::ast::BV::from_u64(&ctx, 42, 64))
                .unwrap(),
        );
        assert_eq!(z3::SatResult::Sat, solver.check());
        let model = solver.get_model().unwrap();
        assert_eq!(Some(S { f: 42 }), val1.eval(&model));
    }

    /*#[test]
    fn func1() {
        let config = z3::Config::new();
        let ctx = z3::Context::new(&config);
        let context = constraint_rs::Context::new(&ctx);
        let typ = <S as constraint_rs::HasConstrainedType>::constrained_type(&context);
        let val1 = typ.fresh_value("val1");
        let solver = z3::Solver::new(&ctx); //todo - do not call z3 directly once corresponding methods was implemented
        solver.assert(
            val1.func1()
                ._eq(&42u64.constrained(context.z3_context()))
                .z3(),
        );
        assert_eq!(z3::SatResult::Sat, solver.check());
        let model = solver.get_model().unwrap();
        assert_eq!(Some(S { f: 42 }), val1.eval(&model));
    }*/
}

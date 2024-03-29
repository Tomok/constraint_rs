use constraint_rs_derive::ConstrainedType;

use constraint_rs::{ConstrainedType, ConstrainedValue};

#[derive(ConstrainedType, PartialEq, Eq, Debug)]
pub struct TestStruct {
    field1: u64,
    field2: u64,
}

#[test]
fn test() {
    let cfg = z3::Config::new();
    let z3_context = z3::Context::new(&cfg);
    let context = constraint_rs::Context::new(&z3_context);
    let constrained_type =
        <TestStruct as constraint_rs::HasConstrainedType>::constrained_type(&context);
    let constrained_value = constrained_type.fresh_value("v");
    let solver = z3::Solver::new(&z3_context);

    constrained_value.field1.assign_value(&solver, &42u64);
    solver.assert(
        constrained_value
            .field1
            ._eq(&constrained_value.field2)
            .val(),
    );

    //todo: should not be necessary to call z3 directly in the future...
    assert_eq!(z3::SatResult::Sat, solver.check());
    let model = solver.get_model().unwrap();
    let value = constrained_value.eval(&model).unwrap();
    assert_eq!(
        TestStruct {
            field1: 42,
            field2: 42
        },
        value
    );
}

#[test]
fn create_datatype_twice() {
    let cfg = z3::Config::new();
    let z3_context = z3::Context::new(&cfg);
    let context = constraint_rs::Context::new(&z3_context);
    let constrained_type1 =
        <TestStruct as constraint_rs::HasConstrainedType>::constrained_type(&context);
    let constrained_type2 =
        <TestStruct as constraint_rs::HasConstrainedType>::constrained_type(&context);
    let constrained_value1 = constrained_type1.fresh_value("v1");
    let constrained_value2 = constrained_type2.fresh_value("v2");
    let solver = z3::Solver::new(&z3_context);

    constrained_value1.assign_value(
        &solver,
        &TestStruct {
            field1: 1,
            field2: 2,
        },
    );
    solver.assert(constrained_value2._eq(&constrained_value1).val());
    //todo: should not be necessary to call z3 directly in the future...
    assert_eq!(z3::SatResult::Sat, solver.check());
    let model = solver.get_model().unwrap();
    let value = constrained_value2.eval(&model).unwrap();
    assert_eq!(
        TestStruct {
            field1: 1,
            field2: 2
        },
        value
    );
}

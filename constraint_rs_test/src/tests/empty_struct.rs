use constraint_rs_derive::constrained_mod;

use constraint_rs::{ConstrainedType, ConstrainedValue, HasConstrainedType};

#[constrained_mod]
mod test_struct {
    use constraint_rs::{ConstrainedType, ConstrainedValue};

    #[derive(PartialEq, Eq, Debug)]
    pub struct TestStruct;

    impl TestStruct {
        #[allow(dead_code)]
        pub fn add(a: u64, b: u64) -> u64 {
            a + b
        }
    }
}
use test_struct::*;

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

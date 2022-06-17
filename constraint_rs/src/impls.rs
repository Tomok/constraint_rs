use z3::ast;

use super::*;
pub struct U64ConstrainedType<'s, 'ctx> {
    context: &'s Context<'ctx>,
}

impl<'s, 'ctx> ConstrainedType<'s, 'ctx> for U64ConstrainedType<'s, 'ctx>
where
    'ctx: 's,
{
    type ValueType = U64ConstrainedValue<'ctx>;

    fn new(context: &'s Context<'ctx>) -> Self {
        Self { context }
    }

    fn fresh_value(&'s self, name_prefix: &str) -> Self::ValueType {
        let val = ast::BV::fresh_const(self.context.z3_context(), name_prefix, 64);
        Self::ValueType { val }
    }
}

impl<'s, 'ctx> HasConstrainedType<'s, 'ctx> for u64
where
    'ctx: 's,
{
    type ConstrainedType = U64ConstrainedType<'s, 'ctx>;
}

pub struct U64ConstrainedValue<'ctx> {
    val: z3::ast::BV<'ctx>,
}

impl<'s, 'ctx> ConstrainedValue<'s, 'ctx> for U64ConstrainedValue<'ctx>
where
    'ctx: 's,
{
    type ValueType = u64;

    fn eval(&'s self, model: &Model<'ctx>) -> Option<Self::ValueType> {
        model.eval(&self.val.to_int(false), false).unwrap().as_u64()
    }
}

#[cfg(test)]
mod test {
    use z3::ast::Ast;

    use super::*;
    #[test]
    fn test_u64() {
        let cfg = z3::Config::new();
        let z3_context = z3::Context::new(&cfg);
        let context = Context::new(&z3_context);
        let constrained_type = u64::constrained_type(&context);
        let constrained_value = constrained_type.fresh_value("v");
        //todo: should not be necessary to call z3 directly in the future...
        let solver = z3::Solver::new(&z3_context);
        solver.assert(
            &constrained_value
                .val
                ._safe_eq(&ast::BV::from_u64(&z3_context, 42, 64))
                .unwrap(),
        );
        assert_eq!(z3::SatResult::Sat, solver.check());
        let model = solver.get_model().unwrap();
        let value = constrained_value.eval(&model).unwrap();
        assert_eq!(42, value);
    }
}

use super::super::*;
use super::BoolConstrainedValue;
use z3::ast;

macro_rules! int_impl {
    ($ValueType:ident, $ConstrainedType:ident, $ConstrainedValue:ident, $bits:literal, $signed:ident) => {
        impl<'s, 'ctx> HasConstrainedType<'s, 'ctx> for $ValueType
        where
            'ctx: 's,
        {
            type ConstrainedType = $ConstrainedType<'s, 'ctx>;
        }

        impl<'s, 'ctx> HasSimpleConstrainedType<'s, 'ctx> for $ValueType
        where
            'ctx: 's,
        {
            type ConstrainedType = $ConstrainedType<'s, 'ctx>;

            fn constrained(
                &self,
                context: &'ctx z3::Context,
            ) -> <Self::ConstrainedType as ConstrainedType<'s, 'ctx>>::ValueType {
                let val = from_x64!($signed, context, *self, $bits);
                $ConstrainedValue { val }
            }
        }

        pub struct $ConstrainedType<'s, 'ctx> {
            context: &'s Context<'ctx>,
            data_type_sort: z3::Sort<'ctx>,
        }

        impl<'s, 'ctx> ConstrainedType<'s, 'ctx> for $ConstrainedType<'s, 'ctx>
        where
            'ctx: 's,
        {
            type ValueType = $ConstrainedValue<'ctx>;

            fn new(context: &'s Context<'ctx>) -> Self {
                let data_type_sort = z3::Sort::bitvector(context.z3_context(), $bits);
                Self {
                    context,
                    data_type_sort,
                }
            }

            fn fresh_value(&'s self, name_prefix: &str) -> Self::ValueType {
                let val = ast::BV::fresh_const(self.context.z3_context(), name_prefix, $bits);
                Self::ValueType { val }
            }

            fn value_from_z3_dynamic(
                &'s self,
                val: z3::ast::Dynamic<'ctx>,
            ) -> Option<Self::ValueType> {
                Some(Self::ValueType { val: val.as_bv()? })
            }

            fn z3_sort(&'s self) -> &'s z3::Sort<'ctx> {
                &self.data_type_sort
            }
        }

        pub struct $ConstrainedValue<'ctx> {
            val: z3::ast::BV<'ctx>,
        }

        impl<'s, 'ctx> ConstrainedValue<'s, 'ctx> for $ConstrainedValue<'ctx>
        where
            'ctx: 's,
        {
            type ValueType = $ValueType;

            fn eval(&'s self, model: &Model<'ctx>) -> Option<Self::ValueType> {
                let a = model
                    .eval(&self.val.to_int(is_signed!($signed)), false)
                    .unwrap();
                let b = as_x64!(a, $signed);
                b?.try_into().ok()
            }

            fn _eq(&'s self, other: &'s Self) -> BoolConstrainedValue {
                ast::Ast::_eq(&self.val, &other.val).into()
            }

            fn assign_value(&'s self, solver: &Solver<'ctx>, value: &Self::ValueType) {
                solver.assert(self._eq(&value.constrained(solver.get_context())).val());
            }
        }

        impl<'ctx> $ConstrainedValue<'ctx> {
            //todo: temporary for tests ... to be replaced when other functions for defining
            //constraints were added
            pub fn val(&self) -> &z3::ast::BV<'ctx> {
                &self.val
            }
        }
    };
}

impl<'ctx> U64ConstrainedValue<'ctx> {}

macro_rules! is_signed {
    (signed) => {
        true
    };
    (unsigned) => {
        false
    };
}

macro_rules! as_x64 {
    ($v:ident, unsigned) => {
        $v.as_u64()
    };
    ($v:ident, signed) => {
        $v.as_i64()
    };
}
macro_rules! from_x64 {
    (unsigned, $context:expr, $val:expr, $bits:literal) => {
        z3::ast::BV::from_u64($context, $val.into(), $bits)
    };
    (signed, $context:expr, $val:expr, $bits:literal) => {
        z3::ast::BV::from_i64($context, $val.into(), $bits)
    };
}

int_impl!(u8, U8ConstrainedType, U8ConstrainedValue, 8, unsigned);
int_impl!(i8, I8ConstrainedType, I8ConstrainedValue, 8, signed);
int_impl!(u16, U16ConstrainedType, U16ConstrainedValue, 16, unsigned);
int_impl!(i16, I16ConstrainedType, I16ConstrainedValue, 16, signed);
int_impl!(u32, U32ConstrainedType, U32ConstrainedValue, 32, unsigned);
int_impl!(i32, I32ConstrainedType, I32ConstrainedValue, 32, signed);
int_impl!(u64, U64ConstrainedType, U64ConstrainedValue, 64, unsigned);
int_impl!(i64, I64ConstrainedType, I64ConstrainedValue, 64, signed);

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

    #[test]
    fn test_u8() {
        let cfg = z3::Config::new();
        let z3_context = z3::Context::new(&cfg);
        let context = Context::new(&z3_context);
        let constrained_type = u8::constrained_type(&context);
        let constrained_value = constrained_type.fresh_value("v");
        //todo: should not be necessary to call z3 directly in the future...
        let solver = z3::Solver::new(&z3_context);
        solver.assert(
            &constrained_value
                .val
                ._safe_eq(&ast::BV::from_u64(&z3_context, 42, 8))
                .unwrap(),
        );
        assert_eq!(z3::SatResult::Sat, solver.check());
        let model = solver.get_model().unwrap();
        let value = constrained_value.eval(&model).unwrap();
        assert_eq!(42u8, value);
    }

    #[test]
    fn test_i8() {
        let cfg = z3::Config::new();
        let z3_context = z3::Context::new(&cfg);
        let context = Context::new(&z3_context);
        let constrained_type = i8::constrained_type(&context);
        let constrained_value = constrained_type.fresh_value("v");
        //todo: should not be necessary to call z3 directly in the future...
        let solver = z3::Solver::new(&z3_context);
        solver.assert(
            &constrained_value
                .val
                ._safe_eq(&ast::BV::from_i64(&z3_context, -42, 8))
                .unwrap(),
        );
        assert_eq!(z3::SatResult::Sat, solver.check());
        let model = solver.get_model().unwrap();
        dbg!(&model);
        let value = constrained_value.eval(&model).unwrap();
        assert_eq!(-42i8, value);
    }
}

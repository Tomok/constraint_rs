use std::{cell::RefCell, collections::HashMap, rc::Rc};

use impls::BoolConstrainedValue;

pub mod impls;

pub trait HasConstrainedType<'s, 'ctx>
where
    'ctx: 's,
{
    type ConstrainedType: ConstrainedType<'s, 'ctx>;

    fn constrained_type(context: &'s Context<'ctx>) -> Self::ConstrainedType {
        Self::ConstrainedType::new(context)
    }
}

/// Types with this trait are simple enough, that they can be directly converted
/// into constrained value objects, without having to go through a
pub trait HasSimpleConstrainedType<'s, 'ctx>
where
    'ctx: 's,
{
    type ConstrainedType: ConstrainedType<'s, 'ctx>;

    fn constrained(
        &'s self,
        context: &'ctx z3::Context, //todo: is it a good idea to pass a z3::Context here?
    ) -> <Self::ConstrainedType as ConstrainedType<'s, 'ctx>>::ValueType;
}

pub trait ConstrainedType<'s, 'ctx>
where
    'ctx: 's,
{
    type ValueType: ConstrainedValue<'s, 'ctx> + Sized;

    fn new(context: &'s Context<'ctx>) -> Self;
    fn fresh_value(&'s self, name_prefix: &str) -> Self::ValueType;
    fn value_from_z3_dynamic(&'s self, val: z3::ast::Dynamic<'ctx>) -> Option<Self::ValueType>;

    fn z3_sort(&'s self) -> &'s z3::Sort<'ctx>;
}

/// A constant variable to be defined via constraints
pub trait ConstrainedValue<'s, 'ctx>
where
    'ctx: 's,
{
    type ValueType: HasConstrainedType<'s, 'ctx>;

    ///get a possible set of values from the given Model
    fn eval(&'s self, model: &Model<'ctx>) -> Option<Self::ValueType>;

    //todo: add functions to iter all solutions

    // comparison functions
    fn _eq(&'s self, other: &'s Self) -> BoolConstrainedValue;

    /**
     * Constrain this object to match the given value
     **/
    fn assign_value(&'s self, solver: &Solver<'ctx>, value: &Self::ValueType);
}

pub type Model<'ctx> = z3::Model<'ctx>;
pub type Solver<'ctx> = z3::Solver<'ctx>;

#[derive(Clone)]
pub struct DataType<'ctx>(Rc<z3::DatatypeSort<'ctx>>);

impl<'ctx> DataType<'ctx> {
    fn new(dt: z3::DatatypeSort<'ctx>) -> Self {
        Self(Rc::new(dt))
    }

    /// get the [z3::DatatypeSort] this should not be necessary to be called outside of generated
    /// code
    pub fn z3_datatype_sort(&self) -> &Rc<z3::DatatypeSort<'ctx>> {
        &self.0
    }
}

pub struct Context<'ctx> {
    z3_context: &'ctx z3::Context,
    registered_datatypes: RefCell<HashMap<&'static str, DataType<'ctx>>>,
}

impl<'ctx> Context<'ctx> {
    pub fn new(z3_context: &'ctx z3::Context) -> Self {
        let registered_datatypes = RefCell::new(HashMap::new());
        Self {
            z3_context,
            registered_datatypes,
        }
    }

    pub fn enter_or_get_datatype<'s, F>(
        &'s self,
        name: &'static str,
        datatype_build_fn: F,
    ) -> DataType<'ctx>
    where
        F: FnOnce(&'ctx z3::Context) -> z3::DatatypeSort<'ctx>,
    {
        self.registered_datatypes
            .borrow_mut()
            .entry(name)
            .or_insert_with(|| DataType::new(datatype_build_fn(self.z3_context)))
            .clone()
    }

    /// get the [z3::Context] ... this should only be necessary if you do not use the dervice
    /// mechanisms and want to implement a [DataType]
    pub fn z3_context(&self) -> &'ctx z3::Context {
        self.z3_context
    }
}

#[cfg(test)]
mod tests {
    use std::ptr;

    use super::*;

    #[test]
    fn enter_or_get_datatype() {
        let config = z3::Config::new();
        let ctx = z3::Context::new(&config);
        let context = Context::new(&ctx);
        let dt1 = context.enter_or_get_datatype("test_data_type", |c| {
            z3::DatatypeBuilder::new(c, "test_data_type")
                .variant("", vec![])
                .finish()
        });
        let dt1_again = context.enter_or_get_datatype("test_data_type", |c| {
            z3::DatatypeBuilder::new(c, "test_data_type")
                .variant("", vec![])
                .finish()
        });
        assert!(ptr::eq(dt1.0.as_ref(), dt1_again.0.as_ref()));
        let dt2 = context.enter_or_get_datatype("test_data_type2", |c| {
            z3::DatatypeBuilder::new(c, "test_data_type2")
                .variant("", vec![])
                .finish()
        });
        assert!(!ptr::eq(dt1.0.as_ref(), dt2.0.as_ref()));
    }

    #[allow(unused)]
    mod empty_struct_derived_code_test {
        use std::ptr;

        use z3::ast::Ast;

        use super::super::*;
        /// Struct for which the derived types and functions are listed below & used for this test
        #[derive(Debug, PartialEq, Eq)]
        pub struct Empty();

        pub struct EmptyConstrainedType<'s, 'ctx> {
            context: &'s Context<'ctx>,
            data_type: DataType<'ctx>,
        }

        impl<'s, 'ctx> ConstrainedType<'s, 'ctx> for EmptyConstrainedType<'s, 'ctx>
        where
            'ctx: 's,
        {
            type ValueType = EmptyConstrainedValue<'s, 'ctx>;

            fn new(context: &'s Context<'ctx>) -> Self {
                let data_type = context.enter_or_get_datatype("TestStruct", |c| {
                    z3::DatatypeBuilder::new(c, "TestStruct")
                        .variant("", vec![])
                        .finish()
                });
                Self { context, data_type }
            }

            fn fresh_value(&'s self, name_prefix: &str) -> Self::ValueType {
                let val = z3::ast::Datatype::fresh_const(
                    self.context.z3_context(),
                    name_prefix,
                    &self.data_type.z3_datatype_sort().sort,
                );
                self.value_from_z3_dynamic(z3::ast::Dynamic::from_ast(&val))
                    .unwrap()
            }

            fn value_from_z3_dynamic(
                &'s self,
                val: z3::ast::Dynamic<'ctx>,
            ) -> Option<Self::ValueType> {
                Some(Self::ValueType {
                    val: val.as_datatype()?,
                    typ: self,
                })
            }

            fn z3_sort(&'s self) -> &'s z3::Sort<'ctx> {
                &self.data_type.z3_datatype_sort().sort
            }
        }

        #[test]
        fn create_constrained_datatypes() {
            let config = z3::Config::new();
            let ctx = z3::Context::new(&config);
            let context = Context::new(&ctx);
            let ts1 = EmptyConstrainedType::new(&context);
            let ts2 = EmptyConstrainedType::new(&context);
            assert!(ptr::eq(ts1.data_type.0.as_ref(), ts2.data_type.0.as_ref()));
        }

        impl<'s, 'ctx> HasConstrainedType<'s, 'ctx> for Empty
        where
            'ctx: 's,
        {
            type ConstrainedType = EmptyConstrainedType<'s, 'ctx>;
        }

        #[test]
        fn has_constrained_datatype() {
            let config = z3::Config::new();
            let ctx = z3::Context::new(&config);
            let context = Context::new(&ctx);
            let ts1 = Empty::constrained_type(&context);
            let ts2 = Empty::constrained_type(&context);
            assert!(ptr::eq(ts1.data_type.0.as_ref(), ts2.data_type.0.as_ref()));
        }

        pub struct EmptyConstrainedValue<'s, 'ctx> {
            typ: &'s EmptyConstrainedType<'s, 'ctx>,
            val: z3::ast::Datatype<'ctx>,
        }

        impl<'s, 'ctx> ConstrainedValue<'s, 'ctx> for EmptyConstrainedValue<'s, 'ctx>
        where
            'ctx: 's,
        {
            type ValueType = Empty;

            fn eval(&'s self, model: &Model<'ctx>) -> Option<Self::ValueType> {
                // as the type is empty, it is always the following, as long as the model exists
                Some(Empty())
            }

            fn _eq(&'s self, other: &'s Self) -> BoolConstrainedValue {
                z3::ast::Ast::_eq(&self.val, &other.val).into()
            }

            fn assign_value(&'s self, solver: &Solver<'ctx>, value: &Self::ValueType) {
                //nothing to do here
            }
        }

        #[test]
        fn constrained_value() {
            let config = z3::Config::new();
            let ctx = z3::Context::new(&config);
            let context = Context::new(&ctx);
            let typ = Empty::constrained_type(&context);
            let val1 = typ.fresh_value("val1");
            let solver = z3::Solver::new(&ctx); //todo - do not call z3 directly once corresponding methods was implemented
            assert_eq!(z3::SatResult::Sat, solver.check());
            let model = solver.get_model().unwrap();
            assert_eq!(Some(Empty()), val1.eval(&model));

            //assert something impossible (Empty type != empty type) to se solver failing
            let val2 = typ.fresh_value("val2");
            solver.assert(&val2.val._safe_eq(&val1.val).expect("Type missmatch").not());
            assert_eq!(z3::SatResult::Unsat, solver.check());
            assert!(solver.get_model().is_none());
        }
    }

    #[allow(unused)]
    mod struct_derived_code_test {
        use std::ptr;

        use z3::ast::Ast;

        use crate::impls::U64ConstrainedValue;

        use super::super::*;
        /// Struct for which the derived types and functions are listed below & used for this test
        #[derive(Debug, PartialEq, Eq)]
        pub struct S {
            f: u64,
        }

        pub struct SConstrainedType<'s, 'ctx> {
            context: &'s Context<'ctx>,
            data_type: DataType<'ctx>,
        }

        impl<'s, 'ctx> ConstrainedType<'s, 'ctx> for SConstrainedType<'s, 'ctx>
        where
            'ctx: 's,
        {
            type ValueType = SConstrainedValue<'s, 'ctx>;

            fn new(context: &'s Context<'ctx>) -> Self {
                let fields = vec![(
                    "f",
                    z3::DatatypeAccessor::Sort(u64::constrained_type(context).z3_sort().clone()),
                )];
                let data_type = context.enter_or_get_datatype("S", |c| {
                    z3::DatatypeBuilder::new(c, "S")
                        .variant("", fields)
                        .finish()
                });
                Self { context, data_type }
            }

            fn fresh_value(&'s self, name_prefix: &str) -> Self::ValueType {
                let val = z3::ast::Datatype::fresh_const(
                    self.context.z3_context(),
                    name_prefix,
                    &self.data_type.z3_datatype_sort().sort,
                );
                self.value_from_z3_dynamic(z3::ast::Dynamic::from_ast(&val))
                    .unwrap()
            }

            fn value_from_z3_dynamic(
                &'s self,
                val: z3::ast::Dynamic<'ctx>,
            ) -> Option<Self::ValueType> {
                let f = u64::constrained_type(self.context).value_from_z3_dynamic(
                    self.data_type.0.variants[0].accessors[0].apply(&[&val]),
                )?;
                Some(Self::ValueType {
                    val: val.as_datatype()?,
                    f,
                })
            }

            fn z3_sort(&'s self) -> &'s z3::Sort<'ctx> {
                &self.data_type.z3_datatype_sort().sort
            }
        }

        #[test]
        fn create_constrained_datatypes() {
            let config = z3::Config::new();
            let ctx = z3::Context::new(&config);
            let context = Context::new(&ctx);
            let ts1 = SConstrainedType::new(&context);
            let ts2 = SConstrainedType::new(&context);
            assert!(ptr::eq(ts1.data_type.0.as_ref(), ts2.data_type.0.as_ref()));
        }

        impl<'s, 'ctx> HasConstrainedType<'s, 'ctx> for S
        where
            'ctx: 's,
        {
            type ConstrainedType = SConstrainedType<'s, 'ctx>;
        }

        #[test]
        fn has_constrained_datatype() {
            let config = z3::Config::new();
            let ctx = z3::Context::new(&config);
            let context = Context::new(&ctx);
            let ts1 = S::constrained_type(&context);
            let ts2 = S::constrained_type(&context);
            assert!(ptr::eq(ts1.data_type.0.as_ref(), ts2.data_type.0.as_ref()));
        }

        pub struct SConstrainedValue<'s, 'ctx> {
            val: z3::ast::Datatype<'ctx>,
            pub f: <<u64 as HasConstrainedType<'s, 'ctx>>::ConstrainedType as ConstrainedType<
                's,
                'ctx,
            >>::ValueType, //U64ConstrainedValue<'ctx>,
        }

        impl<'s, 'ctx> ConstrainedValue<'s, 'ctx> for SConstrainedValue<'s, 'ctx>
        where
            'ctx: 's,
        {
            type ValueType = S;

            fn eval(&'s self, model: &Model<'ctx>) -> Option<Self::ValueType> {
                let f = self.f.eval(model)?;
                Some(S { f })
            }

            fn _eq(&'s self, other: &'s Self) -> BoolConstrainedValue {
                z3::ast::Ast::_eq(&self.val, &other.val).into()
            }

            fn assign_value(&'s self, solver: &Solver<'ctx>, value: &Self::ValueType) {
                self.f.assign_value(solver, &value.f);
            }
        }

        #[test]
        fn constrained_value() {
            let config = z3::Config::new();
            let ctx = z3::Context::new(&config);
            let context = Context::new(&ctx);
            let typ = S::constrained_type(&context);
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
    }
}

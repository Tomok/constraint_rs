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

#[derive(Debug, Clone)]
pub struct FieldAccessorIndices {
    variant: usize,
    accessor: usize,
}

impl FieldAccessorIndices {
    pub fn new(variant: usize, accessor: usize) -> Self {
        Self { variant, accessor }
    }

    /// gets the accessor function from the given datatype
    /// WARNING: Does not check whether datatype belongs to this accessor, if not it might panic or give a different field from that datatype
    pub fn accessor<'d, 'ctx>(&self, datatype: &'d DataType<'ctx>) -> &'d z3::FuncDecl<'ctx> {
        &datatype.0.variants[self.variant].accessors[self.accessor]
    }
}

/// A constant variable to be defined via constraints
pub trait ConstrainedValue<'s, 'ctx>
where
    'ctx: 's,
{
    type ValueType: HasConstrainedType<'s, 'ctx>;
    type AstType: z3::ast::Ast<'ctx>;

    ///get a possible set of values from the given Model
    fn eval(&'s self, model: &Model<'ctx>) -> Option<Self::ValueType>;

    //todo: add functions to iter all solutions

    // comparison functions
    fn _eq(&'s self, other: &'s Self) -> BoolConstrainedValue;

    /**
     * Constrain this object to match the given value
     **/
    fn assign_value(&'s self, solver: &Solver<'ctx>, value: &Self::ValueType);

    fn z3(&'s self) -> &'s Self::AstType;
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

        impl Empty {
            pub fn add(a: u64, b: u64) -> u64 {
                a + b
            }
        }

        pub struct EmptyConstrainedType<'s, 'ctx> {
            context: &'s Context<'ctx>,
            data_type: DataType<'ctx>,
            add: z3::RecFuncDecl<'ctx>,
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
                let add = {
                    let add = z3::RecFuncDecl::new(
                        context.z3_context(),
                        "Empty.add",
                        &[
                            <u64 as HasConstrainedType>::constrained_type(context).z3_sort(),
                            <u64 as HasConstrainedType>::constrained_type(context).z3_sort(),
                        ],
                        <u64 as HasConstrainedType>::constrained_type(context).z3_sort(),
                    );

                    let a = <u64 as HasConstrainedType>::constrained_type(context)
                        .fresh_value("Empty.add#a");
                    let b = <u64 as HasConstrainedType>::constrained_type(context)
                        .fresh_value("Empty.add#b");
                    add.add_def(
                        &[&a.z3().clone().into(), &b.z3().clone().into()],
                        a.add(&b).z3(),
                    );
                    add
                };
                Self {
                    context,
                    data_type,
                    add,
                }
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
            let ts1 = <Empty as HasConstrainedType>::constrained_type(&context);
            let ts2 = <Empty as HasConstrainedType>::constrained_type(&context);
            assert!(ptr::eq(ts1.data_type.0.as_ref(), ts2.data_type.0.as_ref()));
        }

        pub struct EmptyConstrainedValue<'s, 'ctx> {
            typ: &'s EmptyConstrainedType<'s, 'ctx>,
            val: z3::ast::Datatype<'ctx>,
        }

        impl<'s, 'ctx> EmptyConstrainedValue<'s, 'ctx> {
            pub fn add(&self, //todo: this requires self, original method did not
                a: &<<u64 as HasConstrainedType<'s, 'ctx>>::ConstrainedType as ConstrainedType<'s, 'ctx>>::ValueType,
                b: &<<u64 as HasConstrainedType<'s, 'ctx>>::ConstrainedType as ConstrainedType<'s, 'ctx>>::ValueType,
            )-> <<u64 as HasConstrainedType<'s, 'ctx>>::ConstrainedType as ConstrainedType<'s, 'ctx>>::ValueType{
                let applied_fn = self
                    .typ
                    .add
                    .apply(&[&a.z3().clone().into(), &b.z3().clone().into()]);
                <u64 as HasConstrainedType>::constrained_type(self.typ.context)
                    .value_from_z3_dynamic(applied_fn)
                    .unwrap()
            }
        }

        impl<'s, 'ctx> ConstrainedValue<'s, 'ctx> for EmptyConstrainedValue<'s, 'ctx>
        where
            'ctx: 's,
        {
            type ValueType = Empty;
            type AstType = z3::ast::Datatype<'ctx>;

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

            fn z3(&'s self) -> &Self::AstType {
                &self.val
            }
        }

        #[test]
        fn constrained_value() {
            let config = z3::Config::new();
            let ctx = z3::Context::new(&config);
            let context = Context::new(&ctx);
            let typ = <Empty as HasConstrainedType>::constrained_type(&context);
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

        #[test]
        fn test_constrained_value_add() {
            let config = z3::Config::new();
            let ctx = z3::Context::new(&config);
            let context = Context::new(&ctx);
            let a = 40u64.constrained(&context.z3_context());
            let b = 2u64.constrained(&context.z3_context());
            let ty = EmptyConstrainedType::new(&context);
            let to = ty.fresh_value("test_object");
            let add_res = to.add(&a, &b);
            let add_res_expected = 42u64.constrained(&context.z3_context());

            let solver = z3::Solver::new(&ctx); //todo - do not call z3 directly once corresponding methods was implemented
            solver.assert(&add_res._eq(&add_res_expected).z3());
            assert_eq!(z3::SatResult::Sat, solver.check());
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

        impl S {
            /// example for a function to be derived
            pub fn func1(&self) -> u64 {
                self.f
            }
        }

        struct SConstrainedTypeFieldAccessorIndices {
            f: FieldAccessorIndices,
        }

        pub struct SConstrainedType<'s, 'ctx> {
            context: &'s Context<'ctx>,
            data_type: DataType<'ctx>,
            func1: z3::RecFuncDecl<'ctx>,
            field_accessors: SConstrainedTypeFieldAccessorIndices,
        }

        impl<'s, 'ctx> ConstrainedType<'s, 'ctx> for SConstrainedType<'s, 'ctx>
        where
            'ctx: 's,
        {
            type ValueType = SConstrainedValue<'s, 'ctx>;

            fn new(context: &'s Context<'ctx>) -> Self {
                let fields = vec![(
                    "f",
                    z3::DatatypeAccessor::Sort(
                        <u64 as HasConstrainedType>::constrained_type(context)
                            .z3_sort()
                            .clone(),
                    ),
                )];
                let data_type = context.enter_or_get_datatype("S", |c| {
                    z3::DatatypeBuilder::new(c, "S")
                        .variant("", fields)
                        .finish()
                });

                let field_accessors = SConstrainedTypeFieldAccessorIndices {
                    f: FieldAccessorIndices::new(0, 0),
                };
                let func1 = z3::RecFuncDecl::new(
                    context.z3_context(),
                    "S.func1",
                    &[&data_type.z3_datatype_sort().sort],
                    u64::constrained_type(context).z3_sort(),
                );
                {
                    let self_const: z3::ast::Dynamic<'ctx> = z3::ast::Datatype::fresh_const(
                        context.z3_context(),
                        "S1.func1#self",
                        &data_type.z3_datatype_sort().sort,
                    )
                    .into();
                    let ast = field_accessors.f.accessor(&data_type).apply(&[&self_const]);
                    func1.add_def(&[&self_const], &ast);
                }
                Self {
                    context,
                    data_type,
                    field_accessors,
                    func1,
                }
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
                let f = <u64 as HasConstrainedType>::constrained_type(self.context)
                    .value_from_z3_dynamic(
                        self.data_type.0.variants[0].accessors[0].apply(&[&val]),
                    )?;

                Some(Self::ValueType {
                    val: val.as_datatype()?,
                    typ: self,
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
            let ts1 = <S as HasConstrainedType>::constrained_type(&context);
            let ts2 = <S as HasConstrainedType>::constrained_type(&context);
            assert!(ptr::eq(ts1.data_type.0.as_ref(), ts2.data_type.0.as_ref()));
        }

        pub struct SConstrainedValue<'s, 'ctx> {
            val: z3::ast::Datatype<'ctx>,
            pub f: <<u64 as HasConstrainedType<'s, 'ctx>>::ConstrainedType as ConstrainedType<
                's,
                'ctx,
            >>::ValueType, //U64ConstrainedValue<'ctx>,
            typ: &'s SConstrainedType<'s, 'ctx>,
        }

        impl<'s, 'ctx> ConstrainedValue<'s, 'ctx> for SConstrainedValue<'s, 'ctx>
        where
            'ctx: 's,
        {
            type ValueType = S;
            type AstType = z3::ast::Datatype<'ctx>;

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

            fn z3(&'s self) -> &Self::AstType {
                &self.val
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

        impl<'s, 'ctx> SConstrainedValue<'s, 'ctx> {
            pub fn func1(
                &self,
            ) -> <<u64 as HasConstrainedType>::ConstrainedType as ConstrainedType>::ValueType
            {
                let applied_fn = self.typ.func1.apply(&[&self.val.clone().into()]);
                <u64 as HasConstrainedType>::constrained_type(self.typ.context)
                    .value_from_z3_dynamic(applied_fn)
                    .unwrap()
            }
        }

        #[test]
        fn func1() {
            let config = z3::Config::new();
            let ctx = z3::Context::new(&config);
            let context = Context::new(&ctx);
            let typ = <S as HasConstrainedType>::constrained_type(&context);
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
        }
    }
}

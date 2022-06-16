use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub trait HasConstrainedType<'s, 'ctx>
where
    'ctx: 's,
{
    type ConstrainedType: ConstrainedType<'s, 'ctx>;

    fn constrained_type(context: &'s Context<'ctx>) -> Self::ConstrainedType {
        Self::ConstrainedType::new(context)
    }
}

pub trait ConstrainedType<'s, 'ctx>
where
    'ctx: 's,
{
    fn new(context: &'s Context<'ctx>) -> Self;
}

#[derive(Clone)]
pub struct DataType<'ctx>(Rc<z3::DatatypeSort<'ctx>>);

impl<'ctx> DataType<'ctx> {
    fn new(dt: z3::DatatypeSort<'ctx>) -> Self {
        Self(Rc::new(dt))
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

        use super::super::*;
        /// Struct for which the derived types and functions are listed below & used for this test
        struct Empty();

        pub struct EmptyConstrainedType<'s, 'ctx> {
            context: &'s Context<'ctx>,
            data_type: DataType<'ctx>,
        }

        impl<'s, 'ctx> ConstrainedType<'s, 'ctx> for EmptyConstrainedType<'s, 'ctx>
        where
            'ctx: 's,
        {
            fn new(context: &'s Context<'ctx>) -> Self {
                let data_type = context.enter_or_get_datatype("TestStruct", |c| {
                    z3::DatatypeBuilder::new(c, "TestStruct")
                        .variant("", vec![])
                        .finish()
                });
                Self { context, data_type }
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
    }
}

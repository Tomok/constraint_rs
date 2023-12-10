use std::{cell::RefCell, collections::HashMap, rc::Rc};

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
    fn _eq(&'s self, other: &'s Self) -> impls::BoolConstrainedValue;

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

    mod empty_struct_derived_code_test;
    #[allow(unused)]
    mod struct_derived_code_test;
    mod multi_stmt_function_derived_code_test;
}

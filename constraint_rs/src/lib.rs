use std::{collections::HashMap, rc::Rc};

//pub type Context = z3::Context;

pub struct Context<'ctx> {
    context: &'ctx z3::Context,
    data_types: HashMap<&'static str, Rc<z3::DatatypeSort<'ctx>>>,
}

impl<'ctx> Context<'ctx> {
    pub fn new(context: &'ctx z3::Context) -> Self {
        let data_types = HashMap::new();
        Self {
            context,
            data_types
        }
    }

    pub fn get_or_insert_data_type<'s, F>(&'s mut self, name: &'static str, data_type_builder: F) -> Rc<z3::DatatypeSort<'ctx>>
    where
        F: FnOnce (&'ctx z3::Context) -> z3::DatatypeSort<'ctx>
    {
        let ctx = self.context;
        let e = self.data_types.entry(name).or_insert_with(|| Rc::new(data_type_builder(ctx)));
        Rc::clone(e)
    }
}

pub type DataType<'ctx> = z3::DatatypeSort<'ctx>;

#[cfg(test)]
mod tests {
    use z3::DatatypeBuilder;

    use super::*;

    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }

    #[test]
    fn get_or_insert_data_type() {
        let config = z3::Config::new();
        let z3_ctx = z3::Context::new(&config);
        let mut context = Context::new(&z3_ctx);
        let dt = context.get_or_insert_data_type("Test", |c| DatatypeBuilder::new(c, "Test").variant("", vec![]).finish());
        let dt2 = context.get_or_insert_data_type("Test", |c| DatatypeBuilder::new(c, "Test").variant("", vec![]).finish());
        assert!(std::ptr::eq(dt.as_ref(), dt2.as_ref()));
    }
}

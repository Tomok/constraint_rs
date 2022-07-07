use z3::ast;

use super::super::*;

impl<'s, 'ctx> HasConstrainedType<'s, 'ctx> for bool
where
    'ctx: 's,
{
    type ConstrainedType = BoolConstrainedType<'s, 'ctx>;
}

pub struct BoolConstrainedType<'s, 'ctx> {
    context: &'s Context<'ctx>,
    data_type_sort: z3::Sort<'ctx>,
}

impl<'s, 'ctx> ConstrainedType<'s, 'ctx> for BoolConstrainedType<'s, 'ctx>
where
    'ctx: 's,
{
    type ValueType = BoolConstrainedValue<'ctx>;

    fn new(context: &'s Context<'ctx>) -> Self {
        let data_type_sort = z3::Sort::bool(context.z3_context());
        Self {
            context,
            data_type_sort,
        }
    }

    fn fresh_value(&'s self, name_prefix: &str) -> Self::ValueType {
        let val = ast::Bool::fresh_const(self.context.z3_context(), name_prefix);
        Self::ValueType { val }
    }

    fn value_from_z3_dynamic(&'s self, val: z3::ast::Dynamic<'ctx>) -> Option<Self::ValueType> {
        Some(Self::ValueType {
            val: val.as_bool()?,
        })
    }

    fn z3_sort(&'s self) -> &'s z3::Sort<'ctx> {
        &self.data_type_sort
    }
}

pub struct BoolConstrainedValue<'ctx> {
    val: z3::ast::Bool<'ctx>,
}

impl<'s, 'ctx> ConstrainedValue<'s, 'ctx> for BoolConstrainedValue<'ctx>
where
    'ctx: 's,
{
    type ValueType = bool;

    fn eval(&'s self, model: &Model<'ctx>) -> Option<Self::ValueType> {
        let a = model.eval(&self.val, false).unwrap();
        a.as_bool()
    }

    fn _eq(&'s self, other: &'s Self) -> BoolConstrainedValue {
        ast::Ast::_eq(&self.val, &other.val).into()
    }
}

impl<'ctx> From<ast::Bool<'ctx>> for BoolConstrainedValue<'ctx> {
    fn from(val: ast::Bool<'ctx>) -> Self {
        Self { val }
    }
}

impl<'ctx> BoolConstrainedValue<'ctx> {
    //todo: temporary for tests ... to be replaced when other functions for defining
    //constraints were added
    pub fn val(&self) -> &z3::ast::Bool<'ctx> {
        &self.val
    }
}

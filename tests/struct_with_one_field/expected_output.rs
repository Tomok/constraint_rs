pub struct SConstrainedTypeFieldAccessorIndices {
    f: constraint_rs::FieldAccessorIndices,
}

pub struct SConstrainedType<'s, 'ctx> {
    context: &'s constraint_rs::Context<'ctx>,
    data_type: constraint_rs::DataType<'ctx>,
    func1: z3::RecFuncDecl<'ctx>,
    field_accessors: SConstrainedTypeFieldAccessorIndices,
}

impl<'s, 'ctx> constraint_rs::ConstrainedType<'s, 'ctx> for SConstrainedType<'s, 'ctx>
where
    'ctx: 's,
{
    type ValueType = SConstrainedValue<'s, 'ctx>;

    fn new(context: &'s constraint_rs::Context<'ctx>) -> Self {
        let data_type = context.enter_or_get_datatype("S", |c| {
            z3::DatatypeBuilder::new(c, "S")
                .variant(
                    "",
                    vec![(
                        "f",
                        z3::DatatypeAccessor::Sort(
                            <u64 as constraint_rs::HasConstrainedType>::constrained_type(context)
                                .z3_sort()
                                .clone(),
                        ),
                    )],
                )
                .finish()
        });

        let field_accessors = SConstrainedTypeFieldAccessorIndices {
            f: constraint_rs::FieldAccessorIndices::new(0, 0usize),
        };
        /*
        let func1 = {
            let func1 = z3::RecFuncDecl::new(
                context.z3_context(),
                "S.func1",
                &[&data_type.z3_datatype_sort().sort],
                <u64 as constraint_rs::HasConstrainedType>::constrained_type(context).z3_sort(),
            );
            let self_const: z3::ast::Dynamic<'ctx> = z3::ast::Datatype::fresh_const(
                context.z3_context(),
                "S.func1#self",
                &data_type.z3_datatype_sort().sort,
            )
            .into();
            let self_dummy = {
                z3::ast::DataType::fresh_const(
                    context.z3_context(),
                    "S.func1#self",
                    &data_type.z3_datatype_sort().sort,
                )
            }
            func1.add_def(
                &[&self_const],
                &field_accessors.f.accessor(&data_type).apply(&[&self_const]),
            );
            func1
        };*/
        let func1 = z3::RecFuncDecl::new(
            context.z3_context(),
            "S.func1",
            &[&data_type.z3_datatype_sort().sort],
            <u64 as constraint_rs::HasConstrainedType>::constrained_type(context).z3_sort(),
        );
        let res = Self {
            context,
            data_type,
            field_accessors,
            func1,
        };
        {
            let self_dummy = res.fresh_value("S.func1#self");
            res.func1.add_def(
                &[constraint_rs::ConstrainedValue::z3(&self_dummy)],
                constraint_rs::ConstrainedValue::z3(&self_dummy.f),
            );
        }
        res
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

    fn value_from_z3_dynamic(&'s self, val: z3::ast::Dynamic<'ctx>) -> Option<Self::ValueType> {
        let f = <u64 as constraint_rs::HasConstrainedType>::constrained_type(self.context)
            .value_from_z3_dynamic(
                self.data_type.z3_datatype_sort().variants[0].accessors[0usize].apply(&[&val]),
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

impl<'s, 'ctx> constraint_rs::HasConstrainedType<'s, 'ctx> for S
where
    'ctx: 's,
{
    type ConstrainedType = SConstrainedType<'s, 'ctx>;
}

pub struct SConstrainedValue<'s, 'ctx> {
    val: z3::ast::Datatype<'ctx>,
    typ: &'s SConstrainedType<'s, 'ctx>,
    f: <<u64 as constraint_rs::HasConstrainedType<'s, 'ctx>>::ConstrainedType as constraint_rs::ConstrainedType<
        's,
        'ctx,
    >>::ValueType, //U64ConstrainedValue<'ctx>,
}

impl<'s, 'ctx> constraint_rs::ConstrainedValue<'s, 'ctx> for SConstrainedValue<'s, 'ctx>
where
    'ctx: 's,
{
    type ValueType = S;
    type AstType = z3::ast::Datatype<'ctx>;

    fn eval(&'s self, model: &constraint_rs::Model<'ctx>) -> Option<Self::ValueType> {
        let f = self.f.eval(model)?;
        Some(S { f })
    }

    fn assign_value(&'s self, solver: &constraint_rs::Solver<'ctx>, value: &Self::ValueType) {
        self.f.assign_value(solver, &value.f);
    }

    fn _eq(&'s self, other: &'s Self) -> constraint_rs::impls::BoolConstrainedValue {
        z3::ast::Ast::_eq(&self.val, &other.val).into()
    }

    fn z3(&'s self) -> &'s Self::AstType {
        &self.val
    }
}

impl<'s, 'ctx> SConstrainedValue<'s, 'ctx>
where
    'ctx: 's,
{
    pub fn func1(
        &'s self,
    ) -> <<u64 as constraint_rs::HasConstrainedType>::ConstrainedType as constraint_rs::ConstrainedType>::ValueType{
        let applied_fn = self
            .typ
            .func1
            .apply(&[constraint_rs::ConstrainedValue::z3(self)]);
        constraint_rs::ConstrainedType::value_from_z3_dynamic(
            &<u64 as constraint_rs::HasConstrainedType>::constrained_type(self.typ.context),
            applied_fn,
        )
        .unwrap()
    }
}

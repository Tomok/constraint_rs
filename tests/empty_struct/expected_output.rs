pub struct TestStructConstrainedType<'s, 'ctx> {
    context: &'s constraint_rs::Context<'ctx>,
    data_type: constraint_rs::DataType<'ctx>,
    foo_add: z3::RecFuncDecl<'ctx>,
    foo_mul: z3::RecFuncDecl<'ctx>,
    foo_div: z3::RecFuncDecl<'ctx>,
    foo_rem: z3::RecFuncDecl<'ctx>,
}

impl<'s, 'ctx> constraint_rs::ConstrainedType<'s, 'ctx> for TestStructConstrainedType<'s, 'ctx>
where
    'ctx: 's,
{
    type ValueType = TestStructConstrainedValue<'s, 'ctx>;
    fn new(context: &'s constraint_rs::Context<'ctx>) -> Self {
        let data_type = context.enter_or_get_datatype("TestStruct", |c| {
            z3::DatatypeBuilder::new(c, "TestStruct")
                .variant("", vec![])
                .finish()
        });
        let foo_add = z3::RecFuncDecl::new(
            context.z3_context(),
            "TestStruct.foo_add",
            &[
                <u64 as constraint_rs::HasConstrainedType>::constrained_type(context).z3_sort(),
                <u64 as constraint_rs::HasConstrainedType>::constrained_type(context).z3_sort(),
            ],
            <u64 as constraint_rs::HasConstrainedType>::constrained_type(context).z3_sort(),
        );
        let foo_mul = z3::RecFuncDecl::new(
            context.z3_context(),
            "TestStruct.foo_mul",
            &[
                <u64 as constraint_rs::HasConstrainedType>::constrained_type(context).z3_sort(),
                <u64 as constraint_rs::HasConstrainedType>::constrained_type(context).z3_sort(),
            ],
            <u64 as constraint_rs::HasConstrainedType>::constrained_type(context).z3_sort(),
        );
        let foo_div = z3::RecFuncDecl::new(
            context.z3_context(),
            "TestStruct.foo_div",
            &[
                <u64 as constraint_rs::HasConstrainedType>::constrained_type(context).z3_sort(),
                <u64 as constraint_rs::HasConstrainedType>::constrained_type(context).z3_sort(),
            ],
            <u64 as constraint_rs::HasConstrainedType>::constrained_type(context).z3_sort(),
        );
        let foo_rem = z3::RecFuncDecl::new(
            context.z3_context(),
            "TestStruct.foo_rem",
            &[
                <u64 as constraint_rs::HasConstrainedType>::constrained_type(context).z3_sort(),
                <u64 as constraint_rs::HasConstrainedType>::constrained_type(context).z3_sort(),
            ],
            <u64 as constraint_rs::HasConstrainedType>::constrained_type(context).z3_sort(),
        );
        let res = Self {
            context,
            data_type,
            foo_add,
            foo_mul,
            foo_div,
            foo_rem,
        };
        {
            let a = <u64 as constraint_rs::HasConstrainedType>::constrained_type(context)
                .fresh_value("TestStruct.foo_add#a");
            let b = <u64 as constraint_rs::HasConstrainedType>::constrained_type(context)
                .fresh_value("TestStruct.foo_add#b");
            res.foo_add.add_def(
                &[
                    constraint_rs::ConstrainedValue::z3(&a),
                    constraint_rs::ConstrainedValue::z3(&b),
                ],
                constraint_rs::ConstrainedValue::z3(&a.add(&b)),
            );
        }
        {
            let a = <u64 as constraint_rs::HasConstrainedType>::constrained_type(context)
                .fresh_value("TestStruct.foo_mul#a");
            let b = <u64 as constraint_rs::HasConstrainedType>::constrained_type(context)
                .fresh_value("TestStruct.foo_mul#b");
            res.foo_mul.add_def(
                &[
                    constraint_rs::ConstrainedValue::z3(&a),
                    constraint_rs::ConstrainedValue::z3(&b),
                ],
                constraint_rs::ConstrainedValue::z3(&a.mul(&b)),
            );
        }
        {
            let a = <u64 as constraint_rs::HasConstrainedType>::constrained_type(context)
                .fresh_value("TestStruct.foo_div#a");
            let b = <u64 as constraint_rs::HasConstrainedType>::constrained_type(context)
                .fresh_value("TestStruct.foo_div#b");
            res.foo_div.add_def(
                &[
                    constraint_rs::ConstrainedValue::z3(&a),
                    constraint_rs::ConstrainedValue::z3(&b),
                ],
                constraint_rs::ConstrainedValue::z3(&a.div(&b)),
            );
        }
        {
            let a = <u64 as constraint_rs::HasConstrainedType>::constrained_type(context)
                .fresh_value("TestStruct.foo_rem#a");
            let b = <u64 as constraint_rs::HasConstrainedType>::constrained_type(context)
                .fresh_value("TestStruct.foo_rem#b");
            res.foo_rem.add_def(
                &[
                    constraint_rs::ConstrainedValue::z3(&a),
                    constraint_rs::ConstrainedValue::z3(&b),
                ],
                constraint_rs::ConstrainedValue::z3(&a.rem(&b)),
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
        let dummy = std::marker::PhantomData;
        Some(Self::ValueType {
            val: val.as_datatype()?,
            typ: self,
            dummy,
        })
    }
    fn z3_sort(&'s self) -> &'s z3::Sort<'ctx> {
        &self.data_type.z3_datatype_sort().sort
    }
}

impl<'s, 'ctx> constraint_rs::HasConstrainedType<'s, 'ctx> for TestStruct
where
    'ctx: 's,
{
    type ConstrainedType = TestStructConstrainedType<'s, 'ctx>;
}

pub struct TestStructConstrainedValue<'s, 'ctx> {
    val: z3::ast::Datatype<'ctx>,
    typ: &'s TestStructConstrainedType<'s, 'ctx>,
    //TODO: is this one still necessary??
    dummy: std::marker::PhantomData<&'s ()>,
}

impl<'s, 'ctx> constraint_rs::ConstrainedValue<'s, 'ctx> for TestStructConstrainedValue<'s, 'ctx>
where
    'ctx: 's,
{
    type ValueType = TestStruct;
    type AstType = z3::ast::Datatype<'ctx>;

    fn eval(&'s self, model: &constraint_rs::Model<'ctx>) -> Option<Self::ValueType> {
        Some(TestStruct)
    }

    fn assign_value(&'s self, solver: &constraint_rs::Solver<'ctx>, value: &Self::ValueType) {}

    fn _eq(&'s self, other: &'s Self) -> constraint_rs::impls::BoolConstrainedValue {
        z3::ast::Ast::_eq(&self.val, &other.val).into()
    }

    fn z3(&'s self) -> &'s Self::AstType {
        &self.val
    }
}

impl<'s, 'ctx> TestStructConstrainedValue<'s, 'ctx>
where
    'ctx: 's,
{
    pub fn foo_add(
        &'s self,
        a: &<<u64 as constraint_rs::HasConstrainedType<
            's,
            'ctx,
        >>::ConstrainedType as constraint_rs::ConstrainedType<'s, 'ctx>>::ValueType,
        b: &<<u64 as constraint_rs::HasConstrainedType<
            's,
            'ctx,
        >>::ConstrainedType as constraint_rs::ConstrainedType<'s, 'ctx>>::ValueType,
    ) -> <<u64 as constraint_rs::HasConstrainedType>::ConstrainedType as constraint_rs::ConstrainedType>::ValueType{
        let applied_fn = self.typ.foo_add.apply(&[
            constraint_rs::ConstrainedValue::z3(a),
            constraint_rs::ConstrainedValue::z3(b),
        ]);
        constraint_rs::ConstrainedType::value_from_z3_dynamic(
            &<u64 as constraint_rs::HasConstrainedType>::constrained_type(self.typ.context),
            applied_fn,
        )
        .unwrap()
    }
    
    pub fn foo_mul(
        &'s self,
        a: &<<u64 as constraint_rs::HasConstrainedType<
            's,
            'ctx,
        >>::ConstrainedType as constraint_rs::ConstrainedType<'s, 'ctx>>::ValueType,
        b: &<<u64 as constraint_rs::HasConstrainedType<
            's,
            'ctx,
        >>::ConstrainedType as constraint_rs::ConstrainedType<'s, 'ctx>>::ValueType,
    ) -> <<u64 as constraint_rs::HasConstrainedType>::ConstrainedType as constraint_rs::ConstrainedType>::ValueType{
        let applied_fn = self.typ.foo_mul.apply(&[
            constraint_rs::ConstrainedValue::z3(a),
            constraint_rs::ConstrainedValue::z3(b),
        ]);
        constraint_rs::ConstrainedType::value_from_z3_dynamic(
            &<u64 as constraint_rs::HasConstrainedType>::constrained_type(self.typ.context),
            applied_fn,
        )
        .unwrap()
    }
    
    pub fn foo_div(
        &'s self,
        a: &<<u64 as constraint_rs::HasConstrainedType<
            's,
            'ctx,
        >>::ConstrainedType as constraint_rs::ConstrainedType<'s, 'ctx>>::ValueType,
        b: &<<u64 as constraint_rs::HasConstrainedType<
            's,
            'ctx,
        >>::ConstrainedType as constraint_rs::ConstrainedType<'s, 'ctx>>::ValueType,
    ) -> <<u64 as constraint_rs::HasConstrainedType>::ConstrainedType as constraint_rs::ConstrainedType>::ValueType{
        let applied_fn = self.typ.foo_div.apply(&[
            constraint_rs::ConstrainedValue::z3(a),
            constraint_rs::ConstrainedValue::z3(b),
        ]);
        constraint_rs::ConstrainedType::value_from_z3_dynamic(
            &<u64 as constraint_rs::HasConstrainedType>::constrained_type(self.typ.context),
            applied_fn,
        )
        .unwrap()
    }

    pub fn foo_rem(
        &'s self,
        a: &<<u64 as constraint_rs::HasConstrainedType<
            's,
            'ctx,
        >>::ConstrainedType as constraint_rs::ConstrainedType<'s, 'ctx>>::ValueType,
        b: &<<u64 as constraint_rs::HasConstrainedType<
            's,
            'ctx,
        >>::ConstrainedType as constraint_rs::ConstrainedType<'s, 'ctx>>::ValueType,
    ) -> <<u64 as constraint_rs::HasConstrainedType>::ConstrainedType as constraint_rs::ConstrainedType>::ValueType{
        let applied_fn = self.typ.foo_rem.apply(&[
            constraint_rs::ConstrainedValue::z3(a),
            constraint_rs::ConstrainedValue::z3(b),
        ]);
        constraint_rs::ConstrainedType::value_from_z3_dynamic(
            &<u64 as constraint_rs::HasConstrainedType>::constrained_type(self.typ.context),
            applied_fn,
        )
        .unwrap()
    }
}

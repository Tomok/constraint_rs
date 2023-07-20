use super::*;
#[test]
fn derive_module_with_empty_struct_and_one_func() {
    let input: syn::ItemMod = syn::parse_quote!(
        mod test {
            #[derive(Debug)]
            struct Test;

            impl Test {
                pub fn add(a: u64, b: u64) -> u64 {
                    a + b
                }
            }
        }
    );
    let expected: [syn::Item; 6] = [
        syn::parse_quote!(
            pub struct TestConstrainedType<'s, 'ctx> {
                context: &'s constraint_rs::Context<'ctx>,
                data_type: constraint_rs::DataType<'ctx>,
                add: z3::RecFuncDecl<'ctx>,
            }
        ),
        syn::parse_quote!(
            impl<'s, 'ctx> constraint_rs::ConstrainedType<'s, 'ctx> for TestConstrainedType<'s, 'ctx>
            where
                'ctx: 's,
            {
                type ValueType = TestConstrainedValue<'s, 'ctx>;

                fn new(context: &'s constraint_rs::Context<'ctx>) -> Self {
                    let data_type = context.enter_or_get_datatype("Test", |c| {
                        z3::DatatypeBuilder::new(c, "Test")
                            .variant("", vec![])
                            .finish()
                    });
                    let add = {
                        let add = z3::RecFuncDecl::new(
                            context.z3_context(),
                            "Test.add",
                            &[
                                <u64 as HasConstrainedType>::constrained_type(context).z3_sort(),
                                <u64 as HasConstrainedType>::constrained_type(context).z3_sort(),
                            ],
                            <u64 as HasConstrainedType>::constrained_type(context).z3_sort(),
                        );

                        let a = <u64 as HasConstrainedType>::constrained_type(context)
                            .fresh_value("Test.add#a");
                        let b = <u64 as HasConstrainedType>::constrained_type(context)
                            .fresh_value("Test.add#b");
                        add.add_def(
                            &[&a.z3().clone().into(), &b.z3().clone().into()],
                            (a).add(&b).z3(),
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
                    /* TODO: fill fields here, e.g.:
                    let f = u64::constrained_type(self.context).value_from_z3_dynamic(
                        self.data_type.z3_datatype_sort().variants[0].accessors[0].apply(&[&val]),
                    )?;*/
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
        ),
        syn::parse_quote!(
            impl<'s, 'ctx> constraint_rs::HasConstrainedType<'s, 'ctx> for Test
            where
                'ctx: 's,
            {
                type ConstrainedType = TestConstrainedType<'s, 'ctx>;
            }
        ),
        syn::parse_quote!(
            pub struct TestConstrainedValue<'s, 'ctx> {
                val: z3::ast::Datatype<'ctx>,
                typ: &'s TestConstrainedType<'s, 'ctx>,
                dummy: std::marker::PhantomData<&'s ()>,
            }
        ),
        syn::parse_quote!(
            impl<'s, 'ctx> constraint_rs::ConstrainedValue<'s, 'ctx> for TestConstrainedValue<'s, 'ctx>
            where
                'ctx: 's,
            {
                type ValueType = Test;
                type AstType = z3::ast::Datatype<'ctx>;

                fn eval(&'s self, model: &constraint_rs::Model<'ctx>) -> Option<Self::ValueType> {
                    Some(Test)
                }

                fn assign_value(
                    &'s self,
                    solver: &constraint_rs::Solver<'ctx>,
                    value: &Self::ValueType,
                ) {
                }

                fn _eq(&'s self, other: &'s Self) -> constraint_rs::impls::BoolConstrainedValue {
                    z3::ast::Ast::_eq(&self.val, &other.val).into()
                }

                fn z3(&'s self) -> &'s Self::AstType {
                    &self.val
                }
            }
        ),
        syn::parse_quote!(
            impl<'s, 'ctx> TestConstrainedValue<'s, 'ctx>
            where
                'ctx: 's,
            {
                pub fn add(&self,
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
        ),
    ];
    let res = module_items_to_derived_value_items(&input.content.unwrap().1);
    assert_eq!(expected.len(), res.len());
    for (e, r) in expected.into_iter().zip(res) {
        if e != r {
            let expectation_pretty_printed = pretty_print(e.clone());
            let generated_pretty_printed = pretty_print(r.clone());

            if expectation_pretty_printed != generated_pretty_printed {
                panic!(
                    "Generated code did not match expectation:\nExpected:\n{}\n\nGenerated:\n{}",
                    expectation_pretty_printed, generated_pretty_printed
                );
            } else {
                // generated symbols differ, but pretty print was identical..
                // should be fine, since formating messes with the expecation maintaining
                // real equality is not worth the effort
            }
        }
    }
}

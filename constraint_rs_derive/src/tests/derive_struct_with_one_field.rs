use super::*;
#[test]
fn derive_struct_with_one_field() {
    let input: syn::DeriveInput = syn::parse_quote!(
        #[derive(Debug, ConstrainedType)]
        struct Test {
            field: u64,
        }
    );
    let expected: [syn::Item; 7] = [
        syn::parse_quote!(
            pub struct TestConstrainedTypeFieldAccessorIndices {
                field: constraint_rs::FieldAccessorIndices,
            }
        ),
        syn::parse_quote!(
            pub struct TestConstrainedType<'s, 'ctx> {
                context: &'s constraint_rs::Context<'ctx>,
                data_type: constraint_rs::DataType<'ctx>,
                field_accessors: TestConstrainedTypeFieldAccessorIndices,
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
                            .variant(
                                "",
                                vec![(
                                    "field",
                                    z3::DatatypeAccessor::Sort(
                                        <u64 as constraint_rs::HasConstrainedType>::constrained_type(context)
                                            .z3_sort()
                                            .clone(),
                                    ),
                                )],
                            )
                            .finish()
                    });
                    let field_accessors = TestConstrainedTypeFieldAccessorIndices {
                        field: constraint_rs::FieldAccessorIndices::new(0, 0usize),
                    };
                    let res = Self {
                        context,
                        data_type,
                        field_accessors,
                    };
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

                fn value_from_z3_dynamic(
                    &'s self,
                    val: z3::ast::Dynamic<'ctx>,
                ) -> Option<Self::ValueType> {
                    /* TODO: fill fields here, e.g.:
                    let f = u64::constrained_type(self.context).value_from_z3_dynamic(
                        self.data_type.z3_datatype_sort().variants[0].accessors[0].apply(&[&val]),
                    )?;*/
                    let field =
                        <u64 as constraint_rs::HasConstrainedType>::constrained_type(self.context)
                            .value_from_z3_dynamic(
                                self.data_type.z3_datatype_sort().variants[0].accessors[0usize]
                                    .apply(&[&val]),
                            )?;
                    Some(Self::ValueType {
                        val: val.as_datatype()?,
                        typ: self,
                        field,
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
                field:
                    <<u64 as constraint_rs::HasConstrainedType<'s, 'ctx>>::ConstrainedType as constraint_rs::ConstrainedType<
                        's,
                        'ctx,
                    >>::ValueType,
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
                    let field = self.field.eval(model)?;
                    Some(Test { field })
                }

                fn assign_value(
                    &'s self,
                    solver: &constraint_rs::Solver<'ctx>,
                    value: &Self::ValueType,
                ) {
                    self.field.assign_value(solver, &value.field);
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
            impl<'s, 'ctx> TestConstrainedValue<'s, 'ctx> where 'ctx: 's {}
        ),
    ];
    let intermediate = ParsedDeriveInput::from(input);
    let res = intermediate.to_syn_items();
    assert_eq!(expected.len(), res.len());
    for (e, r) in expected.into_iter().zip(res) {
        if e != r {
            let expectation_pretty_printed = e.clone().pretty_print();
            let generated_pretty_printed = r.clone().pretty_print();

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

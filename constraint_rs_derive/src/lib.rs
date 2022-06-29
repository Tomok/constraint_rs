extern crate proc_macro;

use proc_macro2::Span;
use quote::ToTokens;
use syn::DeriveInput;

#[proc_macro_derive(ConstraintType)]
pub fn derive_constraint_type(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse(input).unwrap();
    let derived = _derive_constraint_type(input);
    let derived_tokens: Vec<_> = derived.into_iter().map(|x| x.into_token_stream()).collect();
    let mut res = proc_macro2::TokenStream::new();
    res.extend(derived_tokens);
    res.into()
}

fn _derive_constraint_type(input: DeriveInput) -> [syn::Item; 5] {
    let ident = &input.ident;
    let constraint_struct_ident =
        syn::Ident::new(&format!("{}ConstrainedType", ident), Span::call_site());

    let constrained_value_ident =
        syn::Ident::new(&format!("{}ConstrainedValue", ident), Span::call_site());

    let fields = match input.data {
        syn::Data::Struct(data_struct) => DerivedFields::new(data_struct, ident),
        syn::Data::Enum(_) => todo!(),
        syn::Data::Union(_) => todo!(),
    };

    //split fields for usage in parse_quote later
    let (constrained_type_new_fn, constrained_value_eval_fn, constrained_value_fields) = (
        fields.constrained_type_new_fn,
        fields.constrained_value_eval_fn,
        fields.constrained_value_fields,
    );

    let constraint_struct: syn::ItemStruct = syn::parse_quote!(
        pub struct #constraint_struct_ident<'s, 'ctx> {
            context: &'s constraint_rs::Context<'ctx>,
            data_type: constraint_rs::DataType<'ctx>,
        }
    );
    let constraint_struct_impl: syn::ItemImpl = syn::parse_quote!(
        impl<'s, 'ctx> constraint_rs::ConstrainedType<'s, 'ctx> for #constraint_struct_ident<'s, 'ctx>
        where
            'ctx: 's,
        {
            type ValueType = #constrained_value_ident<'ctx>;

            #constrained_type_new_fn

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
                    self.data_type.0.variants[0].accessors[0].apply(&[&val]),
                )?;*/
                Some(Self::ValueType {
                    val: val.as_datatype()?,
                })
            }

            fn z3_sort(&'s self) -> &'s z3::Sort<'ctx> {
                &self.data_type.z3_datatype_sort().sort
            }
        }
    );
    let struct_impl: syn::ItemImpl = syn::parse_quote!(
        impl<'s, 'ctx> constraint_rs::HasConstrainedType<'s, 'ctx> for #ident
        where
            'ctx: 's,
        {
            type ConstrainedType = #constraint_struct_ident<'s, 'ctx>;
        }
    );

    let value_def: syn::ItemStruct = syn::parse_quote!(
        pub struct #constrained_value_ident<'ctx>
            #constrained_value_fields
    );

    //todo: actual eval implementation for things with fields...
    let value_impl: syn::ItemImpl = syn::parse_quote!(
        impl<'s, 'ctx> constraint_rs::ConstrainedValue<'s, 'ctx> for #constrained_value_ident<'ctx>
        where
            'ctx: 's,
        {
            type ValueType = #ident;

            #constrained_value_eval_fn
        }
    );

    [
        constraint_struct.into(),
        constraint_struct_impl.into(),
        struct_impl.into(),
        value_def.into(),
        value_impl.into(),
    ]
}

struct DerivedFields {
    constrained_type_new_fn: syn::ImplItemMethod,
    constrained_value_eval_fn: syn::ImplItemMethod,
    constrained_value_fields: syn::FieldsNamed,
}

impl DerivedFields {
    //todo: find better name ... new seams wrong here, as syn::* input is converted
    fn new(data_struct: syn::DataStruct, ident: &syn::Ident) -> Self {
        let str_ident = format!("{}", ident);
        let fields: syn::Macro = match &data_struct.fields {
            syn::Fields::Named(fields) => {
                let field_entries = fields
                    .named
                    .iter()
                    .map(|f| {
                        (
                            f.ident
                                .as_ref()
                                .expect("Named field, with None in ident, expected name"),
                            Self::field_for_datatype_type_field(f),
                        )
                    })
                    .map(|(i, f)| {
                        let t: syn::Expr = syn::parse_quote!(
                            (#i, z3::DatatypeAccessor::Sort(#f)),
                        );
                        t
                    });
                syn::parse_quote!(vec![#(#field_entries),*])
            }
            syn::Fields::Unnamed(fields) => {
                let field_entries = fields
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(i, f)| (format!("{}", i), Self::field_for_datatype_type_field(f)))
                    .map(|(i, f)| {
                        let t: syn::Expr = syn::parse_quote!(
                            (#i, z3::DatatypeAccessor::Sort(#f)),
                        );
                        t
                    });
                syn::parse_quote!(vec![#(#field_entries),*])
            }
            syn::Fields::Unit => syn::parse_quote!(vec![]),
        };
        let constrained_type_new_fn = syn::parse_quote!(
            fn new(context: &'s constraint_rs::Context<'ctx>) -> Self {
                let data_type = context.enter_or_get_datatype(#str_ident, |c| {
                z3::DatatypeBuilder::new(c, #str_ident)
                    .variant("", #fields)
                    .finish()
                });
                Self {
                    context,
                    data_type
                }
            }
        );

        let name_tbd: syn::Expr = match &data_struct.fields {
            syn::Fields::Named(fields) => {
                let field_assingments = fields.named.iter().map(|f| {
                    let ident = f
                        .ident
                        .as_ref()
                        .expect("Named field, with None in ident, expected name");
                });
                syn::parse_quote!(
                        #ident{}
                )
            }
            syn::Fields::Unnamed(_) => {
                syn::parse_quote!( //TODO eval fields
                        #ident()
                )
            }
            syn::Fields::Unit => syn::parse_quote!(
                    #ident
            ),
        };
        let constrained_value_eval_fn = syn::parse_quote!(
            fn eval(&'s self, model: &constraint_rs::Model<'ctx>) -> Option<Self::ValueType> {
                Some(#name_tbd)
            }
        );

        let constrained_value_fields = syn::parse_quote!(
            {
                val: z3::ast::Datatype<'ctx>,
            }
        );

        Self {
            constrained_type_new_fn,
            constrained_value_eval_fn,
            constrained_value_fields,
        }
    }

    fn field_for_datatype_type_field(field: &syn::Field) -> syn::Expr {
        match &field.ty {
            syn::Type::Array(_) => todo!(),
            syn::Type::BareFn(_) => todo!(),
            syn::Type::Group(_) => todo!(),
            syn::Type::ImplTrait(_) => todo!(),
            syn::Type::Infer(_) => todo!(),
            syn::Type::Macro(_) => todo!(),
            syn::Type::Never(_) => todo!(),
            syn::Type::Paren(_) => todo!(),
            syn::Type::Path(p) => {
                syn::parse_quote!(
                    z3::DatatypeAccessor::Sort(#p::constrained_type(context).z3_sort().clone()),
                )
            }
            syn::Type::Ptr(_) => todo!(),
            syn::Type::Reference(_) => todo!(),
            syn::Type::Slice(_) => todo!(),
            syn::Type::TraitObject(_) => todo!(),
            syn::Type::Tuple(_) => todo!(),
            syn::Type::Verbatim(_) => todo!(),
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pretty_print(item: syn::Item) -> String {
        let file = syn::File {
            attrs: vec![],
            items: vec![item],
            shebang: None,
        };
        prettyplease::unparse(&file)
    }

    #[test]
    fn derive_empty_struct() {
        let input = syn::parse_quote!(
            #[derive(Debug, ConstraintType)]
            struct Test;
        );
        let expected: [syn::Item; 5] = [
            syn::parse_quote!(
                pub struct TestConstrainedType<'s, 'ctx> {
                    context: &'s constraint_rs::Context<'ctx>,
                    data_type: constraint_rs::DataType<'ctx>,
                }
            ),
            syn::parse_quote!(
                impl<'s, 'ctx> constraint_rs::ConstrainedType<'s, 'ctx> for TestConstrainedType<'s, 'ctx>
                where
                    'ctx: 's,
                {
                    type ValueType = TestConstrainedValue<'ctx>;

                    fn new(context: &'s constraint_rs::Context<'ctx>) -> Self {
                        let data_type = context.enter_or_get_datatype("Test", |c| {
                            z3::DatatypeBuilder::new(c, "Test")
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
                        /* TODO: fill fields here, e.g.:
                        let f = u64::constrained_type(self.context).value_from_z3_dynamic(
                            self.data_type.0.variants[0].accessors[0].apply(&[&val]),
                        )?;*/
                        Some(Self::ValueType {
                            val: val.as_datatype()?,
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
                pub struct TestConstrainedValue<'ctx> {
                    val: z3::ast::Datatype<'ctx>,
                }
            ),
            syn::parse_quote!(
                impl<'s, 'ctx> constraint_rs::ConstrainedValue<'s, 'ctx> for TestConstrainedValue<'ctx>
                where
                    'ctx: 's,
                {
                    type ValueType = Test;

                    fn eval(
                        &'s self,
                        model: &constraint_rs::Model<'ctx>,
                    ) -> Option<Self::ValueType> {
                        Some(Test)
                    }
                }
            ),
        ];
        let res = _derive_constraint_type(input);
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
}

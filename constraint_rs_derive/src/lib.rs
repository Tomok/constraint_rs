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
    let str_ident = format!("{}", ident);
    let constraint_struct_ident =
        syn::Ident::new(&format!("{}ConstrainedType", ident), Span::call_site());

    let constrained_value_ident =
        syn::Ident::new(&format!("{}ConstrainedValue", ident), Span::call_site());

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
            type ValueType = #constrained_value_ident<'s, 'ctx>;

            fn new(context: &'s constraint_rs::Context<'ctx>) -> Self {
                let data_type = context.enter_or_get_datatype(#str_ident, |c| {
                    z3::DatatypeBuilder::new(c, #str_ident)
                        .variant("", vec![])
                        .finish()
                });
                Self {
                    context,
                    data_type
                }
            }

            fn fresh_value(&'s self, name_prefix: &str) -> Self::ValueType {
                let val = z3::ast::Datatype::fresh_const(
                    self.context.z3_context(),
                    name_prefix,
                    &self.data_type.z3_datatype_sort().sort,
                );
                Self::ValueType { val, typ: self }
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
        pub struct #constrained_value_ident<'s, 'ctx>{
            typ: &'s #constraint_struct_ident<'s, 'ctx>,
            val: z3::ast::Datatype<'ctx>,
        }
    );

    //todo: actual eval implementation for things with fields...
    let value_impl: syn::ItemImpl = syn::parse_quote!(
        impl<'s, 'ctx> constraint_rs::ConstrainedValue<'s, 'ctx> for #constrained_value_ident<'s, 'ctx>
        where
            'ctx: 's,
        {
            type ValueType = #ident;

            fn eval(&'s self, model: &constraint_rs::Model<'ctx>,) -> Option<Self::ValueType>{
                Some(#ident())
            }
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
            struct Test();
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
                    type ValueType = TestConstrainedValue<'s, 'ctx>;

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
                        Self::ValueType { val, typ: self }
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
                    typ: &'s TestConstrainedType<'s, 'ctx>,
                    val: z3::ast::Datatype<'ctx>,
                }
            ),
            syn::parse_quote!(
                impl<'s, 'ctx> constraint_rs::ConstrainedValue<'s, 'ctx> for TestConstrainedValue<'s, 'ctx>
                where
                    'ctx: 's,
                {
                    type ValueType = Test;

                    fn eval(
                        &'s self,
                        model: &constraint_rs::Model<'ctx>,
                    ) -> Option<Self::ValueType> {
                        Some(Test())
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
                    assert_eq!(
                        &e, &r,
                        "generated symbols differ, but pretty print was identical"
                    );
                }
            }
        }
    }
}

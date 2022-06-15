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

fn _derive_constraint_type(input: DeriveInput) -> [syn::Item; 3] {
    let ident = &input.ident;
    let str_ident = format!("{}", ident);
    let constrait_struct_ident =
        syn::Ident::new(&format!("{}ConstrainedType", ident), Span::call_site());

    let constraint_struct = syn::parse_quote!(
    pub struct #constrait_struct_ident<'s, 'ctx> {
        context: &'s constraint_rs::Context<'ctx>,
        data_type: constraint_rs::DataType<'ctx>,
    });
    let constraint_struct_impl = syn::parse_quote!(
        impl<'s, 'ctx> #constrait_struct_ident<'s, 'ctx> {
            pub fn new(context: &'s constraint_rs::Context<'ctx>) -> Self {
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
        }
    );
    let struct_impl = syn::parse_quote!(
    impl #ident {
        pub fn constrained_type<'s, 'ctx>(
            context: &'s constraint_rs::Context<'ctx>
        ) -> #constrait_struct_ident<'s, 'ctx> {
            #constrait_struct_ident::new(context)
        }
    });

    [constraint_struct, constraint_struct_impl, struct_impl]
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
        let expected: [syn::Item; 3] = [
            syn::parse_quote!(
                pub struct TestConstrainedType<'s, 'ctx> {
                    context: &'s constraint_rs::Context<'ctx>,
                    data_type: constraint_rs::DataType<'ctx>,
                }
            ),
            syn::parse_quote!(
                impl<'s, 'ctx> TestConstrainedType<'s, 'ctx> {
                    pub fn new(context: &'s constraint_rs::Context<'ctx>) -> Self {
                        let data_type = context.enter_or_get_datatype("Test", |c| {
                            z3::DatatypeBuilder::new(c, "Test")
                                .variant("", vec![])
                                .finish()
                        });
                        Self { context, data_type }
                    }
                }
            ),
            syn::parse_quote!(
                impl Test {
                    pub fn constrained_type<'s, 'ctx>(context: &'s constraint_rs::Context<'ctx>) -> TestConstrainedType<'s, 'ctx> {
                        TestConstrainedType::new(context)
                    }
                }
            ),
        ];
        let res = _derive_constraint_type(input);
        assert_eq!(expected.len(), res.len());
        for (e, r) in expected.into_iter().zip(res) {
            if e != r {
                panic!(
                    "Generated code did not match expectation:\nExpected:\n{}\n\nGenerated:\n{}",
                    pretty_print(e),
                    pretty_print(r)
                );
            }
        }
    }
}

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
        syn::Ident::new(&format!("{}Constrained", ident), Span::call_site());

    let constraint_struct = syn::parse_quote!(
    pub struct #constrait_struct_ident<'ctx> {
        context: &'ctx constraint_rs::Context,
        data_type: constraint_rs::DataType<'ctx>,
    });
    let constraint_struct_impl = syn::parse_quote!(
        impl<'ctx> #constrait_struct_ident<'ctx> {
            pub fn new(context: &'ctx constraint_rs::Context) -> Self {
                let data_type = z3::DatatypeBuilder::new(&context, #str_ident)
                    .variant("", vec![])
                    .finish();
                Self {
                    context,
                    data_type
                }
            }
        }
    );
    let struct_impl = syn::parse_quote!(
    impl #ident {
        pub fn constraint_type<'ctx>(
            context: &'ctx constraint_rs::Context
        ) -> #constrait_struct_ident<'ctx> {
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
            struct TestStruct();
        );
        let expected: [syn::Item; 3] = [
            syn::parse_quote!(
                pub struct TestStructConstrained<'ctx> {
                    context: &'ctx constraint_rs::Context,
                    data_type: constraint_rs::DataType<'ctx>,
                }
            ),
            syn::parse_quote!(
                impl<'ctx> TestStructConstrained<'ctx> {
                    pub fn new(context: &'ctx constraint_rs::Context) -> Self {
                        let data_type = z3::DatatypeBuilder::new(&context, "TestStruct")
                            .variant("", vec![])
                            .finish();
                        Self { context, data_type }
                    }
                }
            ),
            syn::parse_quote!(
            impl TestStruct {
                pub fn constraint_type<'ctx>(context: &'ctx constraint_rs::Context) -> TestStructConstrained<'ctx> {
                    TestStructConstrained::new(context)
                }
            }),
        ];
        let res = _derive_constraint_type(input);
        assert_eq!(expected.len(), res.len());
        for (e, r) in expected.into_iter().zip(res) {
            //assert_eq!(&e, &r);
            if &e != &r {
                panic!(
                    "Generated code did not match expectation:\nExpected:\n{}\n\nGenerated:\n{}",
                    pretty_print(e),
                    pretty_print(r)
                );
            }
        }
    }
}

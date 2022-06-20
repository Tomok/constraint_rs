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
    let (constrained_type_new_fn, constrained_value_eval_fn) = (
        fields.constrained_type_new_fn,
        fields.constrained_value_eval_fn,
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
            type ValueType = #constrained_value_ident<'s, 'ctx>;

            #constrained_type_new_fn

            fn fresh_value(&'s self, name_prefix: &str) -> Self::ValueType {
                let val = z3::ast::Datatype::fresh_const(
                    self.context.z3_context(),
                    name_prefix,
                    &self.data_type.z3_datatype_sort().sort,
                );
                Self::ValueType { val, typ: self }
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
}

impl DerivedFields {
    //todo: find better name ... new seams wrong here, as syn::* input is converted
    fn new(data_struct: syn::DataStruct, ident: &syn::Ident) -> Self {
        let str_ident = format!("{}", ident);
        let constrained_type_new_fn = match &data_struct.fields {
            syn::Fields::Named(_) => todo!(),
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
                let fields: syn::Expr = syn::parse_quote!(vec![#(#field_entries),*]);
                syn::parse_quote!(
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

                )
            }
            syn::Fields::Unit => {
                syn::parse_quote!(
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
                )
            }
        };

        let constrained_value_eval_fn = match &data_struct.fields {
            syn::Fields::Named(_) => todo!(),
            syn::Fields::Unnamed(_) => syn::parse_quote!( //TODO eval fields
                fn eval(&'s self, model: &constraint_rs::Model<'ctx>,) -> Option<Self::ValueType>{
                    Some(#ident())
                }
            ),
            syn::Fields::Unit => syn::parse_quote!(
                fn eval(&'s self, model: &constraint_rs::Model<'ctx>,) -> Option<Self::ValueType>{
                    Some(#ident)
                }
            ),
        };
        Self {
            constrained_type_new_fn,
            constrained_value_eval_fn,
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
                    assert_eq!(
                        &e, &r,
                        "generated symbols differ, but pretty print was identical"
                    );
                }
            }
        }
    }
}

extern crate proc_macro;

use proc_macro2::Span;
use quote::ToTokens;
use syn::{DeriveInput, Token};

#[proc_macro_derive(ConstrainedType)]
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
    let constrained_struct_ident =
        syn::Ident::new(&format!("{}ConstrainedType", ident), Span::call_site());

    let constrained_value_ident =
        syn::Ident::new(&format!("{}ConstrainedValue", ident), Span::call_site());

    match input.data {
        syn::Data::Struct(data_struct) => derive_struct(
            &data_struct,
            ident,
            &constrained_struct_ident,
            &constrained_value_ident,
        ),
        syn::Data::Enum(_) => todo!(),
        syn::Data::Union(_) => todo!(),
    }
}

fn derive_struct(
    data_struct: &syn::DataStruct,
    ident: &syn::Ident,
    constrained_struct_ident: &syn::Ident,
    constrained_value_ident: &syn::Ident,
) -> [syn::Item; 5] {
    let str_ident = format!("{}", ident);
    let fields = ParsedField::parse_fields(&data_struct.fields);
    let struct_type = StructType::from(&data_struct.fields);

    let constrained_type_new_fn = constrained_type_new_fn(&str_ident, &fields);
    let constrained_value_eval_fn = constrained_value_eval_fn(struct_type, ident, &fields);
    let constrained_value_fields = constrained_value_fields(&fields);
    let constrained_value_assign_value_fn = constrained_value_assign_value_fn(&fields);
    let constrained_type_value_from_z3_dynamic = constrained_type_value_from_z3_dynamic(&fields);

    let constraint_struct: syn::ItemStruct = syn::parse_quote!(
        pub struct #constrained_struct_ident<'s, 'ctx> {
            context: &'s constraint_rs::Context<'ctx>,
            data_type: constraint_rs::DataType<'ctx>,
        }
    );
    let constraint_struct_impl: syn::ItemImpl = syn::parse_quote!(
        impl<'s, 'ctx> constraint_rs::ConstrainedType<'s, 'ctx> for #constrained_struct_ident<'s, 'ctx>
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
                self.value_from_z3_dynamic(z3::ast::Dynamic::from_ast(&val))
                    .unwrap()
            }

            #constrained_type_value_from_z3_dynamic

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
            type ConstrainedType = #constrained_struct_ident<'s, 'ctx>;
        }
    );

    let value_def: syn::ItemStruct = syn::parse_quote!(
        pub struct #constrained_value_ident<'s, 'ctx>
            #constrained_value_fields
    );

    let value_impl: syn::ItemImpl = syn::parse_quote!(
            impl<'s, 'ctx> constraint_rs::ConstrainedValue<'s, 'ctx> for #constrained_value_ident<'s, 'ctx>
            where
                'ctx: 's,
            {
                type ValueType = #ident;

                #constrained_value_eval_fn

                #constrained_value_assign_value_fn

                fn _eq(
                    &'s self,
                    other: &'s Self
                ) -> constraint_rs::impls::BoolConstrainedValue {
                    z3::ast::Ast::_eq(&self.val, &other.val).into()
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

struct ParsedField {
    ident: String,
    data_type: syn::Expr, //todo: is there a better type??
}

impl ParsedField {
    fn parse_fields(fields: &syn::Fields) -> Vec<Self> {
        match &fields {
            syn::Fields::Named(fields) => {
                let field_entries = fields
                    .named
                    .iter()
                    .map(|f| {
                        (
                            f.ident
                                .as_ref()
                                .expect("Named field, with None in ident, expected name")
                                .to_string(),
                            Self::field_for_datatype_type_field(f),
                        )
                    })
                    .map(|(ident, data_type)| ParsedField { ident, data_type });
                field_entries.collect()
            }
            syn::Fields::Unnamed(fields) => {
                let field_entries = fields
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(i, f)| (format!("{}", i), Self::field_for_datatype_type_field(f)))
                    .map(|(ident, data_type)| ParsedField { ident, data_type });
                field_entries.collect()
            }
            syn::Fields::Unit => vec![],
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
                syn::parse_quote!( #p )
                //z3::DatatypeAccessor::Sort(#p::constrained_type(context).z3_sort().clone()),
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum StructType {
    Named,
    Unnamed,
    Unit,
}

impl StructType {
    fn wrap<I, T>(&self, value_ident: &syn::Ident, e: I) -> syn::Expr
    where
        I: Iterator<Item = T>,
        T: quote::ToTokens,
    {
        match self {
            StructType::Named => syn::parse_quote!(#value_ident{ #(#e),* }),
            StructType::Unnamed => syn::parse_quote!(#value_ident(#(#e),*)),
            StructType::Unit => syn::parse_quote!(#value_ident),
        }
    }
}

impl From<&syn::Fields> for StructType {
    fn from(fields: &syn::Fields) -> Self {
        match fields {
            syn::Fields::Named(_) => Self::Named,
            syn::Fields::Unnamed(_) => Self::Unnamed,
            syn::Fields::Unit => Self::Unit,
        }
    }
}

fn constrained_type_new_fn(str_ident: &str, fields: &[ParsedField]) -> syn::ImplItemMethod {
    // let fields = vec![(
    //    "f",
    //    z3::DatatypeAccessor::Sort(u64::constrained_type(context).z3_sort().clone()),
    //)];
    let field_entries = fields.iter().map(|f| {
        let i = &f.ident;
        let t = &f.data_type;
        let t: syn::Expr = syn::parse_quote!(
            (#i, z3::DatatypeAccessor::Sort(#t::constrained_type(context).z3_sort().clone()))
        );
        t
    });
    let fields: syn::ExprMacro = syn::parse_quote!(vec![#(#field_entries),*]);
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

fn constrained_value_fields(fields: &[ParsedField]) -> syn::FieldsNamed {
    let field_entries = fields.iter().map(|f| {
        if f.ident == "val" {
            //todo: maybe use different field name instead of failing...
            panic!("Dervied structs may not contain a field val, needed for internal purposes");
        }
        let i = syn::Ident::new(&f.ident, Span::call_site());
        let d = &f.data_type;
        /*pub f: <<u64 as HasConstrainedType<'s, 'ctx>>::ConstrainedType as ConstrainedType<
                's,
                'ctx,
            >>::ValueType, //U64ConstrainedValue<'ctx>,
        */
        let ty: syn::TypePath = syn::parse_quote!(
             <<#d as HasConstrainedType<'s, 'ctx>>::ConstrainedType as ConstrainedType<
                's,
                'ctx
            >>::ValueType
        );

        syn::Field {
            attrs: vec![],
            vis: syn::Visibility::Public(syn::VisPublic {
                pub_token: Token![pub](Span::call_site()),
            }),
            ident: Some(i),
            colon_token: Some(Token![:](Span::call_site())),
            ty: syn::Type::Path(ty),
        }
    });
    //let t2 = quote!({
    //    val: z3::ast::Datatype<'ctx>,
    //    #(#field_entries),*
    //});
    //dbg!(format!("{}", t2.to_token_stream()));
    //todo!()
    let mut named_fields = syn::punctuated::Punctuated::new();
    named_fields.push(syn::Field {
        attrs: vec![],
        vis: syn::Visibility::Inherited,
        ident: Some(syn::Ident::new("val", Span::call_site())),
        colon_token: Some(Token![:](Span::call_site())),
        ty: syn::parse_quote!(z3::ast::Datatype<'ctx>),
    });
    if fields.is_empty() {
        //create dummy field, to ensure lifetime 's is used
        let dummy = syn::Field {
            attrs: vec![],
            vis: syn::Visibility::Inherited,
            ident: Some(syn::Ident::new("dummy", Span::call_site())),
            colon_token: Some(Token![:](Span::call_site())),
            ty: syn::parse_quote!(std::marker::PhantomData<&'s ()>),
        };
        named_fields.push(dummy);
    } else {
        named_fields.extend(field_entries);
    }
    syn::FieldsNamed {
        brace_token: syn::token::Brace {
            span: Span::call_site(),
        },
        named: named_fields,
    }
}

fn constrained_value_assign_value_fn(fields: &[ParsedField]) -> syn::ImplItemMethod {
    let field_assingments = fields.iter().map(|f| {
        let i = syn::Ident::new(&f.ident, Span::call_site());
        let eval_call: syn::ExprMethodCall =
            syn::parse_quote!(self.#i.assign_value(solver, &value.#i));
        let t: syn::Expr = syn::parse_quote!(let #i = #eval_call);
        t
    });

    syn::parse_quote!(
        fn assign_value(&'s self, solver: &constraint_rs::Solver<'ctx>, value: &Self::ValueType) {
            #(#field_assingments;)*
        }
    )
}

fn constrained_type_value_from_z3_dynamic(fields: &[ParsedField]) -> syn::ImplItemMethod {
    /*  fill fields here, e.g.:
    let f = u64::constrained_type(self.context).value_from_z3_dynamic(
        self.data_type.z3_datatype_sort().variants[0].accessors[0].apply(&[&val]),
    )?;*/
    if fields.is_empty() {
        syn::parse_quote!(
            fn value_from_z3_dynamic(
                &'s self,
                val: z3::ast::Dynamic<'ctx>,
            ) -> Option<Self::ValueType> {
                let dummy = std::marker::PhantomData;
                Some(Self::ValueType {
                    val: val.as_datatype()?,
                    dummy,
                })
            }
        )
    } else {
        let field_assingments = fields.iter().enumerate()
            .map(|(index, field)|{
                let f = syn::Ident::new(&field.ident, Span::call_site());
                let d = &field.data_type;
                let apply_call : syn::ExprMethodCall = syn::parse_quote!(
                    self.data_type.z3_datatype_sort().variants[0].accessors[#index].apply(&[&val])
                );
                let val_call: syn::ExprMethodCall = syn::parse_quote!(
                    <#d as HasConstrainedType>::constrained_type(self.context).value_from_z3_dynamic( #apply_call )
                );
                let a: syn::Expr = syn::parse_quote!(
                    let #f = #val_call?
                );
                a
            });
        let class_fields = fields
            .iter()
            .map(|field| syn::Ident::new(&field.ident, Span::call_site()));
        syn::parse_quote!(
            fn value_from_z3_dynamic(
                &'s self,
                val: z3::ast::Dynamic<'ctx>
            ) -> Option<Self::ValueType> {
                #(#field_assingments;)*
                Some(Self::ValueType {
                    val: val.as_datatype()?,
                    #(#class_fields),*
                })
            }
        )
    }
}

fn constrained_value_eval_fn(
    struct_type: StructType,
    value_ident: &syn::Ident,
    fields: &[ParsedField],
) -> syn::ImplItemMethod {
    let field_assingments = fields.iter().map(|f| {
        let i = syn::Ident::new(&f.ident, Span::call_site());
        let eval_call: syn::ExprMethodCall = syn::parse_quote!(self.#i.eval(model));
        let t: syn::Expr = syn::parse_quote!(let #i = #eval_call?);
        t
    });
    let inits = fields
        .iter()
        .map(|f| syn::Ident::new(&f.ident, Span::call_site()));
    let field_creation = struct_type.wrap(value_ident, inits);
    syn::parse_quote!(
        fn eval(&'s self, model: &constraint_rs::Model<'ctx>) -> Option<Self::ValueType> {
            #(#field_assingments;)*
            Some(#field_creation)
        }
    )
}

#[cfg(test)]
mod tests {
    use syn::parse_quote;

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
            #[derive(Debug, ConstrainedType)]
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
                    dummy: std::marker::PhantomData<&'s ()>,
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

                    fn assign_value(
                        &'s self,
                        solver: &constraint_rs::Solver<'ctx>,
                        value: &Self::ValueType,
                    ) {
                    }

                    fn _eq(
                        &'s self,
                        other: &'s Self,
                    ) -> constraint_rs::impls::BoolConstrainedValue {
                        z3::ast::Ast::_eq(&self.val, &other.val).into()
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

    fn one_field() -> Vec<ParsedField> {
        vec![ParsedField {
            ident: "my_u32_field".to_string(),
            data_type: parse_quote! {
                u32
            },
            //z3::DatatypeAccessor::Sort(u32::constrained_type(context).z3_sort().clone())
        }]
    }

    fn harmonize_syn_str(i: &str) -> String {
        let mut result = i.trim().replace('\n', " ").replace('\t', " ");
        while result.contains("  ") {
            result = result.replace("  ", " ");
        }
        result
            .replace(" .", ".")
            .replace(". ", ".")
            .replace(" :: ", "::")
            .replace(" (", "(")
            .replace("( ", "(")
            .replace(" )", ")")
            .replace(" <", "<")
            .replace("< ", "<")
            .replace(" >", ">")
            .replace("> ", ">")
            .replace("& '", "&'")
            .replace("| ", "|")
            .replace(" |", "|")
            .replace(" ,", ",")
            .replace(" :", ":")
            .replace(" !", "!")
            .replace("! ", "!")
            .replace(" ;", ";")
            .replace("; ", ";")
            .replace(" ?", "?")
    }

    #[test]
    fn test_constrained_type_new_fn() {
        let expected = "fn new(context: &'s constraint_rs::Context<'ctx>) -> Self {
                let data_type = context.enter_or_get_datatype(\"MyType\", |c| {
                    z3::DatatypeBuilder::new(c, \"MyType\")
                        .variant(
                            \"\",
                            vec![(
                                \"my_u32_field\",
                                z3::DatatypeAccessor::Sort(
                                    u32::constrained_type(context).z3_sort().clone()
                                )
                            )]
                        )
                        .finish()
                });
                Self { context, data_type }
            }";
        let str_ident = "MyType";
        let generated = constrained_type_new_fn(str_ident, &one_field());
        let generated_str = format!("{}", generated.to_token_stream());
        assert_eq!(
            harmonize_syn_str(expected),
            harmonize_syn_str(&generated_str)
        );
    }

    #[test]
    fn test_constrained_value_fields() {
        let expected = "{
            val: z3::ast::Datatype<'ctx>,
            pub my_u32_field: << u32 as HasConstrainedType<'s, 'ctx>>::ConstrainedType as ConstrainedType<'s, 'ctx>>::ValueType
        }";
        let generated = constrained_value_fields(&one_field());
        let generated_str = format!("{}", generated.to_token_stream());
        assert_eq!(
            harmonize_syn_str(expected),
            harmonize_syn_str(&generated_str)
        );
    }

    #[test]
    fn test_constrained_type_value_from_z3_dynamic() {
        let expected = "
            fn value_from_z3_dynamic(&'s self, val: z3::ast::Dynamic<'ctx>) ->Option<Self::ValueType>
            {
                let my_u32_field =<u32 as HasConstrainedType>::constrained_type(self.context).value_from_z3_dynamic(
                    self.data_type.z3_datatype_sort().variants [0].accessors [0usize].apply(& [& val]))?;
                Some(Self::ValueType {
                    val: val.as_datatype()?,
                    my_u32_field
                })
            }
        ";

        let generated = constrained_type_value_from_z3_dynamic(&one_field());
        let generated_str = format!("{}", generated.to_token_stream());
        assert_eq!(
            harmonize_syn_str(expected),
            harmonize_syn_str(&generated_str)
        );
    }

    #[test]
    fn test_constrained_value_eval_fn() {
        let expected = "
            fn eval(&'s self, model: & constraint_rs::Model<'ctx>) ->Option<Self::ValueType>{
                let my_u32_field = self.my_u32_field.eval(model)?;
                Some(MyType { my_u32_field })
            }
        ";
        let ident = syn::Ident::new("MyType", Span::call_site());
        let generated = constrained_value_eval_fn(StructType::Named, &ident, &one_field());
        let generated_str = format!("{}", generated.to_token_stream());
        assert_eq!(
            harmonize_syn_str(expected),
            harmonize_syn_str(&generated_str)
        );
    }
}

extern crate proc_macro;

use proc_macro2::Span;
use quote::ToTokens;
use syn::Token;

#[proc_macro_derive(ConstrainedType)]
pub fn derive_constraint_type(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: syn::DeriveInput = syn::parse(input).unwrap();
    let parsed = ParsedDeriveInput::from(input);
    let derived_tokens: Vec<_> = parsed
        .to_syn_items()
        .into_iter()
        .map(|x| x.into_token_stream())
        .collect();
    let mut res = proc_macro2::TokenStream::new();
    res.extend(derived_tokens);
    res.into()
}

fn constrained_struct_ident(ident: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("{}ConstrainedType", ident), Span::call_site())
}

fn constrained_value_ident(ident: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("{}ConstrainedValue", ident), Span::call_site())
}

enum ParsedDeriveInput {
    Struct(ParsedStruct),
    Enum(),  //todo
    Union(), //todo
}

impl ParsedDeriveInput {
    pub fn to_syn_items(&self) -> [syn::Item; 5] {
        match self {
            ParsedDeriveInput::Struct(s) => s.to_syn_items(),
            ParsedDeriveInput::Enum() => todo!(),
            ParsedDeriveInput::Union() => todo!(),
        }
    }
}

impl From<syn::DeriveInput> for ParsedDeriveInput {
    fn from(input: syn::DeriveInput) -> Self {
        let ident = &input.ident;
        match input.data {
            syn::Data::Struct(data_struct) => {
                Self::Struct(ParsedStruct::from_data_struct(ident, &data_struct))
            }
            syn::Data::Enum(_) => todo!(),
            syn::Data::Union(_) => todo!(),
        }
    }
}

struct ParsedStruct {
    pub typ: StructType,
    pub ident: syn::Ident,
    pub fields: Vec<ParsedField>,
    pub str_ident: String, //not realy necessary as field, but calculated on construction for reusablilty,
    pub constrained_struct_ident: syn::Ident, //not realy necessary as field, but calculated on construction for reusablilty
    pub constrained_value_ident: syn::Ident, //not realy necessary as field, but calculated on construction for reusablilty,
}

impl ParsedStruct {
    fn new(typ: StructType, ident: syn::Ident, fields: Vec<ParsedField>) -> Self {
        let constrained_struct_ident = constrained_struct_ident(&ident);
        let constrained_value_ident = constrained_value_ident(&ident);
        let str_ident = ident.to_string();
        Self {
            typ,
            ident,
            fields,
            str_ident,
            constrained_struct_ident,
            constrained_value_ident,
        }
    }

    fn from_data_struct(ident: &syn::Ident, data_struct: &syn::DataStruct) -> Self {
        let fields = ParsedField::parse_fields(&data_struct.fields);
        let ident = ident.clone();
        let typ = StructType::from(&data_struct.fields);
        Self::new(typ, ident, fields)
    }

    pub fn constrained_struct(&self) -> syn::ItemStruct {
        let constrained_struct_ident = &self.constrained_struct_ident;
        syn::parse_quote!(
            pub struct #constrained_struct_ident<'s, 'ctx> {
                context: &'s constraint_rs::Context<'ctx>,
                data_type: constraint_rs::DataType<'ctx>,
            }
        )
    }

    pub fn constrained_struct_impl(&self) -> syn::ItemImpl {
        let constrained_struct_ident = &self.constrained_struct_ident;
        let constrained_value_ident = &self.constrained_value_ident;
        let constrained_type_new_fn = self.constrained_type_new_fn();
        let constrained_type_value_from_z3_dynamic = self.constrained_type_value_from_z3_dynamic();
        syn::parse_quote!(
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
        )
    }

    pub fn struct_impl(&self) -> syn::ItemImpl {
        let ident = &self.ident;
        let constrained_struct_ident = &self.constrained_struct_ident;
        syn::parse_quote!(
            impl<'s, 'ctx> constraint_rs::HasConstrainedType<'s, 'ctx> for #ident
            where
                'ctx: 's,
            {
                type ConstrainedType = #constrained_struct_ident<'s, 'ctx>;
            }
        )
    }

    pub fn value_def(&self) -> syn::ItemStruct {
        let constrained_value_ident = &self.constrained_value_ident;

        let constrained_value_fields = self.constrained_value_fields();

        syn::parse_quote!(
            pub struct #constrained_value_ident<'s, 'ctx>
                #constrained_value_fields
        )
    }

    pub fn value_impl(&self) -> syn::ItemImpl {
        let ident = &self.ident;
        let constrained_value_ident = &self.constrained_value_ident;

        let constrained_value_eval_fn = self.constrained_value_eval_fn();
        let constrained_value_assign_value_fn = self.constrained_value_assign_value_fn();
        syn::parse_quote!(
            impl<'s, 'ctx> constraint_rs::ConstrainedValue<'s, 'ctx> for #constrained_value_ident<'s, 'ctx>
            where
                'ctx: 's,
            {
                type ValueType = #ident;
                type AstType = z3::ast::Datatype<'ctx>;

                #constrained_value_eval_fn

                #constrained_value_assign_value_fn

                fn _eq(
                    &'s self,
                    other: &'s Self
                ) -> constraint_rs::impls::BoolConstrainedValue {
                    z3::ast::Ast::_eq(&self.val, &other.val).into()
                }

                fn z3(&'s self) -> &'s Self::AstType {
                    &self.val
                }
            }
        )
    }

    pub fn to_syn_items(&self) -> [syn::Item; 5] {
        [
            self.constrained_struct().into(),
            self.constrained_struct_impl().into(),
            self.struct_impl().into(),
            self.value_def().into(),
            self.value_impl().into(),
        ]
    }

    fn constrained_value_eval_fn(&self) -> syn::ImplItemFn {
        let field_assingments = self.fields.iter().map(|f| {
            let i = syn::Ident::new(&f.ident, Span::call_site());
            let eval_call: syn::ExprMethodCall = syn::parse_quote!(self.#i.eval(model));
            let t: syn::Expr = syn::parse_quote!(let #i = #eval_call?);
            t
        });
        let inits = self
            .fields
            .iter()
            .map(|f| syn::Ident::new(&f.ident, Span::call_site()));
        let field_creation = self.typ.wrap(&self.ident, inits);
        syn::parse_quote!(
            fn eval(&'s self, model: &constraint_rs::Model<'ctx>) -> Option<Self::ValueType> {
                #(#field_assingments;)*
                Some(#field_creation)
            }
        )
    }

    fn constrained_type_value_from_z3_dynamic(&self) -> syn::ImplItemFn {
        /*  fill fields here, e.g.:
        let f = u64::constrained_type(self.context).value_from_z3_dynamic(
            self.data_type.z3_datatype_sort().variants[0].accessors[0].apply(&[&val]),
        )?;*/
        let fields = &self.fields;
        if fields.is_empty() {
            syn::parse_quote!(
                fn value_from_z3_dynamic(
                    &'s self,
                    val: z3::ast::Dynamic<'ctx>,
                ) -> Option<Self::ValueType> {
                    let dummy = std::marker::PhantomData;
                    Some(Self::ValueType {
                        val: val.as_datatype()?,
                        typ: self,
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
                        typ: self,
                        #(#class_fields),*
                    })
                }
            )
        }
    }

    fn constrained_value_fields(&self) -> syn::FieldsNamed {
        let constrained_struct_ident = &self.constrained_struct_ident;
        let field_entries = self.fields.iter().map(|f| {
            if f.ident == "val" {
                //todo: maybe use different field name instead of failing...
                panic!("Dervied structs may not contain a field val, needed for internal purposes");
            }
            if f.ident == "typ" {
                //todo: maybe use different field name instead of failing...
                panic!("Dervied structs may not contain a field typ, needed for internal purposes");
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
                vis: syn::Visibility::Public(Token![pub](Span::call_site())),
                mutability: syn::FieldMutability::None,
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
        let mut named_fields: syn::punctuated::Punctuated<syn::Field, syn::token::Comma> = syn::punctuated::Punctuated::new();
        named_fields.push(syn::Field {
            attrs: vec![],
            vis: syn::Visibility::Inherited,
            mutability: syn::FieldMutability::None,
            ident: Some(syn::Ident::new("val", Span::call_site())),
            colon_token: Some(Token![:](Span::call_site())),
            ty: syn::parse_quote!(z3::ast::Datatype<'ctx>),
        });
        named_fields.push(syn::Field {
            attrs: vec![],
            vis: syn::Visibility::Inherited,
            mutability: syn::FieldMutability::None,
            ident: Some(syn::Ident::new("typ", Span::call_site())),
            colon_token: Some(Token![:](Span::call_site())),
            ty: syn::parse_quote!(&'s #constrained_struct_ident<'s, 'ctx>),
        });
        if self.fields.is_empty() {
            //create dummy field, to ensure lifetime 's is used
            let dummy = syn::Field {
                attrs: vec![],
                vis: syn::Visibility::Inherited,
                mutability: syn::FieldMutability::None,
                ident: Some(syn::Ident::new("dummy", Span::call_site())),
                colon_token: Some(Token![:](Span::call_site())),
                ty: syn::parse_quote!(std::marker::PhantomData<&'s ()>),
            };
            named_fields.push(dummy);
        } else {
            named_fields.extend(field_entries);
        }
        syn::parse_quote!({#named_fields})
    }

    fn constrained_value_assign_value_fn(&self) -> syn::ImplItemFn {
        let field_assingments = self.fields.iter().map(|f| {
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

    fn constrained_type_new_fn(&self) -> syn::ImplItemFn {
        // let fields = vec![(
        //    "f",
        //    z3::DatatypeAccessor::Sort(u64::constrained_type(context).z3_sort().clone()),
        //)];
        let str_ident = &self.str_ident;
        let field_entries = self.fields.iter().map(|f| {
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
        let input: syn::DeriveInput = syn::parse_quote!(
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

                    fn z3(&'s self) -> &'s Self::AstType {
                        &self.val
                    }
                }
            ),
        ];
        let intermediate = ParsedDeriveInput::from(input);
        let res = intermediate.to_syn_items();
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
        let ident = syn::Ident::new("MyType", Span::call_site());
        let parsed = ParsedStruct::new(StructType::Named, ident, one_field());
        let generated = parsed.constrained_type_new_fn();
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
            typ: &'s SConstrainedType<'s, 'ctx>,
            pub my_u32_field: << u32 as HasConstrainedType<'s, 'ctx>>::ConstrainedType as ConstrainedType<'s, 'ctx>>::ValueType
        }";
        let ident = syn::Ident::new("S", Span::call_site());
        let parsed = ParsedStruct::new(StructType::Named, ident, one_field());
        let generated = parsed.constrained_value_fields();
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
                    typ: self,
                    my_u32_field
                })
            }
        ";
        let ident = syn::Ident::new("S", Span::call_site());
        let parsed = ParsedStruct::new(StructType::Named, ident, one_field());

        let generated = parsed.constrained_type_value_from_z3_dynamic();
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
        let parsed = ParsedStruct::new(StructType::Named, ident, one_field());
        let generated = parsed.constrained_value_eval_fn();
        let generated_str = format!("{}", generated.to_token_stream());
        assert_eq!(
            harmonize_syn_str(expected),
            harmonize_syn_str(&generated_str)
        );
    }
}

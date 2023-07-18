use proc_macro2::Span;
use syn::Token;

use crate::parsed_impl::{ParsedImpl, ParsedMethod};

fn constrained_struct_ident(ident: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("{}ConstrainedType", ident), Span::call_site())
}

fn constrained_value_ident(ident: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("{}ConstrainedValue", ident), Span::call_site())
}

pub struct ParsedStruct {
    typ: StructType,
    ident: syn::Ident,
    fields: Vec<ParsedField>,
    str_ident: String, //not realy necessary as field, but calculated on construction for reusablilty,
    constrained_struct_ident: syn::Ident, //not realy necessary as field, but calculated on construction for reusablilty
    constrained_value_ident: syn::Ident, //not realy necessary as field, but calculated on construction for reusablilty,
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

    pub fn from_data_struct(ident: &syn::Ident, data_struct: &syn::DataStruct) -> Self {
        let fields = ParsedField::parse_fields(&data_struct.fields);
        let ident = ident.clone();
        let typ = StructType::from(&data_struct.fields);
        Self::new(typ, ident, fields)
    }

    pub fn from_item_struct(i: &syn::ItemStruct) -> Self {
        let fields = ParsedField::parse_fields(&i.fields);
        let ident = i.ident.clone();
        let typ = StructType::from(&i.fields);
        //todo: handle other fields in ItemStruct ?
        Self::new(typ, ident, fields)
    }

    pub fn constrained_struct(&self, relevant_implementations: &[&ParsedImpl]) -> syn::ItemStruct {
        let constrained_struct_ident = &self.constrained_struct_ident;
        let functions = relevant_implementations
            .iter()
            .flat_map(|x| x.methods().iter())
            .map(|m| m.ident());
        syn::parse_quote!(
            pub struct #constrained_struct_ident<'s, 'ctx> {
                context: &'s constraint_rs::Context<'ctx>,
                data_type: constraint_rs::DataType<'ctx>,
                #(#functions : z3::RecFuncDecl<'ctx>),*
            }
        )
    }

    pub fn constrained_struct_impl(
        &self,
        relevant_implementations: &[&ParsedImpl],
    ) -> syn::ItemImpl {
        let constrained_struct_ident = &self.constrained_struct_ident;
        let constrained_value_ident = &self.constrained_value_ident;
        let constrained_type_new_fn = self.constrained_type_new_fn(relevant_implementations);
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

    pub fn value_trait_impl(&self) -> syn::ItemImpl {
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

    pub fn to_syn_items(&self, relevant_implementations: Vec<&ParsedImpl>) -> [syn::Item; 6] {
        [
            self.constrained_struct(&relevant_implementations).into(),
            self.constrained_struct_impl(&relevant_implementations)
                .into(),
            self.struct_impl().into(),
            self.value_def().into(),
            self.value_trait_impl().into(),
            self.value_impl(&relevant_implementations).into(),
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
        let mut named_fields: syn::punctuated::Punctuated<syn::Field, syn::token::Comma> =
            syn::punctuated::Punctuated::new();
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

    fn constrained_type_new_fn(&self, relevant_implementations: &[&ParsedImpl]) -> syn::ImplItemFn {
        // let fields = vec![(
        //    "f",
        //    z3::DatatypeAccessor::Sort(u64::constrained_type(context).z3_sort().clone()),
        //)];
        if !relevant_implementations.is_empty() {
            todo!("generating functions not yet supported");
        }
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

    pub fn ident(&self) -> &syn::Ident {
        &self.ident
    }

    fn value_impl(&self, relevant_implementations: &[&ParsedImpl]) -> syn::ItemImpl {
        let constrained_value_ident = &self.constrained_value_ident;

        let functions = relevant_implementations
            .iter()
            .flat_map(|i| i.methods())
            .map(ParsedMethod::to_constrained_value_impl_func);

        syn::parse_quote!(
            impl<'s, 'ctx> #constrained_value_ident<'s, 'ctx>
            where
                'ctx: 's,
            {
                #(#functions)*
            }
        )
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
            syn::Type::Array(_) => todo!("syn::Type::Array"),
            syn::Type::BareFn(_) => todo!("syn::Type::BareFn"),
            syn::Type::Group(_) => todo!("syn::Type::Group"),
            syn::Type::ImplTrait(_) => todo!("syn::Type::ImplTrait"),
            syn::Type::Infer(_) => todo!("syn::Type::Infer"),
            syn::Type::Macro(_) => todo!("syn::Type::Macro"),
            syn::Type::Never(_) => todo!("syn::Type::Never"),
            syn::Type::Paren(_) => todo!("syn::Type::Paren"),
            syn::Type::Path(p) => {
                syn::parse_quote!( #p )
                //z3::DatatypeAccessor::Sort(#p::constrained_type(context).z3_sort().clone()),
            }
            syn::Type::Ptr(_) => todo!("syn::Type::Path"),
            syn::Type::Reference(_) => todo!("syn::Type::Reference"),
            syn::Type::Slice(_) => todo!("syn::Type::Slice"),
            syn::Type::TraitObject(_) => todo!("syn::Type::TraitObject"),
            syn::Type::Tuple(_) => todo!("syn::Type::Tuple"),
            syn::Type::Verbatim(_) => todo!("syn::Type::Verbatim"),
            _ => todo!("syn::Type::<unknown>"),
        }
    }
}

#[cfg(test)]
mod test {
    use quote::ToTokens;
    use syn::parse_quote;

    use super::*;

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
        let mut result = i.trim().replace(['\n', '\t'], " ");
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
        let generated = parsed.constrained_type_new_fn(&[]);
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

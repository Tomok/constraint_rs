extern crate proc_macro;

use proc_macro2::Span;
use quote::ToTokens;
use syn::{spanned::Spanned, Token};

use thiserror::Error;

#[derive(Error, Debug)]
enum DeriveConstraintError {
    #[error("{element:?} with value {value:?} ({location:?}) not supported (yet)")]
    NotSupported {
        element: &'static str,
        value: String,
        location: Span,
    },
}

impl DeriveConstraintError {
    fn not_supported(element: &'static str, value: impl std::fmt::Debug + Spanned) -> Self {
        let value = format!("{:#?}", value);
        let location = value.span();
        Self::NotSupported {
            element,
            value,
            location,
        }
    }
}

#[proc_macro_attribute]
pub fn constrained_mod(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    println!("attr: \"{}\"", attr);
    let mut module: syn::ItemMod = syn::parse(item).unwrap();
    let mut parsed_structs = Vec::new();
    let mut parsed_impls = Vec::new();
    if let Some((_, ref mut items)) = module.content {
        for item in items.iter() {
            match item {
                syn::Item::Enum(_) => todo!(),
                syn::Item::Fn(_) => todo!(),
                syn::Item::Impl(i) => {
                    let parsed = ParsedImpl::try_from(i).unwrap();
                    parsed_impls.push(parsed);
                }
                syn::Item::Struct(s) => {
                    let parsed = ParsedStruct::from_item_struct(s);
                    parsed_structs.push(parsed);
                }
                syn::Item::Trait(_) => todo!(),
                syn::Item::TraitAlias(_) => todo!(),
                syn::Item::Type(_) => todo!(),
                syn::Item::Union(_) => todo!(),
                _ => todo!(),
            }
        }
        for p in parsed_structs {
            items.extend(p.to_syn_items());
        }
    } else {
        panic!("Module contents need to be in the same file, otherwise they cannot be parsed")
    }
    module.into_token_stream().into()
    //let bar: syn::ImplItemMethod = syn::parse_quote!("fn bar() -> bool {false}");
    //bar.into_token_stream().into()
}

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

impl From<ParsedStruct> for ParsedDeriveInput {
    fn from(value: ParsedStruct) -> Self {
        Self::Struct(value)
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

    fn from_item_struct(i: &syn::ItemStruct) -> Self {
        let fields = ParsedField::parse_fields(&i.fields);
        let ident = i.ident.clone();
        let typ = StructType::from(&i.fields);
        //todo: handle other fields in ItemStruct ?
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

#[derive(Debug, PartialEq, Eq, Clone)]
struct ParsedImpl<'s> {
    struct_ident: &'s syn::Ident,
    methods: Vec<ParsedMethod<'s>>,
}

impl<'s> TryFrom<&'s syn::ItemImpl> for ParsedImpl<'s> {
    type Error = DeriveConstraintError;

    fn try_from(value: &'s syn::ItemImpl) -> Result<Self, Self::Error> {
        let struct_ident = match &*(value.self_ty) {
            syn::Type::Path(ident_path) => {
                if ident_path.path.segments.len() != 1 {
                    // no derive for Path implementations implemented yet
                    return Err(DeriveConstraintError::not_supported(
                        "Item implementation for Path",
                        ident_path,
                    ));
                } else {
                    &ident_path.path.segments.first().unwrap().ident
                }
            }
            _ => {
                return Err(DeriveConstraintError::not_supported(
                    "implementation self type",
                    &value.self_ty,
                ))
            }
        };

        let mut methods = Vec::with_capacity(value.items.len());
        for item in &value.items {
            match item {
                syn::ImplItem::Const(_) => todo!(),
                syn::ImplItem::Fn(m) => {
                    let parsed = ParsedMethod::try_from(m)?;
                    methods.push(parsed);
                }
                syn::ImplItem::Type(_) => todo!(),
                syn::ImplItem::Macro(_) => todo!(),
                syn::ImplItem::Verbatim(_) => todo!(),
                _ => todo!(),
            }
        }
        Ok(Self {
            struct_ident,
            methods,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct ParsedMethod<'s> {
    signature: ParsedSignature<'s>,
    //todo: other fields
}

impl<'s> TryFrom<&'s syn::ImplItemFn> for ParsedMethod<'s> {
    type Error = DeriveConstraintError;

    fn try_from(value: &'s syn::ImplItemFn) -> Result<Self, Self::Error> {
        let signature = (&value.sig).try_into()?;
        Ok(Self { signature })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct ParsedSignature<'s> {
    ident: &'s syn::Ident,
    inputs: ParsedInputs<'s>,
    output: ParsedReturnType<'s>,
}

impl<'s> TryFrom<&'s syn::Signature> for ParsedSignature<'s> {
    type Error = DeriveConstraintError;

    fn try_from(value: &'s syn::Signature) -> Result<Self, Self::Error> {
        //todo evaluate other fields
        Ok(Self {
            ident: &value.ident,
            inputs: (&value.inputs).try_into()?,
            output: (&value.output).try_into()?,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct ParsedInputs<'s> {
    args: Vec<ParsedFnArg<'s>>,
}

impl<'s> TryFrom<&'s syn::punctuated::Punctuated<syn::FnArg, syn::token::Comma>>
    for ParsedInputs<'s>
{
    type Error = DeriveConstraintError;

    fn try_from(
        value: &'s syn::punctuated::Punctuated<syn::FnArg, syn::token::Comma>,
    ) -> Result<Self, Self::Error> {
        let mut args = Vec::with_capacity(value.len());
        for arg in value.iter() {
            args.push(arg.try_into()?);
        }
        Ok(Self { args })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum ParsedReturnType<'s> {
    Default,
    Type(&'s syn::Type),
}

impl<'s> TryFrom<&'s syn::ReturnType> for ParsedReturnType<'s> {
    type Error = DeriveConstraintError;

    fn try_from(value: &'s syn::ReturnType) -> Result<Self, Self::Error> {
        Ok(match value {
            syn::ReturnType::Default => Self::Default,
            syn::ReturnType::Type(_, t) => Self::Type(t.as_ref()),
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum ParsedFnArg<'s> {
    Receiver(ParsedReceiver),
    Typed(ParsedPatType<'s>),
}

impl<'s> TryFrom<&'s syn::FnArg> for ParsedFnArg<'s> {
    type Error = DeriveConstraintError;

    fn try_from(value: &'s syn::FnArg) -> Result<Self, Self::Error> {
        Ok(match value {
            syn::FnArg::Receiver(r) => Self::Receiver(r.try_into()?),
            syn::FnArg::Typed(t) => Self::Typed(t.try_into()?),
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// the self argument of a method see [syn::Receiver]
struct ParsedReceiver {
    mutable: bool,
}

impl<'s> TryFrom<&'s syn::Receiver> for ParsedReceiver {
    type Error = DeriveConstraintError;

    fn try_from(value: &'s syn::Receiver) -> Result<Self, Self::Error> {
        Ok(Self {
            mutable: value.mutability.is_some(),
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct ParsedPatType<'s> {
    ident: &'s syn::Ident,
    ty: ParsedType<'s>,
}

impl<'s> TryFrom<&'s syn::PatType> for ParsedPatType<'s> {
    type Error = DeriveConstraintError;

    fn try_from(value: &'s syn::PatType) -> Result<Self, Self::Error> {
        fn pat2ident(pat: &syn::Pat) -> Result<&syn::Ident, DeriveConstraintError> {
            if let syn::Pat::Ident(pat_ident) = pat {
                Ok(&pat_ident.ident)
            } else {
                Err(DeriveConstraintError::not_supported("Pat", pat))
            }
        }
        let ident = pat2ident(&value.pat)?;
        let ty = ParsedType::try_from(&*value.ty)?;
        Ok(Self { ident, ty })
    }
}
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ParsedType<'s> {
    Path(&'s syn::Path),
}

impl<'s> ParsedType<'s> {
    fn as_path(&self) -> Option<&&'s syn::Path> {
        if let Self::Path(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl<'s> TryFrom<&'s syn::Type> for ParsedType<'s> {
    type Error = DeriveConstraintError;

    fn try_from(value: &'s syn::Type) -> Result<Self, Self::Error> {
        match value {
            syn::Type::Path(p) => Ok(Self::Path(&p.path)),
            _ => Err(DeriveConstraintError::not_supported("Type", value)),
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

    #[test]
    fn test_parse_pat_type() {
        let input_fn: syn::ImplItemFn = syn::parse_quote! {
            fn func(foo: u64) {}
        };

        if let syn::FnArg::Typed(pat_type) = input_fn.sig.inputs.first().unwrap() {
            let parsed = ParsedPatType::try_from(pat_type).unwrap();
            assert_eq!("foo", &parsed.ident.to_string());
            assert_eq!(
                "u64",
                &parsed.ty.as_path().unwrap().to_token_stream().to_string()
            );
        } else {
            panic!("could not get pat_type from input")
        }
    }
}

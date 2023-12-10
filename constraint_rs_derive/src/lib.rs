extern crate proc_macro;

use itertools::Itertools;
use quote::ToTokens;

mod error;
mod to_rule_generation;

#[proc_macro_attribute]
pub fn constrained_mod(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut module: syn::ItemMod = syn::parse(item).unwrap();

    if let Some((_, ref mut items)) = module.content {
        let additional_items = module_items_to_derived_value_items(items);
        items.extend(additional_items);
    } else {
        panic!("Module contents need to be in the same file, otherwise they cannot be parsed")
    }
    module.into_token_stream().into()
    //let bar: syn::ImplItemMethod = syn::parse_quote!("fn bar() -> bool {false}");
    //bar.into_token_stream().into()
}

fn module_items_to_derived_value_items(items: &[syn::Item]) -> Vec<syn::Item> {
    let mut parsed_structs = Vec::new();
    let mut parsed_impls = Vec::new();
    for item in items.iter() {
        match item {
            syn::Item::Enum(_) => todo!("syn::Item::Enum"),
            syn::Item::Fn(_) => todo!("syn::Item::Fn"),
            syn::Item::Impl(i) => {
                let parsed = ParsedImpl::try_from(i).unwrap();
                parsed_impls.push(parsed);
            }
            syn::Item::Struct(s) => {
                let parsed = ParsedStruct::from_item_struct(s);
                parsed_structs.push(parsed);
            }
            syn::Item::Trait(_) => todo!("syn::Item::Trait"),
            syn::Item::TraitAlias(_) => todo!("syn::Item::TraitAlias"),
            syn::Item::Type(_) => todo!("syn::Item::Type"),
            syn::Item::Union(_) => todo!("syn::Item::Union"),
            syn::Item::Use(_) => { /* nothing to do here */ }
            unknown => todo!("syn::Item::<unknown>: {:#?}", unknown),
        }
    }
    let additional_items_capacity = 7 * parsed_structs.len();
    let mut additional_items = Vec::with_capacity(additional_items_capacity);
    for p in parsed_structs {
        let ident = p.ident();
        let relevant_impls = parsed_impls
            .iter()
            .filter(|i| i.struct_ident() == ident)
            .collect_vec();
        additional_items.extend(p.to_syn_items(relevant_impls));
    }
    assert!(
        additional_items_capacity >= additional_items.len(),
        "reserved ram for additional syn items suboptimal (initial capacity: {}, used: {})",
        additional_items_capacity,
        additional_items.len()
    );
    additional_items
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

enum ParsedDeriveInput {
    Struct(ParsedStruct),
    Enum(),  //todo
    Union(), //todo
}

impl ParsedDeriveInput {
    pub fn to_syn_items(&self) -> Vec<syn::Item> {
        match self {
            ParsedDeriveInput::Struct(s) => s.to_syn_items(vec![]),
            ParsedDeriveInput::Enum() => todo!("ParsedDeriveInput::Enum"),
            ParsedDeriveInput::Union() => todo!("ParsedDeriveInput::Union"),
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
            syn::Data::Enum(_) => todo!("syn::Data::Enum"),
            syn::Data::Union(_) => todo!("syn::Data::Union"),
        }
    }
}

impl From<ParsedStruct> for ParsedDeriveInput {
    fn from(value: ParsedStruct) -> Self {
        Self::Struct(value)
    }
}

mod parsed_struct;
use parsed_struct::ParsedStruct;

mod parsed_impl;
use parsed_impl::ParsedImpl;

#[cfg(test)]
mod tests;

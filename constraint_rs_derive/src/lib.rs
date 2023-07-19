extern crate proc_macro;

use itertools::Itertools;
use quote::ToTokens;

mod error;

#[proc_macro_attribute]
pub fn constrained_mod(
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut module: syn::ItemMod = syn::parse(item).unwrap();
  
    if let Some((_, ref mut items)) = module.content {
        let additional_items = module_items_to_derived_value_items(&items);
        items.extend(additional_items);
    } else {
        panic!("Module contents need to be in the same file, otherwise they cannot be parsed")
    }
    dbg!();
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
            _ => todo!("syn::Item::<unknown>"),
        }
    }
    let additional_items_capacity = 6 * parsed_structs.len();
    let mut additional_items = Vec::with_capacity(additional_items_capacity);
    for p in parsed_structs {
        let ident = p.ident();
        let relevant_impls = parsed_impls
            .iter()
            .filter(|i| i.struct_ident() == ident)
            .collect_vec();
        additional_items.extend(p.to_syn_items(relevant_impls));
    }
    assert_eq!(
        additional_items_capacity,
        additional_items.len(),
        "reserved ram for additional syn items suboptimal"
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
    pub fn to_syn_items(&self) -> [syn::Item; 6] {
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
        let input: syn::DeriveInput = syn::parse_quote!(
            #[derive(Debug, ConstrainedType)]
            struct Test;
        );
        let expected: [syn::Item; 6] = [
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
            syn::parse_quote!(
                impl<'s, 'ctx> TestConstrainedValue<'s, 'ctx> where 'ctx: 's {}
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
}

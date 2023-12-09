use proc_macro2::TokenStream;
use syn::{fold::Fold, Expr};

use super::*;

/// pretty please showed some inconsistencies
/// see input in [tests::test_pretty_print_vec_macro_with_one_param]
/// and [tests::test_pretty_print_vec_macro_with_one_param2]
/// this harmonizes them
struct SynHaromizer;
impl syn::fold::Fold for SynHaromizer {
    fn fold_expr_macro(&mut self, i: syn::ExprMacro) -> syn::ExprMacro {
        fn harmonize_token_stream(t: TokenStream) -> TokenStream {
            if let Ok(e) = syn::parse2::<Expr>(t.clone()) {
                let pretty = e.pretty_print();
                pretty.parse().unwrap()
            } else {
                t
            }
        }

        syn::ExprMacro {
            attrs: i.attrs,
            mac: syn::Macro {
                path: i.mac.path,
                bang_token: i.mac.bang_token,
                delimiter: i.mac.delimiter,
                tokens: harmonize_token_stream(i.mac.tokens),
            },
        }
    }
}

trait PrettyPrintAble {
    fn pretty_print(self) -> String;
}

impl PrettyPrintAble for syn::Expr {
    fn pretty_print(self) -> String {
        let item: syn::Item = syn::parse_quote!(fn f() {#self} );
        let pretty_with_extra = item.pretty_print();
        let open = pretty_with_extra.find('{').unwrap();
        let close = pretty_with_extra.rfind('}').unwrap();
        pretty_with_extra[open + 1..close].trim().to_string()
    }
}

impl PrettyPrintAble for syn::Item {
    fn pretty_print(self) -> String {
        let file = syn::File {
            attrs: vec![],
            items: vec![self],
            shebang: None,
        };
        file.pretty_print()
    }
}

impl PrettyPrintAble for syn::File {
    fn pretty_print(self) -> String {
        let harmonized = SynHaromizer.fold_file(self);
        prettyplease::unparse(&harmonized)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_pretty_print_expr() {
        let expr: syn::Expr = syn::parse_quote!(a + b);
        assert_eq!("a + b", expr.pretty_print());
    }

    const EXPECTED_VEC_MACRO_WITH_ONE_PARAM: &'static str = "vec![
        (\"f\", z3::DatatypeAccessor::Sort(< u64 as constraint_rs::HasConstrainedType
        >::constrained_type(context).z3_sort().clone(),),)
    ]";
    #[test]
    fn test_pretty_print_vec_macro_with_one_param() {
        let expr: syn::Expr = syn::parse_str(
            "vec![(\"f\", z3::DatatypeAccessor::Sort(< u64 as
            constraint_rs::HasConstrainedType >
            ::constrained_type(context).z3_sort().clone(),),)]",
        )
        .unwrap();
        assert_eq!(EXPECTED_VEC_MACRO_WITH_ONE_PARAM, expr.pretty_print());
    }
    #[test]
    fn test_pretty_print_vec_macro_with_one_param2() {
        let expr: syn::Expr = syn::parse_str(
            "vec![(\"f\", z3::DatatypeAccessor::Sort(< u64 as
            constraint_rs::HasConstrainedType >::constrained_type(context).z3_sort().clone(),),)]",
        )
        .unwrap();
        assert_eq!(EXPECTED_VEC_MACRO_WITH_ONE_PARAM, expr.pretty_print());
    }
}

mod derive_empty_struct;

mod derive_struct_with_one_field;

mod include_tests;

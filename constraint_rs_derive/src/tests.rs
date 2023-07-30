use super::*;

fn pretty_print(item: syn::Item) -> String {
    let file = syn::File {
        attrs: vec![],
        items: vec![item],
        shebang: None,
    };
    prettyplease::unparse(&file)
}

mod derive_empty_struct;

mod derive_module_with_empty_struct_and_one_func;

mod derive_struct_with_one_field;

use super::*;

#[test]
fn struct_with_one_field() {
    let input_items_str = include_str!("../../../tests/struct_with_one_field/input.rs");
    let expected_items_str =
        include_str!("../../../tests/struct_with_one_field/expected_output.rs");
    test_input_strings(input_items_str, expected_items_str);
}

#[test]
fn empty_struct() {
    let input_items_str = include_str!("../../../tests/empty_struct/input.rs");
    let expected_items_str = include_str!("../../../tests/empty_struct/expected_output.rs");
    test_input_strings(input_items_str, expected_items_str);
}

fn test_input_strings(input_items_str: &str, expected_items_str: &str) {
    let input: syn::ItemMod = {
        let input_mod_str = format!("mod test {{ {} }}", input_items_str);
        syn::parse_str(&input_mod_str).unwrap()
    };
    let expected: Vec<syn::Item> = {
        //syn does not support parsing to Vec<syn::Item> currently
        //so wrap it in a module and use the modules items
        let dummy_mod_str = format!("mod dummy {{ {} }}", expected_items_str);
        let m: syn::ItemMod = syn::parse_str(&dummy_mod_str).unwrap();
        m.content.unwrap().1
    };

    let res = module_items_to_derived_value_items(&input.content.unwrap().1);
    assert_eq!(expected.len(), res.len());
    for (e, r) in expected.into_iter().zip(res) {
        if e != r {
            let expectation_pretty_printed = e.clone().pretty_print();
            let generated_pretty_printed = r.clone().pretty_print();

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

# Files for tests via include! and include_str!
This folder contains files with multiple purposes:
- to validate, that the code to be derived fullfills the desired functionality it is used in `constraint_rs` tests
- once the derived code has the desired functionality, it can than be used as expected output for testing the derive methods in `constraint_rs_derive`

This way duplication between these areas can be avoided and it is automatically tested, that the generated code does compile.
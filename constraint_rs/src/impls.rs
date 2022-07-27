macro_rules! expose_fn_ref_self {
    ($i: ident ($($n:ident: $t:ty),*) -> $r:ty) => {
        pub fn $i(&self$(, $n: $t)*) -> $r {
            self.val.$i($(&$n.val)*).into()
        }
    };
    //to rename:
    ($i: ident - $inew: ident ($($n:ident: $t:ty),*) -> $r:ty) => {
        pub fn $inew(&self$(, $n: $t)*) -> $r {
            self.val.$i($(&$n.val)*).into()
        }
    };
}

mod int_types;
pub use int_types::*;

mod bool_type;
pub use bool_type::*;

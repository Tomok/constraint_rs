use proc_macro2::Span;
use syn::spanned::Spanned;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum DeriveConstraintError {
    #[error("{element:?} with value {value:?} ({location:?}) not supported (yet)")]
    NotSupported {
        element: &'static str,
        value: String,
        location: Span,
    },
}

impl DeriveConstraintError {
    pub fn not_supported(element: &'static str, value: impl std::fmt::Debug + Spanned) -> Self {
        let value = format!("{:#?}", value);
        let location = value.span();
        Self::NotSupported {
            element,
            value,
            location,
        }
    }
}

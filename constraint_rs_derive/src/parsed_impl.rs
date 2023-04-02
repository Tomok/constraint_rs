use crate::error::DeriveConstraintError;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParsedImpl<'s> {
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
#[cfg(test)]
mod tests {
    use quote::ToTokens;

    use super::*;
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

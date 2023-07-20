use itertools::Itertools;
use syn::ImplItemFn;

use crate::error::DeriveConstraintError;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParsedImpl<'s> {
    struct_ident: &'s syn::Ident,
    methods: Vec<ParsedMethod<'s>>,
}

impl<'s> ParsedImpl<'s> {
    pub fn struct_ident(&self) -> &'s syn::Ident {
        self.struct_ident
    }

    pub fn methods(&self) -> &[ParsedMethod] {
        self.methods.as_ref()
    }
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
                syn::ImplItem::Const(_) => todo!("syn::ImplItem::Const"),
                syn::ImplItem::Fn(m) => {
                    let parsed = ParsedMethod::try_from(m)?;
                    methods.push(parsed);
                }
                syn::ImplItem::Type(_) => todo!("syn::ImplItem::Type"),
                syn::ImplItem::Macro(_) => todo!("syn::ImplItem::Macro"),
                syn::ImplItem::Verbatim(_) => todo!("syn::ImplItem::Verbatim"),
                _ => todo!("syn::ImplItem::<unknown>"),
            }
        }
        Ok(Self {
            struct_ident,
            methods,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParsedMethod<'s> {
    signature: ParsedSignature<'s>,
    visibility: ParsedVisibility<'s>,
    block: ParsedBlock<'s>,
}

impl<'s> ParsedMethod<'s> {
    pub fn ident(&self) -> &'s syn::Ident {
        self.signature.ident
    }

    pub fn signature(&self) -> &ParsedSignature<'s> {
        &self.signature
    }

    pub fn block(&self) -> &ParsedBlock<'s> {
        &self.block
    }

    pub fn to_constrained_value_impl_func(&self) -> ImplItemFn {
        let func_ident = self.ident();
        let func_params = self.signature.inputs.to_constrained_value_impl_func_args();
        let return_type = self.signature.output.to_constrained_type_value_stmt();
        let apply_args = self
            .signature
            .inputs
            .to_constrained_value_impl_func_apply_args();
        let applied_fn_stmt: syn::Stmt = syn::parse_quote! {
            let applied_fn = self.typ.#func_ident.apply(&#apply_args);
        };
        let context_var: syn::Expr = syn::parse_quote! {self.typ.context};
        let constrained_type_call = self.signature.output.constrained_type_call(&context_var);
        let return_stmt: syn::Expr = syn::parse_quote! {
            #constrained_type_call
            .value_from_z3_dynamic(applied_fn)
            .unwrap()
        };
        let vis = self.visibility.0;
        syn::parse_quote! {
            #vis fn #func_ident(#(#func_params),*) -> #return_type {
                #applied_fn_stmt;
                #return_stmt
            }
        }
    }
}

impl<'s> TryFrom<&'s syn::ImplItemFn> for ParsedMethod<'s> {
    type Error = DeriveConstraintError;

    fn try_from(value: &'s syn::ImplItemFn) -> Result<Self, Self::Error> {
        let signature = (&value.sig).try_into()?;
        let visibility = (&value.vis).into();
        let block = (&value.block).into();
        Ok(Self {
            signature,
            visibility,
            block,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParsedSignature<'s> {
    ident: &'s syn::Ident,
    inputs: ParsedInputs<'s>,
    output: ParsedReturnType<'s>,
}

impl<'s> ParsedSignature<'s> {
    pub fn inputs(&self) -> &ParsedInputs<'s> {
        &self.inputs
    }

    pub fn output(&self) -> &ParsedReturnType<'s> {
        &self.output
    }
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
struct ParsedVisibility<'s>(&'s syn::Visibility);

impl<'s> From<&'s syn::Visibility> for ParsedVisibility<'s> {
    fn from(value: &'s syn::Visibility) -> Self {
        Self(value)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParsedBlock<'s>(pub &'s syn::Block);

impl<'s> From<&'s syn::Block> for ParsedBlock<'s> {
    fn from(value: &'s syn::Block) -> Self {
        Self(value)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParsedInputs<'s> {
    args: Vec<ParsedFnArg<'s>>,
}
impl<'s> ParsedInputs<'s> {
    fn to_constrained_value_impl_func_apply_args(&self) -> syn::ExprArray {
        //[&a.z3().clone().into(), &b.z3().clone().into()];
        let params = self
            .iter()
            .map(|p| p.to_constrained_value_impl_func_apply_arg());
        //let params = injected_params.iter().chain(params);
        syn::parse_quote! {[#(&#params),*]}
    }

    fn to_constrained_value_impl_func_args(&self) -> Vec<syn::FnArg> {
        //inject self parameter if not existend, as we need it to get the type to get to the z3 function definition ...
        //TODO: is there a better way?
        let args = if self.args.first().map_or(false, ParsedFnArg::is_receiver) {
            [].iter().chain(self.args.iter())
        } else {
            [ParsedFnArg::Receiver(ParsedReceiver { mutable: false })]
                .iter()
                .chain(self.args.iter())
        };
        args.map(ParsedFnArg::to_constrained_value_impl_func_args)
            .collect_vec()
    }

    pub fn iter(&self) -> impl Iterator<Item = &ParsedFnArg> {
        self.args.iter()
    }
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
pub enum ParsedReturnType<'s> {
    Default,
    Type(&'s syn::Type),
}

impl<'s> ParsedReturnType<'s> {
    pub fn to_constrained_type_value_stmt(&self) -> syn::Type {
        // <<u64 as HasConstrainedType>::ConstrainedType as ConstrainedType<'s, 'ctx>>::ValueType
        let constrained_type_stmt = self.to_constrained_type_stmt();
        syn::parse_quote! {
            <#constrained_type_stmt as ConstrainedType<'s, 'ctx>>::ValueType
        }
    }

    pub fn to_constrained_type_stmt(&self) -> syn::Type {
        // <u64 as HasConstrainedType<'s, 'ctx>>::ConstrainedType
        match self {
            ParsedReturnType::Default => todo!("ParsedReturnType::Default"), //syn::parse_quote!{()},
            ParsedReturnType::Type(t) => syn::parse_quote! {
                <#t as HasConstrainedType<'s, 'ctx>>::ConstrainedType
            },
        }
    }

    pub fn constrained_type_call(&self, context_var_ident: &syn::Expr) -> syn::ExprCall {
        match self {
            ParsedReturnType::Default => todo!("ParsedReturnType::Default"),
            ParsedReturnType::Type(t) => syn::parse_quote! {
                <#t as HasConstrainedType>::constrained_type(#context_var_ident)
            },
        }
    }
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
pub enum ParsedFnArg<'s> {
    Receiver(ParsedReceiver),
    Typed(ParsedPatType<'s>),
}

impl<'s> ParsedFnArg<'s> {
    pub fn constrained_type_call(&self, context_var_expr: &syn::Expr) -> syn::ExprCall {
        match self {
            ParsedFnArg::Receiver(_) => todo!("ParsedFnArg::Receiver"),
            ParsedFnArg::Typed(t) => t.constrained_type_call(context_var_expr),
        }
    }

    pub fn to_constrained_value_impl_func_args(&self) -> syn::FnArg {
        match self {
            ParsedFnArg::Receiver(r) => r.to_constrained_value_impl_func_args(),
            ParsedFnArg::Typed(t) => t.constrained_value_impl_func_args(),
        }
    }

    pub fn ident(&self) -> &syn::Ident {
        match self {
            ParsedFnArg::Receiver(_) => todo!("Ident for ParsedFnArg::Receiver"),
            ParsedFnArg::Typed(t) => t.ident(),
        }
    }

    pub fn to_constrained_value_impl_func_apply_arg(&self) -> syn::Expr {
        match self {
            ParsedFnArg::Receiver(_) => {
                todo!("to_constrained_value_impl_func_apply_arg ParsedFnArg::Receiver")
            }
            //a.z3().clone().into(),
            ParsedFnArg::Typed(t) => t.constrained_value_impl_func_apply_arg(),
        }
    }

    /// Returns `true` if the parsed fn arg is [`Receiver`].
    ///
    /// [`Receiver`]: ParsedFnArg::Receiver
    #[allow(dead_code)]
    #[must_use]
    pub fn is_receiver(&self) -> bool {
        matches!(self, Self::Receiver(..))
    }

    /// Returns `true` if the parsed fn arg is [`Typed`].
    ///
    /// [`Typed`]: ParsedFnArg::Typed
    #[allow(dead_code)]
    #[must_use]
    pub fn is_typed(&self) -> bool {
        matches!(self, Self::Typed(..))
    }
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
pub struct ParsedReceiver {
    mutable: bool,
}
impl ParsedReceiver {
    fn to_constrained_value_impl_func_args(&self) -> syn::FnArg {
        match self.mutable {
            true => syn::parse_quote! {&mut self},
            false => syn::parse_quote! {&self},
        }
    }
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
pub struct ParsedPatType<'s> {
    ident: &'s syn::Ident,
    ty: ParsedType<'s>,
}

impl<'s> ParsedPatType<'s> {
    fn constrained_type_call(&self, context_var_expr: &syn::Expr) -> syn::ExprCall {
        self.ty.constrained_type_call(context_var_expr)
    }

    fn ident(&self) -> &syn::Ident {
        self.ident
    }

    fn constrained_value_impl_func_args(&self) -> syn::FnArg {
        let ty = self.ty.constrained_value_stmt();
        let ident = self.ident;
        syn::parse_quote! { #ident : &#ty }
    }

    fn constrained_value_impl_func_apply_arg(&self) -> syn::Expr {
        //a.z3().clone().into(),
        let ident = self.ident;
        syn::parse_quote! { #ident.z3().clone().into() }
    }
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
pub enum ParsedType<'s> {
    Path(&'s syn::Path),
}

impl<'s> ParsedType<'s> {
    fn constrained_type_stmt(&self) -> syn::TypePath {
        match self {
            ParsedType::Path(p) => syn::parse_quote! {
                <#p as HasConstrainedType<'s, 'ctx>>::ConstrainedType
            },
        }
    }

    fn constrained_value_stmt(&self) -> syn::TypePath {
        //<<<u64> as HasConstrainedType<'s, 'ctx>>::ConstrainedType as ConstrainedType<'s, 'ctx>>::ValueType
        let ty = self.constrained_type_stmt();
        syn::parse_quote! {<#ty as ConstrainedType<'s, 'ctx>>::ValueType}
    }

    fn constrained_type_call(&self, context_var_expr: &syn::Expr) -> syn::ExprCall {
        match self {
            ParsedType::Path(p) => {
                syn::parse_quote! {  <#p as HasConstrainedType>::constrained_type(#context_var_expr) }
            }
        }
    }

    #[cfg(test)]
    pub fn as_path(&self) -> Option<&&'s syn::Path> {
        let Self::Path(v) = self;
        Some(v)
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

use crate::parsed_impl::ParsedBlock;

pub trait ToRuleGeneration {
    type Output;

    ///converts [self] to rust statements generating code to pass the rules equivalent to [self] to z3
    fn to_rule_generation(&self, context_variable_name_prefix: &str) -> Self::Output;
}

impl<'s> ToRuleGeneration for ParsedBlock<'s> {
    type Output = syn::Expr;

    fn to_rule_generation(&self, context_variable_name_prefix: &str) -> Self::Output {
        self.0.to_rule_generation(context_variable_name_prefix)
    }
}

impl ToRuleGeneration for syn::Block {
    type Output = syn::Expr;

    fn to_rule_generation(&self, context_variable_name_prefix: &str) -> Self::Output {
        //let child_stmts = self.stmts.iter().map(|s| s.to_rule_generation_statements(context_variable_name_prefix));
        match self.stmts.len() {
            0 => todo!("Empty syn::Block"),
            1 => match &self.stmts[0] {
                syn::Stmt::Local(_) => todo!("syn::Local"),
                syn::Stmt::Item(_) => todo!("syn::Item"),
                syn::Stmt::Expr(e, None) => e.to_rule_generation(context_variable_name_prefix),
                syn::Stmt::Expr(_e, Some(_)) => todo!("single expression in syn::Block with ;"),
                syn::Stmt::Macro(_) => todo!("syn::Macro"),
            },
            _ => {
                let stmts = self
                    .stmts
                    .iter()
                    .map(|s| s.to_rule_generation(context_variable_name_prefix));
                syn::parse_quote!({#(#stmts)*})
            }
        }
    }
}

impl ToRuleGeneration for syn::Stmt {
    type Output = syn::Stmt;

    fn to_rule_generation(&self, context_variable_name_prefix: &str) -> Self::Output {
        match self {
            syn::Stmt::Local(l) => {
                syn::Stmt::Local(l.to_rule_generation(context_variable_name_prefix))
            }
            syn::Stmt::Item(_) => todo!("syn::Item"),
            syn::Stmt::Expr(_e, Some(_)) => todo!("syn::Expr with Semicolon"),
            syn::Stmt::Expr(e, None) => {
                syn::Stmt::Expr(e.to_rule_generation(context_variable_name_prefix), None)
            }
            syn::Stmt::Macro(_) => todo!("syn::Macro"),
        }
    }
}

impl ToRuleGeneration for syn::Local {
    type Output = syn::Local;

    fn to_rule_generation(&self, context_variable_name_prefix: &str) -> Self::Output {
        if !self.attrs.is_empty() {
            todo!("syn::Local with non empty attributes");
        }
        let let_token = self.let_token.clone();
        let pat = self.pat.to_rule_generation(context_variable_name_prefix);
        let init = self
            .init
            .as_ref()
            .map(|i| i.to_rule_generation(context_variable_name_prefix));
        let semi_token = self.semi_token.clone();
        syn::Local {
            attrs: vec![],
            let_token,
            pat,
            init,
            semi_token,
        }
    }
}

impl ToRuleGeneration for syn::Pat {
    type Output = syn::Pat;

    fn to_rule_generation(&self, context_variable_name_prefix: &str) -> Self::Output {
        match self {
            syn::Pat::Const(_) => todo!("syn::Pat::Const"),
            syn::Pat::Ident(pi) => {
                syn::Pat::Ident(pi.to_rule_generation(context_variable_name_prefix))
            }
            syn::Pat::Lit(_) => todo!("syn::Pat::Lit"),
            syn::Pat::Macro(_) => todo!("syn::Pat::Macro"),
            syn::Pat::Or(_) => todo!("syn::Pat::Or"),
            syn::Pat::Paren(_) => todo!("syn::Pat::Paren"),
            syn::Pat::Path(_) => todo!("syn::Pat::Path"),
            syn::Pat::Range(_) => todo!("syn::Pat::Range"),
            syn::Pat::Reference(_) => todo!("syn::Pat::Reference"),
            syn::Pat::Rest(_) => todo!("syn::Pat::Rest"),
            syn::Pat::Slice(_) => todo!("syn::Pat::Slice"),
            syn::Pat::Struct(_) => todo!("syn::Pat::Struct"),
            syn::Pat::Tuple(_) => todo!("syn::Pat::Tuple"),
            syn::Pat::TupleStruct(_) => todo!("syn::Pat::TupleStruct"),
            syn::Pat::Type(_) => todo!("syn::Pat::Type"),
            syn::Pat::Verbatim(_) => todo!("syn::Pat::Verbatim"),
            syn::Pat::Wild(_) => todo!("syn::Pat::Wild"),
            unknown => todo!("syn::Pat::_ - {:#?}", unknown),
        }
    }
}

impl ToRuleGeneration for syn::PatIdent {
    type Output = syn::PatIdent;

    fn to_rule_generation(&self, _context_variable_name_prefix: &str) -> Self::Output {
        if !self.attrs.is_empty() {
            todo!("syn::PatIdent with non empty attributes");
        }
        if self.by_ref.is_some() {
            todo!("syn::PatIdent with by_ref");
        }
        if self.mutability.is_some() {
            todo!("mutable syn::PatIdent");
        }
        if self.subpat.is_some() {
            todo!("PatIdent with subpat");
        }
        syn::PatIdent {
            attrs: vec![],
            by_ref: None,
            mutability: None,
            ident: self.ident.clone(), //TODO: avoid naming conflicts...
            subpat: None,
        }
    }
}

impl ToRuleGeneration for syn::LocalInit {
    type Output = syn::LocalInit;

    fn to_rule_generation(&self, context_variable_name_prefix: &str) -> Self::Output {
        let eq_token = self.eq_token.clone();
        if self.diverge.is_some() {
            todo!("syn::LocalInit with diverge != None");
        }
        let expr = Box::new(self.expr.to_rule_generation(context_variable_name_prefix));
        syn::LocalInit {
            eq_token,
            expr,
            diverge: None,
        }
    }
}

impl ToRuleGeneration for syn::Expr {
    type Output = syn::Expr;

    fn to_rule_generation(&self, context_variable_name_prefix: &str) -> Self::Output {
        match self {
            syn::Expr::Array(_) => todo!("syn::Expr::Array"),
            syn::Expr::Assign(_) => todo!("syn::Expr::Assign"),
            syn::Expr::Async(_) => todo!("syn::Expr::Async"),
            syn::Expr::Await(_) => todo!("syn::Expr::Await"),
            syn::Expr::Binary(e) => {
                syn::Expr::MethodCall(e.to_rule_generation(context_variable_name_prefix))
            }
            syn::Expr::Block(_) => todo!("syn::Expr::Binary"),
            syn::Expr::Break(_) => todo!("syn::Expr::Break"),
            syn::Expr::Call(_) => todo!("syn::Expr::Call"),
            syn::Expr::Cast(_) => todo!("syn::Expr::Cast"),
            syn::Expr::Closure(_) => todo!("syn::Expr::Closure"),
            syn::Expr::Const(_) => todo!("syn::Expr::Const"),
            syn::Expr::Continue(_) => todo!("syn::Expr::Continue"),
            syn::Expr::Field(f) => f.to_rule_generation(context_variable_name_prefix),
            syn::Expr::ForLoop(_) => todo!("syn::Expr::ForLoop"),
            syn::Expr::Group(_) => todo!("syn::Expr::Group"),
            syn::Expr::If(_) => todo!("syn::Expr::If"),
            syn::Expr::Index(_) => todo!("syn::Expr::Index"),
            syn::Expr::Infer(_) => todo!("syn::Expr::Infer"),
            syn::Expr::Let(_) => todo!("syn::Expr::Let"),
            syn::Expr::Lit(_) => todo!("syn::Expr::Lit"),
            syn::Expr::Loop(_) => todo!("syn::Expr::Loop"),
            syn::Expr::Macro(_) => todo!("syn::Expr::Macro"),
            syn::Expr::Match(_) => todo!("syn::Expr::Match"),
            syn::Expr::MethodCall(_) => todo!("syn::Expr::MethodCall"),
            syn::Expr::Paren(_) => todo!("syn::Expr::Paren"),
            syn::Expr::Path(e) => e.to_rule_generation(context_variable_name_prefix),
            syn::Expr::Range(_) => todo!("syn::Expr::Range"),
            syn::Expr::Reference(_) => todo!("syn::Expr::Reference"),
            syn::Expr::Repeat(_) => todo!("syn::Expr::Repeat"),
            syn::Expr::Return(_) => todo!("syn::Expr::Return"),
            syn::Expr::Struct(_) => todo!("syn::Expr::Struct"),
            syn::Expr::Try(_) => todo!("syn::Expr::Try"),
            syn::Expr::TryBlock(_) => todo!("syn::Expr::TryBlock"),
            syn::Expr::Tuple(_) => todo!("syn::Expr::Tuple"),
            syn::Expr::Unary(_) => todo!("syn::Expr::Unary"),
            syn::Expr::Unsafe(_) => todo!("syn::Expr::Unsafe"),
            syn::Expr::Verbatim(_) => todo!("syn::Expr::Verbatim"),
            syn::Expr::While(_) => todo!("syn::Expr::While"),
            syn::Expr::Yield(_) => todo!("syn::Expr::Yield"),
            _ => todo!("syn::Expr::<unknown>"),
        }
    }
}

impl ToRuleGeneration for syn::ExprBinary {
    type Output = syn::ExprMethodCall;

    fn to_rule_generation(&self, context_variable_name_prefix: &str) -> Self::Output {
        let left = self.left.to_rule_generation(context_variable_name_prefix);
        let right = self.right.to_rule_generation(context_variable_name_prefix);
        let call: syn::ExprMethodCall = match self.op {
            syn::BinOp::Add(_) => syn::parse_quote! {#left.add(&#right)},
            syn::BinOp::Sub(_) => todo!("syn::BinOp::Sub"),
            syn::BinOp::Mul(_) => todo!("syn::BinOp::Mul"),
            syn::BinOp::Div(_) => todo!("syn::BinOp::Div"),
            syn::BinOp::Rem(_) => todo!("syn::BinOp::Rem"),
            syn::BinOp::And(_) => todo!("syn::BinOp::And"),
            syn::BinOp::Or(_) => todo!("syn::BinOp::Or"),
            syn::BinOp::BitXor(_) => todo!("syn::BinOp::BitXor"),
            syn::BinOp::BitAnd(_) => todo!("syn::BinOp::BitAnd"),
            syn::BinOp::BitOr(_) => todo!("syn::BinOp::BitOr"),
            syn::BinOp::Shl(_) => todo!("syn::BinOp::Shl"),
            syn::BinOp::Shr(_) => todo!("syn::BinOp::Shr"),
            syn::BinOp::Eq(_) => todo!("syn::BinOp::Eq"),
            syn::BinOp::Lt(_) => todo!("syn::BinOp::Lt"),
            syn::BinOp::Le(_) => todo!("syn::BinOp::Le"),
            syn::BinOp::Ne(_) => todo!("syn::BinOp::Ne"),
            syn::BinOp::Ge(_) => todo!("syn::BinOp::Ge"),
            syn::BinOp::Gt(_) => todo!("syn::BinOp::Gt"),
            syn::BinOp::AddAssign(_) => todo!("syn::BinOp::AddAssign"),
            syn::BinOp::SubAssign(_) => todo!("syn::BinOp::SubAssign"),
            syn::BinOp::MulAssign(_) => todo!("syn::BinOp::MulAssign"),
            syn::BinOp::DivAssign(_) => todo!("syn::BinOp::DivAssign"),
            syn::BinOp::RemAssign(_) => todo!("syn::BinOp::RemAssign"),
            syn::BinOp::BitXorAssign(_) => todo!("syn::BinOp::BitXorAssign"),
            syn::BinOp::BitAndAssign(_) => todo!("syn::BinOp::BitAndAssign"),
            syn::BinOp::BitOrAssign(_) => todo!("syn::BinOp::BitOrAssign"),
            syn::BinOp::ShlAssign(_) => todo!("syn::BinOp::ShlAssign"),
            syn::BinOp::ShrAssign(_) => todo!("syn::BinOp::ShrAssign"),
            _ => todo!("syn::BinOp::<unknown>"),
        };
        call
    }
}

impl ToRuleGeneration for syn::ExprPath {
    type Output = syn::Expr;

    fn to_rule_generation(&self, _context_variable_name_prefix: &str) -> Self::Output {
        syn::Expr::Path(self.clone())
    }
}

impl ToRuleGeneration for syn::ExprField {
    type Output = syn::Expr;

    fn to_rule_generation(&self, _context_variable_name_prefix: &str) -> Self::Output {
        if !self.attrs.is_empty() {
            todo!("Attributes for syn::ExprField")
        }
        if let &syn::Expr::Path(expr_path) = &self.base.as_ref() {
            if expr_path.path.leading_colon.is_some() || !expr_path.path.is_ident("self") {
                todo!("None self path in syn::ExprPath in syn::ExprField.base");
            }
            if !expr_path.attrs.is_empty() {
                todo!("Attributes from syn::ExprPath in syn::ExprField.base");
            }
            if expr_path.qself.is_some() {
                todo!("QSelf from syn::ExprPath in syn::ExprField.base");
            }
            let member = &self.member;
            return syn::parse_quote! {
                self_dummy.#member
            };
        }

        todo!("syn::ExprField for not self");
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_simple_add_block() {
        let input: syn::Block = syn::parse_quote!({ a + b });
        let expected: syn::Expr = syn::parse_quote!(a.add(&b));
        let res = input.to_rule_generation("A.add");
        assert_eq!(expected, res);
    }
}

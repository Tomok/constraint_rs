use crate::parsed_impl::ParsedBlock;

pub trait ToRuleGenerationExpression {
    ///converts [self] to rust statements generating code to pass the rules equivalent to [self] to z3
    fn to_rule_generation_statements(&self, context_variable_name_prefix: &str) -> syn::Expr;
}

impl<'s> ToRuleGenerationExpression for ParsedBlock<'s> {
    fn to_rule_generation_statements(&self, context_variable_name_prefix: &str) -> syn::Expr {
        self.0
            .to_rule_generation_statements(context_variable_name_prefix)
    }
}

impl ToRuleGenerationExpression for syn::Block {
    fn to_rule_generation_statements(&self, context_variable_name_prefix: &str) -> syn::Expr {
        //let child_stmts = self.stmts.iter().map(|s| s.to_rule_generation_statements(context_variable_name_prefix));
        match self.stmts.len() {
            0 => todo!("Empty Blocks"),
            1 => self.stmts[0].to_rule_generation_statements(context_variable_name_prefix),
            _ => todo!("Blocks with more than one statement"),
        }
    }
}

impl ToRuleGenerationExpression for syn::Stmt {
    fn to_rule_generation_statements(&self, context_variable_name_prefix: &str) -> syn::Expr {
        match self {
            syn::Stmt::Local(_) => todo!("syn::Local"),
            syn::Stmt::Item(_) => todo!("syn::Item"),
            syn::Stmt::Expr(_e, Some(_)) => todo!("syn::Expr with Semicolon"),
            syn::Stmt::Expr(e, None) => {
                e.to_rule_generation_statements(context_variable_name_prefix)
            }
            syn::Stmt::Macro(_) => todo!("syn::Macro"),
        }
    }
}

impl ToRuleGenerationExpression for syn::Expr {
    fn to_rule_generation_statements(&self, context_variable_name_prefix: &str) -> syn::Expr {
        match self {
            syn::Expr::Array(_) => todo!("syn::Expr::Array"),
            syn::Expr::Assign(_) => todo!("syn::Expr::Assign"),
            syn::Expr::Async(_) => todo!("syn::Expr::Async"),
            syn::Expr::Await(_) => todo!("syn::Expr::Await"),
            syn::Expr::Binary(e) => e.to_rule_generation_statements(context_variable_name_prefix),
            syn::Expr::Block(_) => todo!("syn::Expr::Binary"),
            syn::Expr::Break(_) => todo!("syn::Expr::Break"),
            syn::Expr::Call(_) => todo!("syn::Expr::Call"),
            syn::Expr::Cast(_) => todo!("syn::Expr::Cast"),
            syn::Expr::Closure(_) => todo!("syn::Expr::Closure"),
            syn::Expr::Const(_) => todo!("syn::Expr::Const"),
            syn::Expr::Continue(_) => todo!("syn::Expr::Continue"),
            syn::Expr::Field(_) => todo!("syn::Expr::Field"),
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
            syn::Expr::Path(e) => e.to_rule_generation_statements(context_variable_name_prefix),
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

impl ToRuleGenerationExpression for syn::ExprBinary {
    fn to_rule_generation_statements(&self, context_variable_name_prefix: &str) -> syn::Expr {
        let left = self
            .left
            .to_rule_generation_statements(context_variable_name_prefix);
        let right = self
            .right
            .to_rule_generation_statements(context_variable_name_prefix);
        let call: syn::ExprMethodCall = match self.op {
            syn::BinOp::Add(_) => syn::parse_quote! {(#left).add(&#right)},
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
        syn::Expr::MethodCall(call)
    }
}

impl ToRuleGenerationExpression for syn::ExprPath {
    fn to_rule_generation_statements(&self, _context_variable_name_prefix: &str) -> syn::Expr {
        syn::Expr::Path(self.clone())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_simple_add_block() {
        let input: syn::Block = syn::parse_quote!({ a + b });
        let expected: syn::Expr = syn::parse_quote!((a).add(&b));
        let res = input.to_rule_generation_statements("A.add");
        assert_eq!(expected, res);
    }
}
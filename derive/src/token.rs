use std::collections::HashMap;

use quote::{ToTokens, quote};
use syn::{ExprMatch, Ident, parse_macro_input};

use syn::spanned::Spanned;

pub(crate) fn derive_tokens(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let expr = parse_macro_input!(input as ExprMatch);

    let mut tokens = HashMap::new();

    for arm in &expr.arms {
        use syn::{Error, Expr, Lit, Pat};

        if let Some((keyword, guard)) = &arm.guard {
            return Error::new(
                keyword.span().join(guard.span()).unwrap(),
                "[Token] unsupport guard clause: `if ..`",
            )
            .into_compile_error()
            .into();
        }

        let token_ident;

        if let Expr::Path(expr) = &*arm.body {
            use quote::ToTokens;

            if let Ok(ident) = syn::parse::<Ident>(expr.to_token_stream().into()) {
                token_ident = ident;
            } else {
                return Error::new(arm.body.span(), "[Token] only support Ident body.")
                    .into_compile_error()
                    .into();
            }
        } else {
            return Error::new(arm.body.span(), "[Token] only support Ident body.")
                .into_compile_error()
                .into();
        }

        if let Pat::Lit(v) = &arm.pat {
            if let Lit::Str(v) = &v.lit {
                let v = v.value();

                if tokens.contains_key(&v) {
                    return Error::new(arm.pat.span(), "[Token] conflict token defines.")
                        .into_compile_error()
                        .into();
                }

                tokens.insert(v, token_ident);
                continue;
            }
        }

        return Error::new(
            arm.pat.span(),
            "[Token] only support string pattern: \"...\".",
        )
        .into_compile_error()
        .into();
    }

    let mut prefix = HashMap::<&str, Vec<&str>>::new();

    for token in tokens.keys() {
        let mut start_with = vec![];

        for comp in tokens.keys() {
            if comp.starts_with(token) && comp != token {
                start_with.push(comp.as_str());
            }
        }

        prefix.insert(token, start_with);
    }

    let mut stmts = vec![];

    for (key, lookahead) in prefix {
        let ident = &tokens[key];
        let doc = format!("Token `{}`", key);

        stmts.push(quote!  {

            #[doc=#doc]
            #[derive(Debug, PartialEq, Clone)]
            #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
            pub struct #ident<I>(pub I);

            impl<I,E> parserc::syntax::Syntax<I,E> for #ident<I>
            where
                I: parserc::input::Input + parserc::input::StartWith<&'static [u8]> + Clone,
                E: parserc::errors::ParseError<I::Position>,
            {
                 fn parse(input: I) -> parserc::errors::Result<Self, I, E> {
                     use parserc::parser::Parser;
                     #(
                         if let (Some(_),_) = parserc::parser::keyword(#lookahead.as_bytes()).ok().parse(input.clone())? {
                             return Err(parserc::errors::ControlFlow::Recovable(parserc::errors::ErrorKind::Token(#key,input.to_span()).into()));
                         }
                     )*

                     parserc::parser::keyword(#key.as_bytes())
                         .map(|v| Self(v))
                         .map_err(|_:E| parserc::errors::ErrorKind::Token(#key,input.to_span()).into())
                         .parse(input.clone())
                 }
            }

            impl<I> parserc::span::ToSpan<I::Position> for #ident<I>
            where
                I: parserc::input::Input,
            {
                fn to_span(&self) -> parserc::span::Span<I::Position> {
                    self.0.to_span()
                }
            }
        });
    }

    let mut variants = vec![];
    let mut variant_stmts = vec![];

    let mut keys = tokens.keys().collect::<Vec<_>>();

    keys.sort_by(|a, b| b.len().cmp(&a.len()));

    for key in keys {
        let ident = &tokens[key];
        variants.push(quote! {
            #ident(#ident<I>)
        });

        variant_stmts.push(quote! {
            let (token,input) = #ident::into_parser().ok().parse(input)?;

            if let Some(token) = token {
                return Ok((Self::#ident(token),input));
            }
        });
    }

    let ident = &expr.expr;
    let ident_name = expr.expr.to_token_stream().to_string();

    let token_stream = quote! {
        #(#stmts)*

        #[doc="Token parser"]
        #[derive(Debug, PartialEq, Clone)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub enum #ident<I>
        where
            I: parserc::input::Input + parserc::input::StartWith<&'static [u8]> + Clone,
        {
            #(#variants),*
        }

        impl<I,E> parserc::syntax::Syntax<I,E> for #ident<I>
        where
            I: parserc::input::Input + parserc::input::StartWith<&'static [u8]> + Clone,
            E: parserc::errors::ParseError<I::Position>,
        {
             fn parse(input: I) -> parserc::errors::Result<Self, I, E> {
                 use parserc::parser::Parser;
                 #(#variant_stmts)*

                 return Err(parserc::errors::ControlFlow::Recovable(parserc::errors::ErrorKind::Token(#ident_name,input.to_span()).into()));
             }
        }
    }
    .into();

    token_stream
}

use std::collections::HashSet;

use quote::{ToTokens, quote};
use syn::{
    Attribute, Error, ExprMatch, Fields, Index, Item, ItemEnum, ItemStruct, Type,
    parse_macro_input, spanned::Spanned,
};

#[proc_macro_derive(Syntax, attributes(error, input, fatal))]
pub fn derive_syntax_trait(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item = parse_macro_input!(input as Item);

    match item {
        Item::Enum(item) => drive_syntax_enum(item),
        Item::Struct(item) => drive_syntax_struct(item),
        _ => Error::new(item.span(), "Syntax: unsupport type")
            .into_compile_error()
            .into(),
    }
}

fn syntax_error_type(attrs: &[Attribute]) -> Option<proc_macro2::TokenStream> {
    for attr in attrs {
        if attr.path().is_ident("error") {
            let expr: Type = attr.parse_args().unwrap();
            return Some(expr.to_token_stream());
        }
    }

    None
}

fn syntax_input_type(attrs: &[Attribute]) -> Option<proc_macro2::TokenStream> {
    for attr in attrs {
        if attr.path().is_ident("input") {
            let expr: Type = attr.parse_args().unwrap();
            return Some(expr.to_token_stream());
        }
    }

    None
}

struct DriveFields {
    init_stmt: proc_macro2::TokenStream,
    parse_stmt: proc_macro2::TokenStream,
    span_stmt: proc_macro2::TokenStream,
}

fn drive_fields(fields: &Fields, variant_fields: bool) -> DriveFields {
    let mut fatal_fields = HashSet::new();

    for (offset, field) in fields.iter().enumerate() {
        for attr in field.attrs.iter() {
            match attr.to_token_stream().to_string().as_str() {
                "#[fatal]" => {
                    fatal_fields.insert(offset);
                }
                _ => {}
            }
        }
    }

    let mut init_stmts = vec![];
    let mut parse_stmts = vec![];
    let mut span_stmts = vec![];

    for (offset, field) in fields.iter().enumerate() {
        if let Some(ident) = &field.ident {
            init_stmts.push(ident.to_token_stream());
            if variant_fields {
                span_stmts.push(quote! {
                    lhs ^= #ident.to_span();
                });
            } else {
                span_stmts.push(quote! {
                    lhs ^= self.#ident.to_span();
                });
            }
        } else {
            let variable: proc_macro2::TokenStream =
                format!("variable_{}", offset).parse().unwrap();

            init_stmts.push(variable.clone());

            if variant_fields {
                span_stmts.push(quote! {
                    lhs ^= #variable.to_span();
                });
            } else {
                let index = Index::from(offset);
                span_stmts.push(quote! {
                    lhs ^= self.#index.to_span();
                });
            }
        }

        let variable = init_stmts.last().unwrap();

        if fatal_fields.contains(&offset) {
            parse_stmts.push(quote! {
                let (#variable,input) = SyntaxEx::ensure_parse(input)?;
            });
        } else {
            parse_stmts.push(quote! {
                let (#variable,input) = SyntaxEx::parse(input)?;
            });
        };
    }

    DriveFields {
        init_stmt: quote! {
            #(#init_stmts),*
        },
        parse_stmt: quote! {
            #(#parse_stmts)*
        },
        span_stmt: quote! {
            #(#span_stmts)*
        },
    }
}

fn drive_syntax_enum(item: ItemEnum) -> proc_macro::TokenStream {
    let input = if let Some(input) = syntax_input_type(&item.attrs) {
        input
    } else {
        quote! {
            I
        }
    };

    let error = if let Some(error) = syntax_error_type(&item.attrs) {
        error
    } else {
        quote! {
            parserc::errors::ErrorKind<<#input as parserc::input::Input>::Position>
        }
    };

    let item_ident = item.ident.clone();
    let variants_len = item.variants.len();

    let mut stmts = vec![];
    let mut as_spans = vec![];

    for (index, v) in item.variants.iter().enumerate() {
        let DriveFields {
            init_stmt,
            parse_stmt,
            span_stmt,
        } = drive_fields(&v.fields, true);

        let ident = &v.ident;

        let init_stmt = match v.fields {
            syn::Fields::Named(_) => {
                quote! {
                    #item_ident::#ident { #init_stmt }
                }
            }
            _ => {
                quote! {
                    #item_ident::#ident(#init_stmt)
                }
            }
        };

        let ident = format!("parse_{}", v.ident.to_string().to_lowercase())
            .parse::<proc_macro2::TokenStream>()
            .unwrap();

        if variants_len == index + 1 {
            stmts.push(quote! {
                let mut #ident = |mut input: #input| {
                    #parse_stmt

                    Ok((#init_stmt,input))
                };

                #ident.parse(input)
            });
        } else {
            stmts.push(quote! {
                let #ident = |mut input: #input| {
                    #parse_stmt

                    Ok((#init_stmt,input))
                };

                let (#ident,input) = #ident.ok().parse(input)?;

                if let Some(#ident) = #ident {
                    return Ok((#ident,input));
                }
            });
        }

        as_spans.push(quote! {
            #init_stmt => {
                let mut lhs = parserc::span::Span::<<#input as parserc::input::Input>::Position>::None;
                #span_stmt
                lhs
            }
        });
    }

    let (impl_generic, ty_generic, where_clause) = item.generics.split_for_impl();

    quote! {
        impl #impl_generic parserc::syntax::Syntax<#input,#error> for #item_ident #ty_generic #where_clause
        {

            fn parse(input: #input) -> parserc::errors::Result<Self, #input, #error> {
                use parserc::parser::Parser;
                use parserc::syntax::SyntaxEx;
                #(#stmts)*
            }
        }

        impl #impl_generic parserc::span::ToSpan<<#input as parserc::input::Input>::Position> for #item_ident #ty_generic #where_clause
        {

            fn to_span(&self) -> parserc::span::Span<<#input as parserc::input::Input>::Position> {
                match self {
                    #(#as_spans)*
                }
            }
        }
    }
    .into()
}

fn drive_syntax_struct(item: ItemStruct) -> proc_macro::TokenStream {
    let input = if let Some(input) = syntax_input_type(&item.attrs) {
        input
    } else {
        quote! {
            I
        }
    };

    let error = if let Some(error) = syntax_error_type(&item.attrs) {
        error
    } else {
        quote! {
            parserc::errors::ErrorKind<<#input as parserc::input::Input>::Position>
        }
    };

    let ident = &item.ident;

    let DriveFields {
        init_stmt,
        parse_stmt,
        span_stmt,
    } = drive_fields(&item.fields, false);

    let init_stmt = match item.fields {
        syn::Fields::Named(_) => {
            quote! {
                (Self { #init_stmt },input)
            }
        }
        _ => {
            quote! {
                (Self(#init_stmt),input)
            }
        }
    };

    let (impl_generic, ty_generic, where_clause) = item.generics.split_for_impl();

    quote! {
        impl #impl_generic parserc::syntax::Syntax<#input,#error> for #ident #ty_generic #where_clause
        {

            fn parse(input: #input) -> parserc::errors::Result<Self, #input, #error> {
                use parserc::parser::Parser;
                use parserc::syntax::SyntaxEx;
                #parse_stmt

                Ok(#init_stmt)
            }
        }

        impl #impl_generic parserc::span::ToSpan<<#input as parserc::input::Input>::Position> for #ident #ty_generic #where_clause
        {

            fn to_span(&self) -> parserc::span::Span<<#input as parserc::input::Input>::Position> {
                let mut lhs = parserc::span::Span::<<#input as parserc::input::Input>::Position>::None;
                #span_stmt
                lhs
            }
        }
    }.into()
}

/// Drive `Syntax` implemenation for tuple (T,...)
#[proc_macro]
pub fn def_tuple_syntax(_: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut stmts = vec![];
    for i in 2..16 {
        let mut types = vec![];

        let mut pos = vec![];

        for j in 0..i {
            types.push(
                format!("T{}", j)
                    .parse::<proc_macro2::TokenStream>()
                    .unwrap(),
            );

            pos.push(
                format!("self.{}", j)
                    .parse::<proc_macro2::TokenStream>()
                    .unwrap(),
            );
        }

        stmts.push(quote! {
            impl<I,E, #(#types),*> Syntax<I,E> for (#(#types),*)
            where
                I: Input,
                E: ParseError<I::Position>,
                #(#types: Syntax<I,E>),*
            {
                fn parse(input: I) -> Result<Self, I, E> {
                    #(
                        let (#types,input) = #types::parse(input)?;
                    )*

                    Ok(((#(#types),*),input))
                }
            }

            impl<__Pos,#(#types),*> ToSpan<__Pos> for (#(#types),*)
            where
                __Pos: PartialOrd,
                #(#types: ToSpan<__Pos>),*
            {
                fn to_span(&self) -> Span<__Pos> {
                    let mut lhs = Span::<__Pos>::None;

                    #(
                        lhs ^= #pos.to_span();
                    )*

                    lhs
                }
            }
        });
    }

    quote! {
        #(#stmts)*
    }
    .into()
}

#[cfg(feature = "token")]
#[proc_macro]
pub fn tokens(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    use std::collections::HashMap;

    use syn::Ident;

    let expr = parse_macro_input!(input as ExprMatch);

    let mut tokens = HashMap::new();

    for arm in &expr.arms {
        use syn::{Expr, Lit, Pat};

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

        stmts.push(quote! {

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

    quote! {
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
    .into()
}

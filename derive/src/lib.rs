use std::collections::HashSet;

use quote::{ToTokens, quote};
use syn::{
    Attribute, Error, ExprMatch, Fields, Item, ItemEnum, ItemStruct, Type, parse_macro_input,
    spanned::Spanned,
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

fn drive_fields(
    fields: &Fields,
) -> Vec<(
    proc_macro2::TokenStream,
    proc_macro2::TokenStream,
    proc_macro2::TokenStream,
)> {
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

    fields
        .iter()
        .enumerate()
        .map(|(offset, field)| {
            let variable = if let Some(ident) = &field.ident {
                ident.to_token_stream()
            } else {
                format!("variable_{}", offset).parse().unwrap()
            };

            let let_span = if let Some(ident) = &field.ident {
                quote! {
                    let lhs = parserc::inputs::Span::extend_to_inclusive(lhs, self.#ident.as_span());
                }
            } else {
                quote! {
                    let lhs = parserc::inputs::Span::extend_to_inclusive(lhs, self.#offset.as_span());
                }
            };

            let let_stmt = if fatal_fields.contains(&offset) {
                quote! {
                    let (#variable,input) = SyntaxEx::ensure_parse(input)?;
                }
            } else {
                quote! {
                    let (#variable,input) = SyntaxEx::parse(input)?;
                }
            };

            (variable, let_span, let_stmt)
        })
        .collect()
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
            parserc::errors::ErrorKind
        }
    };

    let item_ident = item.ident.clone();
    let variants_len = item.variants.len();

    let mut stmts = vec![];
    let mut as_spans = vec![];

    for (index, v) in item.variants.iter().enumerate() {
        let fields = drive_fields(&v.fields);
        let mut variables = vec![];
        let mut let_spans = vec![];
        let mut let_stmts = vec![];

        for (variable, let_span, stmt) in &fields {
            variables.push(variable);
            let_spans.push(let_span);
            let_stmts.push(stmt);
        }

        let ident = &v.ident;

        let init_stmt = match v.fields {
            syn::Fields::Named(_) => {
                quote! {
                    (#item_ident::#ident { #(#variables),* },input)
                }
            }
            _ => {
                quote! {
                    (#item_ident::#ident(#(#variables),*),input)
                }
            }
        };

        let ident = format!("parse_{}", v.ident.to_string().to_lowercase())
            .parse::<proc_macro2::TokenStream>()
            .unwrap();

        if variants_len == index + 1 {
            stmts.push(quote! {
                let mut #ident = |mut input: I| {
                    #(#let_stmts)*

                    Ok(#init_stmt)
                };

                #ident.parse(input)
            });
        } else {
            stmts.push(quote! {
                let #ident = |mut input: I| {
                    #(#let_stmts)*

                    Ok(#init_stmt)
                };

                let (#ident,input) = #ident.ok().parse(input)?;

                if let Some(#ident) = #ident {
                    return Ok((#ident,input));
                }
            });
        }

        as_spans.push(quote! {
            #init_stmt => {
                let lhs = None;
                #(#let_spans)*
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

        impl #impl_generic parserc::syntax::AsSpan for #item_ident #ty_generic #where_clause
        {

            fn as_span(&self) -> Option<parserc::inputs::Span> {
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
            parserc::errors::ErrorKind
        }
    };

    let ident = &item.ident;

    let fields = drive_fields(&item.fields);
    let mut variables = vec![];
    let mut let_spans = vec![];
    let mut let_stmts = vec![];

    for (variable, let_span, stmt) in &fields {
        variables.push(variable);
        let_spans.push(let_span);
        let_stmts.push(stmt);
    }

    let init_stmt = match item.fields {
        syn::Fields::Named(_) => {
            quote! {
                (Self { #(#variables),* },input)
            }
        }
        _ => {
            quote! {
                (Self(#(#variables),*),input)
            }
        }
    };

    let (impl_generic, ty_generic, where_clause) = item.generics.split_for_impl();

    let token_stream = quote! {

        impl #impl_generic parserc::syntax::Syntax<#input,#error> for #ident #ty_generic #where_clause
        {

            fn parse(input: #input) -> parserc::errors::Result<Self, #input, #error> {
                use parserc::parser::Parser;
                use parserc::syntax::SyntaxEx;
                #(#let_stmts)*

                Ok(#init_stmt)
            }
        }

        impl #impl_generic parserc::syntax::AsSpan for #ident #ty_generic #where_clause
        {

            fn as_span(&self) -> Option<parserc::inputs::Span> {
                let lhs = None;
                #(#let_spans)*

                lhs
            }
        }
    };

    token_stream.into()
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
                E: ParseError,
                #(#types: Syntax<I,E>),*
            {
                fn parse(input: I) -> Result<Self, I, E> {
                    #(
                        let (#types,input) = #types::parse(input)?;
                    )*

                    Ok(((#(#types),*),input))
                }
            }

            impl<#(#types),*> AsSpan for (#(#types),*)
            where
                #(#types: AsSpan),*
            {
                fn as_span(&self) -> Option<Span> {
                    let mut lhs = None;

                    #(
                        lhs = Span::extend_to_inclusive(lhs,#pos.as_span());
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
                I: parserc::inputs::Input + parserc::inputs::StartWith<&'static [u8]> + Clone,
                E: parserc::errors::ParseError,
            {
                 fn parse(input: I) -> parserc::errors::Result<Self, I, E> {
                     use parserc::parser::Parser;
                     #(
                         if let (Some(_),_) = parserc::parser::keyword(#lookahead.as_bytes()).ok().parse(input.clone())? {
                             return Err(parserc::errors::ControlFlow::Recovable(E::expect_token(#key,input)));
                         }
                     )*

                     parserc::parser::keyword(#key.as_bytes())
                         .map(|v| Self(v))
                         .map_err(|_:E| E::expect_token(#key,input.clone()) )
                         .parse(input.clone())
                 }
            }

            impl<I> parserc::syntax::AsSpan for #ident<I>
            where
                I: parserc::inputs::Input,
            {
                fn as_span(&self) -> Option<parserc::inputs::Span> {
                    self.0.span()
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
            I: parserc::inputs::Input + parserc::inputs::StartWith<&'static [u8]> + Clone,
        {
            #(#variants),*
        }

        impl<I,E> parserc::syntax::Syntax<I,E> for #ident<I>
        where
            I: parserc::inputs::Input + parserc::inputs::StartWith<&'static [u8]> + Clone,
            E: parserc::errors::ParseError,
        {
             fn parse(input: I) -> parserc::errors::Result<Self, I, E> {
                 use parserc::parser::Parser;
                 #(#variant_stmts)*

                 return Err(parserc::errors::ControlFlow::Recovable(E::expect_token(#ident_name,input)));
             }
        }
    }
    .into()
}

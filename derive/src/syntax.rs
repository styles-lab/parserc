#[cfg(feature = "profiling")]
use std::time::SystemTime;
use std::vec;

use proc_macro2::TokenStream;
use quote::{ToTokens, format_ident, quote};
use syn::{
    Attribute, Error, Expr, Fields, Ident, Item, ItemEnum, ItemStruct, Path, Result, Token, Type,
    parse::Parse, parse_macro_input, punctuated::Punctuated,
};

pub(crate) fn derive_syntax_trait(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    #[cfg(feature = "profiling")]
    let start_time = SystemTime::now();

    let item = parse_macro_input!(input as Item);

    #[allow(unused)]
    let (ident, result) = match &item {
        Item::Enum(item) => (&item.ident, derive_syntax_for_enum(item)),
        Item::Struct(item) => (&item.ident, derive_syntax_for_struct(item)),
        _ => unreachable!("Only struct/enum can apply `derive proc_macro`"),
    };

    #[cfg(feature = "profiling")]
    println!(
        "      Derive syntax `{}` in {:?}",
        ident,
        start_time.elapsed().unwrap()
    );

    match result {
        Ok(token_stream) => token_stream,
        Err(err) => err.into_compile_error().into(),
    }
}

struct Attr {
    ident: Ident,
    #[allow(unused)]
    eq: Token![=],
    ty: Path,
}

impl Parse for Attr {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let ident = input.parse()?;
        let eq = input.parse()?;
        let ty = input.parse()?;
        Ok(Self { ident, eq, ty })
    }
}

/// The global config for driving `Syntax` trait.
struct Config {
    /// The type stream for input parameter.
    ty_input: TokenStream,
    /// The type stream for error parameter.
    ty_error: TokenStream,
}

impl Config {
    fn parse(attrs: &[Attribute]) -> Result<Self> {
        let mut ty_input = None;
        let mut ty_error = None;

        for attr in attrs {
            if attr.path().is_ident("syntax") {
                let attrs =
                    attr.parse_args_with(Punctuated::<Attr, Token![,]>::parse_terminated)?;

                for attr in attrs {
                    if attr.ident == "input" {
                        if ty_input.is_some() {
                            return Err(Error::new_spanned(
                                attr.ident,
                                "Duplicate definition of `input`",
                            ));
                        }
                        ty_input = Some(attr.ty.to_token_stream());
                    } else if attr.ident == "error" {
                        if ty_error.is_some() {
                            return Err(Error::new_spanned(
                                attr.ident,
                                "Duplicate definition of `error`",
                            ));
                        }
                        ty_error = Some(attr.ty.to_token_stream());
                    } else {
                        return Err(Error::new_spanned(
                            attr.ident,
                            "Invalid value name, only support: `input` or `error`.",
                        ));
                    }
                }
            }
        }

        let ty_input = ty_input.unwrap_or(quote! {I});
        let ty_error = ty_error.unwrap_or(
            quote! { parserc::errors::ErrorKind<<#ty_input as parserc::input::Input>::Position> },
        );

        Ok(Self { ty_input, ty_error })
    }
}

struct Field {
    ident: Ident,
    from_ty: Option<Type>,
    map_err: Option<Expr>,
    fatal: bool,
}

impl TryFrom<(usize, &syn::Field)> for Field {
    type Error = syn::Error;

    #[inline]
    fn try_from((index, field): (usize, &syn::Field)) -> Result<Self> {
        let mut fatal = false;
        let mut from_ty = None;
        let mut map_err_fn = None;

        for attr in &field.attrs {
            let path = attr.path();

            if path.is_ident("from") {
                let expr: Type = attr.parse_args()?;
                from_ty = Some(expr);
            } else if path.is_ident("map_err") {
                let expr: Expr = attr.parse_args()?;
                map_err_fn = Some(expr);
            } else if path.is_ident("fatal") {
                fatal = true;
            }
        }

        match &field.ident {
            Some(ident) => Ok(Self {
                ident: ident.clone(),
                from_ty,
                map_err: map_err_fn,
                fatal,
            }),
            None => Ok(Self {
                ident: format_ident!("v_{}", index),
                from_ty,
                map_err: map_err_fn,
                fatal,
            }),
        }
    }
}

#[inline]
fn derive_fields(fields: &Fields) -> Result<Vec<Field>> {
    fields
        .iter()
        .enumerate()
        .map(|field| Field::try_from(field))
        .collect()
}

#[inline]
fn derive_syntax_for_enum(item: &ItemEnum) -> Result<proc_macro::TokenStream> {
    let Config { ty_input, ty_error } = Config::parse(&item.attrs)?;

    let mut enum_parse_stmts = vec![];
    let mut enum_to_span_stmts = vec![];

    for variant in item.variants.iter() {
        let fields = derive_fields(&variant.fields)?;

        let field_idents = fields.iter().map(|field| &field.ident);

        let parse_stmts = fields.iter().map(|field| {
            let ident = &field.ident;

            let fatal = if field.fatal {
                Some(quote! {.fatal()})
            } else {
                None
            };

            let from_ty = field
                .from_ty
                .as_ref()
                .map(|ty| quote! { .map_control_flow(|v: #ty| v.into()) });

            let map_err = field
                .map_err
                .as_ref()
                .map(|expr| quote! { .map_control_flow_err(#expr) });

            quote! {
                let (#ident,input) = input.parse() #from_ty #map_err #fatal ?;
            }
        });

        let to_span_stmts = fields.iter().map(|field| {
            let ident = &field.ident;

            quote! { #ident.to_span() }
        });

        let variant_ident = &variant.ident;

        let init_stmts = if let Fields::Named(_) = &variant.fields {
            quote! { Self::#variant_ident { #(#field_idents),* } }
        } else {
            quote! { Self::#variant_ident (#(#field_idents),*) }
        };

        enum_parse_stmts.push(quote! {
            let parser = |mut input: #ty_input| {
                #(#parse_stmts)*

                Ok((#init_stmts,input))
            };

            let (v,input) = parser.ok().parse(input)?;

            if let Some(v) = v {
                return Ok((v,input));
            }
        });

        enum_to_span_stmts.push(quote! {
            #init_stmts => {
                #(#to_span_stmts)^*
            }
        });
    }

    let (impl_generic, ty_generic, where_clause) = item.generics.split_for_impl();

    let ident = &item.ident;
    let token_name = item.ident.to_string();

    let token_stream = quote! {
        impl #impl_generic parserc::syntax::Syntax<#ty_input,#ty_error> for #ident #ty_generic #where_clause
        {

            fn parse(input: #ty_input) -> parserc::errors::Result<Self, #ty_input, #ty_error> {
                use parserc::parser::Parser;
                use parserc::errors::Map as _;
                use parserc::errors::MapFatal as _;
                use parserc::errors::MapError as _;

                #(#enum_parse_stmts)*

                Err(parserc::errors::ControlFlow::Recovable(parserc::errors::ErrorKind::Token(#token_name,input.to_span()).into()))
            }
        }

        impl #impl_generic parserc::span::ToSpan<<#ty_input as parserc::input::Input>::Position> for #ident #ty_generic #where_clause
        {

            fn to_span(&self) -> parserc::span::Span<<#ty_input as parserc::input::Input>::Position> {
                match self {
                    #(#enum_to_span_stmts)*
                }
            }
        }
    };

    Ok(token_stream.into())
}

#[inline]
fn derive_syntax_for_struct(item: &ItemStruct) -> Result<proc_macro::TokenStream> {
    let Config { ty_input, ty_error } = Config::parse(&item.attrs)?;

    let fields = derive_fields(&item.fields)?;

    let field_idents = fields.iter().map(|field| &field.ident);

    let parse_stmts = fields.iter().map(|field| {
        let ident = &field.ident;

        let fatal = if field.fatal {
            Some(quote! {.fatal()})
        } else {
            None
        };

        let from_ty = field
            .from_ty
            .as_ref()
            .map(|ty| quote! { .map_control_flow(|v: #ty| v.into()) });

        let map_err = field
            .map_err
            .as_ref()
            .map(|expr| quote! { .map_control_flow_err(#expr) });

        quote! {
            let (#ident,input) = input.parse() #from_ty #map_err #fatal ?;
        }
    });

    let to_span_stmts = fields.iter().map(|field| {
        let ident = &field.ident;
        quote! { #ident.to_span() }
    });

    let init_stmts = if let Fields::Named(_) = &item.fields {
        quote! { Self { #(#field_idents),* } }
    } else {
        quote! { Self(#(#field_idents),*) }
    };

    let (impl_generic, ty_generic, where_clause) = item.generics.split_for_impl();

    let ident = &item.ident;

    let token_stream = quote! {
        impl #impl_generic parserc::syntax::Syntax<#ty_input,#ty_error> for #ident #ty_generic #where_clause
        {

            fn parse(input: #ty_input) -> parserc::errors::Result<Self, #ty_input, #ty_error> {
                use parserc::parser::Parser;
                use parserc::errors::Map as _;
                use parserc::errors::MapFatal as _;
                use parserc::errors::MapError as _;

                #(#parse_stmts)*

                Ok((#init_stmts,input))
            }
        }

        impl #impl_generic parserc::span::ToSpan<<#ty_input as parserc::input::Input>::Position> for #ident #ty_generic #where_clause
        {

            fn to_span(&self) -> parserc::span::Span<<#ty_input as parserc::input::Input>::Position> {

                let #init_stmts = self;

                #(#to_span_stmts)^*
            }
        }
    };

    Ok(token_stream.into())
}

use std::collections::HashSet;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{ToTokens, quote};
use syn::{
    Error, ExprPath, Fields, Ident, Item, ItemEnum, ItemStruct, Token, parse::Parse,
    parse_macro_input, punctuated::Punctuated, spanned::Spanned,
};

mod kw {
    use syn::custom_keyword;

    custom_keyword!(error);
    custom_keyword!(input);
}

#[allow(unused)]
struct InputAttr {
    pub keyword: kw::input,
    pub eq_token: Token![=],
    pub ty: Ident,
}

impl Parse for InputAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let keyword = input.parse::<kw::input>()?;
        let eq_token = input.parse::<Token![=]>()?;
        let ty = input.parse()?;

        Ok(Self {
            keyword,
            eq_token,
            ty,
        })
    }
}
#[allow(unused)]
struct ErrAttr {
    pub keyword: kw::error,
    pub eq_token: Token![=],
    pub ty: ExprPath,
}

impl Parse for ErrAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let keyword = input.parse::<kw::error>()?;
        let eq_token = input.parse::<Token![=]>()?;
        let ty = input.parse()?;

        Ok(Self {
            keyword,
            eq_token,
            ty,
        })
    }
}
#[allow(unused)]
enum Attr {
    Err(ErrAttr),
    Input(InputAttr),
}

impl Parse for Attr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(kw::error) {
            Ok(Attr::Err(input.parse()?))
        } else if input.peek(kw::input) {
            Ok(Attr::Input(input.parse()?))
        } else {
            Err(Error::new(input.span(), "expect attr: error/input,..."))
        }
    }
}

#[allow(unused)]
struct Attrs(Span, Punctuated<Attr, Token![,]>);

impl Parse for Attrs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let span = input.span();
        let mut punct = Punctuated::new();

        loop {
            punct.push_value(Attr::parse(input)?);
            if let Some(comma) = input.parse::<Option<Token![,]>>()? {
                punct.push_punct(comma);
                continue;
            }

            break;
        }

        Ok(Self(span, punct))
    }
}

impl Attrs {
    fn generic_input(&self) -> Option<proc_macro2::TokenStream> {
        for attr in self.1.iter() {
            if let Attr::Input(input) = attr {
                return Some(input.ty.to_token_stream());
            }
        }

        None
    }

    fn error_type(&self) -> Option<proc_macro2::TokenStream> {
        for attr in self.1.iter() {
            if let Attr::Err(err) = attr {
                return Some(err.ty.to_token_stream());
            }
        }

        None
    }
}

fn drive_fields(fields: &mut Fields) -> Vec<(proc_macro2::TokenStream, proc_macro2::TokenStream)> {
    let mut fatal_fields = HashSet::new();

    for (offset, field) in fields.iter_mut().enumerate() {
        let mut attrs = vec![];
        for attr in field.attrs.drain(..) {
            match attr.to_token_stream().to_string().as_str() {
                "#[fatal]" => {
                    fatal_fields.insert(offset);
                }
                _ => {
                    attrs.push(attr);
                }
            }
        }

        field.attrs = attrs;
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

            let let_stmt = if fatal_fields.contains(&offset) {
                quote! {
                    let (#variable,input) = input.ensure_parse()?;
                }
            } else {
                quote! {
                    let (#variable,input) = input.parse()?;
                }
            };

            (variable, let_stmt)
        })
        .collect()
}

fn derive_struct_item(attr: Attrs, mut item: ItemStruct) -> TokenStream {
    let generic_input = if let Some(generic_input) = attr.generic_input() {
        generic_input
    } else {
        return Error::new(attr.0, "Requires attribute `input=...` to be specified")
            .into_compile_error()
            .into();
    };

    let err_type = if let Some(err_type) = attr.error_type() {
        err_type
    } else {
        return Error::new(attr.0, "Requires attribute `error=...` to be specified")
            .into_compile_error()
            .into();
    };

    let (impl_generic, ty_generic, where_clause) = item.generics.split_for_impl();
    let ident = &item.ident;

    let fields = drive_fields(&mut item.fields);

    let variables = fields
        .iter()
        .map(|(variable, _)| variable)
        .collect::<Vec<_>>();

    let let_stmts = fields.iter().map(|(_, stmt)| stmt).collect::<Vec<_>>();

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

    quote! {
        #item

        impl #impl_generic parserc::Parse<#generic_input> for #ident #ty_generic #where_clause {
            type Error = #err_type;

            fn parse(input: #generic_input) -> parserc::Result<Self, #generic_input, Self::Error> {
                use parserc::ParseFromInput;
                #(#let_stmts)*

                Ok(#init_stmt)
            }
        }
    }
    .into()
}

fn derive_enum_item(attrs: Attrs, mut item: ItemEnum) -> TokenStream {
    let generic_input = if let Some(generic_input) = attrs.generic_input() {
        generic_input
    } else {
        return Error::new(attrs.0, "Requires attribute `input=...` to be specified")
            .into_compile_error()
            .into();
    };

    let err_type = if let Some(err_type) = attrs.error_type() {
        err_type
    } else {
        return Error::new(attrs.0, "Requires attribute `error=...` to be specified")
            .into_compile_error()
            .into();
    };

    let (impl_generic, ty_generic, where_clause) = item.generics.split_for_impl();

    let item_ident = item.ident.clone();
    let variants_len = item.variants.len();

    let stmts = item
        .variants
        .iter_mut()
        .enumerate()
        .map(|(index, v)| {
            let fields = drive_fields(&mut v.fields);

            let variables = fields
                .iter()
                .map(|(variable, _)| variable)
                .collect::<Vec<_>>();

            let let_stmts = fields.iter().map(|(_, stmt)| stmt).collect::<Vec<_>>();

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
                quote! {
                    let mut #ident = |mut input: #generic_input| {
                        use parserc::ParseFromInput;
                        #(#let_stmts)*

                        Ok(#init_stmt)
                    };

                    #ident.parse(input)
                }
            } else {
                quote! {
                    let #ident = |mut input: #generic_input| {
                        use parserc::ParseFromInput;
                        #(#let_stmts)*

                        Ok(#init_stmt)
                    };

                    let (#ident,input) = #ident.ok().parse(input)?;

                    if let Some(#ident) = #ident {
                        return Ok((#ident,input));
                    }
                }
            }
        })
        .collect::<Vec<_>>();

    quote! {
        #item

        impl #impl_generic parserc::Parse<#generic_input> for #item_ident #ty_generic #where_clause {
            type Error = #err_type;

            fn parse(input: #generic_input) -> parserc::Result<Self, #generic_input, Self::Error> {
                use parserc::{ParserExt,Parser};
                #(#stmts)*
            }
        }
    }
    .into()
}

#[proc_macro_attribute]
pub fn derive_parse(attr: TokenStream, input: TokenStream) -> TokenStream {
    let attrs = parse_macro_input!(attr as Attrs);

    let item = parse_macro_input!(input as Item);

    match item {
        Item::Enum(item) => derive_enum_item(attrs, item),
        Item::Struct(item) => derive_struct_item(attrs, item),
        _ => Error::new(item.span(), "derive_parse: unsupport item")
            .into_compile_error()
            .into(),
    }
}

#[proc_macro]
pub fn make_tuple_parse_impl(_item: TokenStream) -> TokenStream {
    let mut stmts = vec![];
    for i in 2..16 {
        let mut types = vec![];

        for j in 0..i {
            types.push(
                format!("T{}", j)
                    .parse::<proc_macro2::TokenStream>()
                    .unwrap(),
            );
        }

        stmts.push(quote! {
            impl<I,E, #(#types),*> Parse<I> for (#(#types),*)
            where
                I: Input,
                E: From<Kind> + std::fmt::Debug,
                #(#types: Parse<I,Error = E>),*
            {
                type Error = E;

                fn parse(input: I) -> Result<Self, I, Self::Error> {
                    #(
                        let (#types,input) = #types::parse(input)?;
                    )*

                    Ok(((#(#types),*),input))
                }
            }
        });
    }

    quote! {
        #(#stmts)*
    }
    .into()
}

mod syntax;
mod tuple;

#[cfg(feature = "token")]
mod token;

/// Derive `Syntax` implemenation for tuples (T,...)
#[proc_macro]
pub fn def_tuple_syntax(args: proc_macro::TokenStream) -> proc_macro::TokenStream {
    tuple::def_tuple_syntax(args)
}

/// Derive token tables.
#[cfg(feature = "token")]
#[proc_macro]
pub fn tokens(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    token::derive_tokens(input)
}

/// Derive `Syntax` trait for `structs/enums`.
#[proc_macro_derive(Syntax, attributes(syntax, fatal, from, map_err, try_filter))]
pub fn derive_syntax_trait(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    syntax::derive_syntax_trait(input)
}

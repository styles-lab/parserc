use proc_macro::TokenStream;
use quote::quote;

#[proc_macro_derive(Parse, attributes(helper))]
pub fn derive_helper_attr(_: TokenStream) -> TokenStream {
    quote! {}.into()
}

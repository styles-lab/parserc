use proc_macro::TokenStream;

#[proc_macro_derive(Parse, attributes(helper))]
pub fn derive_helper_attr(item: TokenStream) -> TokenStream {
    item
}

use quote::quote;

pub(crate) fn def_tuple_syntax(_: proc_macro::TokenStream) -> proc_macro::TokenStream {
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

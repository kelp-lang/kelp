use proc_macro::TokenStream;
use quote::quote;

#[proc_macro_derive(Spans)]
pub fn span_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse(input).unwrap();

    impl_span_trait(&ast)
}

fn impl_span_trait(ast: &syn::DeriveInput) -> TokenStream {
    let name = &ast.ident;
    let gen = quote! {
        impl Spans for #name {
            fn spans(&self) -> Span {
                self.span.clone()
            }
        }
    };
    gen.into()
}

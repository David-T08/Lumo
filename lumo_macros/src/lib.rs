use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{DeriveInput, parse_macro_input};

use proc_macro_crate::{FoundCrate, crate_name};

#[proc_macro_derive(AstFormatExt)]
pub fn derive_ast_format_ext(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let ident = input.ident;
    let generics = input.generics;

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let root_path = match crate_name("lumo") {
        Ok(FoundCrate::Itself) => quote!(crate),
        Ok(FoundCrate::Name(name)) => {
            let crate_ident = syn::Ident::new(&name, Span::call_site());
            quote!(::#crate_ident)
        }
        Err(_) => {
            quote!(crate)
        }
    };

    let expanded = quote! {
        impl #impl_generics #root_path::ast::AstFormatExt for #ident #ty_generics #where_clause {
            fn with_cfg<'a>(&'a self, cfg: #root_path::ast::AstFormatConfig) -> #root_path::ast::WithCfg<'a, Self>
            where
                Self: Sized,
            {
                #root_path::ast::WithCfg { node: self, cfg }
            }
        }
    };

    TokenStream::from(expanded)
}
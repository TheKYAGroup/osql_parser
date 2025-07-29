use std::ptr::NonNull;

use deluxe::{ExtractAttributes, HasAttributes};
use proc_macro::TokenStream;
use syn::{DeriveInput, Error, Meta, spanned::Spanned};

#[proc_macro_derive(Ident, attributes(display, ident))]
pub fn make_ident(item: TokenStream) -> TokenStream {
    let input: DeriveInput = syn::parse_macro_input!(item as syn::DeriveInput);

    ident_impl(input)
        .unwrap_or_else(|err| err.into_compile_error())
        .into()
}

use quote::quote;
use syn::Data;

#[derive(deluxe::ExtractAttributes, deluxe::ParseMetaItem, deluxe::ParseAttributes)]
#[deluxe(attributes(display))]
struct DisplayAttr(String);

fn ident_impl(mut input: DeriveInput) -> deluxe::Result<proc_macro2::TokenStream> {
    let enum_name = &input.ident;

    let fields = match &mut input.data {
        Data::Enum(data_enum) => data_enum,
        _ => return Err(deluxe::Error::new(input.span(), format!("Must be enum"))),
    };

    let variants: Vec<_> = fields
        .variants
        .iter_mut()
        .filter(|f| {
            f.attrs
                .iter()
                .find(|attr| attr.path().is_ident("ident"))
                .is_some()
        })
        .map(|variant| {
            variant.attrs.retain(|attr| !attr.path().is_ident("doc"));
            let attrs: DisplayAttr = deluxe::parse_attributes(variant)?;
            let ident = &variant.ident;
            let name = attrs.0.to_lowercase();
            Ok(quote! {
                #name => #enum_name::#ident
            })
        })
        .collect::<Vec<_>>()
        .collect_res()?;

    let out = quote! {
        impl #enum_name {
            pub fn ident_map(input: ecow::EcoString) -> Self {
                match input.to_lowercase().as_str() {
                    #(
                       #variants,
                    )*
                    _ => #enum_name::Ident(input),
                }
            }
        }
    };

    Ok(out.into())
}

trait CollectErrors<T> {
    fn collect_res(self) -> syn::Result<T>;
}

impl<T> CollectErrors<Vec<T>> for Vec<syn::Result<T>> {
    fn collect_res(self) -> syn::Result<Vec<T>> {
        let mut errors: Option<Error> = None;

        let mut output = Vec::new();

        for res in self {
            match res {
                Ok(val) => {
                    if errors.is_none() {
                        output.push(val);
                    }
                }
                Err(err) => {
                    if let Some(error) = &mut errors {
                        error.combine(err);
                    } else {
                        errors = Some(err);
                    }
                }
            }
        }

        match errors {
            Some(errors) => Err(errors),
            None => Ok(output),
        }
    }
}

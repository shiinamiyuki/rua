use proc_macro::Ident;
use proc_macro2::TokenStream;
use quote::{quote, quote_spanned};
use syn::{
    parse_macro_input, parse_quote, Data, DeriveInput, Fields, GenericParam, Generics, Lifetime,
    TypeParamBound,
};

use syn::spanned::Spanned;
#[proc_macro_derive(Bind)]
pub fn derive_bind(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let generics = add_trait_bounds(input.generics);
    let binding = binding(&input.data);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let expanded = quote! {
        // The generated impl.
        impl #impl_generics rua::bind::BInd for #name #ty_generics #where_clause {
            fn bind(module:&mut rua::runtime::Module) {
                #binding
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}
fn prologue(name:&Ident, data:&Data)->TokenStream {
    quote! {
        module.function(stringify!(name).into(), |ctx|{
            let pself = ctx.arg(0)?;
            let pself = pself.cast_ref::<>();
        });
    }
}
fn binding(data: &Data) -> TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let recurse = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let ty = &f.ty;
                    quote_spanned! {f.span()=>
                        {

                        }
                    }
                });
                quote! {
                    0 #(+ #recurse)*
                }
            }
            _ => {
                unimplemented!()
            }
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}
fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!('static));
        }
    }
    generics
}

use darling::{
    ast::{Data, Fields, Style},
    FromDeriveInput, FromField, FromVariant,
};
use proc_macro::TokenStream;
use proc_macro2::{Ident, TokenStream as TokenStream2};
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Generics};

//

#[derive(FromDeriveInput)]
#[darling(attributes(tree))]
struct DebugTreeOpts {
    ident: Ident,
    generics: Generics,

    data: Data<Variant, Field>,
}

#[derive(FromField)]
#[darling(attributes(tree))]
struct Field {
    ident: Option<Ident>,
    // #[darling(default)]
    // field: bool,
    // #[darling(default)]
    // skip: bool,
}

#[derive(FromVariant)]
#[darling(attributes(tree))]
struct Variant {
    ident: Ident,

    fields: Fields<Field>,
}

/* #[proc_macro_derive(ToOwned)]
pub fn derive_to_owned(input: TokenStream) -> TokenStream {
    _to_owned(&parse_macro_input!(input as DeriveInput)).unwrap_or_else(|err| err)
}

fn _to_owned(input: &DeriveInput) -> Result<TokenStream, TokenStream> {
    let input = DebugTreeOpts::from_derive_input(input).map_err(|err| err.write_errors())?;

    let ident = input.ident;
    let (imp, typ, wher) = input.generics.split_for_impl();

    Ok(if input.data.is_enum() {
        todo!()
    } else {
        quote! {
            impl #imp std::borrow::ToOwned for #ident #typ #wher {
                type Owned = #ident <'static>;

                fn to_owned(&self) -> Self::Owned {
                    Self::Owned {

                    }
                }
            }
        }
    }
    .into())
} */

#[proc_macro_derive(DebugTree)]
pub fn derive_debug_tree(input: TokenStream) -> TokenStream {
    _debug_tree(&parse_macro_input!(input as DeriveInput)).unwrap_or_else(|err| err)
}

fn _debug_tree(input: &DeriveInput) -> Result<TokenStream, TokenStream> {
    let input = DebugTreeOpts::from_derive_input(input).map_err(|err| err.write_errors())?;

    let ident = input.ident;
    let name = ident.to_string();
    let (imp, typ, wher) = input.generics.split_for_impl();

    let (fields, nodes, extra) = if input.data.is_enum() {
        let e = input.data.take_enum().unwrap();

        let name = format!("{ident}: ");

        let variant = e.iter().fold(TokenStream2::new(), |mut acc, v| {
            let i = &v.ident;
            let n = format!("{}::{}", name, i);
            acc.extend(match v.fields.style {
                Style::Tuple if v.fields.fields.len() == 1 => {
                    quote! { #ident :: #i (v) => DebugTree::fmt(v, f, depth)?, }
                }
                Style::Tuple => quote! { #ident :: #i () => write!(f, "  {}", #n)?, },
                Style::Struct => todo!(),
                Style::Unit => quote! { #ident :: #i => write!(f, "  {}", #n)?, },
            });
            acc
        });

        /* let variants = e.iter().fold(TokenStream2::new(), |mut acc, v| {
            let i = &v.ident;
            let n = format!("{}::{}", name, i);
            acc.extend(quote! { #ident :: #i => #n, });
            acc
        }); */

        (
            quote! {
                match self {
                    #variant
                }
            },
            TokenStream2::new(),
            quote! {
                write!(f, #name)?;
            },
        )
    } else {
        // let (fields, nodes) = input
        let fields = input.data.take_struct().unwrap().into_iter();
        // .filter(|f| !f.skip)
        // .partition::<Vec<_>, _>(|f| f.field);

        let field_count = fields.len();
        let fields = fields.into_iter().fold(TokenStream2::new(), |mut acc, s| {
            acc.extend(if let Some(ident) = s.ident {
                if field_count >= 2 {
                    let field = ident.to_string();
                    quote! {
                        write!(f, "  {:d$}{}: ", "", #field, d = d)?;
                        DebugTree::fmt(&self.#ident, f, depth + 1)?;
                    }
                } else {
                    quote! {
                        write!(f, "  {:d$}", "", d = d)?;
                        DebugTree::fmt(&self.#ident, f, depth + 1)?;
                    }
                }
            } else {
                quote! {
                    write!(f, "  {:d$}", "", d = d)?;
                    DebugTree::fmt(&self.0, f, depth + 1)?;
                }
            });
            acc
        });

        // let nodes = nodes.into_iter().fold(TokenStream2::new(), |mut acc, s| {
        //     acc.extend(if let Some(ident) = s.ident {
        //         quote! {
        //             write!(f, "  {:d$}: ", "", d = d)?;
        //             DebugTree::fmt(&self.#ident, f, depth + 1)?;
        //         }
        //     } else {
        //         quote! {
        //             write!(f, "  {:d$}", "", d = d)?;
        //             DebugTree::fmt(&self.0, f, depth + 1)?;
        //         }
        //     });
        //     acc
        // });

        (
            fields,
            // nodes,
            TokenStream2::new(),
            quote! {
                writeln!(f, "{}:", #name)?;
            },
        )
    };

    Ok(quote! {
        impl #imp DebugTree for #ident #typ #wher {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>, depth: u8) -> core::fmt::Result {
                let d = depth as usize * 2;

                #extra

                #fields

                #nodes

                Ok(())
            }
        }
    }
    .into())
}

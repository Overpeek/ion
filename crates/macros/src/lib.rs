use darling::{
    ast::{Data, Fields, Style},
    FromDeriveInput, FromField, FromVariant,
};
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Generics};

//

#[derive(FromDeriveInput)]
#[darling(attributes(to_static))]
struct DebugTreeOpts {
    ident: Ident,
    generics: Generics,

    data: Data<Variant, Field>,

    result: Option<String>,
    #[darling(default)]
    debug: bool,
}

#[derive(FromField)]
#[darling(attributes(to_static))]
struct Field {
    ident: Option<Ident>,
    // #[darling(default)]
    // field: bool,
    // #[darling(default)]
    // skip: bool,
}

#[derive(FromVariant)]
#[darling(attributes(to_static))]
struct Variant {
    ident: Ident,

    fields: Fields<Field>,
}

#[proc_macro_derive(ToStatic, attributes(to_static))]
pub fn derive_to_static(input: TokenStream) -> TokenStream {
    _to_static(&parse_macro_input!(input as DeriveInput)).unwrap_or_else(|err| err)
}

fn _to_static(input: &DeriveInput) -> Result<TokenStream, TokenStream> {
    let input: DebugTreeOpts =
        DebugTreeOpts::from_derive_input(input).map_err(|err| err.write_errors())?;

    let data_ident = input.ident;
    let (imp, typ, wher) = input.generics.split_for_impl();

    let constructor = match input.data {
        Data::Enum(variants) => {
            let matchers = variants.into_iter().fold(quote! {}, |acc, new| {
                let variant_ident = new.ident;

                let pattern = to_static_pattern(&new.fields);
                let constructor = to_static_constructor(&new.fields);

                quote! {
                    #acc
                    Self::#variant_ident #pattern => #data_ident::#variant_ident #constructor,
                }
            });

            quote! {
                match self {
                    #matchers
                }
            }
        }
        Data::Struct(fields) => {
            let pattern = to_static_pattern(&fields);
            let constructor = to_static_constructor(&fields);
            quote! {
                let Self #pattern = self;
                #data_ident #constructor
            }
        }
    };

    let result = if let Some(result) = input.result {
        let result = Ident::new(&result, Span::call_site());
        quote! { #result }
    } else {
        quote! { #data_ident <'static> }
    };

    let impl_tokens = quote! {
        impl #imp ToStatic for #data_ident #typ #wher {
            type Static = #result;

            fn to_static(&self) -> Self::Static {
                #constructor
            }
        }
    };

    if input.debug {
        panic!("Debug: {}", impl_tokens);
    }

    Ok(impl_tokens.into())
}

fn to_static_pattern(val: &Fields<Field>) -> TokenStream2 {
    match val.style {
        Style::Tuple => {
            let pattern = val
                .fields
                .iter()
                .enumerate()
                .fold(quote! {}, |acc, (i, _)| {
                    let ident = format!("_{i}");
                    let ident = Ident::new(&ident, Span::call_site());
                    let comma = if i == 0 {
                        quote! {}
                    } else {
                        quote! {,}
                    };
                    quote! { #acc #comma #ident }
                });

            quote! {
                (#pattern)
            }
        }
        Style::Struct => {
            let pattern = val
                .fields
                .iter()
                .enumerate()
                .fold(quote! {}, |acc, (i, new)| {
                    let ident = new.ident.as_ref().expect("Tuple structs are not supported");
                    let comma = if i == 0 {
                        quote! {}
                    } else {
                        quote! {,}
                    };
                    quote! { #acc #comma #ident }
                });

            quote! {
                { #pattern }
            }
        }
        Style::Unit => quote! {},
    }
}

fn to_static_constructor(/* path: TokenStream2, */ val: &Fields<Field>) -> TokenStream2 {
    match val.style {
        Style::Tuple => {
            let constructor = val
                .fields
                .iter()
                .enumerate()
                .fold(quote! {}, |acc, (i, _)| {
                    let ident = format!("_{i}");
                    let ident = Ident::new(&ident, Span::call_site());
                    // let ident = syn::Member::Unnamed(syn::Index::from(i));
                    let comma = if i == 0 {
                        quote! {}
                    } else {
                        quote! {,}
                    };
                    quote! { #acc #comma ToStatic::to_static(#ident) }
                });

            quote! {
                (#constructor)
            }
        }
        Style::Struct => {
            let constructor = val
                .fields
                .iter()
                .enumerate()
                .fold(quote! {}, |acc, (i, new)| {
                    let ident = new.ident.as_ref().expect("Tuple structs are not supported");
                    let comma = if i == 0 {
                        quote! {}
                    } else {
                        quote! {,}
                    };
                    quote! { #acc #comma #ident : ToStatic::to_static(#ident) }
                });

            quote! {
                { #constructor }
            }
        }
        Style::Unit => quote! { {} },
    }
}

/* #[proc_macro_derive(DebugTree)]
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
} */

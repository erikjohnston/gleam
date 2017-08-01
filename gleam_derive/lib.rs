#![recursion_limit="256"]

extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;


use proc_macro::TokenStream;


#[proc_macro_derive(GleamFromHttpStr)]
pub fn gleam_from_http_str(input: TokenStream) -> TokenStream {
    let s = input.to_string();
    let ast = syn::parse_derive_input(&s).unwrap();
    let gen = impl_from_http_str(ast);
    gen.parse().unwrap()
}


fn impl_from_http_str(ast: syn::MacroInput) -> quote::Tokens {
    let name = &ast.ident;

    quote! {
        impl ::gleam::request_parsing::FromHttpStr for #name {
            fn parse_from_http(s: &str) -> Result<Self, String>
            {
                <#name as ::std::str::FromStr>::from_str(s).map_err(
                    |e| format!("{}", e)
                )
            }
        }
    }
}



#[proc_macro_derive(GleamFromRequest)]
pub fn gleam_from_request(input: TokenStream) -> TokenStream {
    let s = input.to_string();
    let ast = syn::parse_derive_input(&s).unwrap();
    let gen = impl_from_request(ast);
    gen.parse().unwrap()
}

fn impl_from_request(ast: syn::MacroInput) -> quote::Tokens {
    let name = &ast.ident;

    let variant_data = if let syn::Body::Struct(variant_data) = ast.body {
        variant_data
    } else {
        panic!("#[GleamFromRequest] can only be used on structs")
    };

    let fields = match variant_data {
        syn::VariantData::Struct(fields) => fields,
        syn::VariantData::Tuple(_) => {
            panic!("#[GleamFromRequest] can't be used on tuple structs");
        }
        syn::VariantData::Unit => {
            panic!("#[GleamFromRequest] can't be used on unit structs");
        }
    };

    let mut parsing_chunks = Vec::new();
    let mut field_idents = Vec::new();
    let mut request_types = std::collections::HashSet::new();
    let mut from_request_cbs = Vec::new();
    let mut future_idents = Vec::new();
    let mut unrwap_idents = Vec::new();

    for field in fields {
        let field_ident = field.ident.expect("field to be named");

        if field_ident.as_ref().starts_with("header_") {
            let header_name = &field_ident.as_ref()[7..].replace("_", "-");
            let p = parse_header(&header_name, &field_ident, field.ty);
            parsing_chunks.push(p);
            unrwap_idents.push(quote! {
                let #field_ident = #field_ident?;
            });
        } else if field_ident.as_ref().starts_with("path_") {
            let path_name = &field_ident.as_ref()[5..];
            let p = parse_path_segment(path_name, &field_ident, field.ty);
            parsing_chunks.push(p);
            unrwap_idents.push(quote! {
                let #field_ident = #field_ident?;
            });
        } else if field_ident.as_ref().starts_with("path") {
            let p = parse_path(&field_ident);
            parsing_chunks.push(p);
            unrwap_idents.push(quote! {
                let #field_ident = #field_ident?;
            });
        } else if field_ident.as_ref().starts_with("param_") {
            let param_name = &field_ident.as_ref()[6..];
            let p = parse_param(param_name, &field_ident, field.ty);
            parsing_chunks.push(p);
            unrwap_idents.push(quote! {
                let #field_ident = #field_ident?;
            });
        } else if field_ident.as_ref().starts_with("req_") {
            if !request_types.insert(field.ty.clone()) {
                panic!("multiple req_* fields have same type");
            }

            from_request_cbs.push(parse_request_cb(&field_ident, &field.ty));
            let p = parse_request_ident(&field_ident, field.ty);
            unrwap_idents.push(p);
            future_idents.push(field_ident.clone());
        } else {
            panic!("field '{}' doesn't start with known prefix", field_ident);
        }

        field_idents.push(field_ident);
    }

    quote! {
        impl ::gleam::FromRequest for #name {
            type Error = ::hyper::Response;
            type Future = Box<::futures::Future<Item=Self, Error=Self::Error>>;

            #[allow(unused_variables)]
            #[allow(unused_imports)]
            fn from_request(
                server: &Server,
                ctx: &::gleam::Ctx,
                req: &::hyper::server::Request,
                params: &::gleam::Params,
            ) -> Self::Future {
                use ::gleam::request_parsing::{
                    FromHeader, FromPathParam, FromQueryParam,
                    parse_query_string,
                };
                use ::gleam::{FromRequest, IntoResponse};
                use ::std::iter::empty;
                use ::futures::{future, Future, IntoFuture};
                use ::anymap;

                let query_params = parse_query_string(&req);

                #( #parsing_chunks )*
                
                #( #from_request_cbs )*

                let fut = future::ok(anymap::AnyMap::new())
                    #(
                        .and_then( move |mut type_map| {
                            #future_idents
                                .map(move |res| {
                                    type_map.insert(res);
                                    type_map
                                })
                        }) 
                    )*
                    .and_then(
                        #[allow(unused_mut)]
                        move |mut type_map| -> Result<_, hyper::Response> {
                            #(
                                #unrwap_idents
                            )*

                            Ok(#name {
                                #( 
                                    #field_idents
                                ),*
                            })
                        }
                    );

                Box::new(fut)
            }
        } 
    }
}


fn parse_header(
    header_name: &str,
    field_ident: &syn::Ident,
    ty: syn::Ty,
) -> quote::Tokens {
    quote! {
        let #field_ident = {
            let raw_header = req.headers().get_raw(#header_name);
            if let Some(hdr) = raw_header {
                <#ty as FromHeader>::parse_header(#header_name, hdr.iter())
            } else {
                <#ty as FromHeader>::parse_header(#header_name, empty())
            }
        };
    }
}

fn parse_param(
    path_name: &str,
    field_ident: &syn::Ident,
    ty: syn::Ty,
) -> quote::Tokens {
    quote! {
        let #field_ident = {
            <#ty as FromQueryParam>::parse_query_param(#path_name, &query_params)
        };
    }
}


fn parse_path_segment(
    path_name: &str,
    field_ident: &syn::Ident,
    ty: syn::Ty,
) -> quote::Tokens {
    quote! {
        let #field_ident = {
            <#ty as FromPathParam>::parse_path_param(#path_name, params)
        };
    }
}

fn parse_path(
    field_ident: &syn::Ident,
) -> quote::Tokens {
    quote! {
        let #field_ident = req.path().into();
    }
}

fn parse_request_cb(field_ident: &syn::Ident, ty: &syn::Ty) -> quote::Tokens {
    quote! {
        let #field_ident = <#ty as FromRequest>::from_request(server, &ctx, req, params)
            .into_future();
    }
}


fn parse_request_ident(field_ident: &syn::Ident, ty: syn::Ty) -> quote::Tokens {
    quote! {
        let #field_ident: #ty = {
            type_map.remove().expect("expected type map to have FromRequest type")
        };
    }
}


#[proc_macro_derive(GleamState)]
pub fn gleam_state(input: TokenStream) -> TokenStream {
    let s = input.to_string();
    let ast = syn::parse_derive_input(&s).unwrap();
    let gen = impl_gleam_state(ast);
    gen.parse().unwrap()
}

fn impl_gleam_state(ast: syn::MacroInput) -> quote::Tokens {
    let name = &ast.ident;

    let variant_data = if let syn::Body::Struct(variant_data) = ast.body {
        variant_data
    } else {
        panic!("#[GleamState] can only be used on structs")
    };

    let fields = match variant_data {
        syn::VariantData::Struct(fields) => fields,
        syn::VariantData::Tuple(_) => {
            panic!("#[GleamState] can't be used on tuple structs");
        }
        syn::VariantData::Unit => {
            return quote! {
                impl ::gleam::FromRequest for #name {
                    type Error = String;

                    fn get_state(
                        _: &::gleam::Server
                    ) -> Result<Self, String> {
                        Ok(#name)
                    }
                } 
            }
        }
    };

    let mut parsing_chunks = Vec::new();
    let mut field_idents = Vec::new();

    for field in fields {
        let field_ident = field.ident.expect("field to be named");
        let ty = field.ty;

        if field_ident.as_ref().starts_with("state_") {
            parsing_chunks.push(quote! {
                let #field_ident = {
                    use ::gleam::IntoResponse;
                    match <#ty as ::gleam::State>::get_state(server) {
                        Ok(s) => s,
                        Err(e) => return Err(e.into_response()),
                    }
                };
            });
        } else {
            parsing_chunks.push(quote! {
                let #field_ident = {
                    server.get_state()
                        .cloned()
                        .ok_or_else(|| {
                            use ::hyper::StatusCode::InternalServerError;
                            ::hyper::Response::new()
                                .with_status(InternalServerError)
                                .with_body(format!("Missing state: {}", stringify!(#field_ident)))
                        })?
                };
            });
        }

        field_idents.push(field_ident);
    }

    quote! {
        impl ::gleam::State for #name {
            type Error = hyper::Response;

            #[allow(unused_variables)]
            fn get_state(
                server: &::gleam::Server,
            ) -> Result<Self, Self::Error> {
                #( 
                    #parsing_chunks
                )*

                Ok(#name {
                    #( 
                        #field_idents
                    ),*
                })
            }
        } 
    }
}

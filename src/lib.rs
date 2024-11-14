// src/lib.rs in the macro crate
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Expr, Field, Lit, TypePath};

#[proc_macro_derive(FromEnv, attributes(env))]
pub fn from_env(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let name = &ast.ident;

    // Get the fields of the struct
    let fields = if let syn::Data::Struct(s) = &ast.data {
        &s.fields
    } else {
        panic!("PrintFields can only be used on structs");
    };

    let field_names = fields.iter().map(|f| assign(f));
    // .collect();

    // Generate code to print each field name and value
    let expanded = quote! {
        impl #name {
            pub fn init_from_env(matches: clap::ArgMatches) -> Self {
                #name {
                    #(#field_names)*
                }
            }
        }
    };

    TokenStream::from(expanded)
}

fn assign(field: &Field) -> proc_macro2::TokenStream {
    match &field.ty {
        syn::Type::Array(_type_array) => todo!(),
        syn::Type::BareFn(_type_bare_fn) => todo!(),
        syn::Type::Group(_type_group) => todo!(),
        syn::Type::ImplTrait(_type_impl_trait) => todo!(),
        syn::Type::Infer(_type_infer) => todo!(),
        syn::Type::Macro(_type_macro) => todo!(),
        syn::Type::Never(_type_never) => todo!(),
        syn::Type::Paren(_type_paren) => todo!(),
        syn::Type::Path(type_path) => handle_path(field, type_path),
        syn::Type::Ptr(_type_ptr) => todo!(),
        syn::Type::Reference(_type_reference) => todo!(),
        syn::Type::Slice(_type_slice) => todo!(),
        syn::Type::TraitObject(_type_trait_object) => todo!(),
        syn::Type::Tuple(_type_tuple) => todo!(),
        syn::Type::Verbatim(_token_stream) => todo!(),
        _ => todo!(),
    }
}

fn handle_path(field: &Field, type_path: &TypePath) -> proc_macro2::TokenStream {
    let Field { ident, attrs, .. } = field;
    let TypePath { path, .. } = type_path;
    let mut field_default: Option<proc_macro2::TokenStream> = None;

    attrs.iter().for_each(|a| match &a.meta {
        syn::Meta::List(meta_list) => {
            let expr = meta_list.parse_args::<Expr>().unwrap();
            field_default = handle_default_macro(&expr);
        }
        syn::Meta::Path(_path) => todo!(),
        syn::Meta::NameValue(_meta_name_value) => todo!(),
    });

    // assuming string at the moment

    if path.segments.len() == 1 {
        let type_identifier = &path.segments[0].ident.to_string();

        let type_type = get_type(type_identifier);
        let parser = get_parser(type_identifier);
        let envvar_conversion = parser(quote! {envvar});
        let default_conversion = match field_default {
            Some(default_value) => default_value,
            None => quote! {panic!("failed to parse")},
        };

        quote! { #ident: {
            match matches.get_one::<#type_type>(stringify!(#ident)) {
                Some(cli) => cli.clone(),
                None => match std::env::var(stringify!(#ident).to_uppercase()) {
                    Ok(envvar) => #envvar_conversion,
                    Err(_) => #default_conversion,
                },
            }
        },}
    } else {
        panic!("path segments len > 1 unhandled");
    }
}

fn get_parser(type_identifer: &str) -> fn(proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    match type_identifer {
        "String" => handle_string,
        "i32" => handle_i32,
        "Uuid" => handle_uuid,
        _ => todo!("unhandled type"),
    }
}

fn handle_string(var_name: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    quote! {#var_name}
}

fn handle_i32(var_name: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    quote! { match #var_name.parse::<i32>() {
        Ok(envvar) => envvar,
        Err(e) => panic!("failed to parse i32 {e:?}"),
    }}
}

fn handle_uuid(var_name: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    quote! {match uuid::Uuid::parse_str(&#var_name) {

        Ok(envvar) => envvar,
        Err(e) => panic!("failed to parse uuid {e:?}"),

    }}
}
fn get_type(type_identifer: &str) -> proc_macro2::TokenStream {
    match type_identifer {
        "String" => quote! {String},
        "i32" => quote! {i32},
        "Uuid" => quote! {uuid::Uuid},
        _ => todo!(),
    }
}

fn handle_default_macro(expr: &Expr) -> Option<proc_macro2::TokenStream> {
    match expr {
        Expr::Assign(expr_assign) => handle_default_macro(&expr_assign.right),
        Expr::Array(_expr_array) => todo!(),
        Expr::Async(_expr_async) => todo!(),
        Expr::Await(_expr_await) => todo!(),
        Expr::Binary(_expr_binary) => todo!(),
        Expr::Block(_expr_block) => todo!(),
        Expr::Break(_expr_break) => todo!(),
        Expr::Call(_expr_call) => todo!(),
        Expr::Cast(_expr_cast) => todo!(),
        Expr::Closure(_expr_closure) => todo!(),
        Expr::Const(_expr_const) => todo!(),
        Expr::Continue(_expr_continue) => todo!(),
        Expr::Field(_expr_field) => todo!(),
        Expr::ForLoop(_expr_for_loop) => todo!(),
        Expr::Group(_expr_group) => todo!(),
        Expr::If(_expr_if) => todo!(),
        Expr::Index(_expr_index) => todo!(),
        Expr::Infer(_expr_infer) => todo!(),
        Expr::Let(_expr_let) => todo!(),
        Expr::Lit(expr_lit) => handle_literal(expr_lit),
        Expr::Loop(_expr_loop) => todo!(),
        Expr::Macro(_expr_macro) => todo!(),
        Expr::Match(_expr_match) => todo!(),
        Expr::MethodCall(_expr_method_call) => todo!(),
        Expr::Paren(_expr_paren) => todo!(),
        Expr::Path(_expr_path) => todo!(),
        Expr::Range(_expr_range) => todo!(),
        Expr::RawAddr(_expr_raw_addr) => todo!(),
        Expr::Reference(_expr_reference) => todo!(),
        Expr::Repeat(_expr_repeat) => todo!(),
        Expr::Return(_expr_return) => todo!(),
        Expr::Struct(_expr_struct) => todo!(),
        Expr::Try(_expr_try) => todo!(),
        Expr::TryBlock(_expr_try_block) => todo!(),
        Expr::Tuple(_expr_tuple) => todo!(),
        Expr::Unary(_expr_unary) => todo!(),
        Expr::Unsafe(_expr_unsafe) => todo!(),
        Expr::Verbatim(_token_stream) => todo!(),
        Expr::While(_expr_while) => todo!(),
        Expr::Yield(_expr_yield) => todo!(),
        _ => todo!(),
    }
}

fn handle_literal(expr_lit: &syn::ExprLit) -> Option<proc_macro2::TokenStream> {
    match &expr_lit.lit {
        Lit::Str(lit_str) => {
            // let string_value = lit_str.value();
            Some(quote! {#lit_str.to_owned()})
        }
        Lit::ByteStr(_lit_byte_str) => todo!(),
        Lit::CStr(_lit_cstr) => todo!(),
        Lit::Byte(_lit_byte) => todo!(),
        Lit::Char(_lit_char) => todo!(),
        Lit::Int(lit_int) => Some(quote! {#lit_int}),
        Lit::Float(_lit_float) => todo!(),
        Lit::Bool(_lit_bool) => todo!(),
        Lit::Verbatim(_literal) => todo!(),
        _ => todo!(),
    }
}

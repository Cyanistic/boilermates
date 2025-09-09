#![doc = include_str!("../README.md")]

use std::collections::{HashMap, HashSet};

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, Attribute, AttributeArgs, Data, DeriveInput, Field,
    Fields, Lit, NestedMeta,
};

#[derive(Clone)]
struct FieldConfig {
    field: Field,
    default: bool,
}

impl FieldConfig {
    fn new(field: Field, default: bool) -> Self {
        Self { field, default }
    }

    fn name(&self) -> Ident {
        self.field
            .ident
            .clone()
            .unwrap_or_else(|| panic!("Can't get field name. This should never happen."))
    }
    
    fn has_same_type(&self, other: &Self) -> bool {
        // Compare the type tokens to check if types are the same
        let self_type = &self.field.ty;
        let other_type = &other.field.ty;
        quote!(#self_type).to_string() == quote!(#other_type).to_string()
    }
}

impl PartialEq for FieldConfig {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name()
    }
}

impl From<Field> for FieldConfig {
    fn from(field: Field) -> Self {
        Self::new(field, false)
    }
}

impl From<FieldConfig> for Field {
    fn from(field_config: FieldConfig) -> Self {
        field_config.field
    }
}

struct Struct {
    attrs: Vec<Attribute>,
    fields: Vec<FieldConfig>,
}

impl Struct {
    fn missing_fields_from(&self, other: &Self) -> Vec<FieldConfig> {
        self.fields.iter().fold(vec![], |mut acc, field| {
            if !other.fields.contains(field) {
                acc.push(field.clone())
            }
            acc
        })
    }

    fn same_fields_as(&self, other: &Self) -> Vec<FieldConfig> {
        self.fields.iter().fold(vec![], |mut acc, field| {
            if other.fields.contains(field) {
                acc.push(field.clone())
            }
            acc
        })
    }
    
    fn has_compatible_fields_with(&self, other: &Self) -> bool {
        // Check if all common fields have the same type
        self.fields.iter().all(|field| {
            other.fields.iter()
                .find(|f| f.name() == field.name())
                .map_or(true, |other_field| field.has_same_type(other_field))
        })
    }
}

#[proc_macro_attribute]
pub fn boilermates(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut structs = HashMap::<String, Struct>::new();
    let mut structs_with_custom_attrs = HashSet::new();

    let main = parse_macro_input!(item as DeriveInput);

    let generics = &main.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let forwarded_attrs: Vec<Attribute> = main
        .attrs
        .iter()
        .filter(|attr| !attr.path.is_ident("boilermates"))
        .cloned()
        .collect();

    let Data::Struct(data_struct) = main.data.clone() else {
        panic!("Expected a struct");
    };

    let Fields::Named(fields) = data_struct.fields.clone() else {
        panic!("Expected a struct with named fields");
    };

    let args = parse_macro_input!(attr as AttributeArgs);
    
    // Reworked parsing logic for the main attribute
    args.into_iter().for_each(|arg| match arg {
        // Handles arguments like "CreateUser"
        NestedMeta::Lit(Lit::Str(lit)) => {
            let struct_name = lit.value().trim_matches('"').to_owned();
            structs.insert(
                struct_name,
                Struct {
                    attrs: vec![],
                    fields: vec![],
                },
            );
        }
        // Handles arguments like `attr_for(...)`
        NestedMeta::Meta(syn::Meta::List(list)) => {
            let Some(ident) = list.path.get_ident() else {
                panic!("Expected an identifier in the meta list");
            };
            match ident.to_string().as_str() {
                "attr_for" => match (
                    list.nested.len(),
                    list.nested.iter().next(),
                    list.nested.iter().nth(1),
                ) {
                    (
                        2,
                        Some(NestedMeta::Lit(Lit::Str(strukt))),
                        Some(NestedMeta::Lit(Lit::Str(attr_lit))),
                    ) => {
                        let struct_name = strukt.value().trim_matches('"').to_string();
                        structs_with_custom_attrs.insert(struct_name.clone());

                        let attr_tokens: TokenStream2 = attr_lit
                            .value()
                            .trim_matches('"')
                            .parse()
                            .unwrap_or_else(|e| panic!("Could not parse attribute: {}", e));
                        let q = quote! {#attr_tokens};
                        let attr = parse_quote!(#q);
                        
                        // Ensure the struct exists before trying to add attributes
                        if !structs.contains_key(&struct_name) {
                             structs.insert(struct_name.clone(), Struct { attrs: vec![], fields: vec![] });
                        }
                        
                        structs
                            .get_mut(&struct_name)
                            .unwrap() // Should not panic due to the check above
                            .attrs
                            .push(attr);
                    }
                    _ => panic!(
                        "`#[boilermates(attr_for(...))]` must have two string literal arguments"
                    ),
                },
                _ => panic!("Unknown attribute `#[boilermates({})]`", ident),
            }
        }
        _ => panic!("Expected a string literal or a meta list in the boilermates attribute"),
    });


    fn extract_nested_list(meta_list: &syn::MetaList) -> Vec<String> {
        meta_list
            .nested
            .iter()
            .map(|n| match n {
                NestedMeta::Lit(Lit::Str(lit)) => lit.value().trim_matches('"').to_owned(),
                _ => panic!("Expected a string literal"),
            })
            .collect()
    }

    structs.insert(
        main.ident.to_string(),
        Struct {
            // We strip boilermates attributes from the main struct before generating it
            attrs: main.attrs.iter().filter(|a| !a.path.is_ident("boilermates")).cloned().collect(),
            fields: vec![],
        },
    );

    for (name, strukt) in structs.iter_mut() {
        if name != &main.ident.to_string() && !structs_with_custom_attrs.contains(name) {
            strukt.attrs.extend(forwarded_attrs.clone());
        }
    }

    fields.named.iter().for_each(|field| {
        let mut add_to = structs.keys().cloned().collect::<Vec<_>>();
        let mut default = false;
        
        let mut clean_field = field.clone();
        clean_field.attrs.retain(|attr| {
            let Ok(meta) = attr.parse_meta() else { return true };
            let syn::Meta::List(list) = meta  else { return true };
            let Some(name) = list.path.get_ident() else { return true };
            if name != "boilermates" { return true }
            match list.nested.first() {
                Some(syn::NestedMeta::Meta(syn::Meta::List(nv))) => {
                    let Some(ident) = nv.path.get_ident() else { panic!("#[boilermates] parsing error") };
                    let ident = ident.to_string();
                    if ident == "only_in" {
                        let nested = extract_nested_list(nv);
                        if nested.is_empty() {
                            panic!(
                                "`#[boilermates(only_in(...))]` must have at least one argument"
                            );
                        }
                        nested.iter().for_each(|n| {
                            if !structs.contains_key(n) {
                                panic!(
                                    "`#[boilermates(only_in(...))]` has undeclared struct name `{}`",
                                    n
                                );
                            }
                        });
                        add_to.retain(|s| nested.iter().any(|n| s == n.as_str()));
                    } else if ident == "not_in" {
                        let nested = extract_nested_list(nv);
                        if nested.is_empty() {
                            panic!(
                                "`#[boilermates(not_in(...))]` must have at least one argument"
                            );
                        }
                        nested.iter().for_each(|n| {
                            if !structs.contains_key(n) {
                                panic!(
                                    "`#[boilermates(not_in(...))]` has undeclared struct name `{}`",
                                    n
                                );
                            }
                        });
                        add_to.retain(|s| !nested.iter().any(|n| s == n.as_str()));
                    } else {
                        panic!("Unknown attribute `#[boilermates({})]`", ident);
                    }
                }

                Some(syn::NestedMeta::Meta(syn::Meta::Path(path))) => {
                    let Some(ident) = path.get_ident() else { panic!("#[boilermates] parsing error") };
                    match ident.to_string().as_str() {
                        "default" => default = true,
                        "only_in_self" => add_to = vec![main.ident.to_string()],
                        _ => panic!("Unknown attribute `#[boilermates({})]`", ident),
                    }
                }
                _ => return true,
            }
            false
        });

        let field_config = FieldConfig::new(clean_field, default);

        for (struct_name, strukt) in structs.iter_mut() {
            if add_to.contains(struct_name) {
                // Check if a field with the same name already exists
                let field_exists = strukt.fields.iter().any(|f| f.name() == field_config.name());
                if !field_exists {
                    strukt.fields.push(field_config.clone());
                }
            }
        }
    });

    let mut struct_defs = quote! {};
    let mut impl_blocks = quote! {};

    for (name, strukt) in structs.iter() {
        let struct_ident = Ident::new(name, Span::call_site());
        let struct_attrs = &strukt.attrs;
        let struct_fields = strukt.fields.iter().map(|f_conf| &f_conf.field);
        let vis = &main.vis;

        struct_defs = quote! {
            #struct_defs
            #(#struct_attrs)*
            #vis struct #struct_ident #generics #where_clause {
                #(#struct_fields),*
            }
        };
    }

    for (name, strukt) in structs.iter() {
        for (other_name, other) in structs.iter() {
            if name == other_name {
                continue;
            }

            let name_ident = Ident::new(name, Span::call_site());
            let other_name_ident = Ident::new(other_name, Span::call_site());
            
            let missing_fields = strukt.missing_fields_from(other);
            let missing_fields_without_defaults: Vec<&FieldConfig> = missing_fields
                .iter()
                .filter(|f| !f.default)
                .collect();
            
            let default_field_setters = missing_fields.iter().filter(|f| f.default).fold(quote!{}, |acc, field| {
                let field_name = field.name();
                quote! {
                    #acc
                    #field_name: Default::default(),
                }
            });

            if missing_fields_without_defaults.is_empty() && strukt.has_compatible_fields_with(other) {
                let common_field_setters = strukt.same_fields_as(other).iter().fold(quote!{}, |acc, field| {
                    let field_name = &field.name();
                    quote! {
                        #acc
                        #field_name: other.#field_name,
                    }
                });

                impl_blocks = quote! {
                    #impl_blocks
                    impl #impl_generics From<#other_name_ident #ty_generics> for #name_ident #ty_generics #where_clause {
                        fn from(other: #other_name_ident #ty_generics) -> Self {
                            Self {
                                #common_field_setters
                                #default_field_setters
                            }
                        }
                    }
                };
            }
            if !missing_fields.is_empty() && strukt.has_compatible_fields_with(other) {
                let common_field_setters = strukt.same_fields_as(other).iter().fold(quote!{}, |acc, field| {
                    let field_name = field.name();
                    quote! {
                        #acc
                        #field_name: self.#field_name,
                    }
                });
                
                let into_args = missing_fields.iter().fold(quote!{}, |acc, field| {
                    let field_name = field.name();
                    let field_ty = &field.field.ty;
                    quote! {
                        #acc
                        #field_name: #field_ty,
                    }
                });

                let into_defaults_args = missing_fields_without_defaults.iter().fold(quote!{}, |acc, field| {
                    let field_name = field.name();
                    let field_ty = &field.field.ty;
                    quote! {
                        #acc
                        #field_name: #field_ty,
                    }
                });

                let into_missing_setters = missing_fields
                    .iter()
                    .fold(quote! {}, |acc, field| {
                        let field_name = field.name();
                        quote! { #acc #field_name, }
                    });

                let into_defaults_missing_setters = missing_fields_without_defaults
                    .iter()
                    .fold(quote! {}, |acc, field| {
                        let field_name = field.name();
                        quote! { #acc #field_name, }
                    });

                let into_defaults_fn_name = Ident::new(
                    &pascal_to_snake(&format!("into_{}_with_defaults", name)),
                    Span::call_site()
                );
                
                let into_fn_name = Ident::new(
                    &pascal_to_snake(&format!("into_{}", name)),
                    Span::call_site()
                );

                impl_blocks = quote! {
                    #impl_blocks
                    impl #impl_generics #other_name_ident #ty_generics #where_clause {
                        pub fn #into_fn_name(self, #into_args) -> #name_ident #ty_generics {
                            #name_ident {
                                #common_field_setters
                                #into_missing_setters
                            }
                        }

                        pub fn #into_defaults_fn_name(self, #into_defaults_args) -> #name_ident #ty_generics {
                            #name_ident {
                                #common_field_setters
                                #default_field_setters
                                #into_defaults_missing_setters
                            }
                        }
                    }
                };
            }
        }
    }

    quote! {
        #struct_defs
        #impl_blocks
    }.into()
}

fn pascal_to_snake(s: &str) -> String {
    let mut result = String::new();
    for (i, c) in s.chars().enumerate() {
        if i > 0 && c.is_uppercase() {
            result.push('_');
        }
        result.push(c.to_ascii_lowercase());
    }
    result
}

#![doc = include_str!("../README.md")]

use std::collections::HashMap;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, Attribute, AttributeArgs, Data, DataStruct, DeriveInput, Field,
    Fields, FieldsNamed, Lit, NestedMeta,
};

#[derive(Clone)]
struct FieldConfig {
    field: Field,
    default: bool,
}

impl FieldConfig {
    fn new(field: Field, default: bool) -> Self {
        Self {
            field,
            default,
        }
    }

    fn name(&self) -> Ident {
        self.field.ident.clone().unwrap_or_else(|| panic!("Can't get field name. This should never happen."))
    }

    fn trait_name(&self) -> Ident {
        Ident::new(&format!("Has{}", snake_to_pascal(&self.name().to_string())), Span::call_site())
    }

    fn neg_trait_name(&self) -> Ident {
        Ident::new(&format!("HasNo{}", snake_to_pascal(&self.name().to_string())), Span::call_site())
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
            if !other.fields.contains(field) { acc.push(field.clone()) }
            acc
        })
    }

    fn same_fields_as(&self, other: &Self) -> Vec<FieldConfig> {
        self.fields.iter().fold(vec![], |mut acc, field| {
            if other.fields.contains(field) { acc.push(field.clone()) }
            acc
        })
    }
}

#[proc_macro_attribute]
pub fn boilermates(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut structs = HashMap::<String, Struct>::new();

    // Parse the input item (the struct this attribute is attached to)
    let mut main = parse_macro_input!(item as DeriveInput);
    
    // --- START of MODIFICATIONS ---

    // NEW: Find and clone all #[derive(...)] attributes from the original struct.
    let forwarded_derives: Vec<Attribute> = main
        .attrs
        .iter()
        .filter(|attr| attr.path.is_ident("derive"))
        .cloned()
        .collect();

    // --- END of MODIFICATIONS ---

    let Data::Struct(data_struct) = main.data.clone() else {
        panic!("Expected a struct");
    };
    
    let Fields::Named(mut fields) = data_struct.fields.clone() else {
        panic!("Expected a struct with named fields");
    };

    // Parse the attribute arguments to declare new structs
    let args = parse_macro_input!(attr as AttributeArgs);
    args.into_iter().for_each(|arg| {
        match arg {
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
            _ => panic!("Expected a string literal"),
        }
    });

    // Parse #[boilermates(attr_for(...))] to add attributes to specific structs
    main.attrs.retain(|attr| {
        let Ok(meta) = attr.parse_meta() else { return true };
        let syn::Meta::List(list) = meta  else { return true };
        let Some(name) = list.path.get_ident() else { return true };
        if name != "boilermates" {
            return true;
        }
        match list.nested.first() {
            Some(syn::NestedMeta::Meta(syn::Meta::List(nv))) => {
                let Some(ident) = nv.path.get_ident() else { return true };
                match ident.to_string().as_str() {
                    "attr_for" => match (
                        nv.nested.len(),
                        nv.nested.iter().next(),
                        nv.nested.iter().nth(1),
                    ) {
                        (
                            2,
                            Some(NestedMeta::Lit(Lit::Str(strukt))),
                            Some(NestedMeta::Lit(Lit::Str(attr_lit))),
                        ) => {
                            let attr_tokens: TokenStream2 = attr_lit
                                .value()
                                .trim_matches('"')
                                .parse()
                                .unwrap_or_else(|e| panic!("Could not parse attribute: {}", e));
                            let q = quote! {#attr_tokens};
                            let attr = parse_quote!(#q);
                            structs
                                .get_mut(strukt.value().trim_matches('"'))
                                .unwrap_or_else(|| panic!("Struct `{}` not declared", strukt.value()))
                                .attrs
                                .push(attr);
                        }
                        _ => panic!(
                            "`#[boilermates(attr_for(...))]` must have two string literal arguments"
                        ),
                    },
                    _ => panic!("Unknown attrbute `#[boilermates({})]`", ident),
                }
            }
            _ => return true,
        }
        false
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

    // Add the main struct to the list of structs to be generated
    structs.insert(
        main.ident.to_string(),
        Struct {
            attrs: main.attrs.clone(),
            fields: vec![],
        },
    );
    
    // NEW: Add the captured #[derive(...)] attributes to every struct we plan to generate.
    // This ensures that both the main struct and all new structs get the derives.
    for (name, strukt) in structs.iter_mut() {
        // The main struct already has its attributes, so we only add derives to the new ones.
        if name != &main.ident.to_string() {
            strukt.attrs.extend(forwarded_derives.clone());
        }
    }

    let mut traits = quote! {};

    // Process fields and their attributes
    fields.named.iter_mut().for_each(|field| {
        let mut add_to = structs.keys().cloned().collect::<Vec<_>>();
        let mut default = false;
        field.attrs.retain(|attr| {
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
                            if !add_to.iter().any(|s| s == n.as_str()) {
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
                            if !add_to.iter().any(|s| s == n.as_str()) {
                                panic!(
                                    "`#[boilermates(not_in(...))]` has undeclared struct name `{}`",
                                    n
                                );
                            }
                        });
                        add_to.retain(|s| !nested.iter().any(|n| s == n.as_str()));
                    } else {
                        panic!("Unknown attrbute `#[boilermates({})]`", ident);
                    }
                }

                Some(syn::NestedMeta::Meta(syn::Meta::Path(path))) => {
                    let Some(ident) = path.get_ident() else { panic!("#[boilermates] parsing error") };
                    match ident.to_string().as_str() {
                        "default" => default = true,
                        "only_in_self" => add_to = vec![main.ident.to_string()],
                        _ => panic!("Unknown attrbute `#[boilermates({})]`", ident),
                    }
                }

                _ => return true,
            }
            
            false
        });

        let field = FieldConfig::new(field.clone(), default);
        let trait_name = field.trait_name();
        let neg_trait_name = field.neg_trait_name();
        let field_name = field.name();
        let setter_fn = Ident::new(&format!("set_{}", field_name), Span::call_site());
        let field_ty = &field.field.ty;
        traits = quote! {
            #traits
            trait #trait_name {
                fn #field_name(&self) -> &#field_ty;
                fn #setter_fn(&mut self, value: #field_ty);
            }

            trait #neg_trait_name {}
        };

        structs.iter_mut().for_each(|(struct_name, strukt)| {
            let struct_ident = Ident::new(struct_name, Span::call_site());

            if add_to.contains(struct_name) {
                strukt.fields.push(field.clone());
                
                traits = quote! {
                    #traits
                    impl #trait_name for #struct_ident {
                        fn #field_name(&self) -> &#field_ty {
                            &self.#field_name
                        }

                        fn #setter_fn(&mut self, value: #field_ty) {
                            self.#field_name = value;
                        }
                    }
                };
            } else {
                traits = quote! {
                    #traits
                    impl #neg_trait_name for #struct_ident {}
                };
            }

        });
    });

    // Generate the output code
    let mut output = quote! {};
    structs.iter().for_each(|(name, strukt)| {
        // Build the final struct definition
        let out_struct = DeriveInput {
            attrs: strukt.attrs.clone(),
            data: Data::Struct(DataStruct {
                fields: Fields::Named(FieldsNamed {
                    named: strukt
                        .fields
                        .iter()
                        .cloned()
                        .map(Into::<Field>::into)
                        .collect(),
                    ..fields.clone()
                }),
                ..data_struct.clone()
            }),
            ident: Ident::new(name, Span::call_site()),
            ..main.clone()
        };
        output = quote! {
            #output
            #out_struct
        };

        // Generate From/Into implementations between structs
        structs.iter().for_each(|(other_name, other)| {

            if name == other_name { return }
            let name = Ident::new(name, Span::call_site());
            let other_name = Ident::new(other_name, Span::call_site());
            let missing_fields = strukt.missing_fields_from(other);
            let missing_fields_without_defaults = missing_fields
                .iter()
                .filter(|f| !f.default)
                .collect::<Vec<_>>();

            
            let default_field_setters = missing_fields.iter().filter(|f| f.default).fold(quote!{}, |acc, field| {
                let field_name = field.name();
                quote! {
                    #acc
                    #field_name: Default::default(),
                }
            });
            
            if missing_fields_without_defaults.is_empty() {
                let common_field_setters = strukt.same_fields_as(other).iter().fold(quote!{}, |acc, field| {
                    let field_name = &field.name();
                    quote! {
                        #acc
                        #field_name: other.#field_name,
                    }
                });

                output = quote! {
                    #output
                    impl From<#other_name> for #name {
                        fn from(other: #other_name) -> Self {
                            Self {
                                #common_field_setters
                                #default_field_setters
                            }
                        }
                    }
                };
            }
            if !missing_fields.is_empty() {
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

                output = quote! {
                    #output
                    impl #other_name {
                        pub fn #into_fn_name(self, #into_args) -> #name {
                            #name {
                                #common_field_setters
                                #into_missing_setters
                            }
                        }

                        pub fn #into_defaults_fn_name(self, #into_defaults_args) -> #name {
                            #name {
                                #common_field_setters
                                #default_field_setters
                                #into_defaults_missing_setters
                            }
                        }
                    }
                };
            }

        })
    });

    output = quote! {
        #output
        #traits
    };

    output.into()
}

// --- Helper functions (unchanged) ---

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

fn snake_to_pascal(s: &str) -> String {
    let mut result = String::new();
    let mut capitalize = true;
    for c in s.chars() {
        if c == '_' {
            capitalize = true;
        } else if capitalize {
            result.push(c.to_ascii_uppercase());
            capitalize = false;
        } else {
            result.push(c);
        }
    }
    result
}

use proc_macro2::{Ident, TokenStream, TokenTree};
use proc_macro_error::abort;
use quote::ToTokens;
use syn::{
    parse2, parse_quote, parse_str, punctuated::Punctuated, spanned::Spanned, token::Or, Arm,
    Attribute, Expr, ExprLit, ExprRange, ImplItemMethod, ItemEnum, ItemImpl, Lit, LitChar, Pat,
    PatLit, PatOr,
};

enum AttributeArg {
    Literal(LitChar),
    Or(Vec<LitChar>),
    Range(ExprRange),
}

struct VariantAttribute {
    argument: AttributeArg,
    identifier: Ident,
}

fn parse_or_pattern(cell_attribute: &Attribute) -> Result<Vec<LitChar>, ()> {
    let tokens = cell_attribute.parse_args::<TokenStream>().unwrap();

    let mut iter = tokens.into_iter();
    let mut alternatives: Vec<LitChar> = Vec::new();

    // Parse a list of char literals separated by '|' delimiter.
    while let Some(token) = iter.next() {
        match token {
            // Consume one char literal.
            TokenTree::Literal(literal) => {
                if let Ok(char_lit) = parse_str::<LitChar>(&literal.to_string()) {
                    alternatives.push(char_lit);
                } else {
                    abort!(
                        cell_attribute.span(),
                        "Currently only supporting char literals."
                    );
                }
                // Expect one '|' delimiter if any token left.
                if let Some(next) = iter.next() {
                    match next {
                        TokenTree::Punct(punct) => {
                            if punct.as_char() != '|' {
                                abort!(cell_attribute.span(), "Expected '|' delimiter.");
                            }
                        }
                        _ => abort!(cell_attribute.span(), "Expected '|' delimiter."),
                    };
                }
            }
            _ => abort!(
                cell_attribute.span(),
                "Currently only supporting char literals."
            ),
        };
    }

    Ok(alternatives)
}

fn extract_argument(cell_attribute: &Attribute) -> Result<AttributeArg, ()> {
    if let Ok(char_lit) = cell_attribute.parse_args::<LitChar>() {
        Ok(AttributeArg::Literal(char_lit))
    } else if let Ok(range) = cell_attribute.parse_args::<ExprRange>() {
        Ok(AttributeArg::Range(range))
    } else if let Ok(alternatives) = parse_or_pattern(cell_attribute) {
        Ok(AttributeArg::Or(alternatives))
    } else {
        Err(())
    }
}

fn construct_or_pattern(alternatives: &Vec<LitChar>) -> PatOr {
    let mut cases = Punctuated::<Pat, Or>::new();
    for char_lit in alternatives {
        cases.push(Pat::Lit(PatLit {
            attrs: vec![],
            expr: Box::new(Expr::Lit(ExprLit {
                attrs: vec![],
                lit: Lit::Char(char_lit.clone()),
            })),
        }));
    }

    PatOr {
        cases,
        attrs: vec![],
        leading_vert: None,
    }
}

fn construct_try_from_match_arms(
    enum_identifier: &Ident,
    variants: &Vec<VariantAttribute>,
) -> Vec<Arm> {
    let mut arms = Vec::new();

    for variant in variants {
        let ident = &variant.identifier;
        match &variant.argument {
            AttributeArg::Literal(char_literal) => {
                arms.push(parse_quote!( #char_literal => Ok(#enum_identifier::#ident),));
            }
            AttributeArg::Range(_) => todo!(),
            AttributeArg::Or(alternatives) => {
                let or_pattern = construct_or_pattern(alternatives);
                arms.push(parse_quote!( #or_pattern => Ok(#enum_identifier::#ident),));
            }
        }
    }

    arms.push(parse_quote!( _ => Err(ParseCellError(value)),));
    arms
}

fn generate_try_from_char_impl(
    enum_identifier: &Ident,
    variants: &Vec<VariantAttribute>,
) -> ItemImpl {
    let match_arms: Vec<Arm> = construct_try_from_match_arms(enum_identifier, variants);
    let try_from_char_fn: ImplItemMethod = parse_quote!(
        fn try_from(value: char) -> Result<Self, Self::Error> {
            match value {
                #(#match_arms)*
            }
        }
    );

    let implementation_base: ItemImpl = parse_quote!(
        impl TryFrom<char> for #enum_identifier {
            type Error = ParseCellError;
            #try_from_char_fn
        }
    );

    implementation_base
}

fn construct_char_from_cell_match_arms(
    enum_identifier: &Ident,
    variants: &Vec<VariantAttribute>,
) -> Vec<Arm> {
    let mut arms = Vec::new();

    for variant in variants {
        let ident = &variant.identifier;
        match &variant.argument {
            AttributeArg::Literal(char_literal) => {
                arms.push(parse_quote!( #enum_identifier::#ident => #char_literal,));
            }
            AttributeArg::Range(_) => todo!(),
            AttributeArg::Or(alternatives) => {
                let char_literal = alternatives.first().unwrap();
                arms.push(parse_quote!( #enum_identifier::#ident  => #char_literal,))
            }
        }
    }

    arms
}

fn generate_char_from_cell(enum_identifier: &Ident, variants: &Vec<VariantAttribute>) -> ItemImpl {
    let match_arms: Vec<Arm> = construct_char_from_cell_match_arms(enum_identifier, variants);

    let implementation: ItemImpl = parse_quote!(
        impl From<#enum_identifier> for char {
            fn from(cell: #enum_identifier) -> char {
                match cell {
                    #(#match_arms)*
                }
            }
        }
    );

    implementation
}

pub fn derive_grid_cell(input: TokenStream) -> TokenStream {
    let input_enum = match parse2::<ItemEnum>(input.clone()) {
        Ok(syntax_tree) => syntax_tree,
        Err(error) => return error.to_compile_error(),
    };

    let enum_identifier = input_enum.ident;

    let mut variants: Vec<VariantAttribute> = Vec::new();

    for variant in input_enum.variants {
        let variant_cell_attribute = variant
            .attrs
            .iter()
            .find(|attr| attr.path.segments.first().unwrap().ident == "cell");

        if let Some(cell_attribute) = variant_cell_attribute {
            if let Ok(argument) = extract_argument(cell_attribute) {
                variants.push(VariantAttribute {
                    argument,
                    identifier: variant.ident,
                });
            } else {
                abort!(
                    variant_cell_attribute.span(),
                    format!(
                        "'cell' attribute arguments for variant '{}' is invalid. \
                        The argument must be a char literal (ex: 'a') or a range of chars (ex: 'a'..'z').",
                        variant.ident
                    )
                );
            }
        } else {
            abort!(input.span(), format!("Missing 'cell' attribut for enum variant '{}'. \
            All variants must have a 'cell' attribute to derive 'GridCell', consider adding a #[cell(..)] attribute.", variant.ident));
        }
    }

    let try_form_char_impl = generate_try_from_char_impl(&enum_identifier, &variants);
    let char_form_cell_impl = generate_char_from_cell(&enum_identifier, &variants);

    let mut tokens = try_form_char_impl.to_token_stream();
    tokens.extend(char_form_cell_impl.to_token_stream().into_iter());
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    #[test]
    #[should_panic]
    fn test_missing_cell_attribute() {
        let stream = quote!(
            enum A {
                Wall,
            }
        );

        derive_grid_cell(stream);
    }

    #[test]
    #[should_panic]
    fn test_missing_mallformed_attribute() {
        let stream = quote!(
            enum A {
                #[cell]
                Wall,
            }
        );

        derive_grid_cell(stream);
    }

    #[test]
    #[should_panic]
    fn invalid_cell_attribute_arg() {
        let stream = quote!(
            enum A {
                #[cell(1)]
                Wall,
            }
        );

        derive_grid_cell(stream);
    }

    #[test]
    fn test_basic_passing_code() {
        let stream = quote!(
            enum A {
                #[cell('#')]
                Wall,
                #[cell('.')]
                Empty,
            }
        );

        let output_stream = derive_grid_cell(stream);
        assert!(!output_stream.is_empty());
    }

    #[test]
    fn test_or_args() {
        let stream = quote!(
            enum A {
                #[cell('.'|' ')]
                Empty,
            }
        );

        let output_stream = derive_grid_cell(stream);
        assert!(!output_stream.is_empty());
    }

    #[test]
    #[should_panic]
    fn test_wrong_synthax() {
        let stream = quote!(
            enum A {
                #[cell('.'|)]
                Empty,
            }
        );

        let output_stream = derive_grid_cell(stream);
        assert!(!output_stream.is_empty());
    }

    #[test]
    #[should_panic]
    fn test_wrong_synthax2() {
        let stream = quote!(
            enum A {
                #[cell('.'|a)]
                Empty,
            }
        );

        let output_stream = derive_grid_cell(stream);
        assert!(!output_stream.is_empty());
    }
}

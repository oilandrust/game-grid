use proc_macro2::{Ident, TokenStream};
use proc_macro_error::abort;
use quote::ToTokens;
use syn::{
    parse2, parse_quote, spanned::Spanned, Arm, Attribute, Expr, ExprMatch, ExprRange, ImplItem,
    ImplItemMethod, ItemEnum, ItemImpl, LitChar, Stmt,
};

enum AttributeArg {
    Literal(LitChar),
    Range(ExprRange),
}

struct VariantAttribute {
    argument: AttributeArg,
    identifier: Ident,
}

fn extract_argument(cell_attribute: &Attribute) -> Result<AttributeArg, ()> {
    if let Ok(char_lit) = cell_attribute.parse_args::<LitChar>() {
        Ok(AttributeArg::Literal(char_lit))
    } else if let Ok(range) = cell_attribute.parse_args::<ExprRange>() {
        Ok(AttributeArg::Range(range))
    } else {
        Err(())
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
        }
    }

    arms.push(parse_quote!( _ => Err(()),));

    arms
}

fn generate_try_from_char_impl(
    enum_identifier: &Ident,
    variants: &Vec<VariantAttribute>,
) -> ItemImpl {
    let try_from_char_fn = {
        let match_arms: Vec<Arm> = construct_try_from_match_arms(enum_identifier, variants);
        let mut try_from_fn: ImplItemMethod = parse_quote!(
            fn try_from(value: char) -> Result<Self, Self::Error> {
                match value {}
            }
        );

        let new_match = if let Stmt::Expr(Expr::Match(old_match)) = &try_from_fn.block.stmts[0] {
            ExprMatch {
                arms: match_arms,
                ..old_match.clone()
            }
        } else {
            panic!("should not happen")
        };

        try_from_fn.block.stmts[0] = Stmt::Expr(Expr::Match(new_match));
        try_from_fn
    };

    let try_from_impl_type = parse_quote!(
        type Error = ();
    );
    let implementation_base: ItemImpl = parse_quote!(
        impl TryFrom<char> for #enum_identifier {
        }
    );

    ItemImpl {
        items: vec![try_from_impl_type, ImplItem::Method(try_from_char_fn)],
        ..implementation_base
    }
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
                arms.push(parse_quote!( #enum_identifier::#ident => #char_literal));
            }
            AttributeArg::Range(_) => todo!(),
        }
    }

    arms
}

fn generate_char_from_cell(enum_identifier: &Ident, variants: &Vec<VariantAttribute>) -> ItemImpl {
    let char_from_cell_fn = {
        let match_arms: Vec<Arm> = construct_char_from_cell_match_arms(enum_identifier, variants);
        let mut char_from_cell: ImplItemMethod = parse_quote!(
            fn from(cell: #enum_identifier) -> char {
                match cell {}
            }
        );

        let new_match = if let Stmt::Expr(Expr::Match(old_match)) = &char_from_cell.block.stmts[0] {
            ExprMatch {
                arms: match_arms,
                ..old_match.clone()
            }
        } else {
            panic!("should not happen")
        };

        char_from_cell.block.stmts[0] = Stmt::Expr(Expr::Match(new_match));
        char_from_cell
    };

    let implementation_base: ItemImpl = parse_quote!(
        impl From<#enum_identifier> for char {
        }
    );

    ItemImpl {
        items: vec![ImplItem::Method(char_from_cell_fn)],
        ..implementation_base
    }
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
    fn test_derive_grid_cell() {
        // Empty enum.
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
}

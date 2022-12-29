use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(GridCell, attributes(cell))]
pub fn grid_cell(input: TokenStream) -> TokenStream {
    let derive_input = parse_macro_input!(input as DeriveInput);

    let enum_identifier = derive_input.ident;

    let implementation = quote!(
        impl GridCell for #enum_identifier {
            const EMPTY: Self = Self::Empty;
        }

        impl TryFrom<char> for #enum_identifier {
            type Error = ();

            fn try_from(value: char) -> Result<Self, Self::Error> {
                match value {
                    _ => Err(()),
                }
            }
        }
    );

    implementation.into()
}

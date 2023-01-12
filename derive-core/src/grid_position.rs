use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse2, ItemStruct};

pub fn derive_grid_position(input: TokenStream) -> TokenStream {
    let input_struct = match parse2::<ItemStruct>(input) {
        Ok(syntax_tree) => syntax_tree,
        Err(error) => return error.to_compile_error(),
    };

    let struct_identifier = input_struct.ident;
    let implementation_base = quote!(
        impl GridPosition for #struct_identifier {
            fn new(x: i32, y: i32) -> Self {
                Self::new(x, y)
            }

            fn x(&self) -> i32 {
                self.x
            }

            fn y(&self) -> i32 {
                self.y
            }
        }
    );

    implementation_base
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;

    #[test]
    fn test_baisc() {
        let stream = quote!(
            struct Position {
                x: i32,
                y: i32,
            }
        );

        let output_stream = derive_grid_position(stream);
        assert!(!output_stream.is_empty());
    }
}

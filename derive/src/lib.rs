use derive_core::derive_grid_cell;
use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;

#[proc_macro_error]
#[proc_macro_derive(GridCell, attributes(cell))]
pub fn grid_cell(input: TokenStream) -> TokenStream {
    let implementation = derive_grid_cell(input.into());
    implementation.into()
}

use game_grid_derive_core::{derive_grid_cell, derive_grid_position};
use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;

#[proc_macro_error]
#[proc_macro_derive(GridCell, attributes(cell))]
pub fn grid_cell(input: TokenStream) -> TokenStream {
    let implementation = derive_grid_cell(input.into());
    implementation.into()
}

#[proc_macro_error]
#[proc_macro_derive(GridPosition)]
pub fn grid_position(input: TokenStream) -> TokenStream {
    let implementation = derive_grid_position(input.into());
    implementation.into()
}

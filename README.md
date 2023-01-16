# Game Grid
`game-grid` provides a simple 2D grid that can be used to prototype games.

## Key features:

* Easy parsing of string literal to typed 2D grid thanks to a derive macro.
* Indexing with a 2D vector struct ex: `Point { x: i32, y: i32 }` instead of always writing the usual `i = y * width + x`
* std-like iterators and utilities.

## Description

The main struct is `Grid` that implements a grid able to contain values of a user `Cell` type. The user cell can be any type but it works best with enums that implement the `GridCell` trait. The `GridCell` derive macro allows to implement automatically conversions to and from `char`, allowing to convert a grid to an from strings. `Grid` provides access to the cells with 2D indexing with user types that implement the `GridPosition` trait. On top of that `Grid` provides iterators and other utilities.

## Using the Grid with Bevy IVec2

One of the core features of `game-grid` is to be able to index the grid with 2D vector structs that we use to make games.
If you are using this with Bevy, the feature `bevy-ivec2` includes a trait implementation of `game_grid::GridPosition` for `IVec2` that allows to use `IVec2` as index.
To use it add this line to you `Cargo.toml`:

```
[dependencies]
game-grid = { git = "https://github.com/oilandrust/game-grid.git", features = ["bevy-ivec2"] }
```

## Example

```
use game_grid::*;
// A custom Cell type deriving the trait GridCell with associated char literals.
#[derive(GridCell, Copy, Clone, Debug, PartialEq, Eq, Default)]
enum Cell {
    // Wall cells are represented by '#'.
    #[cell('#')]
    Wall,

    // Empty cells are represented by both ' ' and '.', the former will be used for display.
    // A default value can be used by some Grid functionalities.
    #[cell(' '|'.')]
    #[default]
    Empty,
     
    #[cell('o')]
    Food,

    // It is also possible to provide a range, the actual character can be captured.
    #[cell('A'..='Z')]
    Player(char),
}

// A 2D point struct deriving GridPosition in order to be used as index into the grid.
#[derive(GridPosition, PartialEq, Eq, Debug)]
struct Point {
    x: i32,
    y: i32,
}

// Create a grid of cells by parsing a string literal.
let grid: Grid<Cell> = "#####\n\
                        #A o#\n\
                        #####".parse().unwrap();

// Use iter() to iterate over the cells with associated position.
let food_position: Point = grid.iter().find(|(_, cell)| *cell == Cell::Food).unwrap().0;
assert_eq!(food_position, Point{ x: 3, y: 1 });

// Index into the grid with 2D point type and retrieved the enum value captured during parsing.
if let Cell::Player(player_char) = grid[Point::new(1, 1)] {
    println!("Player id: {player_char}");
}

// Print the grid.
print!("{grid}");
// outputs:
// #####
// #A o#
// #####
```
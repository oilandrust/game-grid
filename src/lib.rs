//! # Game Grid
//!
//! A simple 2D grid for prototyping games. Including easy parsing, indexing and iterators.
//!
//! ## Key features:
//! * Easy parsing of string literal to typed 2D grid thanks to a derive macro.
//! * Indexing with a 2D vector struct ex: Point { x: i32, y: i32 } instead of always writing the usual i = y * width + x
//! * Iterators and utilities.
//!
//! ## Description
//!
//! The main struct is `Grid` that implements a grid able to contain values of a user `Cell` type.
//! The user cell can be any type but it works best with enums that implement the GridCell trait.
//! The GridCell derive macro allows to implement automatically conversions to and from char, allowing to convert a grid to an from strings.
//! `Grid` provides access to the cells with 2D indexing with user types that implement the `GridPosition` trait.
//! On top of that `Grid` provides iterators and other utilities.
//!
//! ## Using the Grid with Bevy IVec2
//! One of the core features of game-grid is to be able to index the grid with 2D vector structs that we use to make games.
//! If you are using this with Bevy, the feature bevy-ivec2 includes a trait implementation of game_grid::GridPosition for IVec2 that allows to use IVec2 as index.
//! To use it add this line to you Cargo.toml: game-grid = { git = "https://github.com/oilandrust/game-grid.git", features = ["bevy-ivec2"] }
//!
//! ## Example:
//!
//! ```
//! use game_grid::*;
//! // A custom Cell type deriving the trait GridCell with associated char literals.
//! #[derive(GridCell, Copy, Clone, Debug, PartialEq, Eq, Default)]
//! enum Cell {
//!     // Wall cells are represented by '#'.
//!     #[cell('#')]
//!     Wall,
//!
//!     // Empty cells are represented by both ' ' and '.', the former will be used for display.
//!     // A default value can be used by some Grid functionalities.
//!     #[cell(' '|'.')]
//!     #[default]
//!     Empty,
//!     
//!     #[cell('o')]
//!     Food,
//!
//!     // It is also possible to provide a range, the actual character can be captured.
//!     #[cell('A'..='Z')]
//!     Player(char),
//! }
//!
//! // A 2D point struct deriving GridPosition in order to be used as index into the grid.
//! #[derive(GridPosition, PartialEq, Eq, Debug)]
//! struct Point {
//!     x: i32,
//!     y: i32,
//! }
//!
//! // Create a grid of cells by parsing a string literal.
//! let grid: Grid<Cell> = "#####\n\
//!                         #A o#\n\
//!                         #####".parse().unwrap();
//!
//! // Use iter() to iterate over the cells with associated position.
//! let food_position: Point = grid.iter().find(|(_, cell)| *cell == Cell::Food).unwrap().0;
//! assert_eq!(food_position, Point{ x: 3, y: 1 });
//!
//! // Index into the grid with 2D point type and retrieved the enum value captured during parsing.
//! if let Cell::Player(player_char) = grid[Point::new(1, 1)] {
//!     println!("Player id: {player_char}");
//! }
//!
//! // Print the grid.
//! print!("{grid}");
//! // outputs:
//! // #####
//! // #A o#
//! // #####
//!
//! ```
use core::slice::Iter;
use std::error::Error;
use std::marker::PhantomData;
use std::ops::Index;
use std::slice::IterMut;
use std::{fmt::Display, str::FromStr};

pub use derive::*;

/// Trait to implement a type that can be used as a grid cell with pparsing and display functionalities.
///
/// The trait itself is empty but requires to implement `TryFrom<char>`.
/// This trait is most useful by using the derive macro and specifying assiciated char values.
///
/// ```
/// use game_grid::*;
/// #[derive(GridCell, Copy, Clone, Debug, PartialEq, Eq, Default)]
/// enum Cell {
///     // Wall cells are represented by '#'.
///     #[cell('#')]
///     Wall,
///
///     // Empty cells are represented by both ' ' and '.', the former will be used for display.
///     // A default value can be used by some Grid functionalities.
///     #[cell(' '|'.')]
///     #[default]
///     Empty,
///     
///     #[cell('o')]
///     Food,
///
///     // It is also possible to provide a range, the actual character can be captured.
///     #[cell('A'..='Z')]
///     Player(char),
/// }
/// ```
pub trait GridCell: TryFrom<char> + Clone + Copy + PartialEq + Eq {}

/// Trait to implement a type that can be used as a grid position.
///
/// This trait provides access to the x and y coordinates as well as a constructor used by the grid internaly.
pub trait GridPosition {
    /// Construct a position from x and y coordinates.
    fn new(x: i32, y: i32) -> Self;

    /// Access the x coordinate of a position.
    fn x(&self) -> i32;

    /// Access the y coordinate of a position.
    fn y(&self) -> i32;
}

#[cfg(feature = "bevy-ivec2")]
impl GridPosition for bevy::prelude::IVec2 {
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

/// A struct maintaining a grid usable for game prototyping.
///
/// The grid is stored as a linear `Vec` containing cells and Grid provides
/// functions to look up and write to the grid with 2-dimentional vector types implementing the trait `GridPosition`
///
/// ```
/// use game_grid::*;
/// #[derive(Clone, Copy)]
/// enum Cell {
///     Wall,
///     Empty,
/// }
///
/// #[derive(GridPosition, PartialEq, Eq, Debug)]
/// struct Point {
///     x: i32,
///     y: i32,
/// }
///
/// // Create a 2x2 grid with empty cells.
/// let mut grid: Grid<Cell> = Grid::new(2, 2, Cell::Empty);
/// assert_eq!(grid.width(), 2);
/// assert_eq!(grid.height(), 2);
///
/// // Add a wall at cell (0, 0).
/// grid.set_cell(Point::new(0, 0), Cell::Wall);
/// ```
#[derive(Debug, Clone)]
pub struct Grid<Cell> {
    cells: Vec<Cell>,
    width: usize,
    height: usize,
}

impl<Cell> Grid<Cell>
where
    Cell: Clone + Default,
{
    /// Construct a grid from a slice and the desired row width.
    ///
    /// ```
    /// use game_grid::*;
    /// // Create a 2x2 grid with some data.
    /// let grid: Grid<i32> = Grid::from_slice(2, &[0, 1, 2, 3]);
    /// assert_eq!(grid.width(), 2);
    /// assert_eq!(grid.height(), 2);
    /// ```
    ///
    /// Any slice with width equal to 0 will produce an empty grid.
    /// If the length of input slice is not a multiple of width,
    /// the last row will be filled with default cell values so that the grid is square.
    pub fn from_slice(width: usize, data: &[Cell]) -> Self {
        if width == 0 {
            return Self {
                cells: vec![],
                width,
                height: 0,
            };
        }
        let height = data.len() / width;
        let mut cells: Vec<Cell> = data.into();

        if data.len() < width * height {
            cells.resize(width * height, Cell::default());
        }

        Self {
            cells,
            width,
            height,
        }
    }
}

impl<Cell> Grid<Cell>
where
    Cell: Clone,
{
    /// Construct a grid from a slice and the desired row width.
    /// Any slice with width equal to 0 will produce an empty grid.
    /// The function will panic if the length of the input slice is not a multiple of width.
    pub fn from_slice_exact(width: usize, data: &[Cell]) -> Self {
        if width == 0 {
            return Self {
                cells: vec![],
                width,
                height: 0,
            };
        }
        let height = data.len() / width;

        if data.len() != width * height {
            panic!("'from_slice_exact' expects the input data's length to be a multiple of width.");
        }

        Self {
            cells: data.into(),
            width,
            height,
        }
    }
}

impl<Cell> Grid<Cell>
where
    Cell: Clone + Copy,
{
    /// Flips the order of the lines vertically. Useful when the game's y axis is upwards.
    /// # Example:
    /// ```
    /// use game_grid::Grid;
    ///
    /// let string_grid = "aaa
    /// bbb
    /// ccc";
    ///
    /// let grid = string_grid.parse::<Grid<char>>().unwrap().flip_y();
    ///
    /// let string_grid_flipped = "ccc
    /// bbb
    /// aaa";
    ///
    /// assert_eq!(grid.to_string(), string_grid_flipped);
    /// ```
    pub fn flip_y(mut self) -> Self {
        self.cells = self
            .cells
            .chunks(self.width)
            .rev()
            .flatten()
            .copied()
            .collect();
        self
    }

    /// Get the cell value at some position.
    pub fn cell_at<Point: GridPosition>(&self, position: Point) -> Cell {
        self.cells[self.index_for_position(position)]
    }

    /// Construct a new grid with width, height and an initial value.
    pub fn new(width: usize, height: usize, value: Cell) -> Self {
        let mut cells = Vec::new();
        cells.resize(width * height, value);
        Self {
            width,
            height,
            cells,
        }
    }
}

impl<Cell> Grid<Cell> {
    /// Set the cell value at some position.
    pub fn set_cell<Point: GridPosition>(&mut self, position: Point, value: Cell) {
        let index = self.index_for_position(position);
        self.cells[index] = value;
    }

    /// An iterator visiting the cells in order of memory.
    pub fn cells(&self) -> Iter<'_, Cell> {
        self.cells.iter()
    }

    /// An iterator visiting the cells mutably in order of memory.
    pub fn mut_cells(&mut self) -> IterMut<'_, Cell> {
        self.cells.iter_mut()
    }

    /// An iterator visiting the cell and associated position in the grid.
    pub fn iter<Point: GridPosition>(&self) -> GridIter<Cell, Point> {
        GridIter {
            current: 0,
            grid: self,
            phantom: PhantomData,
        }
    }

    /// Get the 2D position for an index in the linear array. index = y * width + x
    ///
    /// ```
    /// use game_grid::*;
    /// // A 2D point struct deriving GridPosition.
    /// #[derive(GridPosition, PartialEq, Eq, Debug)]
    /// struct Point {
    ///     x: i32,
    ///     y: i32,
    /// }
    /// let grid = Grid::<i32>::new(2, 2, 0);
    ///
    /// assert_eq!(grid.position_for_index::<Point>(3), Point::new(1, 1));
    /// ```
    pub fn position_for_index<Point: GridPosition>(&self, index: usize) -> Point {
        Point::new((index % self.width) as i32, (index / self.width) as i32)
    }

    /// Get the index in the linear array for a 2D position. Index = y * width + x.
    ///
    /// ```
    /// use game_grid::*;
    /// // A 2D point struct deriving GridPosition.
    /// #[derive(GridPosition, PartialEq, Eq, Debug)]
    /// struct Point {
    ///     x: i32,
    ///     y: i32,
    /// }
    /// let grid = Grid::<i32>::new(2, 2, 0);
    ///
    /// assert_eq!(grid.index_for_position(Point::new(1, 1)), 3);
    /// ```
    pub fn index_for_position<Point: GridPosition>(&self, position: Point) -> usize {
        position.x() as usize + self.width * position.y() as usize
    }

    /// Returns the number of cells in the grid.
    pub fn len(&self) -> usize {
        self.cells.len()
    }

    /// Check whether teh grid is empty.
    pub fn is_empty(&self) -> bool {
        self.cells.len() == 0
    }

    /// Returns the width of the grid.
    pub fn width(&self) -> usize {
        self.width
    }

    /// Returns the height of the grid.
    pub fn height(&self) -> usize {
        self.height
    }

    /// Check if a position is in the grid bounds.
    pub fn is_in_bounds<Point: GridPosition>(&self, position: Point) -> bool {
        position.x() >= 0
            && position.x() < self.width as i32
            && position.y() >= 0
            && position.y() < self.height as i32
    }
}

impl<Cell> Index<usize> for Grid<Cell> {
    type Output = Cell;

    fn index(&self, index: usize) -> &Self::Output {
        &self.cells[index]
    }
}

impl<Cell, Point: GridPosition> Index<Point> for Grid<Cell> {
    type Output = Cell;

    fn index(&self, position: Point) -> &Self::Output {
        &self.cells[self.index_for_position(position)]
    }
}

impl<Cell> Display for Grid<Cell>
where
    char: From<Cell>,
    Cell: Copy,
{
    fn fmt(&self, formater: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut output_string = String::with_capacity(self.cells.len() + (self.height - 1));
        for (index, line) in self.cells.chunks(self.width).enumerate() {
            output_string.extend(line.iter().map(|cell| char::from(*cell)));
            if index != self.height - 1 {
                output_string.push('\n');
            }
        }
        write!(formater, "{output_string}")
    }
}

/// An iterator over a grid that gives access to a tupple `(Point, Cell)`
pub struct GridIter<'a, Cell, Point> {
    current: usize,
    grid: &'a Grid<Cell>,
    phantom: PhantomData<Point>,
}

impl<'a, Cell, Point> Iterator for GridIter<'a, Cell, Point>
where
    Point: GridPosition,
    Cell: Copy,
{
    type Item = (Point, Cell);

    fn next(&mut self) -> Option<Self::Item> {
        if self.current == self.grid.len() {
            return None;
        }

        let result = (
            self.grid.position_for_index(self.current),
            self.grid[self.current],
        );

        self.current += 1;

        Some(result)
    }
}

/// Error that can be raised when parsing a cell, repoting the character that could not be read.
#[derive(Debug, PartialEq, Eq)]
pub struct ParseCellError(pub char);

impl Display for ParseCellError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid character '{}'", self.0)
    }
}

impl Error for ParseCellError {}

/// Error that can be raised when parsing a grid.
#[derive(Debug)]
pub struct ParseGridError<UserError> {
    source: UserError,
}

impl<UserError: Error> Display for ParseGridError<UserError> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error parsing grid: {}", self.source)
    }
}

impl<UserError: Error> Error for ParseGridError<UserError> {}

impl<UserError> From<UserError> for ParseGridError<UserError> {
    fn from(value: UserError) -> Self {
        ParseGridError { source: value }
    }
}

impl<Cell> FromStr for Grid<Cell>
where
    Cell: Default + TryFrom<char> + Clone,
{
    type Err = ParseGridError<<Cell as TryFrom<char>>::Error>;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        let lines: Result<Vec<Vec<Cell>>, _> = string
            .split('\n')
            .map(|line| line.chars().map(|char| char.try_into()).collect())
            .collect();

        match lines {
            Ok(mut lines) => {
                let width = lines.iter().max_by_key(|line| line.len()).unwrap().len();
                let height = lines.len();

                for line in &mut lines {
                    line.resize(width, Cell::default());
                }

                let cells: Vec<Cell> = lines.into_iter().flatten().collect();
                Ok(Grid {
                    cells,
                    width,
                    height,
                })
            }
            Err(err) => Err(ParseGridError { source: err }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Using an enum.
    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    enum Cell {
        Wall(i32),
        Empty,
    }

    impl Default for Cell {
        fn default() -> Self {
            Cell::Empty
        }
    }

    impl From<Cell> for char {
        fn from(cell: Cell) -> char {
            match cell {
                Cell::Wall(_) => '#',
                Cell::Empty => ' ',
            }
        }
    }

    impl TryFrom<char> for Cell {
        type Error = ();

        fn try_from(value: char) -> Result<Self, Self::Error> {
            match value {
                '#' => Ok(Cell::Wall(0)),
                ' ' => Ok(Cell::Empty),
                _ => Err(()),
            }
        }
    }

    // A 2D point struct.
    #[derive(GridPosition)]
    struct Point {
        x: i32,
        y: i32,
    }

    #[test]
    fn test_char_grid() {
        // Valid input.
        let result = "abc".parse::<Grid<char>>();
        assert!(result.is_ok());
        let result = result.unwrap();
        assert_eq!(result.to_string(), "abc");
    }

    #[test]
    fn test_struct_grid() {
        // Using a stuct.
        #[derive(Default, Copy, Clone, Debug, PartialEq, Eq)]
        struct StructCell {
            c: char,
        }

        impl From<StructCell> for char {
            fn from(cell: StructCell) -> char {
                cell.c
            }
        }

        impl TryFrom<char> for StructCell {
            type Error = ();

            fn try_from(value: char) -> Result<Self, Self::Error> {
                Ok(StructCell { c: value })
            }
        }

        // Valid input.
        let result = "abc".parse::<Grid<StructCell>>();
        assert!(result.is_ok());
        let result = result.unwrap();
        assert_eq!(result.to_string(), "abc");
    }

    #[test]
    fn test_enum_grid() {
        // Empty string.
        let result = "".parse::<Grid<Cell>>();
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.is_empty());

        // Valid input.
        let result = "## #".parse::<Grid<Cell>>();
        assert!(result.is_ok());
        let result = result.unwrap();
        assert_eq!(result.to_string(), "## #");

        // Wrong character is error.
        let result = "a".parse::<Grid<Cell>>();
        assert!(result.is_err());
    }

    #[test]
    fn test_enum_grid_without_impl() {
        #[derive(Copy, Clone, PartialEq, Eq, Debug)]
        enum Plain {
            A,
        }

        let grid: Grid<Plain> = Grid::from_slice_exact(1, &[Plain::A]);
        assert_eq!(grid[0], Plain::A);
    }

    #[test]
    fn test_indicing() {
        let grid: Grid<char> = Grid::from_slice(2, &['a', 'b', 'c', 'd']);
        assert_eq!(grid[0], 'a');
        assert_eq!(grid[Point::new(0, 0)], 'a');
    }

    #[test]
    fn test_derive() {
        use derive::GridCell;

        #[derive(GridCell, PartialEq, Eq, Copy, Clone, Debug)]
        enum Cell {
            // #[cell('A'..='Z')]
            // Wall,
            #[cell('.')]
            Empty,

            #[cell('a'|'b')]
            AOrB,
        }

        // Existing single entry.
        assert_eq!(Cell::try_from('.'), Ok(Cell::Empty));
        assert_eq!(char::from(Cell::Empty), '.');

        // Non existing entry.
        assert!(Cell::try_from(',').is_err());

        // Or entries.
        assert_eq!(Cell::try_from('a'), Ok(Cell::AOrB));
        assert_eq!(Cell::try_from('b'), Ok(Cell::AOrB));
        assert_eq!(char::from(Cell::AOrB), 'a');
    }

    #[test]
    fn test_derive_range() {
        use derive::GridCell;

        #[derive(GridCell, PartialEq, Eq, Copy, Clone, Debug, Default)]
        enum Cell {
            #[cell('a'..='z')]
            Char(char),

            #[default]
            #[cell(' ')]
            Empty,
        }

        // Existing single entry.
        assert_eq!(Cell::try_from('a'), Ok(Cell::Char('a')));
        assert_eq!(Cell::try_from('b'), Ok(Cell::Char('b')));

        let result = "ab".parse::<Grid<Cell>>();
        assert!(result.is_ok());
        let result = result.unwrap();
        assert_eq!(result[0], Cell::Char('a'));
        assert_eq!(result[1], Cell::Char('b'));
    }
}

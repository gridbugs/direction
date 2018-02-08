//! Representations of directions
extern crate cgmath;
extern crate grid_2d;
#[macro_use]
extern crate enum_primitive;
extern crate serde;
#[macro_use]
extern crate serde_derive;

use std::ops::{BitOr, BitOrAssign, BitAnd, BitAndAssign};
use cgmath::Vector2;
use grid_2d::Coord;
use enum_primitive::FromPrimitive;

pub const NUM_DIRECTIONS: usize = 8;
pub const NUM_CARDINAL_DIRECTIONS: usize = 4;
pub const NUM_ORDINAL_DIRECTIONS: usize = 4;
pub const ALL_DIRECTIONS_BITMAP_RAW: u8 = 0xff;
pub const NO_DIRECTIONS_BITMAP_RAW: u8 = 0;

pub const ALL_DIRECTIONS_BITMAP: DirectionBitmap =
    DirectionBitmap { raw: ALL_DIRECTIONS_BITMAP_RAW };
pub const NO_DIRECTIONS_BITMAP: DirectionBitmap = DirectionBitmap { raw: NO_DIRECTIONS_BITMAP_RAW };

enum_from_primitive! {
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Direction {
    North,
    NorthEast,
    East,
    SouthEast,
    South,
    SouthWest,
    West,
    NorthWest,
}
}

enum_from_primitive! {
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum CardinalDirection {
    North,
    East,
    South,
    West
}
}

enum_from_primitive! {
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum OrdinalDirection {
    NorthEast,
    SouthEast,
    SouthWest,
    NorthWest,
}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub enum DirectionType {
    Cardinal(CardinalDirection),
    Ordinal(OrdinalDirection),
}

impl Direction {
    pub fn opposite(self) -> Direction {
        match self {
            Direction::North => Direction::South,
            Direction::NorthEast => Direction::SouthWest,
            Direction::East => Direction::West,
            Direction::SouthEast => Direction::NorthWest,
            Direction::South => Direction::North,
            Direction::SouthWest => Direction::NorthEast,
            Direction::West => Direction::East,
            Direction::NorthWest => Direction::SouthEast,
        }
    }

    pub fn vector(self) -> Vector2<i32> {
        match self {
            Direction::North => Vector2::new(0, -1),
            Direction::NorthEast => Vector2::new(1, -1),
            Direction::East => Vector2::new(1, 0),
            Direction::SouthEast => Vector2::new(1, 1),
            Direction::South => Vector2::new(0, 1),
            Direction::SouthWest => Vector2::new(-1, 1),
            Direction::West => Vector2::new(-1, 0),
            Direction::NorthWest => Vector2::new(-1, -1),
        }
    }

    pub fn coord(self) -> Coord {
        match self {
            Direction::North => Coord::new(0, -1),
            Direction::NorthEast => Coord::new(1, -1),
            Direction::East => Coord::new(1, 0),
            Direction::SouthEast => Coord::new(1, 1),
            Direction::South => Coord::new(0, 1),
            Direction::SouthWest => Coord::new(-1, 1),
            Direction::West => Coord::new(-1, 0),
            Direction::NorthWest => Coord::new(-1, -1),
        }
    }

    pub fn left90(self) -> Direction {
        match self {
            Direction::North => Direction::West,
            Direction::NorthEast => Direction::NorthWest,
            Direction::East => Direction::North,
            Direction::SouthEast => Direction::NorthEast,
            Direction::South => Direction::East,
            Direction::SouthWest => Direction::SouthEast,
            Direction::West => Direction::South,
            Direction::NorthWest => Direction::SouthWest,
        }
    }

    pub fn right90(self) -> Direction {
        match self {
            Direction::North => Direction::East,
            Direction::NorthEast => Direction::SouthEast,
            Direction::East => Direction::South,
            Direction::SouthEast => Direction::SouthWest,
            Direction::South => Direction::West,
            Direction::SouthWest => Direction::NorthWest,
            Direction::West => Direction::North,
            Direction::NorthWest => Direction::NorthEast,
        }
    }

    pub fn left45(self) -> Direction {
        match self {
            Direction::North => Direction::NorthWest,
            Direction::NorthEast => Direction::North,
            Direction::East => Direction::NorthEast,
            Direction::SouthEast => Direction::East,
            Direction::South => Direction::SouthEast,
            Direction::SouthWest => Direction::South,
            Direction::West => Direction::SouthWest,
            Direction::NorthWest => Direction::West,
        }
    }

    pub fn right45(self) -> Direction {
        match self {
            Direction::North => Direction::NorthEast,
            Direction::NorthEast => Direction::East,
            Direction::East => Direction::SouthEast,
            Direction::SouthEast => Direction::South,
            Direction::South => Direction::SouthWest,
            Direction::SouthWest => Direction::West,
            Direction::West => Direction::NorthWest,
            Direction::NorthWest => Direction::North,
        }
    }

    pub fn left135(self) -> Direction {
        match self {
            Direction::North => Direction::SouthWest,
            Direction::NorthEast => Direction::West,
            Direction::East => Direction::NorthWest,
            Direction::SouthEast => Direction::North,
            Direction::South => Direction::NorthEast,
            Direction::SouthWest => Direction::East,
            Direction::West => Direction::SouthEast,
            Direction::NorthWest => Direction::South,
        }
    }

    pub fn right135(self) -> Direction {
        match self {
            Direction::North => Direction::SouthEast,
            Direction::NorthEast => Direction::South,
            Direction::East => Direction::SouthWest,
            Direction::SouthEast => Direction::West,
            Direction::South => Direction::NorthWest,
            Direction::SouthWest => Direction::North,
            Direction::West => Direction::NorthEast,
            Direction::NorthWest => Direction::East,
        }
    }

    pub fn bitmap_raw(self) -> u8 {
        1 << self as usize
    }

    pub fn bitmap(self) -> DirectionBitmap {
        DirectionBitmap::new(self.bitmap_raw())
    }

    pub fn is_cardinal(self) -> bool {
        match self {
            Direction::North => true,
            Direction::NorthEast => false,
            Direction::East => true,
            Direction::SouthEast => false,
            Direction::South => true,
            Direction::SouthWest => false,
            Direction::West => true,
            Direction::NorthWest => false,
        }
    }

    pub fn is_ordinal(self) -> bool {
        match self {
            Direction::North => false,
            Direction::NorthEast => true,
            Direction::East => false,
            Direction::SouthEast => true,
            Direction::South => false,
            Direction::SouthWest => true,
            Direction::West => false,
            Direction::NorthWest => true,
        }
    }

    pub fn typ(self) -> DirectionType {
        match self {
            Direction::North => DirectionType::Cardinal(CardinalDirection::North),
            Direction::NorthEast => DirectionType::Ordinal(OrdinalDirection::NorthEast),
            Direction::East => DirectionType::Cardinal(CardinalDirection::East),
            Direction::SouthEast => DirectionType::Ordinal(OrdinalDirection::SouthEast),
            Direction::South => DirectionType::Cardinal(CardinalDirection::South),
            Direction::SouthWest => DirectionType::Ordinal(OrdinalDirection::SouthWest),
            Direction::West => DirectionType::Cardinal(CardinalDirection::West),
            Direction::NorthWest => DirectionType::Ordinal(OrdinalDirection::NorthWest),
        }
    }
}

impl CardinalDirection {
    pub fn direction(self) -> Direction {
        match self {
            CardinalDirection::North => Direction::North,
            CardinalDirection::East => Direction::East,
            CardinalDirection::South => Direction::South,
            CardinalDirection::West => Direction::West,
        }
    }

    pub fn opposite(self) -> CardinalDirection {
        match self {
            CardinalDirection::North => CardinalDirection::South,
            CardinalDirection::East => CardinalDirection::West,
            CardinalDirection::South => CardinalDirection::North,
            CardinalDirection::West => CardinalDirection::East,
        }
    }

    pub fn vector(self) -> Vector2<i32> {
        match self {
            CardinalDirection::North => Vector2::new(0, -1),
            CardinalDirection::East => Vector2::new(1, 0),
            CardinalDirection::South => Vector2::new(0, 1),
            CardinalDirection::West => Vector2::new(-1, 0),
        }
    }

    pub fn coord(self) -> Coord {
        match self {
            CardinalDirection::North => Coord::new(0, -1),
            CardinalDirection::East => Coord::new(1, 0),
            CardinalDirection::South => Coord::new(0, 1),
            CardinalDirection::West => Coord::new(-1, 0),
        }
    }

    pub fn left90(self) -> CardinalDirection {
        match self {
            CardinalDirection::North => CardinalDirection::West,
            CardinalDirection::East => CardinalDirection::North,
            CardinalDirection::South => CardinalDirection::East,
            CardinalDirection::West => CardinalDirection::South,
        }
    }

    pub fn right90(self) -> CardinalDirection {
        match self {
            CardinalDirection::North => CardinalDirection::East,
            CardinalDirection::East => CardinalDirection::South,
            CardinalDirection::South => CardinalDirection::West,
            CardinalDirection::West => CardinalDirection::North,
        }
    }

    pub fn left45(self) -> OrdinalDirection {
        match self {
            CardinalDirection::North => OrdinalDirection::NorthWest,
            CardinalDirection::East => OrdinalDirection::NorthEast,
            CardinalDirection::South => OrdinalDirection::SouthEast,
            CardinalDirection::West => OrdinalDirection::SouthWest,
        }
    }

    pub fn right45(self) -> OrdinalDirection {
        match self {
            CardinalDirection::North => OrdinalDirection::NorthEast,
            CardinalDirection::East => OrdinalDirection::SouthEast,
            CardinalDirection::South => OrdinalDirection::SouthWest,
            CardinalDirection::West => OrdinalDirection::NorthWest,
        }
    }

    pub fn left135(self) -> OrdinalDirection {
        match self {
            CardinalDirection::North => OrdinalDirection::SouthWest,
            CardinalDirection::East => OrdinalDirection::NorthWest,
            CardinalDirection::South => OrdinalDirection::NorthEast,
            CardinalDirection::West => OrdinalDirection::SouthEast,
        }
    }

    pub fn right135(self) -> OrdinalDirection {
        match self {
            CardinalDirection::North => OrdinalDirection::SouthEast,
            CardinalDirection::East => OrdinalDirection::SouthWest,
            CardinalDirection::South => OrdinalDirection::NorthWest,
            CardinalDirection::West => OrdinalDirection::NorthEast,
        }
    }
}

impl OrdinalDirection {
    pub fn direction(self) -> Direction {
        match self {
            OrdinalDirection::NorthEast => Direction::NorthEast,
            OrdinalDirection::SouthEast => Direction::SouthEast,
            OrdinalDirection::SouthWest => Direction::SouthWest,
            OrdinalDirection::NorthWest => Direction::NorthWest,
        }
    }

    pub fn opposite(self) -> OrdinalDirection {
        match self {
            OrdinalDirection::NorthEast => OrdinalDirection::SouthWest,
            OrdinalDirection::SouthEast => OrdinalDirection::NorthWest,
            OrdinalDirection::SouthWest => OrdinalDirection::NorthEast,
            OrdinalDirection::NorthWest => OrdinalDirection::SouthEast,
        }
    }

    pub fn vector(self) -> Vector2<i32> {
        match self {
            OrdinalDirection::NorthEast => Vector2::new(1, -1),
            OrdinalDirection::SouthEast => Vector2::new(1, 1),
            OrdinalDirection::SouthWest => Vector2::new(-1, 1),
            OrdinalDirection::NorthWest => Vector2::new(-1, -1),
        }
    }

    pub fn coord(self) -> Coord {
        match self {
            OrdinalDirection::NorthEast => Coord::new(1, -1),
            OrdinalDirection::SouthEast => Coord::new(1, 1),
            OrdinalDirection::SouthWest => Coord::new(-1, 1),
            OrdinalDirection::NorthWest => Coord::new(-1, -1),
        }
    }

    pub fn left90(self) -> OrdinalDirection {
        match self {
            OrdinalDirection::NorthEast => OrdinalDirection::NorthWest,
            OrdinalDirection::SouthEast => OrdinalDirection::NorthEast,
            OrdinalDirection::SouthWest => OrdinalDirection::SouthEast,
            OrdinalDirection::NorthWest => OrdinalDirection::SouthWest,
        }
    }

    pub fn right90(self) -> OrdinalDirection {
        match self {
            OrdinalDirection::NorthEast => OrdinalDirection::SouthEast,
            OrdinalDirection::SouthEast => OrdinalDirection::SouthWest,
            OrdinalDirection::SouthWest => OrdinalDirection::NorthWest,
            OrdinalDirection::NorthWest => OrdinalDirection::NorthEast,
        }
    }

    pub fn left45(self) -> CardinalDirection {
        match self {
            OrdinalDirection::NorthEast => CardinalDirection::North,
            OrdinalDirection::SouthEast => CardinalDirection::East,
            OrdinalDirection::SouthWest => CardinalDirection::South,
            OrdinalDirection::NorthWest => CardinalDirection::West,
        }
    }

    pub fn right45(self) -> CardinalDirection {
        match self {
            OrdinalDirection::NorthEast => CardinalDirection::East,
            OrdinalDirection::SouthEast => CardinalDirection::South,
            OrdinalDirection::SouthWest => CardinalDirection::West,
            OrdinalDirection::NorthWest => CardinalDirection::North,
        }
    }

    pub fn left135(self) -> CardinalDirection {
        match self {
            OrdinalDirection::NorthEast => CardinalDirection::West,
            OrdinalDirection::SouthEast => CardinalDirection::North,
            OrdinalDirection::SouthWest => CardinalDirection::East,
            OrdinalDirection::NorthWest => CardinalDirection::South,
        }
    }

    pub fn right135(self) -> CardinalDirection {
        match self {
            OrdinalDirection::NorthEast => CardinalDirection::South,
            OrdinalDirection::SouthEast => CardinalDirection::West,
            OrdinalDirection::SouthWest => CardinalDirection::North,
            OrdinalDirection::NorthWest => CardinalDirection::East,
        }
    }

    pub fn from_cardinals(a: CardinalDirection, b: CardinalDirection) -> Option<Self> {
        match a {
            CardinalDirection::North => {
                match b {
                    CardinalDirection::East => return Some(OrdinalDirection::NorthEast),
                    CardinalDirection::West => return Some(OrdinalDirection::NorthWest),
                    _ => return None,
                }
            }
            CardinalDirection::East => {
                match b {
                    CardinalDirection::North => return Some(OrdinalDirection::NorthEast),
                    CardinalDirection::South => return Some(OrdinalDirection::SouthEast),
                    _ => return None,
                }
            }
            CardinalDirection::South => {
                match b {
                    CardinalDirection::East => return Some(OrdinalDirection::SouthEast),
                    CardinalDirection::West => return Some(OrdinalDirection::SouthWest),
                    _ => return None,
                }
            }
            CardinalDirection::West => {
                match b {
                    CardinalDirection::North => return Some(OrdinalDirection::NorthWest),
                    CardinalDirection::South => return Some(OrdinalDirection::SouthWest),
                    _ => return None,
                }
            }
        }
    }

    pub fn to_cardinals(self) -> (CardinalDirection, CardinalDirection) {
        use self::OrdinalDirection::*;
        use self::CardinalDirection::*;
        match self {
            NorthEast => (North, East),
            SouthEast => (East, South),
            SouthWest => (South, West),
            NorthWest => (West, North),
        }
    }

    pub fn cardinal_bitmap(self) -> DirectionBitmap {
        let (a, b) = self.to_cardinals();
        a.direction().bitmap() | b.direction().bitmap()
    }
}

impl From<CardinalDirection> for Direction {
    fn from(c: CardinalDirection) -> Self {
        c.direction()
    }
}

impl From<OrdinalDirection> for Direction {
    fn from(o: OrdinalDirection) -> Self {
        o.direction()
    }
}

macro_rules! make_direction_iter {
    ($col_name:ident, $iter_name:ident, $type:ident) => {
        /// Iterator over all directions of the respectively-named type of direction
        pub struct $iter_name(u8);
        impl Iterator for $iter_name {
            type Item = $type;
            fn next(&mut self) -> Option<Self::Item> {
                let d = Self::Item::from_u8(self.0);
                self.0 += 1;
                d
            }
        }

        /// Represents a collection of the respectively-named type of direction
        #[derive(Clone, Copy)]
        pub struct $col_name;
        impl IntoIterator for $col_name {
            type Item = $type;
            type IntoIter = $iter_name;
            fn into_iter(self) -> Self::IntoIter {
                $iter_name(0)
            }
        }
    }
}

// IntoIter implementations for iterating over all directions of a type. E.g.:
// for direction in CardinalDirections { ... }
make_direction_iter!{Directions, DirectionIter, Direction}
make_direction_iter!{CardinalDirections, CardinalDirectionIter, CardinalDirection}
make_direction_iter!{OrdinalDirections, OrdinalDirectionIter, OrdinalDirection}

macro_rules! make_subdirection_iter {
    ($col_name:ident, $backing_col_name:ident, $iter_name:ident, $backing_iter_name:ident) => {
        /// Iterator over a particular collection of `Direction`s
        pub struct $iter_name($backing_iter_name);
        impl Iterator for $iter_name {
            type Item = Direction;
            fn next(&mut self) -> Option<Self::Item> {
                self.0.next().map(|d| d.direction())
            }
        }

        #[derive(Clone, Copy)]
        /// Represents a particular collection of `Direction`s
        pub struct $col_name;
        impl IntoIterator for $col_name {
            type Item = Direction;
            type IntoIter = $iter_name;
            fn into_iter(self) -> Self::IntoIter {
                $iter_name($backing_col_name.into_iter())
            }
        }
    }
}

// IntoIter implementations for iterating over a subset of directions. E.g.:
// for direction in DirectionsCardinal { ... }
make_subdirection_iter!{
    DirectionsCardinal,
    CardinalDirections,
    DirectionCardinalIter,
    CardinalDirectionIter
}
make_subdirection_iter!{
    DirectionsOrdinal,
    OrdinalDirections,
    DirectionOrdinalIter,
    OrdinalDirectionIter
}

impl From<Direction> for Vector2<i32> {
    fn from(direction: Direction) -> Self {
        direction.vector()
    }
}

impl From<CardinalDirection> for Vector2<i32> {
    fn from(direction: CardinalDirection) -> Self {
        direction.vector()
    }
}

impl From<OrdinalDirection> for Vector2<i32> {
    fn from(direction: OrdinalDirection) -> Self {
        direction.vector()
    }
}

/// Set of directions implemented as a bitmap
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct DirectionBitmap {
    pub raw: u8,
}

impl DirectionBitmap {
    pub fn new(raw: u8) -> Self {
        Self { raw }
    }

    pub fn empty() -> Self {
        NO_DIRECTIONS_BITMAP
    }

    pub fn all() -> Self {
        ALL_DIRECTIONS_BITMAP
    }

    pub fn has(self, direction: Direction) -> bool {
        self.raw & (1 << direction as usize) != 0
    }

    pub fn is_empty(self) -> bool {
        self.raw == 0
    }
}

impl Default for DirectionBitmap {
    fn default() -> Self {
        Self::empty()
    }
}

impl BitOr for DirectionBitmap {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        DirectionBitmap::new(self.raw | rhs.raw)
    }
}

impl BitOrAssign for DirectionBitmap {
    fn bitor_assign(&mut self, rhs: Self) {
        self.raw |= rhs.raw;
    }
}

impl BitAnd for DirectionBitmap {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        DirectionBitmap::new(self.raw & rhs.raw)
    }
}

impl BitAndAssign for DirectionBitmap {
    fn bitand_assign(&mut self, rhs: Self) {
        self.raw &= rhs.raw;
    }
}

impl From<CardinalDirection> for [i32; 2] {
    fn from(c: CardinalDirection) -> [i32; 2] {
        use self::CardinalDirection::*;
        match c {
            North => [0, -1],
            East => [1, 0],
            South => [0, 1],
            West => [-1, 0],
        }
    }
}
impl From<CardinalDirection> for (i32, i32) {
    fn from(c: CardinalDirection) -> (i32, i32) {
        use self::CardinalDirection::*;
        match c {
            North => (0, -1),
            East => (1, 0),
            South => (0, 1),
            West => (-1, 0),
        }
    }
}

impl From<OrdinalDirection> for [i32; 2] {
    fn from(o: OrdinalDirection) -> [i32; 2] {
        use self::OrdinalDirection::*;
        match o {
            NorthWest => [-1, -1],
            NorthEast => [1, -1],
            SouthEast => [1, 1],
            SouthWest => [-1, 1],
        }
    }
}
impl From<OrdinalDirection> for (i32, i32) {
    fn from(o: OrdinalDirection) -> (i32, i32) {
        use self::OrdinalDirection::*;
        match o {
            NorthWest => (-1, -1),
            NorthEast => (1, -1),
            SouthEast => (1, 1),
            SouthWest => (-1, 1),
        }
    }
}

impl From<Direction> for [i32; 2] {
    fn from(d: Direction) -> [i32; 2] {
        use self::Direction::*;
        match d {
            North => [0, -1],
            East => [1, 0],
            South => [0, 1],
            West => [-1, 0],
            NorthWest => [-1, -1],
            NorthEast => [1, -1],
            SouthEast => [1, 1],
            SouthWest => [-1, 1],
        }
    }
}
impl From<Direction> for (i32, i32) {
    fn from(d: Direction) -> (i32, i32) {
        use self::Direction::*;
        match d {
            North => (0, -1),
            East => (1, 0),
            South => (0, 1),
            West => (-1, 0),
            NorthWest => (-1, -1),
            NorthEast => (1, -1),
            SouthEast => (1, 1),
            SouthWest => (-1, 1),
        }
    }
}

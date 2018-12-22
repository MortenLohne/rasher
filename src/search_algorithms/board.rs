//! Traits for abstract game board representations.
//!
//! General game-agnostic tools and engines can be built on this module
//! Represents any 2-player sequential, deterministic, perfect-information game. This includes many popular games such as chess, go, xiangqi, othello, connect four and tic-tac-toe.

use std::ops;
use std::fmt;
use self::Color::*;
use std::hash;


/// Represents a player's color
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Color {
    White = 0,
    Black = 1,
}

impl ops::Not for Color {
    type Output = Color;

    fn not(self) -> Self {
        match self {
            White => Black,
            Black => White,
        }
    }
} 

impl fmt::Display for Color {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.write_str(match *self {
            White => ("White"),
            Black => ("Black"),
        })
    }
}

impl Color {
    /// Returns the color's discriminant. 0 for white, 1 for black
    /// # Examples
    /// ```rust
    /// assert_eq!(Color::White.disc(), 0);
    /// assert_eq!(Color::Black.disc(), 1);
    /// ```
    pub fn disc(self) -> usize {
        self as u16 as usize
    }

    /// Returns the color's multiplier. -1 for black, 1 for white.
    /// # Examples
    /// ```rust
    /// assert_eq!(Color::White.multiplier(), 1);
    /// assert_eq!(Color::Black.multiplier(), -1);
    /// ```
    pub fn multiplier(self) -> isize {
        self as u16 as isize * -2 + 1
    }
}

/// The result of a game after it has finished.
#[derive(PartialEq, Eq, Clone, Debug, Copy)]
pub enum GameResult {
    WhiteWin,
    BlackWin,
    Draw,
}

impl ops::Not for GameResult {
    type Output = Self;
    fn not(self) -> Self {
        match self {
            GameResult::WhiteWin => GameResult::BlackWin,
            GameResult::BlackWin => GameResult::WhiteWin,
            GameResult::Draw => GameResult::Draw,
        }
    }
}

/// The simplest abstract representation of a game board. Together, the provided methods encode all the rules of the game.
pub trait Board {
    /// The type for moves in the game.
    type Move: Eq + Clone + fmt::Debug;
    /// The type for a reverse move in the game.
    type ReverseMove;

    /// Returns the starting position for the game. This function always produces identical values.
    fn start_board() -> Self;

    /// Returns the side to move for the current board.
    fn side_to_move(&self) -> Color;

    /// Generates all legal moves for the side to move, and appends them to the provided vector.
    fn generate_moves(&self, moves: &mut Vec<Self::Move>);

    /// Plays a move on the board. Also returns an ReverseMove do take the move back.
    ///
    /// Doing and then undoing a move always restores the board to exactly the same state.
    fn do_move(&mut self, mv: Self::Move) -> Self::ReverseMove;

    /// Reverse a move made by `do_move`.
    ///
    /// Doing and then undoing a move always restores the board to exactly the same state.
    fn reverse_move(&mut self, mv: Self::ReverseMove);

    /// Returns the result if the game is decided, otherwise returns None.
    /// If the winning player always plays the last move (as in chess), implementations are allowed
    /// to only return a win when the losing player is to move
    fn game_result(&self) -> Option<GameResult>;
}

/// A game board that also includes a heuristic static evaluation function.
/// Enables the use of many game-playing algorithms, such as minimax.
pub trait EvalBoard : Board + PartialEq + Clone {
    /// A fast, static evaluation of the current board position
    /// Returns a number between -100 and 100, where 0.0 is a draw, positive number means better for white, and negative number means better for black
    fn static_eval(&self) -> f32;
}

/// An extended game representation, which includes many additional methods to help game-playing algorithms search more effectively.
pub trait ExtendedBoard : EvalBoard {

    // A representation of the board that can be hashed. Can be Self, or unit if no hashing is desired.
    type HashBoard : hash::Hash + Eq;

    fn hash_board(&self) -> Self::HashBoard;

    /// Checks if a move is legal in the current position.
    /// Enables minimax algorithms to use the killer-move heuristic in their search.
    fn move_is_legal(&self, mv: Self::Move) -> bool {
        let mut moves = vec![];
        self.generate_moves(&mut moves);
        moves.contains(&mv)
    }

    /// Generates only the "active" moves in a position, and appends them to the provided vector. These are moves that radically change the static evaluation of a position, e.g. captures or promotions in chess.
    /// Search algorithms may recursively search all active moves, so eventually, no moves will be appended.
    /// Required for search algorithms to use quiescence search.
    fn active_moves(&self, moves: &mut Vec<Self::Move>);

    /// Returns an estimate for the average branch factor of the game.
    /// Helps search algorithms guide pruning and time management.
    const BRANCH_FACTOR: u64 = 20;
}
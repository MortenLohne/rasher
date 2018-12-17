use std::ops;
use std::fmt;
use self::Color::*;
use std::hash;

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
    pub fn disc(self) -> usize {
        self as u16 as usize
    }

    pub fn multiplier(self) -> isize {
        self as u16 as isize * -2 + 1
    }
}

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

pub trait Board {
    /// The type for moves in the game.
    type Move: Eq + Clone + fmt::Debug;
    /// The type for a reverse move in the game.
    type UndoMove;

    /// Returns the starting position for the game. This function always produces identical values.
    fn start_board() -> Self;

    /// Returns the side to move for the current board.
    fn side_to_move(&self) -> Color;

    /// Generates all legal moves for the side to move, and appends them to a provided vector.
    fn generate_moves(&self, moves: &mut Vec<Self::Move>);

    /// Plays a move on the board. Also returns an UndoMove do take the move back.
    ///
    /// Doing and then undoing a move always restores the board to exactly the same state.
    fn do_move(&mut self, mv: Self::Move) -> Self::UndoMove;

    /// Reverse a move made by `do_move`.
    ///
    /// Doing and then undoing a move always restores the board to exactly the same state.
    fn undo_move(&mut self, mv: Self::UndoMove);

    /// Returns the result if the game is decided, otherwise returns None.
    /// If the winning player always plays the last move (as in chess), implementations are allowed
    /// to only return a win when the losing player is to move
    fn game_result(&self) -> Option<GameResult>;
}

pub trait EvalBoard : Board + PartialEq + Clone {
    // A representation of the board that can be hashed. Can be Self, or unit if no hashing is desired.
    type HashBoard : hash::Hash + Eq;

    fn move_is_legal(&self, mv: Self::Move) -> bool {
        let mut moves = vec![];
        self.generate_moves(&mut moves);
        moves.contains(&mv)
    }

    fn active_moves(&self) -> Vec<Self::Move> {
        vec![]
    }
    
    fn static_eval(&self) -> f32;

    fn hash_board(&self) -> Self::HashBoard;
    
    const BRANCH_FACTOR: u64 = 20;
}

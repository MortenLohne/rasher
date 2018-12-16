use std::ops;
use std::fmt;
use self::Color::*;
use std::hash;

use rand;

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

pub trait EvalBoard : PartialEq + Clone {
    type Move : Clone + fmt::Debug + PartialEq + Eq;
    type UndoMove : Clone + fmt::Debug;
    // A representation of the board that can be hashed. Can be Self, or unit if no hashing is desired.
    type HashBoard : hash::Hash + Eq;

    /// Returns whose turn it is
    fn side_to_move(&self) -> Color;

    fn do_move(&mut self, mv: Self::Move) -> Self::UndoMove;
    
    fn undo_move(&mut self, mv: Self::UndoMove);

    fn start_board() -> Self;

    fn generate_moves(&self, moves: &mut Vec<Self::Move>);

    fn move_is_legal(&self, mv: Self::Move) -> bool {
        let mut moves = vec![];
        self.generate_moves(&mut moves);
        moves.contains(&mv)
    }

    fn active_moves(&self) -> Vec<Self::Move> {
        vec![]
    }

    /// Returns the result if the game is decided, otherwise returns None.
    /// This function should return quickly if the game is not decided yet.
    /// If the game is over, it must be the losing player's turn,
    /// otherwise the function may return anything
    fn game_result(&self) -> Option<GameResult>;
    
    fn static_eval(&self) -> f32;

    fn hash_board(&self) -> Self::HashBoard;
    
    fn do_random_move<R: rand::Rng>(&mut self, rng: &mut R) {
        let mut moves = vec![];
        self.generate_moves(&mut moves);
        self.do_move(moves[rng.gen_range(0, moves.len())].clone());
    }
    
    const BRANCH_FACTOR: u64 = 20;
}

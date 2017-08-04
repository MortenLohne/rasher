use std::ops;
use std::fmt;
use self::Color::*;
use rand;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Color {
    White,
    Black,
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

    /// Returns whose turn it is
    fn to_move(&self) -> Color;

    fn do_move(&mut self, Self::Move) -> Self::UndoMove;
    fn undo_move(&mut self, Self::UndoMove);

    fn start_board() -> Self;

    fn all_legal_moves(&self) -> Vec<Self::Move>;

    fn active_moves(&self) -> Vec<Self::Move> {
        vec![]
    }

    /// Returns the result if the game is decided, otherwise returns None.
    /// This function should return quickly if the game is not decided yet.
    /// If the game is over, it must be the losing player's turn,
    /// otherwise the function may return anything
    fn game_result(&self) -> Option<GameResult>;
    
    fn eval_board(&self) -> f32;

    fn do_random_move<R: rand::Rng>(&mut self, rng: &mut R) {
        let moves = self.all_legal_moves();
        self.do_move(moves[rng.gen_range(0, moves.len())].clone());
    }
    
    fn branch_factor() -> u64 {
        20
    }
}

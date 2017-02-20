use search_algorithms::game_move;
use std::ops;
use std::fmt;
use self::Color::*;
use rand;
use rand::Rng;

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
        let _ = fmt.write_str( match self {
            &White => ("White"),
            &Black => ("Black"),
        });
        
        Ok(())   
    }
}
#[derive(PartialEq, Eq, Clone, Debug, Copy)]
pub enum GameResult {
    WhiteWin,
    BlackWin,
    Draw,
}

pub trait EvalBoard : PartialEq + Clone {
    type Move : game_move::Move + Clone;
    type UndoMove : Clone;

    /// Returns whose turn it is
    fn to_move(&self) -> Color;

    fn do_move(&mut self, Self::Move) -> Self::UndoMove;
    fn undo_move(&mut self, Self::UndoMove);

    fn start_board() -> &'static Self;

    fn all_legal_moves(&self) -> Vec<Self::Move>;

    /// Returns the result if the game is decided, otherwise returns None.
    /// This function should return quickly if the game is not decided yet.
    fn game_result(&self) -> Option<GameResult>;
    
    fn eval_board(&self) -> f32;

    fn do_random_move<R: rand::Rng>(&mut self, rng: &mut R) {
        let moves = self.all_legal_moves();
        self.do_move(moves[rng.gen_range(0, moves.len())].clone());
    }
}

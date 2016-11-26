use board::game_move;
use std::ops;
use std::fmt;
use self::Color::*;

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

pub trait Board : PartialEq + Clone + Send {
    type Move : game_move::Move + PartialEq + Eq + Clone;
    type UndoMove : PartialEq + Eq + Clone;

    fn to_move(&self) -> Color;

    fn do_move(&mut self, Self::Move) -> Self::UndoMove;
    fn undo_move(&mut self, Self::UndoMove);

    fn start_board() -> &'static Self;

    fn all_legal_moves(&self) -> Vec<Self::Move>;

    /// Returns a score which is either mate or stalemate. ASSUMES all_legal_moves.len() == 0
    fn is_mate_or_stalemate(&self) -> ::Score;

    fn score_board(&self) -> ::Score;
}

use board::std_move::{ChessMove, ChessUndoMove};
use board::std_board::Square;
use board::std_board::PieceType;
use board::sjadam_board::SjadamBoard;

use search_algorithms::board::Color::*;
use search_algorithms::board::EvalBoard;
use search_algorithms::game_move::Move;

use std::fmt;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SjadamUndoMove {
    pub from: Square,
    pub sjadam_square: Square,
    pub to: Square,
    pub piece_moved: PieceType,
    pub capture: PieceType,
    pub old_castling_en_passant: u8, // Not used if chess_move is present
    pub old_half_move_clock: u8, // Not used if chess_move is present
}

impl SjadamUndoMove {
    /// Extract the underlying chess undo move
    /// Assumes the move has not been undone on the board
    pub fn chess_move(&self, board: &SjadamBoard) -> Option<ChessUndoMove> {
        if self.sjadam_square == self.to {
            None
        }
        else {
            Some(ChessUndoMove {
                from: self.sjadam_square, to: self.to, capture: self.capture,
                prom: self.piece_moved == PieceType::Pawn && {
                    let (_, rank) = self.to.file_rank();
                    board.to_move() == Black && rank == 7
                        || board.to_move() == White && rank == 0
                } ,
                old_castling_en_passant: self.old_castling_en_passant,
                old_half_move_clock: self.old_half_move_clock
            })
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct SjadamMove {
    pub from: Square,
    pub sjadam_square: Square,
    pub to: Square,
}

impl SjadamMove {
    /// Creates a Sjadam Move with only a regular move
    pub fn from_chess_move(mv: &ChessMove) -> Self {
        SjadamMove { from: mv.from, sjadam_square: mv.from, to: mv.to}
    }

    pub fn from_sjadam_move(from: Square, to: Square) -> Self {
        SjadamMove { from: from, sjadam_square: to, to: to }
    }

    pub fn from_to_squares(&self) -> (Square, Square) {
        (self.from, self.to)
    }

    pub fn chess_move(&self, board: &SjadamBoard) -> Option<ChessMove> {
        if self.sjadam_square == self.to {
            None
        }
        else {
            if (board.base_board.to_move() == White && self.sjadam_square.0 < 8
                && board.base_board[self.sjadam_square].is_empty())
                || (board.base_board.to_move() == Black && self.sjadam_square.0 >= 56
                && board.base_board[self.sjadam_square].is_empty())
            {
                Some(ChessMove::new_prom(self.sjadam_square, self.to, PieceType::Queen))
            }
            else {
                Some(ChessMove::new(self.sjadam_square, self.to))
            }
        }
    }
}

impl Move for SjadamMove {
    fn from_alg(input: &str) -> Result<Self, String> {
        let mut chars = input.chars().peekable();
        if input.len() < 5 {
            return Err(format!("Move \"{}\" was too short to parse", input))
        }
        if *chars.peek().unwrap() == '-' {
            chars.next().unwrap();
            let chess_move = ChessMove::from_alg(&chars.collect::<String>())?;
            return Ok(SjadamMove::from_chess_move(&chess_move))
        }
        let alg : String = chars.by_ref().collect();
        let from = Square::from_alg(&alg[0..2]).ok_or("Illegal square")?;
        let sjadam = Square::from_alg(&alg[2..4]).ok_or("Illegal square")?;
        if alg.chars().last().unwrap() == '-' {
            Ok(SjadamMove::from_sjadam_move(from, sjadam))
        }
        else {
            let to = Square::from_alg(&alg[4..]).ok_or("Illegal square")?;
            Ok(SjadamMove { from: from, sjadam_square: sjadam, to: to })
        }
    }
    fn to_alg(&self) -> String {
        self.to_string()
    }
}

impl fmt::Debug for SjadamMove {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}{}",
               if self.from == self.sjadam_square {
                   "-".to_string()
               }
               else {
                    self.from.to_string()
               },
               self.sjadam_square,
               if self.sjadam_square == self.to {
                   "-".to_string()
               }
               else {
                   self.to.to_string()
               })
    }
}

impl fmt::Display for SjadamMove {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

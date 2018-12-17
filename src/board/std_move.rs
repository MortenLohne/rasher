use board::std_board::*;
use uci::UciBoard;
use search_algorithms::board::Board;

use std::fmt;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct ChessReverseMove {
    pub from : Square,
    pub to : Square,
    pub capture : PieceType,
    pub prom : bool,
    pub old_castling_en_passant : u8,
    pub old_half_move_clock : u8,
}

impl ChessReverseMove {
    /// Returns the corresponding undo move for a move
    /// Must be called before the move was done on the board
    pub fn from_move(c_move: ChessMove, board: &ChessBoard) -> ChessReverseMove {
        ChessReverseMove { from: c_move.from, to: c_move.to, capture: board[c_move.to].piece_type(),
                        prom: c_move.prom.is_some(),
                        old_castling_en_passant: board.castling_en_passant,
                        old_half_move_clock: board.half_move_clock
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ChessMove {
    pub from : Square,
    pub to : Square,
    pub prom : Option<PieceType>,
}

impl fmt::Display for ChessMove {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.write_str(&format!("{}", ChessBoard::start_board().to_alg(self))).unwrap();
        Ok(())
    }
}

impl fmt::Debug for ChessMove {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Display::fmt(self, fmt)
    }
}

impl ChessMove {
    pub fn new(from : Square, to : Square) -> ChessMove {
        ChessMove { from: from, to: to, prom: None }
    }
    
    pub fn new_prom(from : Square, to : Square, prom : PieceType) -> ChessMove {
        ChessMove { from: from, to: to, prom: Some(prom) }
    }
}

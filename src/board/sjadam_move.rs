use board::std_board::Square;
use board::std_board::{Piece, PieceType};
use board::sjadam_board::SjadamBoard;

use std::fmt;
use search_algorithms::board::Board;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SjadamReverseMove {
    pub from: Square,
    pub to: Square,
    pub en_passant: bool,
    pub castling: bool,
    pub piece_moved: PieceType,
    pub old_last_move: Option<SjadamMove>,
    pub capture: PieceType,
    pub old_castling_en_passant: u8,
    pub old_half_move_clock: u8,
    pub old_repetitions: u8,
    pub old_hash: u64,
}

impl SjadamReverseMove {
    pub fn from(&self) -> Square {
        self.from
    }
    pub fn to(&self) -> Square {
        self.to
    }
    pub fn castling(&self) -> bool {
        self.castling
    }
    pub fn en_passant(&self) -> bool {
        self.en_passant
    }
}

#[derive(PartialEq, Eq, Clone, Ord, PartialOrd)]
pub struct SjadamMove {
    from: Square,
    to: Square,
    piece_moved: PieceType,
    castling: bool,
}

impl SjadamMove {
    pub fn new(from: Square, to: Square, castling: bool, piece_moved: PieceType) -> Self {
        Self { from: from, to: to, castling: castling, piece_moved: piece_moved }
    }
    
    pub fn from_to_squares(&self) -> (Square, Square) {
        (self.from, self.to)
    }

    pub fn en_passant_bitboard(&self, board: &SjadamBoard) -> bool {
         (self.from().file() as i8 - self.to().file() as i8).abs() % 2 == 1
            && !board.all_pieces().get(self.to)
            && board.piece_at_square(Piece::new(PieceType::Pawn, board.side_to_move()), self.from)
    }

    pub fn castling(&self) -> bool {
        self.castling
    }
    pub fn from(&self) -> Square {
        self.from
    }
    pub fn to(&self) -> Square {
        self.to
    }
    pub fn piece_moved(&self) -> PieceType {
        self.piece_moved
    }
}

impl fmt::Debug for SjadamMove {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (from, to) = self.from_to_squares();
        write!(f, "{}{}{}",
               from.to_string(),
               to.to_string(),
               if self.castling { "c" } else { "" }
        )
    }
}

impl fmt::Display for SjadamMove {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct SjadamReverseNullMove {
    pub old_last_move: Option<SjadamMove>,
    pub old_castling_en_passant: u8,
    pub old_hash: u64,
}
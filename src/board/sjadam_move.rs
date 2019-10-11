use chess_bitboard::bitboard::Square;
use board::chess::board::{Piece, PieceType};
use board::sjadam_board::SjadamBoard;

use std::fmt;
use board_game_traits::board::Board;

#[derive(Serialize, Deserialize)]
#[serde(remote = "Square")]
struct SquareRef(u8);

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct SjadamReverseMove {
    #[serde(with = "SquareRef")]
    pub from: Square,
    #[serde(with = "SquareRef")]
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

#[derive(PartialEq, Eq, Clone, Ord, PartialOrd, Serialize, Deserialize)]
pub struct SjadamMove {
    #[serde(with = "SquareRef")]
    from: Square,
    #[serde(with = "SquareRef")]
    to: Square,
    piece_moved: PieceType,
    castling: bool,
}

impl SjadamMove {
    pub fn new(from: Square, to: Square, castling: bool, piece_moved: PieceType) -> Self {
        Self { from, to, castling, piece_moved }
    }
    
    pub fn from_to_squares(&self) -> (Square, Square) {
        (self.from, self.to)
    }

    pub fn is_en_passant_move(&self, board: &SjadamBoard) -> bool {
         (self.from().file() as i8 - self.to().file() as i8).abs() % 2 == 1
            && !board.all_pieces().get(self.to)
            && board.piece_at_square(Piece::from_type_color(PieceType::Pawn, board.side_to_move()), self.from)
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
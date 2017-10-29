use board::std_board::Square;
use board::std_board::PieceType;
use board::sjadam_board::SjadamBoard;

use search_algorithms::game_move::Move;

use std::fmt;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SjadamUndoMove {
    pub from: Square,
    pub to: Square,
    pub en_passant: bool,
    pub castling: bool,
    pub piece_moved: PieceType,
    pub capture: PieceType,
    pub old_castling_en_passant: u8,
    pub old_half_move_clock: u8,
}

impl SjadamUndoMove {
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

#[derive(PartialEq, Eq, Clone)]
pub struct SjadamMove {
    from: Square,
    to: Square,
    castling: bool,
}

impl SjadamMove {
    pub fn new(from: Square, to: Square, castling: bool) -> Self {
        Self { from: from, to: to, castling: castling }
    }
    
    pub fn from_to_squares(&self) -> (Square, Square) {
        (self.from, self.to)
    }
    
    pub fn en_passant(&self, board: &SjadamBoard) -> bool {
        (self.from().file() as i8 - self.to().file() as i8).abs() % 2 == 1
            && board.base_board[self.to].is_empty()
            && board.base_board[self.from].piece_type() == PieceType::Pawn
    }

    pub fn castling(&self) -> bool {
        self.castling
    }
    pub fn from(&self) -> Square {
        self.from_to_squares().0
    }
    pub fn to(&self) -> Square {
        self.from_to_squares().1
    }
}

impl Move for SjadamMove {
    fn from_alg(input: &str) -> Result<Self, String> {
        if input.len() < 4 {
            return Err(format!("Move \"{}\" was too short to parse", input))
        }
        if input.len() > 5 {
            return Err(format!("Move \"{}\" was too long to parse", input))
        }
        let from = Square::from_alg(&input[0..2]).ok_or("Illegal square")?;
        let to = Square::from_alg(&input[2..4]).ok_or("Illegal square")?;
        match input.len() {
            4 => Ok(Self::new(from, to, false)),
            5 if input.as_bytes()[4] == 'c' as u8 => Ok(Self::new(from, to, true)),
            _ => Err(format!("Couldn't parse move {}", input))
        }
    }
    fn to_alg(&self) -> String {
        self.to_string()
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

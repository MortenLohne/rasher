use board::std_board::Square;
use board::std_board::PieceType;
use board::sjadam_board::SjadamBoard;

use search_algorithms::game_move::Move;

use std::fmt;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SjadamUndoMove {
    pub from: Square,
    // pub sjadam_square: Square,
    pub to: Square,
    pub en_passant: bool,
    pub castling: bool,
    pub piece_moved: PieceType,
    pub capture: PieceType,
    pub old_castling_en_passant: u8, // Not used if chess_move is present
    pub old_half_move_clock: u8, // Not used if chess_move is present
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
    pub fn from_sjadam_move(from: Square, to: Square) -> Self {
        Self::new(from, to, false)
    }
    pub fn from_to_squares(&self) -> (Square, Square) {
        (self.from, self.to)
    }
    pub fn en_passant(&self, board: &SjadamBoard) -> bool {
        (self.from().file() - self.from().file()) / 2 == 1 && board.base_board[self.to].is_empty()
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
            4 => Ok(Self::new(from, to, false)), //Ok(SjadamMove::Normal(from, to)),
            5 if input.as_bytes()[4] == 'c' as u8 => {
                /*
                if from == Square::E1 {
                    if to == Square::G1 {
                        Ok(SjadamMove::ShortWhite)
                    }
                    else if from == Square::C1 {
                        Ok(SjadamMove::LongWhite)
                    }
                    else {
                        Err(format!("Illegal castling in {}", input))
                    }
                }
                else if from == Square::E8 {
                    if to == Square::G8 {
                        Ok(SjadamMove::ShortBlack)
                    }
                    else if from == Square::C8 {
                        Ok(SjadamMove::LongBlack)
                    }
                    else {
                        Err(format!("Illegal castling in {}", input))
                    }
                }
                else {
                    Err(format!("Illegal castling in {}", input))
                }
                 */
                Ok(Self::new(from, to, true))
            },
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
/*
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
*/

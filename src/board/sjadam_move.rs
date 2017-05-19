use board::std_move::ChessMove;
use board::std_board::Square;
use board::sjadam_board::SjadamBoard;

use search_algorithms::game_move::Move;

use std::fmt;

#[derive(Clone)]
pub struct SjadamMove {
    pub old_castling_en_passant: u8, // Not used if chess_move is present
    pub old_half_move_clock: u8, // Not used if chess_move is present
    pub checkers: Option<(Square, Square)>, // All checkers moves are done in one go
    pub chess_move: Option<ChessMove>,
}

impl SjadamMove {
    // Creates a Sjadam Move with only a regular move
    pub fn from_chess_move(mv: &ChessMove) -> Self {
        SjadamMove { old_castling_en_passant: mv.old_castling_en_passant,
                     old_half_move_clock: mv.old_half_move_clock,
                     checkers: None, chess_move: Some(mv.clone()) }
    }

    pub fn from_sjadam_move(from: Square, to: Square, board: &SjadamBoard) -> Self {
        SjadamMove { checkers: Some((from, to)),
                     chess_move: None,
                     old_castling_en_passant: board.base_board.castling_en_passant,
                     old_half_move_clock: board.base_board.half_move_clock }
    }

    pub fn from_to_squares(&self) -> (Square, Square) {
        match (self.checkers, self.chess_move) {
            (Some((from, _)), Some(mv)) => (from, mv.to),
            (Some((from, to)), None) => (from, to),
            (None, Some(mv)) => (mv.from, mv.to),
            (None, None) => panic!("Cannot have empty move"),
        }
    }
}

impl PartialEq for SjadamMove {
    fn eq(&self, other: &Self) -> bool {
        self.chess_move == other.chess_move && self.checkers == other.checkers
    }
}

impl Eq for SjadamMove {}

impl Move for SjadamMove {
    fn from_alg(input: &str) -> Result<Self, String> {
        let mut chars = input.chars().peekable();
        
        if *try!(chars.peek().ok_or("Empty move notation")) == '-' {
            chars.next().unwrap();
            let chess_move = try!(ChessMove::from_alg(&chars.collect::<String>()));
            Ok(SjadamMove::from_chess_move(&chess_move))
        }
        else {
            let alg : String = chars.collect();
            if alg.len() < 5 {
                return Err(format!("Notation too short, couldn't parse: {}.", input))
            }
            let from = try!(Square::from_alg(&alg[0..2]).ok_or("Illegal square"));
            let to = try!(Square::from_alg(&alg[2..4]).ok_or("Illegal square"));
            if alg.chars().nth(4).unwrap() == '-' {
                Ok(SjadamMove { checkers: Some((from, to)), chess_move: None,
                                old_castling_en_passant: 0, old_half_move_clock: 0 })
            }
            else {
                Ok(SjadamMove { checkers: Some((from, to)),
                                chess_move: Some(try!(ChessMove::from_alg(&alg[4..]))),
                                old_castling_en_passant: 0, old_half_move_clock: 0 })
            }
        }
    }
    fn to_alg(&self) -> String {
        self.to_string()
    }
}

impl fmt::Debug for SjadamMove {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}",
               self.checkers.map_or("-".to_string(), |(from, to)| format!("{}{}", from, to)),
               self.chess_move.map_or("-".to_string(), |mv| mv.to_string()))
    }
}

impl fmt::Display for SjadamMove {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

use board::std_board::PieceType::*;
use search_algorithms::board::Color::*;
use board::std_board::*;
use search_algorithms::game_move::Move;

use std::fmt;
use std::cmp;

#[derive(Clone, Copy)]
pub struct ChessMove {
    pub from : Square,
    pub to : Square,
    pub prom : Option<Piece>,
    pub capture : PieceType,
    pub old_castling_en_passant : u8,
    pub old_half_move_clock : u8,
}

impl fmt::Display for ChessMove {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.write_str(&format!("{}", self.to_alg())).unwrap();
        Ok(())
    }
}

impl fmt::Debug for ChessMove {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.write_str(&format!("{}", self.to_alg())).unwrap();
        Ok(())
    }
}

impl cmp::PartialEq for ChessMove {
    fn eq(&self, other: &Self) -> bool {
        self.from == other.from && self.to == other.to && self.prom == other.prom
    }
}
impl cmp::Eq for ChessMove { }

impl ChessMove {
    pub fn new(board: &ChessBoard, from : Square, to : Square) -> ChessMove {
        let capture = board.piece_at(to).0;
        let old_castling_en_passant = board.castling_en_passant;

        ChessMove { from: from, to: to, prom: None, capture: capture, 
               old_castling_en_passant: old_castling_en_passant, 
               old_half_move_clock: board.half_move_clock }
    }
    
    pub fn new_prom(board: &ChessBoard, from : Square, to : Square, prom : Piece) -> ChessMove {
        let capture = board.piece_at(to).0;
        let old_castling_en_passant = board.castling_en_passant;

        ChessMove { from: from, to: to, prom: Some(prom), capture: capture, 
               old_castling_en_passant: old_castling_en_passant, 
               old_half_move_clock: board.half_move_clock }
    }
}
impl Move for ChessMove {
    fn to_alg(&self) -> String {
        let (file_from, rank_from) = self.from.file_rank();
        let (file_to, rank_to) = self.to.file_rank();
        let mut s : String = "".to_string();
        s.push_str(&format!("{}{}{}{}", (file_from + 'a' as u8) as char,
                            (8 - rank_from + '0' as u8) as char,
                            (file_to + 'a' as u8) as char, (8 - rank_to + '0' as u8) as char));
        match self.prom {
            Some(Piece(Queen, _)) => s.push('q'),
            Some(Piece(Rook, _)) => s.push('r'),
            Some(Piece(Knight, _)) => s.push('k'),
            Some(Piece(Bishop, _)) => s.push('b'),
            None => (),
            _ => panic!("Illegal promotion move"),
        }
        s
    }

    // Parse a ChessMove from short algebraic notation (e2e4, g2g1Q, etc)
    fn from_alg(alg : &str) -> Result<Self, String> {
        // Some GUIs send moves as "e2-e4" instead of "e2e4".
        // In that case, remove the dash and try again
        if alg.chars().nth(2) == Some('-') {
            let mut fixed_alg = alg.to_string();
            fixed_alg.remove(2);
            Self::from_alg(&fixed_alg)
        }
        else if alg.len() == 4 || alg.len() == 5 {
            let from = Square::from_alg(&alg[0..2]).unwrap_or(Square(0));
            let to = Square::from_alg(&alg[2..4]).unwrap_or(Square(0));
            if alg.len() == 4 {
                Ok(ChessMove { from: from, to: to, prom: None, capture: Empty, 
                               old_castling_en_passant: 0, old_half_move_clock: 0 })
            }
            else {
                Ok(ChessMove { from: from, to: to, prom: Some(
                    match alg.chars().nth(4) {
                        Some('Q') => Piece(Queen, White),
                        Some('q') => Piece(Queen, Black),
                        Some('R') => Piece(Rook, White),
                        Some('r') => Piece(Rook, Black),
                        Some('N') => Piece(Knight, White),
                        Some('n') => Piece(Knight, Black),
                        Some('B') => Piece(Bishop, White),
                        Some('b') => Piece(Bishop, Black),
                        Some(ch) => return Err(format!("Bad promotion letter {} in move {}", ch, alg)),
                        None => return Err(format!("No promotion letter in move {}", alg)),
                    }), 
                               capture: Empty, 
                               old_castling_en_passant: 0, old_half_move_clock: 0 })
            }
        }
        
        else {
            Err(format!("Move {} had incorrect length: Found {}, expected 4/5", alg, alg.len()))
        }
    }
}

use board::std_board::PieceType::*;
use board::std_board::Color::*;
use board::std_board::*;

use std::fmt;

type Board = ChessBoard;

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Move {
    pub from : Square,
    pub to : Square,
    pub prom : Option<Piece>,
    pub capture : PieceType,
    pub old_castling_en_passant : u8,
    pub old_half_move_clock : u8,
}

impl fmt::Display for Move {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.write_str(&format!("{}", self.to_alg())).unwrap();
        Ok(())
    }
}

impl fmt::Debug for Move {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.write_str(&format!("{}", self.to_alg())).unwrap();
        Ok(())
    }
}

impl Move {
    pub fn new(board: &Board, from : Square, to : Square) -> Move {
        let capture = board.piece_at(to).0;
        let old_castling_en_passant = board.castling_en_passant;

        Move { from: from, to: to, prom: None, capture: capture, 
               old_castling_en_passant: old_castling_en_passant, 
               old_half_move_clock: board.half_move_clock }
    }

    pub fn simple_eq(&self, other: Move) -> bool {
        self.from == other.from && self.to == other.to
    }
    
    pub fn new_prom(board: &Board, from : Square, to : Square, prom : Piece) -> Move {
        let capture = board.piece_at(to).0;
        let old_castling_en_passant = board.castling_en_passant;

        Move { from: from, to: to, prom: Some(prom), capture: capture, 
               old_castling_en_passant: old_castling_en_passant, 
               old_half_move_clock: board.half_move_clock }
    }
    pub fn to_alg(&self) -> String {
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
    
    pub fn from_alg(alg : &str) -> Result<Self, String> {
        if alg.len() == 5 || alg.len() == 6 {
            if alg.chars().nth(2).unwrap() == '-' {
                let from = Square::from_alg(&alg[0..2]).unwrap_or(Square(0));
                let to = Square::from_alg(&alg[3..5]).unwrap_or(Square(0));
                if alg.len() == 5 {
                    Ok(Move { from: from, to: to, prom: None, capture: Empty, 
                              old_castling_en_passant: 0, old_half_move_clock: 0 })
                }
                else {
                    Ok(Move { from: from, to: to, prom: Some(
                              match alg.chars().nth(5) {
                                  Some('Q') => Piece(Queen, White),
                                  Some('q') => Piece(Queen, Black),
                                  Some('R') => Piece(Rook, White),
                                  Some('r') => Piece(Rook, Black),
                                  Some('N') => Piece(Knight, White),
                                  Some('n') => Piece(Knight, Black),
                                  Some('B') => Piece(Bishop, White),
                                  Some('b') => Piece(Bishop, Black),
                                  _ => return Err("Bad promotion letter".to_string()),
                              }), 
                              capture: Empty, 
                              old_castling_en_passant: 0, old_half_move_clock: 0 })
                    }
            }
            else {
                Err(format!("Move {} had incorrect 3rd character '{}', expected '-'",
                            alg, alg.chars().nth(2).unwrap()))
            }
        }
        else {
            Err(format!("Move {} had incorrect length: Found {}, expected 5/6", alg, alg.len()))
        }
    }
    pub fn from_short_alg(alg : &str) -> Result<Self, String> {
        if alg.len() != 4 && alg.len() != 5 {
            Err(format!("Wrong move length: Expected 4/5, found {}", alg.len()).to_string())
        }
        else {
            let mut temp = alg.to_string();
            temp.insert(2, '-');
            Self::from_alg(&temp)
        }
    }
}

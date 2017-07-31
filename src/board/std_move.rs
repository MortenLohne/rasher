use board::std_board::PieceType::*;
use board::std_board::*;
use search_algorithms::game_move::Move;

use std::fmt;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct ChessUndoMove {
    pub from : Square,
    pub to : Square,
    pub capture : PieceType,
    pub prom : bool,
    pub old_castling_en_passant : u8,
    pub old_half_move_clock : u8,
}

impl ChessUndoMove {
    /// Returns the corresponding undo move for a move
    /// Must be called before the move was done on the board
    pub fn from_move(c_move: ChessMove, board: &ChessBoard) -> ChessUndoMove {
        ChessUndoMove { from: c_move.from, to: c_move.to, capture: board[c_move.to].piece_type(),
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

impl ChessMove {
    pub fn new(from : Square, to : Square) -> ChessMove {
        ChessMove { from: from, to: to, prom: None }
    }
    
    pub fn new_prom(from : Square, to : Square, prom : PieceType) -> ChessMove {
        ChessMove { from: from, to: to, prom: Some(prom) }
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
            Some(Queen) => s.push('q'),
            Some(Rook) => s.push('r'),
            Some(Knight) => s.push('n'),
            Some(Bishop) => s.push('b'),
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
                Ok(ChessMove { from: from, to: to, prom: None })
            }
            else {
                Ok(ChessMove { from: from, to: to, prom: Some(
                    match alg.chars().nth(4) {
                        Some('Q') => Queen,
                        Some('q') => Queen,
                        Some('R') => Rook,
                        Some('r') => Rook,
                        Some('N') => Knight,
                        Some('n') => Knight,
                        Some('B') => Bishop,
                        Some('b') => Bishop,
                        Some(ch) => return Err(format!("Bad promotion letter {} in move {}", ch, alg)),
                        None => return Err(format!("No promotion letter in move {}", alg)),
                    })
                })
            }
        }
        
        else {
            Err(format!("Move {} had incorrect length: Found {}, expected 4/5", alg, alg.len()))
        }
    }
}

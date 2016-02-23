use self::PieceType::*;
use self::Color::*;

use std;
use std::collections::HashMap;
use std::fmt::Error;
use std::fmt::Display;
use std::fmt::Formatter;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Color {
    White,
    Black,
}
impl Display for Color {
    fn fmt(&self, fmt : &mut Formatter) -> Result<(), Error> {
        let _ = fmt.write_str( match self {
            &White => ("White"),
            &Black => ("Black"),
        });
        
        Ok(())   
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
    Empty,
}

impl PieceType {
    fn value(&self) -> f32 {
        match *self {
            Pawn => 1.0,
            Knight => 3.0,
            Bishop => 3.0,
            Rook => 5.0,
            Queen => 9.0,
            King => 0.0,
            Empty => 0.0,
        }
    }
}

impl Display for PieceType {
    fn fmt(&self, fmt : &mut Formatter) -> Result<(), Error> {
        let _ = fmt.write_str(match self {
            &Pawn => "Pawn",
            &Knight => "Knight",
            &Bishop => "Bishop",
            &Rook => "Rook",
            &Queen => "Queen",
            &King => "King",
            &Empty => "Empty square",
        }
                              );
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Piece (pub PieceType, pub Color);

impl Display for Piece {
    fn fmt(&self, fmt : &mut Formatter) -> Result<(), Error> {
        let _ = fmt.write_str(&format!("{}", PIECE_CHAR_MAP.get(self).unwrap()));
        Ok(())
    }
}

impl Piece {
    #[inline]
    pub fn value(&self) -> f32 {
        let &Piece(piece_type, piece_color) = self;
        if piece_color == White {
            piece_type.value()
        }
        else { piece_type.value() * -1.0 }
    }
    pub fn is_empty(&self) -> bool {
        self.0 == Empty
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Square(pub u8);

impl Display for Square {
    fn fmt(&self, fmt : &mut Formatter) -> Result<(), Error> {
        let (file, rank) = self.file_rank();
        
        let _ = fmt.write_str(&format!("{}{}", (file + 'a' as u8) as char, (rank + '0' as u8) as char));
        Ok(())   
    }
}

impl Square {
    pub fn from_alg (alg : &str) -> Option<Self> {
        if alg.len() != 2 { None }
        else {
            let (file, rank) = (alg.chars().nth(0).unwrap(), alg.chars().nth(1).unwrap());
            let (file_u8, rank_u8) = (file as u8 - 'a' as u8,
                                      8 - (rank as u8 - '0' as u8));

            let square = rank_u8 * 8 + file_u8;
            
            // println!("file_u8: {}, rank_u8: {}: {} = {}", file_u8, rank_u8, alg, square);
            if square > 64 { None } else { Some(Square(square)) }
        }
    }
    pub fn from_ints (file : u8, rank : u8) -> Self {
        debug_assert!(file < 8 && rank < 8);
        Square((rank << 3) | file)
    }
    pub fn file_rank (&self) -> (u8, u8) {
        (self.0 & 0b0000_0111, self.0 >> 3)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Move {
    pub from : Square,
    pub to : Square,
    pub prom : Option<Piece>,
}
impl Move {
    pub fn new(from : Square, to : Square) -> Move {
        Move { from: from, to: to, prom: None }
    }
    pub fn new_prom(from : Square, to : Square, prom : Piece) -> Move {
        Move { from: from, to: to, prom: Some(prom) }
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
                    Ok(Move::new(from, to))
                }
                else {
                    Ok(Move::new_prom(from, to,
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
                                      } ))
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

impl Display for Move {
    fn fmt(&self, fmt : &mut Formatter) -> Result<(), Error> {
        let _ = fmt.write_str(&format!("{}-{}", self.from, self.to));
        Ok(())   
    }
}

// use std::hash::{Hash, Hasher, SipHasher};

#[derive(Clone, Eq, Debug, PartialEq)]
pub struct Board {
    pub board : [[Piece; 8]; 8],
    pub to_move : Color,
    pub castling : [bool; 4],
    pub en_passant : Option<Square>,
    pub half_move_clock : u16,
    pub move_num : u16,
    pub moves : Vec<Move>,
    //pub hash : Option<Hasher>,
}

/*impl Hash for Board {
    fn hash<H: Hasher>(&self, state: &mut H) {
        
    }
}*/

impl Display for Board {
    fn fmt(&self, fmt : &mut Formatter) -> Result<(), Error> {
        for rank in self.board.iter() {
            for piece in rank.iter() {
                let _ = fmt.write_str(&format!("[{}]", piece));
            }
            let _ = fmt.write_str("\n");
        }
        for c_move in &self.moves {
            let _ = fmt.write_str(&format!("[{}],", &c_move));
        }
        Ok(())   
    }
}
impl Board {
    #[inline]
    pub fn piece_at(&self, square : Square) -> Piece {
        let Square(i) = square;
        debug_assert!(i < 64, format!("Tried to find piece at pos {}!", i));
        self.board[i as usize >> 3][i as usize & 0b0000_0111]
    }
    /// Returns a clone of the board, viewed from the other side.
    /// This screws up everything related to pawn movement, castling, etc,
    /// and should only be used to print the board as seen from the black side
    #[allow(dead_code)]
    pub fn upside_down(&self) -> Board {
        let mut new_board = self.clone();
        for i in 0..8 {
            new_board.board[i] = self.board[7-i].clone();
            for j in 0..8 {
                new_board.board[i][j] = self.board[7-i][7-j];
            }
        }
        new_board
    }

    pub fn king_pos(&self) -> Square {
        self.pos_of(Piece(King, self.to_move)).unwrap()
    }
    
    pub fn pos_of(&self, piece : Piece) -> Option<Square> {
        for i in 0..64 {
            if self.piece_at(Square(i)) == piece {
                return Some(Square(i));
            }
        }
        None
    }
    
    // Creates a new board from a given FEN string
    pub fn from_fen (fen : &str) -> Result<Board, String> {
        let mut board = [[Piece(Empty, White); 8]; 8];
        let fen_split : Vec<&str> = fen.split(" ").collect();
        if fen_split.len() != 6 {
            return Err(format!("Invalid FEN string \"{}\": Had {} fields instead of 6",
                               fen, fen_split.len()));
        }
        let ranks : Vec<&str> = fen_split[0].split("/").collect();
        if ranks.len() != 8 {
            return Err(format!("Invalid FEN string \"{}\": Had {} ranks instead of 8",
                               fen, ranks.len()));
        }
        for i in 0..8 {
            let mut cur_rank : Vec<Piece> = Vec::new();
            for c in ranks[i].chars() {
                match CHAR_PIECE_MAP.get(&c) {
                    Some(piece) => cur_rank.push(*piece),
                    None => match c.to_digit(10) {
                        Some(mut i) => {
                            while i > 0 {
                                cur_rank.push(Piece(Empty, White));
                                i -= 1;
                            }
                        },
                        None => return Err(format!("Invalid FEN string: Illegal character {}", c)),
                    },
                }   
            }
            if cur_rank.len() != 8 {
                return Err(format!("Invalid FEN string: Specified {} pieces on rank {}.",
                                   cur_rank.len(), i))
            }
            else {
                for j in 0..8 {
                    board[i][j] = *cur_rank.get(j).unwrap();
                }
            }
        }
        if fen_split[1].len() != 1 {
            return Err("Invalid FEN string: Error in side to move-field".to_string());
        }
        let char_to_move = fen_split[1].chars().collect::<Vec<_>>()[0];
        let to_move = if char_to_move == 'w' { White }
        else if char_to_move == 'b' { Black }
        else { return Err("Invalid FEN string: Error in side to move-field".to_string()) };

        let mut castling_rights = [false; 4];
        for c in fen_split[2].chars() {
            match c {
                '-' => break,
                'K' => castling_rights[0] = true,
                'Q' => castling_rights[1] = true,
                'k' => castling_rights[2] = true,
                'q' => castling_rights[3] = true,
                _ => return Err("Invalid FEN string: Error in castling field.".to_string()),
            }
        }
        let en_passant = if fen_split[3] == "-" { None }
        else {
            match Square::from_alg(fen_split[3]) {
                Some(square) => Some(square),
                None => return Err(format!("Invalid en passant square {}.", fen_split[3])),
            }
        };

        let (half_clock, move_num) : (u16, u16) =
            match ((fen_split[4]).parse(), (fen_split[5]).parse()) {
                (Ok(n1), Ok(n2)) => (n1, n2),
                _ => return Err("Invalid halfmove clock or move num".to_string()),
            };
        
        Ok(Board{ board: board, to_move: to_move, castling: castling_rights,
                         en_passant: en_passant, half_move_clock: half_clock,
                         move_num: move_num, moves: vec![]})
    }
    pub fn do_move(&self, c_move : Move) -> Board {
        let mut new_board = self.clone();
        new_board.moves.push(c_move);
        // Increment or reset the half-move clock
        match (new_board.piece_at(c_move.from).0, new_board.piece_at(c_move.to).0) {
            (Pawn, _) => new_board.half_move_clock = 0,
            (_, Empty) => new_board.half_move_clock += 1,
            (_, _) => new_board.half_move_clock = 0,
        }

        // Destruct into helper variables
        let (file_from, rank_from) = c_move.from.file_rank();
        let (file_to, rank_to) = c_move.to.file_rank();
        let color = new_board.to_move;
        
        // Do special stuff if castling
        // This method just does the move, it does not keep track of
        // en passant squares or castling ability. It will castle regardless of whether
        // it is legal to do so
        if new_board.piece_at(c_move.from).0 == King &&
            (file_from as i8 - file_to as i8).abs() == 2 {

                // Simple helper closure that moves a piece, emptying the square it came from
                // Checks nothing, not even if the square has a piece. Use carefully.
                let mut do_simple_move = |f_from, r_from, f_to, r_to| {
                    new_board.board[r_to as usize][f_to as usize]
                        = new_board.board[r_from as usize][f_from as usize];
                    new_board.board[r_from as usize][f_from as usize] = Piece(Empty, White);
                };
                // Assume castling is legal, and move the king and rook to where they should go
                match (color, file_to) {
                    (White, 2) => { do_simple_move(4, 7, 2, 7);
                                    do_simple_move(0, 7, 3, 7);
                    },
                    (White, 6) => { do_simple_move(4, 7, 6, 7);
                                    do_simple_move(7, 7, 5, 7);
                    },
                    (Black, 2) => { do_simple_move(4, 0, 2, 0);
                                    do_simple_move(0, 0, 3, 0);
                    },
                    (Black, 6) => { do_simple_move(4, 0, 6, 0);
                                    do_simple_move(7, 0, 5, 0);
                    },
                    (_, _) => panic!(format!(
                        "Error: Tried to castle to the {}th file. ", file_to)),
                }
            }
        // If a pawn takes towards an empty square, assume it is doing a legal en passant capture
        else if new_board.piece_at(c_move.from).0 == Pawn && file_from != file_to &&
            new_board.piece_at(c_move.to) == Piece(Empty, White) {
                new_board.board[rank_to as usize][file_to as usize]
                    = new_board.board[rank_from as usize][file_from as usize];

                new_board.board[rank_from as usize][file_to as usize] = Piece(Empty, White);
                new_board.board[rank_from as usize][file_from as usize] = Piece(Empty, White);
                
                
            }
        // If it is not a special move
        else {
            // Does the move, depending on whether the move promotes or not
            match c_move.prom {
                Some(piece) => new_board.board[rank_to as usize][file_to as usize] = piece,
                None => new_board.board[rank_to as usize][file_to as usize]
                    = new_board.board[rank_from as usize][file_from as usize],
                
            }
            new_board.board[rank_from as usize][file_from as usize] = Piece(Empty, White);
        }
            
        // Remove any en passant square. If it was available to this player,
        // it has already been used. Any new en passant square is added below.

       new_board.en_passant = None;

        // If the pawn went two squares forward, add the square behind it as en passant square
        if new_board.piece_at(c_move.to).0 == Pawn &&
            (rank_from as i8 - rank_to as i8).abs() == 2 {
                new_board.en_passant = Some(Square::from_ints(file_from,
                                                         (rank_from + rank_to) / 2));
            }

        // Remove castling rights on king/rook moves
        // Does not check when rooks are captured, it is assumed that the
        // function looking for moves checks that rooks are present
        if new_board.piece_at(c_move.to).0 == King {
            if color == White {
                new_board.castling[0] = false;
                new_board.castling[1] = false;
            }
            else {
                new_board.castling[2] = false;
                new_board.castling[3] = false;
            }
        }
        // If a rook was moved, check if it came from a corner
        if new_board.piece_at(c_move.to).0 == Rook {
            match c_move.from {
                Square(0) => new_board.castling[3] = false,
                Square(7) => new_board.castling[2] = false,
                Square(56) => new_board.castling[1] = false,
                Square(63) => new_board.castling[0] = false,
                _ => (),
            }
        }
        // If any piece is moved, check if it goes into the corner
        match c_move.to {
            Square(0) => new_board.castling[3] = false,
            Square(7) => new_board.castling[2] = false,
            Square(56) => new_board.castling[1] = false,
            Square(63) => new_board.castling[0] = false,
            _ => (),
        }
        
        if new_board.to_move == White {
            new_board.to_move = Black
        }
        else { new_board.to_move = White };
        
        new_board
    }
}

// Stores time information for the game, in milliseconds
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct TimeInfo {
    pub white_time : u32,
    pub black_time : u32,
    pub white_inc : u32,
    pub black_inc : u32,
    pub moves_to_go : Option<u16>, // Number of moves to the next time control

    
}

lazy_static! {
    pub static ref START_BOARD: Board = {
        
        let mut board = [[Piece(Empty, White); 8]; 8];
        board[0] = [Piece(Rook, Black), Piece(Knight, Black), Piece(Bishop, Black),
                    Piece(Queen, Black), Piece(King, Black), Piece(Bishop, Black),
                    Piece(Knight, Black), Piece(Rook, Black)];
        board[1] = [Piece(Pawn, Black); 8];
        board[6] = [Piece(Pawn, White); 8];
        for i in 0..board[0].len() {
            let Piece(p_type, _) = board[0][i];
            board[7][i] = Piece(p_type, White);
        }
        Board {board : board, to_move : White, castling : [true; 4], en_passant : None,
               half_move_clock : 0, move_num : 0 , moves : vec![]}
    };
    
    pub static ref PIECE_CHAR_MAP : HashMap<Piece, char> = {
        let mut map = HashMap::new();
        map.insert(Piece(Rook, White), 'R');
        map.insert(Piece(Rook, Black), 'r');
        map.insert(Piece(Knight, White), 'N');
        map.insert(Piece(Knight, Black), 'n');
        map.insert(Piece(Bishop, White), 'B');
        map.insert(Piece(Bishop, Black), 'b');
        map.insert(Piece(Queen, White), 'Q');
        map.insert(Piece(Queen, Black), 'q');
        map.insert(Piece(King, White), 'K');
        map.insert(Piece(King, Black), 'k');
        map.insert(Piece(Pawn, White), 'P');
        map.insert(Piece(Pawn, Black), 'p');
        map.insert(Piece(Empty, White), ' ');
        map
    };
    pub static ref CHAR_PIECE_MAP : HashMap<char, Piece> = {
        rev_map(PIECE_CHAR_MAP.clone())
    };
}

fn rev_map<K, V> (in_map : HashMap<V, K>) -> HashMap<K, V>
    where K: Eq + Clone + std::hash::Hash, V: Eq + Clone + std::hash::Hash {
    let mut new_map = HashMap::new();
    for (value, key) in in_map.iter() {
        new_map.insert(key.clone(), value.clone());
    }
    new_map
}

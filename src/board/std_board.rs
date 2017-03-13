use self::PieceType::*;
use board::std_move;
use search_algorithms::board;
use search_algorithms::board::EvalBoard;
use board::std_move_gen::move_gen;
use search_algorithms::board::Color;
use search_algorithms::board::Color::*;

use std::ops;
use std::hash;
use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialOrd, Ord, PartialEq)]
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
    pub fn value(&self) -> f32 {
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

impl fmt::Display for PieceType {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
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

impl fmt::Display for Piece {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
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
    pub fn empty() -> Self {
        Piece(Empty, White)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Square(pub u8);

impl fmt::Display for Square {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let (file, rank) = self.file_rank();
        let actual_rank = ((rank as i8 - 8).abs() as u8 + ('0' as u8)) as char;
        
        let _ = fmt.write_str(&format!("{}{}",
                                       (file + 'a' as u8) as char,
                                       actual_rank));
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



// use std::hash::{Hash, Hasher, SipHasher};

#[derive(Clone, Eq, PartialEq)]
pub struct ChessBoard {
    pub board : [[Piece; 8]; 8],
    pub to_move : Color,
    pub castling_en_passant : u8,
    pub half_move_clock : u8,
    pub move_num : u16,
    pub moves : Vec<std_move::ChessMove>,
}

pub struct BoardIter {
    current_x: u8,
    current_y: u8,
}

impl BoardIter {
    pub fn new() -> Self {
        BoardIter { current_x: 0, current_y: 0 }
    }
}

impl Iterator for BoardIter {
    type Item = Square;
    fn next(&mut self) -> Option<Square> {
        if self.current_y >= 8 {
            None
        }
        else if self.current_x == 7 {
            self.current_x = 0;
            self.current_y += 1;
            Some(Square::from_ints(7, self.current_y - 1))
        }
        else {
            self.current_x += 1;
            Some(Square::from_ints(self.current_x - 1, self.current_y))
        }
    }
}

impl IntoIterator for ChessBoard {
    type Item = Square;
    type IntoIter = BoardIter;
    fn into_iter(self) -> Self::IntoIter {
        BoardIter::new()
    }
}



// Parses the first token in a FEN string, which describes the positions
/// of the pieces
fn parse_fen_board(fen_board : &str) -> Result<[[Piece;8];8], String> {
    let mut board = [[Piece(Empty, White); 8]; 8];
    let ranks : Vec<&str> = fen_board.split("/").collect();
    if ranks.len() != 8 {
        return Err(format!("Invalid FEN board string \"{}\": Had {} ranks instead of 8",
                           fen_board, ranks.len()));
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
    Ok(board)
}

fn parse_fen_to_move (to_move : &str) -> Result<Color, String> {
    let char_to_move = to_move.chars().collect::<Vec<_>>()[0];
    if char_to_move == 'w' { Ok(White) }
    else if char_to_move == 'b' { Ok(Black) }
    else { Err(format!("Invalid FEN string: Found {} in side-to-move-field, expected 'w' or 'b'",
                       to_move)) }
}

fn parse_fen_castling_rights(castling_str : &str, board: &mut ChessBoard) -> Result<(), String> {
    let mut castling_rights = [false; 4];
    for c in castling_str.chars() {
        match c {
            '-' => break,
            'K' => castling_rights[0] = true,
            'Q' => castling_rights[1] = true,
            'k' => castling_rights[2] = true,
            'q' => castling_rights[3] = true,
            _ => return Err("Invalid FEN string: Error in castling field.".to_string()),
        }
    }
    if !castling_rights[0] { board.disable_castling_kingside(White) }
    if !castling_rights[1] { board.disable_castling_queenside(White) }
    if !castling_rights[2] { board.disable_castling_kingside(Black) }
    if !castling_rights[3] { board.disable_castling_queenside(Black) }
    Ok(())
}

impl ::uci::UciBoard for ChessBoard {    
    fn from_fen(fen : &str) -> Result<Self, String> {
        let fen_split : Vec<&str> = fen.split(" ").collect();
        if fen_split.len() < 4 || fen_split.len() > 6 {
            return Err(format!("Invalid FEN string \"{}\": Had {} fields instead of [4, 5, 6]",
                               fen, fen_split.len()));
        }

        let mut board = Self::start_board();
        board.board = try!(parse_fen_board(fen_split[0]));
        
        if fen_split[1].len() != 1 {
            return Err("Invalid FEN string: Error in side to move-field".to_string());
        }

        // Check side to move
        board.to_move = try!(parse_fen_to_move(fen_split[1]));

        // Check castling rights field
        try!(parse_fen_castling_rights(fen_split[2], &mut board));

        // Check en passant field
        if fen_split[3] != "-" {
            match Square::from_alg(fen_split[3]) {
                Some(square) => board.set_en_passant_square(Some(square)),
                None => return Err(format!("Invalid en passant square {}.", fen_split[3])),
            }
        };

        let (half_clock, move_num) : (u8, u16) =
            if fen_split.len() > 4 {
                (try!(fen_split[4].parse().map_err(|_|"Invalid half_move number in FEN string")),
                 try!(fen_split[5].parse().map_err(|_|"Invalid half_move number in FEN string")))
            }
        else {
            (0, 0)
        };

        board.half_move_clock = half_clock;
        board.move_num = move_num;
    
        Ok(board)
    }
    // TODO: Implement
    fn to_fen(&self) -> String {
        panic!()
    }
}

impl EvalBoard for ChessBoard {

    type Move = std_move::ChessMove;
    type UndoMove = std_move::ChessMove;
    
    fn to_move(&self) -> Color {
        self.to_move
    }

    fn all_legal_moves(&self) -> Vec<Self::Move> {
        move_gen::all_legal_moves(self)
    }

    fn game_result(&self) -> Option<board::GameResult> {
        // TODO: This shouldn't call all_legal_moves(), but instead store whether its mate or not
        
        if self.all_legal_moves().len() == 0 {            
            if move_gen::is_attacked(self, self.king_pos(self.to_move())) {
                if self.to_move == White {
                    Some(board::GameResult::BlackWin)
                }
                else {
                    Some(board::GameResult::WhiteWin)
                }
            }
            else {
                Some(board::GameResult::Draw)
            }
        }
        else { None }
    }
    
    #[inline(never)]
    fn eval_board (&self) -> f32 {
        const POS_VALS : [[u8; 8]; 8] = 
            [[0, 0, 0, 0, 0, 0, 0, 0],
             [0, 1, 1, 1, 1, 1, 1, 0],
             [0, 1, 2, 2, 2, 2, 1, 0],
             [0, 1, 2, 3, 3, 2, 1, 0],
             [0, 1, 2, 3, 3, 2, 1, 0],
             [0, 1, 2, 2, 2, 2, 1, 0],
             [0, 1, 1, 1, 1, 1, 1, 0],
             [0, 0, 0, 0, 0, 0, 0, 0]];
        let mut value = 0.0;
        for rank in 0..8 {
            for file in 0..8 {
                let piece_val = self.board[rank][file].value();
                let pos_val = POS_VALS[rank][file] as f32 *
                    match self.board[rank][file] {
                        Piece(Bishop, White) => 0.15,
                        Piece(Bishop, Black) => -0.15,
                        Piece(Knight, White) => 0.3,
                        Piece(Knight, Black) => -0.3,
                        Piece(Queen, White) => 0.3,
                        Piece(Queen, Black) => -0.3,
                        Piece(Pawn, White) => 0.00,
                        Piece(Pawn, Black) => -0.00,
                        Piece(Rook, Black) => -0.1,
                        Piece(Rook, White) => 0.1,
                        _ => 0.0,
                    };
                let pawn_val = match self.board[rank][file] {
                    Piece(Pawn, _) => (rank as f32 - 3.5) * -0.1,
                    _ => 0.0,
                };
                value += piece_val + pos_val + pawn_val;
                //println!("Value at {} is {}, {}, total: {}",
                //         Square::from_ints(file as u8, rank as u8), piece_val, pos_val, value);
            }
        }
        value
    }

    fn do_move(&mut self, mut c_move : Self::Move) -> Self::UndoMove {
        // Helper variables
        let (file_from, rank_from) = c_move.from.file_rank();
        let (file_to, rank_to) = c_move.to.file_rank();
        let color = self.to_move;
        let piece_moved = self.piece_at(c_move.from).0;

        c_move.old_half_move_clock = self.half_move_clock;
        c_move.old_castling_en_passant = self.castling_en_passant;
        
        // Increment or reset the half-move clock
        match (piece_moved, self.piece_at(c_move.to).0) {
            (Pawn, _) => self.half_move_clock = 0,
            (_, Empty) => self.half_move_clock += 1,
            (_, _) => self.half_move_clock = 0,
        }

        self.moves.push(c_move);
        self.move_num += 1;
        
        
        // Perform castling
        // It will castle regardless of whether it is legal to do so
        if piece_moved == King &&
            (file_from as i8 - file_to as i8).abs() == 2 {

                // Simple helper closure that moves a piece, emptying the square it came from
                // Checks nothing, not even if the square has a piece. Use carefully.
                let mut do_simple_move = |f_from, r_from, f_to, r_to| {
                    self.board[r_to as usize][f_to as usize]
                        = self.board[r_from as usize][f_from as usize];
                    self.board[r_from as usize][f_from as usize] = Piece(Empty, White);
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
        else if piece_moved == Pawn && file_from != file_to &&
            c_move.capture == Empty {
                self.board[rank_to as usize][file_to as usize]
                    = self.board[rank_from as usize][file_from as usize];

                self.board[rank_from as usize][file_to as usize] = Piece(Empty, White);
                self.board[rank_from as usize][file_from as usize] = Piece(Empty, White);
                
                
            }
        // If it is not a special move
        else {
            // Does the move, depending on whether the move promotes or not
            match c_move.prom {
                Some(piece) => self.board[rank_to as usize][file_to as usize] = piece,
                None => self.board[rank_to as usize][file_to as usize]
                    = self.board[rank_from as usize][file_from as usize],
                
            }
            self.board[rank_from as usize][file_from as usize] = Piece(Empty, White);
        }
            
        // Remove any en passant square. If it was available to this player,
        // it has already been used. Any new en passant square is added below.

       self.set_en_passant_square(None);

        // If the pawn went two squares forward, add the square behind it as en passant square
        if piece_moved == Pawn &&
            (rank_from as i8 - rank_to as i8).abs() == 2
        {
            self.set_en_passant_square(
                Some(Square::from_ints(file_from, (rank_from + rank_to) / 2)));
            debug_assert!(self.en_passant_square().is_some(), "Failed to set en passant square");
        }

        // Remove castling rights if necessary
        if self.castling_en_passant & 0b0000_1111 > 0 {
            // Remove castling rights on king/rook moves
            // Does not check when rooks are captured, it is assumed that the
            // function looking for moves checks that rooks are present
            if piece_moved == King {
                self.disable_castling(color);
            }
            // If a rook was moved, check if it came from a corner
            if piece_moved == Rook {
                match c_move.from {
                    Square(0) => self.disable_castling_queenside(Black),
                    Square(7) => self.disable_castling_kingside(Black),
                    Square(56) => self.disable_castling_queenside(White),
                    Square(63) => self.disable_castling_kingside(White),
                    _ => (),
                }
            }
            // For any piece, check if it goes into the corner
            match c_move.to {
                Square(0) => self.disable_castling_queenside(Black),
                Square(7) => self.disable_castling_kingside(Black),
                Square(56) => self.disable_castling_queenside(White),
                Square(63) => self.disable_castling_kingside(White),
                _ => (),
            }
        }
        
        self.to_move = !self.to_move;
        c_move
    }
    fn undo_move(&mut self, c_move : Self::UndoMove) {
        
        let (file_from, rank_from) = c_move.from.file_rank();
        let (file_to, rank_to) = c_move.to.file_rank();
        let piece_moved = self.piece_at(c_move.to).0;
        let color = !self.to_move;

        if piece_moved == King &&
            (file_from as i8 - file_to as i8).abs() == 2
        {
            // Simple helper closure that moves a piece, emptying the square it came from
            // Checks nothing, not even if the square has a piece. Use carefully.
            let mut do_simple_move = |f_from, r_from, f_to, r_to| {
                self.board[r_to as usize][f_to as usize]
                    = self.board[r_from as usize][f_from as usize];
                self.board[r_from as usize][f_from as usize] = Piece(Empty, White);
            };
            // Assume castling is legal, and move the king and rook to where they should go
            match (color, file_to) {
                (White, 2) => { do_simple_move(2, 7, 4, 7);
                                do_simple_move(3, 7, 0, 7);
                },
                (White, 6) => { do_simple_move(6, 7, 4, 7);
                                do_simple_move(5, 7, 7, 7);
                },
                (Black, 2) => { do_simple_move(2, 0, 4, 0);
                                do_simple_move(3, 0, 0, 0);
                },
                (Black, 6) => { do_simple_move(6, 0, 4, 0);
                                do_simple_move(5, 0, 7, 0);
                },
                (_, _) => panic!(format!(
                    "Error: Tried to castle to the {}th file. ", file_to)),
            }
            
        }
        // Undo en passant capture
        else if piece_moved == Pawn && file_from != file_to && c_move.capture == Empty {
            self.board[rank_from as usize][file_from as usize] =
                self.board[rank_to as usize][file_to as usize];
            self.board[rank_to as usize][file_to as usize] =Piece(Empty, White);
            
            match color {
                Black => self.board[rank_to as usize - 1][file_to as usize] = Piece(Pawn, White),
                White => self.board[rank_to as usize + 1][file_to as usize] = Piece(Pawn, Black),
            }
        }
        else {
            if c_move.prom.is_some() {
                self.board[rank_from as usize][file_from as usize] = 
                    Piece(Pawn, color);
            }
            else {
                self.board[rank_from as usize][file_from as usize] =
                    self.board[rank_to as usize][file_to as usize];
            }
            
            if c_move.capture != Empty
            {
                self.board[rank_to as usize][file_to as usize] =
                    Piece(c_move.capture, self.to_move);
            }
            else {
                self.board[rank_to as usize][file_to as usize] = Piece(Empty, White);
            }
        }
        self.castling_en_passant = c_move.old_castling_en_passant;
        self.half_move_clock = c_move.old_half_move_clock;
        self.move_num -= 1;
        self.moves.pop();

        self.to_move = !self.to_move;
    }
    
    fn start_board() -> Self {
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
        ChessBoard {board : board, to_move : White, castling_en_passant : 0b0000_1111,
                    half_move_clock : 0, move_num : 0 , moves : vec![]}
        
    }
}

impl fmt::Display for ChessBoard {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.write_str("\n").unwrap();
        for rank in self.board.iter() {
            for piece in rank.iter() {
                write!(fmt, "[{}]", piece).unwrap();
            }
            write!(fmt, "\n").unwrap();
        }
        write!(fmt, "To move: {}, move_number: {}, flags: {:b}, half_move_clock: {}\n",
               self.to_move, self.move_num, self.castling_en_passant, self.half_move_clock)
            .unwrap();
        for c_move in &self.moves {
            write!(fmt, "[{}],", &c_move).unwrap();
        }
        Ok(())   
    }
}

impl fmt::Debug for ChessBoard {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Display::fmt(self, fmt)
    }
}

impl ops::Index<Square> for ChessBoard {
    type Output = Piece;
    fn index(&self, square: Square) -> &Piece {
        let Square(i) = square;
        debug_assert!(i < 64, format!("Tried to find piece at pos {} on board{}!", i, self));
        &self.board[i as usize >> 3][i as usize & 0b0000_0111]
    }
}

impl ops::IndexMut<Square> for ChessBoard {
    fn index_mut(&mut self, square: Square) -> &mut Piece {
        let Square(i) = square;
        debug_assert!(i < 64, format!("Tried to find piece at pos {} on board{}!", i, self));
        &mut self.board[i as usize >> 3][i as usize & 0b0000_0111]
    }
}

impl ChessBoard {
    // TODO: Remove in favour of indexing operator
    pub fn piece_at(&self, square : Square) -> Piece {
        self[square].clone()
    }
    /// Returns a clone of the board, viewed from the other side.
    /// This screws up everything related to pawn movement, castling, etc,
    /// and should only be used to print the board as seen from the black side
    #[allow(dead_code)]
    pub fn upside_down(&self) -> Self {
        let mut new_board = self.clone();
        for i in 0..8 {
            new_board.board[i] = self.board[7-i].clone();
            for j in 0..8 {
                new_board.board[i][j] = self.board[7-i][7-j];
            }
        }
        new_board
    }

    pub fn king_pos(&self, color: Color) -> Square {
        match self.pos_of(Piece(King, color)) {
            Some(square) => square,
            None => panic!("Error: There is no king on the board:\n{}", self),
        }
    }
    
    pub fn pos_of(&self, piece : Piece) -> Option<Square> {
        for i in 0..64 {
            if self.piece_at(Square(i)) == piece {
                return Some(Square(i));
            }
        }
        None
    }
    pub fn disable_castling(&mut self, color: Color) {
        match color {
            White => self.castling_en_passant = self.castling_en_passant & 0b1111_1100,
            Black => self.castling_en_passant = self.castling_en_passant & 0b1111_0011,
        }
    }
    pub fn disable_castling_queenside(&mut self, color: Color) {
        match color {
            White => self.castling_en_passant = self.castling_en_passant & 0b1111_1101,
            Black => self.castling_en_passant = self.castling_en_passant & 0b1111_0111,
        }
    }
    pub fn disable_castling_kingside(&mut self, color: Color) {
        match color {
            White => self.castling_en_passant = self.castling_en_passant & 0b1111_1110,
            Black => self.castling_en_passant = self.castling_en_passant & 0b1111_1011,
        }
    }
    
    pub fn can_castle_kingside(&self, color: Color) -> bool {
        match color {
            White => self.castling_en_passant & 0b0000_0001 > 0,
            Black => self.castling_en_passant & 0b0000_0100 > 0,
        }
    }
    pub fn can_castle_queenside(&self, color: Color) -> bool {
        match color {
            White => self.castling_en_passant & 0b0000_0010 > 0,
            Black => self.castling_en_passant & 0b0000_1000 > 0,
        }
    }
    
    pub fn en_passant_square(&self) -> Option<Square> {
        if self.castling_en_passant & 0b1111_0000 != 0 {
            let rank = if self.to_move == Black { 5 } else { 2 };
            let file = (self.castling_en_passant & 0b0111_1111) >> 4;
            debug_assert!(file < 8);

            Some(Square::from_ints(file, rank))
        }
        else { None }
    }

    pub fn set_en_passant_square(&mut self, square: Option<Square>) {
        
        match square {
            Some(square) => {
                let (mut byte, _) = square.file_rank();
                byte = byte | 0b1000_0000;
                //println!("byte << 4 was {:b}, byte={:b}", byte << 4, byte);
                self.castling_en_passant = self.castling_en_passant | ((byte << 4) | 0b1000_0000);
                //println!("Bitmap set to {:b}", self.castling_en_passant);
                //panic!();
            },
            None => self.castling_en_passant = self.castling_en_passant & 0b0000_1111,
        }
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
        map.insert(Piece(Empty, Black), '.');
        map
    };
    pub static ref CHAR_PIECE_MAP : HashMap<char, Piece> = {
        rev_map(PIECE_CHAR_MAP.clone())
    };
}

fn rev_map<K, V> (in_map : HashMap<V, K>) -> HashMap<K, V>
    where K: Eq + Clone + hash::Hash, V: Eq + Clone + hash::Hash {
    let mut new_map = HashMap::new();
    for (value, key) in in_map.iter() {
        new_map.insert(key.clone(), value.clone());
    }
    new_map
}

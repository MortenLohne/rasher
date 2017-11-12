use board::std_board::{ChessBoard, Piece, PieceType, Square, BoardIter};
use board::std_board::PieceType::*;
use board::sjadam_move::{SjadamMove, SjadamUndoMove};
use board::sjadam_move_gen;

use search_algorithms::board::EvalBoard;
use search_algorithms::board::Color;
use search_algorithms::board::Color::*;
use search_algorithms::board::GameResult;

#[cfg(feature = "legacy_sjadam_move_format")]
use board::std_board::PieceType::*;

use std::fmt;
use uci::UciBoard;

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct BitBoard {
    pub board: u64,
}

impl BitBoard {
    pub fn empty() -> Self {
        BitBoard { board: 0 }
    }
    pub fn from_u64(n: u64) -> Self {
        BitBoard { board: n }
    }
    
    /// Returns a bitboard representation of the board, with all bits set
    /// where f(piece) is true for the piece on the square
    #[allow(dead_code)]
    pub fn from_board<F: Fn(Piece) -> bool> (board: &ChessBoard, f: F) -> Self {
        let mut bit_board = BitBoard::empty();
        for square in BoardIter::new() {
            if f(board[square]) {
                bit_board.set(square);
            }
        }
        bit_board
    }
    #[allow(dead_code)]
    pub fn all_from_board(board: &ChessBoard) -> Self {
        Self::from_board(board, |piece| !piece.is_empty())
    }
     
    pub fn get(&self, idx: Square) -> bool {
        let Square(i) = idx;
        debug_assert!(i < 64, format!("Tried to index pos {} on board{:?}!", idx, self));
        self.board & (1<<i) != 0
    }
    // Sets the square to true
    pub fn set(&mut self, idx: Square) {
        let Square(i) = idx;
        debug_assert!(i < 64, format!("Tried to index pos {} on board{:?}!", idx, self));
        self.board |= 1<<i;
    }
    #[allow(dead_code)]
    // Sets the square to false
    pub fn clear(&mut self, idx: Square) {
        let Square(i) = idx;
        debug_assert!(i < 64, format!("Tried to index pos {} on board{:?}!", idx, self));
        self.board &= !(1<<i);
    }

    #[allow(dead_code)]
    pub fn popcount(&self) -> u32 {
        self.board.count_ones()
    }
    
    pub fn rank(self, rank: u8) -> u8 {
        (self.board >> (rank * 8)) as u8
    }
    #[allow(dead_code)]
    pub fn file(self, file: u8) -> u8 {
        self.rotate().rank(file)
    }
    pub fn rotate(&self) -> Self {
        self.flip_vertical().flip_diagonal()
    }
    pub fn rotate_270(&self) -> Self {
        self.flip_diagonal().flip_vertical()
    }
    pub fn rotate_45(&self) -> Self {
        let k1 = 0xAAAAAAAAAAAAAAAA;
        let k2 = 0xCCCCCCCCCCCCCCCC;
        let k4 = 0xF0F0F0F0F0F0F0F0;
        let mut x = self.board;
        x ^= k1 & (x ^ x.rotate_right(8));
        x ^= k2 & (x ^ x.rotate_right(16));
        x ^= k4 & (x ^ x.rotate_right(32));
        Self::from_u64(x)
    }
    pub fn rotate_315(&self) -> Self {
        let k1 = 0x5555555555555555;
        let k2 = 0x3333333333333333;
        let k4 = 0x0f0f0f0f0f0f0f0f;
        let mut x = self.board;
        x ^= k1 & (x ^ x.rotate_right(8));
        x ^= k2 & (x ^ x.rotate_right(16));
        x ^= k4 & (x ^ x.rotate_right(32));
        Self::from_u64(x)
    }
    #[allow(dead_code)]
    pub fn flip_horizontal(&self) -> Self {
        let k1 = 0x5555555555555555;
        let k2 = 0x3333333333333333;
        let k4 = 0x0f0f0f0f0f0f0f0f;
        let mut x = self.board;
        x = ((x >> 1) & k1) +  2*(x & k1);
        x = ((x >> 2) & k2) +  4*(x & k2);
        x = ((x >> 4) & k4) + 16*(x & k4);
        BitBoard::from_u64(x)
    }
    pub fn flip_vertical(&self) -> Self {
        BitBoard::from_u64(self.board.swap_bytes())
    }

    pub fn flip_diagonal(&self) -> Self {
        let k1 = 0x5500550055005500;
        let k2 = 0x3333000033330000;
        let k4 = 0x0f0f0f0f00000000;
        let mut x = self.board;
        let mut t = k4 & (x ^ (x << 28));
        x ^=       t ^ (t >> 28) ;
        t  = k2 & (x ^ (x << 14));
        x ^=       t ^ (t >> 14) ;
        t  = k1 & (x ^ (x <<  7));
        x ^=       t ^ (t >>  7) ;
        BitBoard::from_u64(x)
    }
    /// Diagonal_id is rank + file
    /// Assumes board is already rotated
    pub fn diagonal(&self, diagonal_id: u8) -> u8 {
        let len = if diagonal_id >= 8 { 15 - diagonal_id } else { diagonal_id + 1 };
        let n = diagonal_id as i8 - 7;
        //let offset = ((7 * n + 8)) % 64;
        let offset = if n <= 0 { n * (-8)} else { 8 * (8 - n) + n };
        ((self.board >> offset) & !(!0 << len)) as u8
    }

    /// Diagonal_id is rank - file
    /// Assumes board is already rotated
    pub fn antidiagonal(&self, diagonal_id: i8) -> u8 {
        let len = 8 - diagonal_id.abs(); // Between 1 and 8
        let n = diagonal_id as i16;
        let offset = if n <= 0 { n * (-8) - n} else { 8 * (8 - n) };
        ((self.board >> offset) & !(!0 << len as u64)) as u8
    }
}

impl fmt::Debug for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for n in 0..8 {
            write!(f, "{:08b}\n", (self.board >> (n * 8)) as u8).unwrap();
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct SjadamBoard {
    white_pieces: BitBoard,
    black_pieces: BitBoard,
    bitboards: [BitBoard; 12],
    to_move: Color,
    castling_en_passant: u8,
    half_move_clock: u8,
    move_num: u16,
}

impl PartialEq for SjadamBoard {
    fn eq(&self, other: &Self) -> bool {
        self.bitboards == other.bitboards
            && self.to_move == other.to_move
            && self.castling_en_passant == other.castling_en_passant
    }
}

impl Eq for SjadamBoard {}


use std::hash::Hasher;
use std::hash::Hash;

impl Hash for SjadamBoard {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.bitboards.hash(state);
        self.to_move.hash(state);
        self.castling_en_passant.hash(state);
    }
}

impl SjadamBoard {
    pub fn from_chess_board(other: &ChessBoard) -> Self {
        let mut board = Self { bitboards: [BitBoard::empty(); 12],
                               white_pieces: BitBoard::empty(), black_pieces: BitBoard::empty(),
                               to_move: other.to_move(),
                               castling_en_passant: other.castling_en_passant,
                               half_move_clock: other.half_move_clock,
                               move_num: other.move_num };
        for square in BoardIter::new() {
            if !other[square].is_empty() {
                board.set_piece_at_square(other[square], square);
            }
        }
        board
    }

    pub fn to_chess_board(&self) -> ChessBoard {
        let mut board = ChessBoard::empty();
        for square in BoardIter::new() {
            board[square] = self.get_square(square);
        }
        board.castling_en_passant = self.castling_en_passant;
        board.to_move = self.to_move();
        board.half_move_clock = self.half_move_clock;
        board.move_num = self.move_num;
        board
    }

    fn is_empty(&self, square: Square) -> bool {
        !self.all_pieces().get(square)
    }
    
    pub fn get_piece(&self, piece: Piece) -> BitBoard {
        self.bitboards[piece as u32 as usize - 2]
    }

    pub fn get_square(&self, square: Square) -> Piece {
        if self.white_pieces().get(square) {
            [0, 2, 4, 6, 8, 10].iter()
                .map(|&i| self.bitboards[i])
                .enumerate()
                .find(|&(_, bitboard)| bitboard.get(square))
                .map(|(i, _)| Piece::new(PieceType::from_disc(i as u32 + 1).unwrap(), White))
                .unwrap()
        }
        else if self.black_pieces().get(square) {
            [1, 3, 5, 7, 9, 11].iter()
                .map(|&i| self.bitboards[i])
                .enumerate()
                .find(|&(_, bitboard)| bitboard.get(square))
                .map(|(i, _)| Piece::new(PieceType::from_disc(i as u32 + 1).unwrap(), Black))
                .unwrap()
        }
        else {
            Piece::empty()
        }
    }

    pub fn white_pieces(&self) -> BitBoard {
        self.white_pieces
    }

    pub fn black_pieces(&self) -> BitBoard {
        self.black_pieces
    }

    pub fn all_pieces(&self) -> BitBoard {
        BitBoard::from_u64(self.white_pieces().board | self.black_pieces().board)
    }

    pub fn piece_at_square(&self, piece: Piece, square: Square) -> bool {
        self.get_piece(piece).get(square)
    }

    pub fn set_piece_at_square(&mut self, piece: Piece, square: Square) {
        debug_assert!(!piece.is_empty());
        self.bitboards[piece as u8 as usize - 2].set(square);
        if piece.color().unwrap() == White {
            self.white_pieces.set(square);
        }
        else {
            self.black_pieces.set(square);
        }
    }

    pub fn clear_piece_at_square(&mut self, piece: Piece, square: Square) {
        debug_assert!(!piece.is_empty());
        self.bitboards[piece as u8 as usize - 2].clear(square);
        if piece.color().unwrap() == White {
            self.white_pieces.clear(square);
        }
        else {
            self.black_pieces.clear(square);
        }
    }

    pub fn move_piece(&mut self, piece: Piece, from: Square, to: Square) {
        debug_assert!(self.piece_at_square(piece, from), "No {} to move from {} to {} on \n{:?}",
                      piece, from, to, self);
        self.set_piece_at_square(piece, to);
        self.clear_piece_at_square(piece, from);
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
                self.castling_en_passant = self.castling_en_passant | ((byte << 4) | 0b1000_0000);
            },
            None => self.castling_en_passant = self.castling_en_passant & 0b0000_1111,
        }
    }
}

impl fmt::Debug for SjadamBoard {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Debug::fmt(&self.to_chess_board(), fmt)
    }
}

impl UciBoard for SjadamBoard {
    fn from_fen(fen: &str) -> Result<Self, String> {
        ChessBoard::from_fen(fen).map(|b| Self::from_chess_board(&b))
    }

    fn to_fen(&self) -> String {
        self.to_chess_board().to_fen()
    }

    fn from_alg(&self, input: &str) -> Result<Self::Move, String> {
        if input.len() < 4 {
            return Err(format!("Move \"{}\" was too short to parse", input))
        }
        #[cfg(feature = "legacy_sjadam_move_format")]
        {
            if input.len() == 5 && (input.as_bytes()[4] as char == '-' || input.as_bytes()[0] as char == '-') || input.len() == 6 {
                let mut chars = input.chars().peekable();
                if *chars.peek().unwrap() == '-' {
                    let from = Square::from_alg(&input[1..3]).ok_or("Illegal square")?;
                    let to = Square::from_alg(&input[3..5]).ok_or("Illegal square")?;
                    // TODO: Check for castling
                    return Ok(SjadamMove::new(from, to, false))
                }
                let alg : String = chars.by_ref().collect();
                let from = Square::from_alg(&alg[0..2]).ok_or("Illegal square")?;
                let sjadam = Square::from_alg(&alg[2..4]).ok_or("Illegal square")?;
                if alg.chars().last().unwrap() == '-' {
                    return Ok(SjadamMove::new(from, sjadam, false));
                }
                else {
                    let to = Square::from_alg(&alg[4..]).ok_or("Illegal square")?;
                    return Ok(SjadamMove::new(from, to, false));
                }
            }
        }
        if input.len() > 5 {
            return Err(format!("Move \"{}\" was too long to parse", input))
        }
        let from = Square::from_alg(&input[0..2]).ok_or("Illegal square")?;
        let to = Square::from_alg(&input[2..4]).ok_or("Illegal square")?;
        match input.len() {
            4 => Ok(SjadamMove::new(from, to, false)),
            5 if input.as_bytes()[4] == 'c' as u8 => Ok(SjadamMove::new(from, to, true)),
            _ => Err(format!("Couldn't parse move {}", input))
        }
    }
    fn to_alg(&self, mv: &Self::Move) -> String {
        #[cfg(feature = "legacy_sjadam_move_format")]
        {
            // println!("Converting move {:?} on\n{:?}", mv, self);
            let dia_neighbours = |square: i8| [square - 9, square - 7, square + 7, square + 9]
                .iter().cloned()
                .filter(|&sq| sq >= 0 && sq < 64)
                .map(|sq| Square(sq as u8))
                .filter(|&sq| self.is_empty(sq) || sq == mv.from())
                .collect::<Vec<Square>>();
            let orth_neighbours = |square: i8| [square - 8, square - 1, square + 1, square + 8]
                .iter().cloned()
                .filter(|&sq| sq >= 0 && sq < 64)
                .map(|sq| Square(sq as u8))
                .filter(|&sq| self.is_empty(sq) || sq == mv.from())
                .collect::<Vec<Square>>();
            let knight_neighbours = |square: i8| [square - 17, square - 15, square - 9, square - 6,
                                                  square + 6, square + 9, square + 15, square + 17]
                .iter().cloned()
                .filter(|&sq| sq >= 0 && sq < 64)
                .map(|sq| Square(sq as u8))
                .filter(|&sq| self.is_empty(sq) || sq == mv.from())
                .collect::<Vec<Square>>();

            let pawn_neighbours = |square: i8| [square + 7, square + 9]
                .iter().cloned()
                .map(|sq| if self.to_move() == Black { sq - 16 } else { sq } )
                .filter(|&sq| sq >= 0 && sq < 64)
                .map(|sq| Square(sq as u8))
                .inspect(|&sq| println!("Inspecting pawn square {}", sq))
                .filter(|&sq| self.is_empty(sq) || sq == mv.from())
                .inspect(|&sq| println!("Approved pawn square {}", sq))
                .collect::<Vec<Square>>();

            // If destination square is empty, do it as a pure sjadam move
            let sjadam_square = if self.is_empty(mv.to()) {
                if mv.castling() {
                    mv.from()
                }
                else {
                    mv.to() // TODO: Does not create en passant squares
                }
            }
            else {
                match self.get_square(mv.from()).piece_type() {
                    Bishop => *dia_neighbours(mv.to().0 as i8).get(0).expect(&mv.to_string()),
                    Rook => *orth_neighbours(mv.to().0 as i8).get(0).expect(&mv.to_string()),
                    Queen | King => { let mut neighbs = orth_neighbours(mv.to().0 as i8);
                                      neighbs.append(&mut dia_neighbours(mv.to().0 as i8));
                                      *neighbs.get(0).expect(&mv.to_string())
                    },
                    Knight => *knight_neighbours(mv.to().0 as i8).get(0).expect(&mv.to_string()),
                    Pawn => *pawn_neighbours(mv.to().0 as i8).get(0).expect(&mv.to_string()),
                    
                    Empty => unreachable!(),
                }
            };
            
            let mut f = String::new();
            use fmt::Write;
            write!(f, "{}{}{}",
                   if mv.from() == sjadam_square {
                       "-".to_string()
                   }
                   else {
                       mv.from().to_string()
                   },
                   sjadam_square,
                   if sjadam_square == mv.to() {
                       "-".to_string()
                   }
                   else {
                       mv.to().to_string()
                   }).unwrap();
            return f;
        }
        mv.to_string()
    }
}

impl EvalBoard for SjadamBoard {
    type Move = SjadamMove;
    type UndoMove = SjadamUndoMove;
    
    fn to_move(&self) -> Color {
        self.to_move
    }
    
    fn start_board() -> Self {
        Self::from_chess_board(&ChessBoard::start_board())
    }

    fn game_result(&self) -> Option<GameResult> {
        // In sjadam, king may be actually captured.
        // Check if the king is gone
        match (self.get_piece(Piece::new(King, White)).board != 0,
               self.get_piece(Piece::new(King, Black)).board != 0) {
            (true, false) => Some(GameResult::WhiteWin),
            (false, true) => Some(GameResult::BlackWin),
            (false, false) => panic!("Neither side has a king on the board:\n{:?}", self),
            (true, true) if self.half_move_clock > 50 => Some(GameResult::Draw),
            (true, true) => None,
        }
    }

    fn do_move(&mut self, mv: Self::Move) -> Self::UndoMove {
        let start_color = self.to_move();
        debug_assert_ne!(mv.from(), mv.to());
        debug_assert!(!self.is_empty(mv.from()),
                      "Tried to do move {} from empty square at \n{:?}", mv, self);

        let en_passant = mv.en_passant_bitboard(&self);
        // TODO: Store piece moved in move struct?
        let piece_moved = self.get_square(mv.from()).piece_type();
        let undo_move = SjadamUndoMove {
            from: mv.from(), to: mv.to(),
            castling: mv.castling(), en_passant: en_passant,
            capture: self.get_square(mv.to()).piece_type(),
            piece_moved: piece_moved,
            old_castling_en_passant: self.castling_en_passant,
            old_half_move_clock: self.half_move_clock
        };
        debug_assert!(undo_move.piece_moved != PieceType::Empty,
                      "Empty piece_moved when doing {} on \n{:?}", mv, self);

        self.set_en_passant_square(None);
        
        // Remove castling priviledges on king moves
        if undo_move.piece_moved == PieceType::King {
            let color = self.to_move();
            self.disable_castling(color);
        }
        
        // Remove castling priviledges if anything moves from or to a corner
        match mv.from() {
            square if square == Square::A1 =>
                self.disable_castling_queenside(White),
            square if square == Square::H1 =>
                self.disable_castling_kingside(White),
            square if square == Square::A8 =>
                self.disable_castling_queenside(Black),
            square if square == Square::H8 =>
                self.disable_castling_kingside(Black),
            _ => (),
        }

        match mv.to() {
            square if square == Square::A1 =>
                self.disable_castling_queenside(White),
            square if square == Square::H1 =>
                self.disable_castling_kingside(White),
            square if square == Square::A8 =>
                self.disable_castling_queenside(Black),
            square if square == Square::H8 =>
                self.disable_castling_kingside(Black),
            _ => (),
        }

        // Set en passant square
        let mid_sq = Square::from_ints(mv.to().file(), (mv.from().rank() + mv.to().rank()) / 2);
        if piece_moved == Pawn && mv.from().file() == mv.to().file()
            && i8::abs(mv.from().rank() as i8 - mv.to().rank() as i8) == 2
            && !self.piece_at_square(Piece::new(Pawn, start_color), mid_sq)
        {
            if start_color == White && mv.from().rank() == 6 {
                self.set_en_passant_square(Some(mid_sq));
            }
            else if start_color == Black && mv.from().rank() == 1 {
                self.set_en_passant_square(Some(mid_sq));
            }
        }
        
        self.move_piece(Piece::new(piece_moved, start_color), mv.from(), mv.to());
        
        if undo_move.capture != Empty {
            self.clear_piece_at_square(Piece::new(undo_move.capture, !start_color), mv.to());
        }
        
        if mv.castling() {
            // Move the rook too
            let (rook_from, rook_to) = if mv.to().file() == 6 { (7, 5) } else { (0, 3) };
            self.move_piece(Piece::new(Rook, start_color),
                            Square::from_ints(rook_from, mv.from().rank()),
                            Square::from_ints(rook_to, mv.from().rank()));
        }
        else if en_passant {
            // Remove the captured pawn
            let ep_square_rank = if start_color == Black { mv.to().rank() - 1 } else { mv.to().rank() + 1 };
            self.clear_piece_at_square(Piece::new(Pawn, !start_color),
                                       Square::from_ints(mv.to().file(), ep_square_rank));
        }
        else if (start_color == White && mv.to().rank() == 0)
            || (start_color == Black && mv.to().rank() == 7)
        {
            // Promote to queen
            debug_assert!(!self.is_empty(mv.to()));
            if !self.piece_at_square(Piece::new(King, start_color), mv.to()) {
                self.clear_piece_at_square(Piece::new(piece_moved, start_color), mv.to());
                self.set_piece_at_square(Piece::new(Queen, start_color), mv.to());
            }
        }
        
        self.to_move = !self.to_move();
        debug_assert_ne!(start_color, self.to_move());
        debug_assert!(self.castling_en_passant & 15 <= undo_move.old_castling_en_passant & 15);
        undo_move
    }

    fn undo_move(&mut self, mv: Self::UndoMove) {
        let start_color = !self.to_move();
        if mv.piece_moved != Queen
            && self.piece_at_square(Piece::new(Queen, start_color), mv.to()) {
                self.clear_piece_at_square(Piece::new(Queen, start_color), mv.to());
                self.set_piece_at_square(Piece::new(mv.piece_moved, start_color), mv.from());
        }
        else { // If move was not promotion
            self.move_piece(Piece::new(mv.piece_moved, start_color), mv.to(), mv.from());
        }
        
        if mv.capture != Empty {
            self.set_piece_at_square(Piece::new(mv.capture, !start_color), mv.to());
        }

        if mv.castling() {
            // Move the rook too
            let (rook_from, rook_to) = if mv.to().file() == 6 { (7, 5) } else { (0, 3) };
            self.move_piece(Piece::new(Rook, start_color),
                            Square::from_ints(rook_to, mv.from().rank()),
                            Square::from_ints(rook_from, mv.from().rank()));
        }
        else if mv.en_passant() {
            // Replace the captured pawn
            let ep_square_rank = if start_color == Black { mv.to().rank() - 1 } else { mv.to().rank() + 1 };
            self.set_piece_at_square(Piece::new(Pawn, !start_color),
                                     Square::from_ints(mv.to().file(), ep_square_rank));
        }
        
        self.half_move_clock = mv.old_half_move_clock;
        self.castling_en_passant = mv.old_castling_en_passant;

        self.to_move = !self.to_move();
        debug_assert_ne!(!start_color, self.to_move());
        debug_assert!(!self.is_empty(mv.from()));
    }

    fn all_legal_moves(&self) -> Vec<Self::Move> {
        let (mut active_moves, mut moves) = sjadam_move_gen::all_legal_moves(&self);
        active_moves.append(&mut moves);
        active_moves
    }

    fn active_moves (&self) -> Vec<Self::Move> {
        let (active_moves, _) = sjadam_move_gen::all_legal_moves(&self);
        active_moves
    }
    
    #[inline(never)]
    fn eval_board (&self) -> f32 {
        debug_assert!(self.game_result() == None);
        let centre1 = 0b00000000_00000000_00000000_00011000_00011000_00000000_00000000_00000000;
        let centre2 = 0b00000000_00000000_00111100_00111100_00111100_00111100_00000000_00000000;
        let centre3 = 0b00000000_01111110_01111110_01111110_01111110_01111110_01111110_00000000;

        let centre_value = |bits: u64| (bits & centre1).count_ones() + (bits & centre2).count_ones()
            + (bits & centre3).count_ones();

        let value_modifiers = [(1.0, 0.0), (3.0, 0.3), (3.0, 0.15),
                               (5.0, 0.1), (9.0, 0.3), (0.0, 0.0)];
        
        let pieces_value = |piece_ids: [usize; 6]| piece_ids.iter()
            .map(|i| {
                
                let (piece_val, piece_center_val) = value_modifiers[i / 2];
                let piece_bitboard = self.bitboards[*i];
                piece_center_val * centre_value(piece_bitboard.board) as f32 +
                    piece_val * piece_bitboard.popcount() as f32
            })
            .sum();

        let white_val: f32 = pieces_value([0, 2, 4, 6, 8, 10]);
        let black_val: f32 = pieces_value([1, 3, 5, 7, 9, 11]);

        let tempo_bonus = (white_val + black_val.abs()) / 100.0;

        match self.to_move {
            White => white_val - black_val + tempo_bonus,
            Black => white_val - black_val - tempo_bonus,
        }
        
        /*
        TODO: Put pawn advancement eval back
        let pawn_val = match self.board[rank][file].piece_type() {
        Pawn => (rank as f32 - 3.5) * -0.1,
        _ => 0.0,
         */    
        
    }

    fn branch_factor() -> u64 {
        30
    }
}

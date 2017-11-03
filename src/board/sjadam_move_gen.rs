use board::sjadam_move::{SjadamMove, SjadamUndoMove};
use board::std_board::BoardIter;
use board::std_board::ChessBoard;
use board::std_board::Piece;
use board::std_board::Square;
use board::std_board::PieceType;
use board::std_board::PieceType::*;

use uci::UciBoard;
use search_algorithms::board::Color;
use search_algorithms::board::Color::*;
use search_algorithms::board::EvalBoard;
use search_algorithms::board::GameResult;
use board::sjadam_board::SjadamBoard;

use std::fmt;

lazy_static! {
    static ref ROOK_TABLE : [[u8; 256]; 32] = {
        let mut table = [[0; 256]; 32];
        for sjadam_squares in (0..256)
            .filter(|i| i & 0b1010_1010 == 0 || i & 0b0101_0101 == 0)
            .map(BitBoard::from_u64)
        {
            for all_pieces in (0..256).map(BitBoard::from_u64) {
                let mut target_squares = sjadam_squares.clone();
                for file in 0..8 {
                    if sjadam_squares.get(Square(file)) {
                        let mut cur_file = file.overflowing_sub(1).0;
                        while cur_file < 8 {
                            target_squares.set(Square(cur_file));
                            if all_pieces.get(Square(cur_file)) {
                                break;
                            }
                            cur_file = cur_file.overflowing_sub(1).0;
                        }
                        cur_file = file + 1;
                        while cur_file < 8 {
                            target_squares.set(Square(cur_file));
                            if all_pieces.get(Square(cur_file)) {
                                break;
                            }
                            cur_file += 1;
                        }
                    }
                }
                debug_assert_eq!(table[sjadam_lookup_index(sjadam_squares.rank(0))]
                                 [all_pieces.rank(0) as usize], 0);
                table[sjadam_lookup_index(sjadam_squares.rank(0))]
                    [all_pieces.rank(0) as usize]
                    = target_squares.rank(0);
            }
        }
        table
    };
    static ref KNIGHT_TABLE : [u64; 256] = {
        let mut table = [0; 256];
        let c = 1 << 18;
        let attacks = c << 17 | c << 15 | c << 10 | c << 6 | c | c >> 6 | c >> 10 | c >> 15 | c >> 17;
        let left_side = 0b11111100_11111100_11111100_11111100_11111100_11111100_11111100_11111100;
        let right_side = 0b00111111_00111111_00111111_00111111_00111111_00111111_00111111_00111111;
        for pieces in 0..256 {
            let mut targets : u64 = 0;
            for file in 0..2 {
                if (pieces >> file) % 2 != 0 {
                    targets |= right_side & (attacks >> (2 - file))
                }
            }
            for file in 2..6 {
                if (pieces >> file) % 2 != 0 {
                    targets |= attacks << (file - 2)
                }
            }
            for file in 6..8 {
                if (pieces >> file) % 2 != 0 {
                    targets |= left_side & (attacks << (file - 2))
                }
            }
            table[pieces] = targets;
        }
        table
    };
    static ref KING_TABLE : [u64; 256] = {
        let mut table = [0; 256];
        let attacks = 0b11100000_11100000_111;
        let left_side = 0b11111100_11111100_11111100_11111100_11111100_11111100_11111100_11111100;
        let right_side = 0b00111111_00111111_00111111_00111111_00111111_00111111_00111111_00111111;

        for pieces in 0..256 {
            let mut targets : u64 = 0;
            if pieces % 2 != 0 {
                targets |= right_side & (attacks >> 1);
            }
            for file in 1..7 {
                if (pieces >> file) % 2 != 0 {
                    targets |= attacks << (file - 1)
                }
            }
            if (pieces >> 7) % 2 != 0 {
                targets |= left_side & (attacks << 6);
            }
            table[pieces] = targets;
        }
        table
    };
}

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
    pub fn from_board<F: Fn(Piece) -> bool> (board: &ChessBoard, f: F) -> Self {
        let mut bit_board = BitBoard::empty();
        for square in BoardIter::new() {
            if f(board[square]) {
                bit_board.set(square);
            }
        }
        bit_board
    }
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

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct SjadamBitBoard {
    white_pieces: BitBoard,
    black_pieces: BitBoard,
    bitboards: [BitBoard; 12],
    to_move: Color,
    castling_en_passant: u8,
    half_move_clock: u8,
    move_num: u16,
}
impl SjadamBitBoard {
    fn from_chess_board(other: &ChessBoard) -> Self {
        let mut board = Self { white_pieces: BitBoard::empty(), black_pieces: BitBoard::empty(),
                               bitboards: [BitBoard::empty(); 12], to_move: other.to_move(),
                               castling_en_passant: other.castling_en_passant,
                               half_move_clock: other.half_move_clock,
                               move_num: other.move_num };
        for square in BoardIter::new() {
            if !other[square].is_empty() {
                board.set_piece_at_square(other[square], square);
            }
        }
        board.white_pieces = [0, 2, 4, 6, 8, 10].iter()
            .map(|&i| board.bitboards[i])
            .fold(BitBoard::empty(),
                  |acc, bitboard| BitBoard::from_u64(bitboard.board | acc.board));
        
        board.black_pieces = [1, 3, 5, 7, 9, 11].iter()
            .map(|&i| board.bitboards[i])
            .fold(BitBoard::empty(),
                  |acc, bitboard| BitBoard::from_u64(bitboard.board | acc.board));
        board
    }

    fn to_chess_board(&self) -> ChessBoard {
        let mut board = ChessBoard::start_board();
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
    
    fn get_piece(&self, piece: Piece) -> BitBoard {
        self.bitboards[piece as u32 as usize - 2]
    }

    fn get_square(&self, square: Square) -> Piece {
        if self.white_pieces.get(square) {
            [0, 2, 4, 6, 8, 10].iter()
                .map(|&i| self.bitboards[i])
                .enumerate()
                .find(|&(_, bitboard)| bitboard.get(square))
                .map(|(i, _)| Piece::new(PieceType::from_disc(i as u32 + 1).unwrap(), White))
                .unwrap()
        }
        else if self.black_pieces.get(square) {
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

    pub fn all_pieces(&self) -> BitBoard {
        BitBoard::from_u64(self.white_pieces.board | self.black_pieces.board)
    }

    pub fn piece_at_square(&self, piece: Piece, square: Square) -> bool {
        self.get_piece(piece).get(square)
    }

    fn set_piece_at_square(&mut self, piece: Piece, square: Square) {
        debug_assert!(!piece.is_empty());
        self.bitboards[piece as u8 as usize - 2].set(square);
    }

    fn clear_piece_at_square(&mut self, piece: Piece, square: Square) {
        debug_assert!(!piece.is_empty());
        self.bitboards[piece as u8 as usize - 2].clear(square);
    }

    fn move_piece(&mut self, piece: Piece, from: Square, to: Square) {
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
                //println!("byte << 4 was {:b}, byte={:b}", byte << 4, byte);
                self.castling_en_passant = self.castling_en_passant | ((byte << 4) | 0b1000_0000);
                //println!("Bitmap set to {:b}", self.castling_en_passant);
                //panic!();
            },
            None => self.castling_en_passant = self.castling_en_passant & 0b0000_1111,
        }
    }
}



impl UciBoard for SjadamBitBoard {
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
        //debug_assert_eq!(self.base_board[mv.from()].color(), Some(self.to_move()));
        //debug_assert_ne!(self.base_board[mv.to()].color(), Some(self.to_move()));
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

impl EvalBoard for SjadamBitBoard {
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
        debug_assert!(!self.get_square(mv.from()).is_empty(),
                      "Tried to do move {} from empty square at \n{:?}", mv, self);

        let en_passant = mv.en_passant_bitboard(&self);
        // TODO: Store piece moved in move struct?
        let piece_moved = self.get_square(mv.from()).piece_type();
        let undo_move = SjadamUndoMove {
            from: mv.from(), to: mv.to(),
            castling: mv.castling(), en_passant: en_passant,
            capture: self.get_square(mv.from()).piece_type(),
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
            && !self.piece_at_square(Piece::new(Pawn, start_color), mid_sq) {
                if start_color == White && mv.from().rank() == 6 {
                    self.set_en_passant_square(Some(mid_sq));
                }
                else if start_color == Black && mv.from().rank() == 1 {
                    self.set_en_passant_square(Some(mid_sq));
                }
            }
        self.move_piece(Piece::new(piece_moved, start_color), mv.from(), mv.to());
        
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
            self.clear_piece_at_square(Piece::new(Pawn, start_color),
                                       Square::from_ints(mv.to().file(), ep_square_rank));
        }
        else if (start_color == White && mv.to().rank() == 0)
            || (start_color == Black && mv.to().rank() == 7) {
                debug_assert!(!self.get_square(mv.to()).is_empty());
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

        self.move_piece(Piece::new(mv.piece_moved, start_color), mv.to(), mv.from());
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
            let ep_square_rank = if start_color == Black { mv.to().rank() + 1 } else { mv.to().rank() - 1 };
            self.set_piece_at_square(Piece::new(Pawn, !start_color),
                                     Square::from_ints(mv.to().file(), ep_square_rank));
        }
        self.half_move_clock = mv.old_half_move_clock;
        self.castling_en_passant = mv.old_castling_en_passant;

        self.to_move = !self.to_move();
        debug_assert_ne!(!start_color, self.to_move());
        debug_assert!(!self.get_square(mv.from()).is_empty());
    }

    fn all_legal_moves(&self) -> Vec<Self::Move> {
        let mut moves = Vec::with_capacity(300);

        let (friendly_pieces, opponent_pieces) = if self.to_move() == White {
            (self.white_pieces, self.black_pieces)
        }
        else {
            (self.black_pieces, self.white_pieces)
        };
        
        let all_pieces = BitBoard::from_u64(opponent_pieces.board | friendly_pieces.board);

        // TODO: Remove this loop, to avoid repeating bitboard.get_square(square) calls
        for square in BoardIter::new() {
            if !self.get_square(square).is_empty()
                && self.get_square(square).color().unwrap() == self.to_move()
            {
                legal_moves_for_square(&self, square, self.get_square(square).piece_type(),
                                       &mut moves);
            }
        }
        if self.to_move() == White {
            if self.can_castle_kingside(White)
                && all_pieces.rank(7) & 0b01100000 == 0 {
                    moves.push(SjadamMove::new(Square::E1, Square::G1, true));
                }
            if self.can_castle_queenside(White)
                && all_pieces.rank(7) & 0b1110 == 0 {
                    moves.push(SjadamMove::new(Square::E1, Square::C1, true));
                }
        }
        else {
            if self.can_castle_kingside(Black)
                && all_pieces.rank(0) & 0b0110_0000 == 0 {
                    moves.push(SjadamMove::new(Square::E8, Square::G8, true));
                }
            if self.can_castle_queenside(Black)
                && all_pieces.rank(0) & 0b1110 == 0 {
                    moves.push(SjadamMove::new(Square::E8, Square::C8, true));
                }
        }
        moves
    }
    
    #[inline(never)]
    fn eval_board (&self) -> f32 {

        let centre1 = 0b00000000_00000000_00000000_00011000_00011000_00000000_00000000_00000000;
        let centre2 = 0b00000000_00000000_00111100_00111100_00111100_00111100_00000000_00000000;
        let centre3 = 0b00000000_01111110_01111110_01111110_01111110_01111110_01111110_00000000;

        let centre_value = |bits: u64| (bits | centre1).count_ones() + (bits | centre2).count_ones()
            + (bits | centre3).count_ones();

        let pieces_value = |pieces: &[PieceType], color| pieces.iter()
            .map(|&piece_type| {
                let (piece_val, piece_center_val) = match piece_type {
                        Bishop => (3.0, 0.15),
                        Knight => (3.0, 0.3),
                        Queen => (9.0, 0.3),
                        Pawn => (1.0, 0.0),
                        Rook => (5.0, 0.1),
                        King => (0.0, 0.0),
                        Empty => unreachable!(),
                };
                let piece_bitboard = self.get_piece(Piece::new(piece_type, color));
                piece_center_val * centre_value(piece_bitboard.board) as f32 +
                    piece_val * piece_bitboard.popcount() as f32
            })
            .sum();

        let pieces = [Pawn, Knight, Bishop, Rook, Queen, King];
        
        let white_val: f32 = pieces_value(&pieces, White);
        let black_val: f32 = pieces_value(&pieces, Black);
        
        /*
        TODO: Put pawn advancement eval back
        let pawn_val = match self.board[rank][file].piece_type() {
        Pawn => (rank as f32 - 3.5) * -0.1,
        _ => 0.0,
         */    
        white_val - black_val
    }
}


#[inline(never)]
pub fn all_legal_moves(board: &SjadamBoard) -> Vec<SjadamMove> {
    let mut moves = Vec::with_capacity(300);
    let friendly_pieces =
        BitBoard::from_board(&board.base_board,
                             |piece| !piece.is_empty() &&
                             piece.color().unwrap() == board.to_move());
    let opponent_pieces =
        BitBoard::from_board(&board.base_board,
                             |piece| !piece.is_empty() &&
                             piece.color().unwrap() != board.to_move());
    
    let all_pieces = BitBoard::from_u64(opponent_pieces.board | friendly_pieces.board);

    let bitboard = SjadamBitBoard::from_chess_board(&board.base_board);

    // TODO: Remove this loop, to avoid repeating bitboard.get_square(square) calls
    for square in BoardIter::new() {
        if !board.base_board[square].is_empty()
            && board.base_board[square].color().unwrap() == board.to_move()
        {
            legal_moves_for_square(&bitboard, square, bitboard.get_square(square).piece_type(),
                                   &mut moves);
        }
    }
    if board.to_move() == White {
        if board.base_board.can_castle_kingside(White)
            && all_pieces.rank(7) & 0b01100000 == 0 {
                moves.push(SjadamMove::new(Square::E1, Square::G1, true));
            }
        if board.base_board.can_castle_queenside(White)
            && all_pieces.rank(7) & 0b1110 == 0 {
                moves.push(SjadamMove::new(Square::E1, Square::C1, true));
            }
    }
    else {
        if board.base_board.can_castle_kingside(Black)
            && all_pieces.rank(0) & 0b0110_0000 == 0 {
                moves.push(SjadamMove::new(Square::E8, Square::G8, true));
            }
        if board.base_board.can_castle_queenside(Black)
            && all_pieces.rank(0) & 0b1110 == 0 {
                moves.push(SjadamMove::new(Square::E8, Square::C8, true));
            }
    }
    moves
}

fn legal_moves_for_square(board: &SjadamBitBoard, square: Square, piece_type: PieceType,
                          bitboard_moves: &mut Vec<SjadamMove>) {

    //println!("Finding moves for {} at {} at board\n{:?}", piece_type, square, board);
    
    let mut sjadam_squares = BitBoard::empty();
    sjadam_squares.set(square);

    let (friendly_pieces, opponent_pieces) = if board.to_move() == White {
        (board.white_pieces, board.black_pieces)
    }
    else {
        (board.black_pieces, board.white_pieces)
    };
        
    let all_pieces = BitBoard::from_u64(opponent_pieces.board | friendly_pieces.board);
    
    sjadam_friendly_moves(&mut sjadam_squares, &friendly_pieces,
                          &all_pieces, square);

    sjadam_opponent_moves(&mut sjadam_squares, &opponent_pieces, &all_pieces);

    let moves : BitBoard = match piece_type {
        Rook => rook_moves(sjadam_squares, friendly_pieces, all_pieces),
        Bishop => bishop_moves(sjadam_squares, friendly_pieces, all_pieces),
        Queen => {
            let mut queen_moves = bishop_moves(sjadam_squares, friendly_pieces, all_pieces);
            queen_moves.board |= rook_moves(sjadam_squares, friendly_pieces, all_pieces).board;
            queen_moves
        },
        Knight => knight_moves(sjadam_squares, friendly_pieces),
        Pawn => {
            let mut all_pieces_pawns = all_pieces.clone();
            if let Some(ep_square) = board.en_passant_square() {
                all_pieces_pawns.set(ep_square);
            }
            if board.to_move() == White {
                pawn_moves_white(sjadam_squares, friendly_pieces, all_pieces_pawns)
            }
            else {
                pawn_moves_black(sjadam_squares, friendly_pieces, all_pieces_pawns)
            }
        },
        King => king_moves(sjadam_squares, friendly_pieces),
        _ => BitBoard::empty(),
    };
    
    for i in 0..64 {
        if moves.get(Square(i)) && Square(i) != square {
            bitboard_moves.push(SjadamMove::new(square, Square(i), false));
        }
    }
     
    /*
    for chess_move_square in BoardIter::new()
        .filter(|&i| moves.get(i) && i != square)
    { 
        debug_assert!(!board.base_board[square].is_empty());
        bitboard_moves.push(SjadamMove::new(square, chess_move_square, false));
    }*/
}

/// Recursively sets all available sjadam-move squares
#[inline(never)]
fn sjadam_friendly_moves(sjadam_squares: &mut BitBoard, friendly_pieces: &BitBoard,
                         all_pieces: &BitBoard, square: Square) {
    let Square(i) = square;
    for x in &[-1, 0, 1] {
        for y in &[-1, 0, 1] {
            let (file, rank) = (square.file_rank().0 as i8, square.file_rank().1 as i8);
            if (file <= 1 && *x == -1) || (file >= 6 && *x == 1)
                || (rank <= 1 && *y == -1) || (rank >= 6 && *y == 1) {
                    continue
                }
            let dest_square = Square(i.wrapping_add((16 * y) as u8).wrapping_add((2 * x) as u8));
            let jumping_square = Square(i.wrapping_add((8 * y) as u8).wrapping_add(*x as u8));
            if friendly_pieces.get(jumping_square) && !all_pieces.get(dest_square) &&
                !sjadam_squares.get(dest_square) {
                    sjadam_squares.set(dest_square);
                    sjadam_friendly_moves(sjadam_squares, friendly_pieces,
                                          all_pieces, dest_square);
                }
        }
    }
}

/// Sets all available opponent sjadam-move squares
#[inline(never)]
fn sjadam_opponent_moves(sjadam_squares: &mut BitBoard, opponent_pieces: &BitBoard,
                         all_pieces: &BitBoard) {
    let type1_mask = 0b00000000_01010101_00000000_01010101_00000000_01010101_00000000_01010101;
    let type2_mask = 0b00000000_10101010_00000000_10101010_00000000_10101010_00000000_10101010;
    let type3_mask = 0b01010101_00000000_01010101_00000000_01010101_00000000_01010101_00000000;
    
    let color_offset = if type1_mask & sjadam_squares.board != 0 { 0 }
    else if type2_mask & sjadam_squares.board != 0 { 1 }
    else if type3_mask & sjadam_squares.board != 0 { 8 }
    else { 9 };
    let old_sjadam_squares = sjadam_squares.clone();
    for i in [0, 2, 4, 6, 16, 18, 20, 22, 32, 34, 36, 38, 48, 50, 52, 54].iter()
        .map(|&i| i + color_offset)
        .filter(|&i| old_sjadam_squares.get(Square(i)))
    {
        let square = Square(i);
    //for square in BoardIter::new()
    //    .filter(|i|old_sjadam_squares.get(*i))
    //{
    //    let Square(i) = square;
        for x in &[-1, 0, 1] {
            for y in &[-1, 0, 1] {
                let (file, rank) = (square.file_rank().0 as i8, square.file_rank().1 as i8);
                if (file <= 1 && *x == -1) || (file >= 6 && *x == 1)
                    || (rank <= 1 && *y == -1) || (rank >= 6 && *y == 1) {
                        continue
                    }
                let dest_square = Square(i.wrapping_add((16 * y) as u8).wrapping_add((2 * x) as u8));
                let jumping_square = Square(i.wrapping_add((8 * y) as u8).wrapping_add(*x as u8));
                if opponent_pieces.get(jumping_square) && !all_pieces.get(dest_square) {
                    sjadam_squares.set(dest_square);
                }
            }
        }
    }
}

#[inline(never)]
fn king_moves(sjadam_squares: BitBoard, friendly_pieces: BitBoard) -> BitBoard {
    let mut moves = 0;
    for rank in 0..1 {
        let index = ((sjadam_squares.board >> (rank * 8)) & 255) as usize;
        moves |= KING_TABLE[index] >> ((- rank * 8) + 8);
    }
    for rank in 1..8 {
        let index = ((sjadam_squares.board >> (rank * 8)) & 255) as usize;
        moves |= KING_TABLE[index] << ((rank * 8) - 8);
    }
    BitBoard::from_u64(moves & !friendly_pieces.board)
}

#[inline(never)]
fn knight_moves(sjadam_squares: BitBoard, friendly_pieces: BitBoard) -> BitBoard {
    let mut moves = 0;
    for rank in 0..2 {
        let index = ((sjadam_squares.board >> (rank * 8)) & 255) as usize;
        moves |= KNIGHT_TABLE[index] >> ((- rank * 8) + 16);
    }
    for rank in 2..8 {
        let index = ((sjadam_squares.board >> (rank * 8)) & 255) as usize;
        moves |= KNIGHT_TABLE[index] << ((rank * 8) - 16);
    }
    BitBoard::from_u64(moves & !friendly_pieces.board)
}

const RIGHT_MASK : u64 = 0b01111111_01111111_01111111_01111111_01111111_01111111_01111111_01111111;
const LEFT_MASK : u64 = 0b11111110_11111110_11111110_11111110_11111110_11111110_11111110_11111110;
#[inline(never)]
fn pawn_moves_black(sjadam_squares: BitBoard, friendly_pieces: BitBoard,
              all_pieces: BitBoard) -> BitBoard {
    let opponent_pieces = BitBoard::from_u64(all_pieces.board ^ friendly_pieces.board);
    let left_captures = ((sjadam_squares.board & LEFT_MASK) << 7) & opponent_pieces.board; // & LEFT_MASK;
    let right_captures = ((sjadam_squares.board & RIGHT_MASK) << 9) & opponent_pieces.board; // & RIGHT_MASK;
    let forward = sjadam_squares.board << 8 & !all_pieces.board;
    let forward_two = (sjadam_squares.board & (255 << 8)) << 16 & !all_pieces.board & !(all_pieces.board << 8);
    BitBoard::from_u64(sjadam_squares.board | left_captures | right_captures | forward | forward_two)
}

#[inline(never)]
fn pawn_moves_white(sjadam_squares: BitBoard, friendly_pieces: BitBoard,
              all_pieces: BitBoard) -> BitBoard {
    let opponent_pieces = BitBoard::from_u64(all_pieces.board ^ friendly_pieces.board);
    
    let left_captures = ((sjadam_squares.board & RIGHT_MASK) >> 7) & opponent_pieces.board;
    let right_captures = ((sjadam_squares.board & LEFT_MASK) >> 9) & opponent_pieces.board;
    
    let forward = sjadam_squares.board >> 8 & !all_pieces.board;
    let forward_two = (sjadam_squares.board & (255 << 48)) >> 16 & !all_pieces.board & !(all_pieces.board >> 8);
    BitBoard::from_u64(sjadam_squares.board | left_captures | right_captures | forward | forward_two)
}

#[inline(never)]
fn bishop_moves(sjadam_squares: BitBoard, friendly_pieces: BitBoard,
                all_pieces: BitBoard) -> BitBoard {
    let mut bishop_moves = sjadam_squares.clone();
    let mut sjadam_squares_45 = sjadam_squares.rotate_45();
    
    let all_pieces_45 = all_pieces.rotate_45();
    for diagonal in 0..15 {
        let diagonal_bits = sjadam_squares_45.diagonal(diagonal);
        let mut target_rank = ROOK_TABLE
            [sjadam_lookup_index(diagonal_bits)][all_pieces_45.diagonal(diagonal) as usize];

        let len : u64 = if diagonal >= 8 { 15 - diagonal } else { diagonal + 1 } as u64;
        target_rank &= !(!(0 as u64) << len) as u8;
        let n = diagonal as i8 - 7;
        let offset = if n <= 0 { n * (-8)} else { 8 * (8 - n) + n };

        sjadam_squares_45.board |= (target_rank as u64) << offset;
    }
    let diagonal_moves = (0..7).fold(sjadam_squares_45, |acc, _| acc.rotate_45());

    let mut sjadam_squares_315 = sjadam_squares.rotate_315();
    let all_pieces_315 = all_pieces.rotate_315();
    
    for antidiagonal in (-7)..8 {
        let diagonal_bits = sjadam_squares_315.antidiagonal(antidiagonal);
        let mut target_rank = ROOK_TABLE
            [sjadam_lookup_index(diagonal_bits)][all_pieces_315.antidiagonal(antidiagonal) as usize];
        let len = 8 - antidiagonal.abs();
        target_rank &= !(!(0 as u64) << len) as u8;
        let n = antidiagonal;
        let offset = if n <= 0 { n * (-8) - n} else { 8 * (8 - n) };

        sjadam_squares_315.board |= (target_rank as u64) << offset;
    }
    sjadam_squares_315 = (0..7).fold(sjadam_squares_315, |acc, _| acc.rotate_315());
    
    bishop_moves.board |= diagonal_moves.board;
    bishop_moves.board |= sjadam_squares_315.board;
    bishop_moves.board &= !friendly_pieces.board;
    bishop_moves
}
#[inline(never)]
fn rook_moves(sjadam_squares: BitBoard, friendly_pieces: BitBoard,
              all_pieces: BitBoard) -> BitBoard {
    let mut sjadam_squares_rotated = sjadam_squares.rotate();
    let all_pieces_rotated = all_pieces.rotate();
    
    for file in 0..8 {
        lookup_rook(file, &mut sjadam_squares_rotated, all_pieces_rotated);
    }
    let mut rook_moves = sjadam_squares_rotated.rotate_270();
    
    rook_moves.board &= !friendly_pieces.board;
    let mut horizontal_moves = sjadam_squares.clone();
    for rank in 0..8 {
        lookup_rook(rank, &mut horizontal_moves, all_pieces);
    }
    horizontal_moves.board &= !friendly_pieces.board;
    rook_moves.board |= horizontal_moves.board;
    rook_moves
}

/// Take the index of a rank, and a bitboard of available sjadam squares,
/// and set all available rook moves on the rank
/// Also sets captures of own pieces
fn lookup_rook(rank: u8, sjadam_squares: &mut BitBoard, all_pieces: BitBoard) {
    let rank_bits = sjadam_squares.rank(rank);

    let target_rank = ROOK_TABLE
        [sjadam_lookup_index(rank_bits)][all_pieces.rank(rank) as usize];
    sjadam_squares.board |= (target_rank as u64) << (8 * rank as u64);
}
    
fn sjadam_lookup_index(rank_bits: u8) -> usize {
    let mut index;
    if rank_bits.leading_zeros() % 2 == 0 {
        index = 16;
        index |= (rank_bits & 2) >> 1;
        index |= (rank_bits & 8) >> 2;
        index |= (rank_bits & 32) >> 3;
        index |= (rank_bits & 128) >> 4;
    }
    else {
        index = 0;
        index |= rank_bits & 1;
        index |= (rank_bits & 4) >> 1;
        index |= (rank_bits & 16) >> 2;
        index |= (rank_bits & 64) >> 3;
    }
    debug_assert!(index < 32);
    index as usize
}

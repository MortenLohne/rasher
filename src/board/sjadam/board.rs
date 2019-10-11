use board::chess::board;
use board::chess::board::{ChessBoard, Piece, PieceType};
use board::chess::board::PieceType::*;
use board::sjadam::mv::{SjadamMove, SjadamReverseMove};
use board::sjadam::move_gen;
use chess_bitboard::bitboard::BitBoard;
use chess_bitboard::bitboard::Square;
use chess_bitboard::bitboard;

use board_game_traits::board::EvalBoard;
use board_game_traits::board::Color;
use board_game_traits::board::Color::*;
use board_game_traits::board::GameResult;

use pgn_traits::pgn;
use board_tuning::TunableBoard;

use std::fmt;
use pgn_traits::pgn::PgnBoard;

use rand;
use rand::Rng;
use rand::SeedableRng;

const WHITE_SQUARES : BitBoard = BitBoard {
    board: 0b10101010_01010101_10101010_01010101_10101010_01010101_10101010_01010101
};

const BLACK_SQUARES : BitBoard = BitBoard {
    board: 0b01010101_10101010_01010101_10101010_01010101_10101010_01010101_10101010
};

pub const SJADAM_SQUARE_TYPES : [BitBoard; 4] =
    [   BitBoard {
        board: 0b00000000_01010101_00000000_01010101_00000000_01010101_00000000_01010101
    },
        BitBoard {
            board: 0b00000000_10101010_00000000_10101010_00000000_10101010_00000000_10101010
        },
        BitBoard {
            board: 0b01010101_00000000_01010101_00000000_01010101_00000000_01010101_00000000
        },
    BitBoard {
        board: 0b10101010_00000000_10101010_00000000_10101010_00000000_10101010_00000000
    }];

lazy_static! {
    static ref DIAGONAL_NEIGHBOURS : [BitBoard; 64] = {
        let mut table : [BitBoard; 64] = [BitBoard::empty(); 64];

        for i in 0..64 {
            let mut board = BitBoard::empty();
            let (file, rank) = Square(i).file_rank();
            
            for &x in &[u8::overflowing_sub(file, 1).0, file + 1] {
                for &y in &[u8::overflowing_sub(rank, 1).0, rank + 1] {
                    if x < 8 && y < 8 {
                        board = board.set(Square::from_ints(x, y));
                    }
                }
            }
            table[i as usize] = board;
        }
        table
    };

    static ref RANK_NEIGHBOURS : [BitBoard; 64] = {
        let mut table : [BitBoard; 64] = [BitBoard::empty(); 64];
        for i in 0..64 {
            let (file, rank) = Square(i).file_rank();
            let mut board = BitBoard::empty();
            if file > 0 {
                board = board.set(Square::from_ints(file - 1, rank));
            }
            if file < 7 {
                board = board.set(Square::from_ints(file + 1, rank));
            }
            table[i as usize] = board;
        }
        table
    };

    static ref FILE_NEIGHBOURS : [BitBoard; 64] = {
        let mut table : [BitBoard; 64] = [BitBoard::empty(); 64];
        
        for i in 0..64 {
            let (file, rank) = Square(i).file_rank();
            let mut board = BitBoard::empty();
            if rank > 0 {
                board = board.set(Square::from_ints(file, rank - 1));
            }
            if rank < 7 {
                board = board.set(Square::from_ints(file, rank + 1));
            }
            table[i as usize] = board;
        }
        table
    };


    static ref ORTHOGONAL_NEIGHBOURS : [BitBoard; 64] = {
        let mut table : [BitBoard; 64] = [BitBoard::empty(); 64];

        for i in 0..64 {
            table[i] = RANK_NEIGHBOURS[i] | FILE_NEIGHBOURS[i];
        }
        table
    };

    /// For each square and color, store squares from which the player's pawns can attack the square
    /// Always has 1 or 2 bits set
    static ref PAWN_ATTACK_SQUARES : [[BitBoard; 2]; 64] = {
        let mut table : [[BitBoard; 2]; 64] = [[BitBoard::empty(); 2]; 64];

        for i in 0..64 {
            let square = Square(i as u8);
            let dia_neighbours = diagonal_neighbours(square);
            table[i][0] = dia_neighbours.into_iter().filter(|sq| sq.rank() > square.rank()).collect();
            table[i][1] = dia_neighbours.into_iter().filter(|sq| sq.rank() < square.rank()).collect();
        }
        table
    };

    pub static ref SQUARE_SQUARES : [BitBoard; 16] = {
        let mut table = [BitBoard::empty(); 16];
        for i in 0..16 {
            let square : u8 = (i % 4) * 2 + (i / 4) * 16;
            for j in [0, 1, 8, 9].iter() {
                table[i as usize] = table[i as usize].set(Square(square + j));
            }
        }
        table
    };

    // 768 keys for each piece on square, 8 for en passant squares,
    // 1 for side to move, 16 for castling rights, 5 for repetitions
    static ref ZOBRIST_KEYS : [u64; 798] = {
        let mut table = [0; 798];
        let mut rng = rand::rngs::StdRng::seed_from_u64(42);
        for entry in table.iter_mut() {
            *entry = rng.gen();
        }
        table
    };
}

pub fn pawn_attack_squares(square: Square, color: Color) -> BitBoard {
    PAWN_ATTACK_SQUARES[square.0 as usize][color.disc()]
}

pub fn diagonal_neighbours(square: Square) -> BitBoard {
    DIAGONAL_NEIGHBOURS[square.0 as usize]
}

pub fn orthogonal_neighbours(square: Square) -> BitBoard {
    ORTHOGONAL_NEIGHBOURS[square.0 as usize]
}

#[derive(Clone)]
pub struct SjadamBoard {
    white_pieces: BitBoard,
    black_pieces: BitBoard,
    hash: u64,
    bitboards: [BitBoard; 12],
    to_move: Color,
    last_move: Option<SjadamMove>,
    castling_en_passant: u8,
    half_move_clock: u8,
    move_num: u16,
    move_history: Vec<u64>,
    repetitions: u8, // starts at 1 ("number of occurrences")
}

impl PartialEq for SjadamBoard {
    fn eq(&self, other: &Self) -> bool {
        self.bitboards == other.bitboards
            && self.to_move == other.to_move
            && self.castling_en_passant == other.castling_en_passant
            && self.repetitions == other.repetitions
    }
}

impl Eq for SjadamBoard {}

use std::hash::Hasher;
use std::hash::Hash;
use board_game_traits::board::Board;
use board_game_traits::board::ExtendedBoard;
use board::sjadam::mv::SjadamReverseNullMove;

impl Hash for SjadamBoard {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}

impl SjadamBoard {
    fn hash_from_scratch(&self) -> u64 {
        let mut hash = 0;
        for (i, bitboard) in self.bitboards.iter().enumerate() {
            for Square(square) in bitboard.into_iter() {
                hash ^= ZOBRIST_KEYS[i * 64 + square as usize];
            }
        }
        if self.to_move == Black {
            hash ^= ZOBRIST_KEYS[768];
        }
        if let Some(en_passant_square) = self.en_passant_square() {
            hash ^= ZOBRIST_KEYS[769 + en_passant_square.file() as usize];
        }
        hash ^= ZOBRIST_KEYS[777 + (self.castling_en_passant & 0b1111) as usize];
        debug_assert!(self.repetitions > 0 && self.repetitions <= 5);
        hash ^= ZOBRIST_KEYS[792 + self.repetitions as usize];
        hash
    }
    
    pub fn from_chess_board(other: &ChessBoard) -> Self {
        let mut board = Self { bitboards: [BitBoard::empty(); 12],
                               white_pieces: BitBoard::empty(), black_pieces: BitBoard::empty(),
                               hash: 0,
                               to_move: other.side_to_move(),
                               last_move: None,
                               castling_en_passant: other.castling_en_passant,
                               half_move_clock: other.half_move_clock,
                               move_num: other.move_num,
                               move_history: Vec::with_capacity(10),
                               repetitions: 1};

        for square in board::BoardIter::new() {
            let (file, rank) = square.file_rank();
            if !other[square].is_empty() {
                board.set_piece_at_square(other[square], Square::from_ints(file, 7 - rank));
            }
        }
        board.hash = board.hash_from_scratch();
        debug_assert!(board.half_move_clock <= 100);
        board
    }

    pub fn to_chess_board(&self) -> ChessBoard {
        let mut board = ChessBoard::empty();
        for square in bitboard::SquaresIterator::new() {
            let (file, rank) = square.file_rank();
            board[board::Square::from_ints(file, 7 - rank)] = self.get_square(square);
        }
        board.castling_en_passant = self.castling_en_passant;
        board.to_move = self.side_to_move();
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
                .map(|(i, _)| Piece::from_type_color(PieceType::from_disc(i as u32 + 1).unwrap(), White))
                .unwrap()
        }
        else if self.black_pieces().get(square) {
            [1, 3, 5, 7, 9, 11].iter()
                .map(|&i| self.bitboards[i])
                .enumerate()
                .find(|&(_, bitboard)| bitboard.get(square))
                .map(|(i, _)| Piece::from_type_color(PieceType::from_disc(i as u32 + 1).unwrap(), Black))
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

    pub fn last_move(&self) -> Option<SjadamMove> { self.last_move.clone() }

    pub fn all_pieces(&self) -> BitBoard {
        self.white_pieces() | self.black_pieces()
    }

    pub fn piece_at_square(&self, piece: Piece, square: Square) -> bool {
        self.get_piece(piece).get(square)
    }

    pub fn set_piece_at_square(&mut self, piece: Piece, square: Square) {
        debug_assert!(!piece.is_empty());
        self.bitboards[piece as u8 as usize - 2] = self.bitboards[piece as u8 as usize - 2].set(square);
        if piece.color().unwrap() == White {
            self.white_pieces = self.white_pieces.set(square);
        }
        else {
            self.black_pieces = self.black_pieces.set(square);
        }
        self.hash ^= ZOBRIST_KEYS[(64 * (piece as u16 - 2) + square.0 as u16) as usize]
    }

    pub fn clear_piece_at_square(&mut self, piece: Piece, square: Square) {
        debug_assert!(!piece.is_empty());
        self.bitboards[piece as u8 as usize - 2] = self.bitboards[piece as u8 as usize - 2].clear(square);
        if piece.color().unwrap() == White {
            self.white_pieces = self.white_pieces.clear(square);
        }
        else {
            self.black_pieces = self.black_pieces.clear(square);
        }
        self.hash ^= ZOBRIST_KEYS[(64 * (piece as u16 - 2) + square.0 as u16) as usize]
    }

    pub fn move_piece(&mut self, piece: Piece, from: Square, to: Square) {
        debug_assert!(self.piece_at_square(piece, from), "No {} to move from {} to {} on \n{:?}",
                      piece, from, to, self);
        self.set_piece_at_square(piece, to);
        self.clear_piece_at_square(piece, from);
        debug_assert!(!self.piece_at_square(piece, from), "{} is not empty after {} from {} to {} on \n{:?}",
                      from, piece, from, to, self);
    }

    fn castling_en_passant_key(&self) -> u64 {
        let mut hash = 0;
        if let Some(en_passant_square) = self.en_passant_square() {
            hash ^= ZOBRIST_KEYS[769 + en_passant_square.file() as usize];
        }
        hash ^= ZOBRIST_KEYS[777 + (self.castling_en_passant & 0b1111) as usize];
        hash
    }

    pub fn disable_castling(&mut self, color: Color) {
        self.hash ^= ZOBRIST_KEYS[777 + (self.castling_en_passant & 0b1111) as usize];
        match color {
            White => self.castling_en_passant &= 0b1111_1100,
            Black => self.castling_en_passant &= 0b1111_0011,
        }
        self.hash ^= ZOBRIST_KEYS[777 + (self.castling_en_passant & 0b1111) as usize];
    }
    pub fn disable_castling_queenside(&mut self, color: Color) {
        self.hash ^= ZOBRIST_KEYS[777 + (self.castling_en_passant & 0b1111) as usize];
        match color {
            White => self.castling_en_passant &= 0b1111_1101,
            Black => self.castling_en_passant &= 0b1111_0111,
        }
        self.hash ^= ZOBRIST_KEYS[777 + (self.castling_en_passant & 0b1111) as usize];

    }
    pub fn disable_castling_kingside(&mut self, color: Color) {
        self.hash ^= ZOBRIST_KEYS[777 + (self.castling_en_passant & 0b1111) as usize];
        match color {
            White => self.castling_en_passant &= 0b1111_1110,
            Black => self.castling_en_passant &= 0b1111_1011,
        }
        self.hash ^= ZOBRIST_KEYS[777 + (self.castling_en_passant & 0b1111) as usize];
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
            let rank = if self.to_move == Black { 2 } else { 5 };
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
                byte |= 0b1000_0000;
                self.castling_en_passant |= (byte << 4) | 0b1000_0000;
            },
            None => self.castling_en_passant &= 0b0000_1111,
        }
    }
}

impl fmt::Debug for SjadamBoard {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Debug::fmt(&self.to_chess_board(), fmt)?;
        writeln!(fmt, "Hash: {}, repetitions: {}", self.hash, self.repetitions)?;
        write!(fmt, "Move history: {:?}, last move: {:?}", self.move_history, self.last_move)
    }
}

impl PgnBoard for SjadamBoard {
    fn from_fen(fen: &str) -> Result<Self, pgn::Error> {
        ChessBoard::from_fen(fen).map(|b| Self::from_chess_board(&b))
    }

    fn to_fen(&self) -> String {
        self.to_chess_board().to_fen()
    }

    fn move_from_lan(&self, input: &str) -> Result<Self::Move, pgn::Error> {
        if input.len() < 4 {
            return Err(pgn::Error::new(
                pgn::ErrorKind::ParseError,
                format!("Move \"{}\" was too short to parse", input)))
        }
        #[cfg(feature = "legacy_sjadam_move_format")]
        {
            if input.len() == 5 && (input.as_bytes()[4] as char == '-' || input.as_bytes()[0] as char == '-') || input.len() == 6 {
                let mut chars = input.chars().peekable();
                if *chars.peek().unwrap() == '-' {
                    let from = square_from_alg(&input[1..3])?;
                    let to = square_from_alg(&input[3..5])?;
                    // TODO: Check for castling
                    return Ok(SjadamMove::new(from, to, false,
                                              self.get_square(from).piece_type()))
                }
                let alg : String = chars.by_ref().collect();
                let from = square_from_alg(&alg[0..2])?;
                let sjadam = square_from_alg(&alg[2..4])?;
                if alg.chars().last().unwrap() == '-' {
                    return Ok(SjadamMove::new(from, sjadam,
                                              false, self.get_square(from).piece_type()));
                }
                else {
                    let to = square_from_alg(&alg[4..])?;
                    return Ok(SjadamMove::new(from, to, false,
                                              self.get_square(from).piece_type()));
                }
            }
        }
        if input.len() > 5 {
            return Err(pgn::Error::new(
                pgn::ErrorKind::ParseError,
                format!("Move \"{}\" was too long to parse", input)))
        }
        let from = square_from_alg(&input[0..2]).map_err(|err|
            pgn::Error::new_caused_by(
            pgn::ErrorKind::ParseError,
            format!("Invalid square in move {}", input), err))?;
        let to = square_from_alg(&input[2..4]).map_err(|err|
            pgn::Error::new_caused_by(
                pgn::ErrorKind::ParseError,
                format!("Invalid square in move {}", input), err))?;
        debug_assert!(!self.get_square(from).is_empty(), "Cannot parse move {} on \n{:?}",
                      input, self);
        match input.len() {
            4 => Ok(SjadamMove::new(from, to, false,
                                    self.get_square(from).piece_type())),
            5 if input.as_bytes()[4] == b'c' =>
                Ok(SjadamMove::new(from, to, true,
                                   self.get_square(from).piece_type())),
            _ => Err(pgn::Error::new(
                pgn::ErrorKind::ParseError,
                format!("Couldn't parse move {}", input)))
        }
    }
    fn move_to_lan(&self, mv: &Self::Move) -> String {
        #[cfg(feature = "legacy_sjadam_move_format")]
        {
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
            let knight_neighbours = |square: i8| [square - 17, square - 15, square - 10, square - 6,
                                                  square + 6, square + 10, square + 15, square + 17]
                .iter().cloned()
                .filter(|&sq| sq >= 0 && sq < 64)
                .map(|sq| Square(sq as u8))
                .filter(|&sq| self.is_empty(sq) || sq == mv.from())
                .collect::<Vec<Square>>();

            let pawn_neighbours = |square: i8| [square + 7, square + 9]
                .iter().cloned()
                .map(|sq| if self.side_to_move() == Black { sq - 16 } else { sq } )
                .filter(|&sq| sq >= 0 && sq < 64)
                .map(|sq| Square(sq as u8))
                .filter(|&sq| self.is_empty(sq) || sq == mv.from())
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
                    Knight => *knight_neighbours(mv.to().0 as i8).get(0)
                        .expect(&format!("Couldn't reconstruct move {} on \n{:?}",
                                        mv, self)),
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
        #[cfg(not(feature = "legacy_sjadam_move_format"))]
        mv.to_string()
    }

    fn move_to_san(&self, mv: &<Self as Board>::Move) -> String {
        let color = self.side_to_move();
        let piece_type = self.get_square(mv.from()).piece_type();

        let mut output = String::new();

        if mv.castling() && mv.to().file() == 2 {
            output.push_str("0-0-0");
        }
            else if mv.castling() && mv.to().file() == 6 {
                output.push_str("0-0");
            }
                else {
                    if piece_type != Pawn {
                        output.push(piece_type.letter());
                    }

                    let mut moves = vec![];
                    self.generate_moves(&mut moves);

                    let alternative_from_squares = moves.iter()
                        .filter(|&cand_mv| cand_mv.from() != mv.from())
                        .filter(|&cand_mv|
                            cand_mv.to() == mv.to() && self.get_square(cand_mv.from()) == self.get_square(mv.from()))
                        .map(|cand_mv| cand_mv.from())
                        .collect::<Vec<_>>();

                    if !(alternative_from_squares.is_empty()
                        && (piece_type != Pawn || mv.from().file() == mv.to().file()))
                    {
                        // Disambiguate with departure file
                        if alternative_from_squares.iter().all(|square| square.file() != mv.from().file()) {
                            output.push(mv.from().to_string().chars().next().unwrap());
                        }
                        // Disambiguate with departure rank
                        else if alternative_from_squares.iter().all(|square| square.rank() != mv.from().rank()) {
                            output.push(mv.from().to_string().chars().last().unwrap());
                        }
                        // Disambiguate with full square
                        else {
                            output.push_str(&mv.from().to_string());
                        };
                    }

                    if self.get_square(mv.to()) != Piece::Empty {
                        output.push('x');
                    }

                    output.push_str(&mv.to().to_string());

                    match (color, mv.to().rank()) {
                        (White, 7) | (Black, 0) if piece_type != Queen && piece_type != King => output.push_str("=Q"),
                        _ => (),
                    };
                }

        // TODO: Add check or checkmate annotations

        return output;
    }

    fn move_from_san(&self, input: &str) -> Result<<Self as Board>::Move, pgn::Error> {
        if input == "0-0-0" || input == "0-0-0+" || input == "0-0-0#" {
            match self.side_to_move() {
                White => return self.move_from_lan("e1c1c"),
                Black => return self.move_from_lan("e8c8c"),
            }
        }

            else if input == "0-0" || input == "0-0+" || input == "0-0#" {
                match self.side_to_move() {
                    White => return self.move_from_lan("e1g1c"),
                    Black => return self.move_from_lan("e8g8c"),
                }
            }

        if input.chars().count() < 2 {
            return Err(pgn::Error::new(pgn::ErrorKind::ParseError,
                                       format!("Invalid move {}: Too short at length {:?}",
                                               input, input.chars().count())));
        }

        let piece_type = PieceType::from_letter(input.chars().next().unwrap())
            .unwrap_or(PieceType::Pawn);

        let mut reverse_chars = input.chars().rev().peekable();

        if reverse_chars.peek() == Some(&'+') || reverse_chars.peek() == Some(&'#') {
            reverse_chars.next();
        }

        if reverse_chars.peek() == Some(&'Q') {
            reverse_chars.next();
            if reverse_chars.next() != Some('=') {
                return Err(pgn::Error::new(pgn::ErrorKind::ParseError,
                                           format!("Illegal promotion on {}", input)));
            }
        }

        let dest_square =
            square_from_alg(&[reverse_chars.next(), reverse_chars.next()]
                .iter().rev()
                .filter_map(|&ch| ch)
                .collect::<String>())
                .map_err(|err| pgn::Error::new_caused_by(
                    pgn::ErrorKind::ParseError,
                    format!("Illegal move {}", input),
                    err))?;

        if reverse_chars.peek() == Some(&'x') {
            reverse_chars.next();
        }

        let mut disambig_string = reverse_chars
            .take_while(|&ch| PieceType::from_letter(ch).is_none())
            .collect::<String>();
        disambig_string = disambig_string.chars().rev().collect();

        let move_filter : Box<Fn(&Self::Move) -> bool> =
            match (disambig_string.chars().count(), disambig_string.chars().next()) {
                (0, None) => Box::new(|_| true),

                (1, Some(rank)) if rank >= '1' && rank <= '8' =>
                    Box::new(move |mv: &Self::Move| mv.from().rank() == (rank as u8 - b'1')),

                (1, Some(file)) if file >= 'a' && file <= 'h' =>
                    Box::new(move |mv: &Self::Move| mv.from().file() == (file as u8 - b'a')),

                (2, _) if square_from_alg(&disambig_string).is_ok() =>
                    Box::new(move |mv: &Self::Move| mv.from() == square_from_alg(&disambig_string).unwrap()),

                _ => return Err(pgn::Error::new(pgn::ErrorKind::ParseError,
                                                format!("Invalid move disambiguation {} in {}",
                                                        disambig_string, input))),
            };

        let mut moves = vec![];
        self.generate_moves(&mut moves);
        let filtered_moves = moves.iter()
            .filter(|mv|
                mv.to() == dest_square
                    && self.get_square(mv.from()).piece_type() == piece_type
                    && !mv.castling())
            .filter(|mv| move_filter(mv))
            .collect::<Vec<_>>();

        if filtered_moves.len() > 1 {
            Err(pgn::Error::new(pgn::ErrorKind::AmbiguousMove, format!("{} could be any of {:?}", input, filtered_moves)))
        }
            else if filtered_moves.is_empty() {
                Err(pgn::Error::new(pgn::ErrorKind::IllegalMove,
                                    format!("{} ({} to {}) {:?} {:?} is not a legal move in the position:\n{:?}",
                                            input, piece_type, dest_square, moves.iter().filter(|mv| move_filter(mv)).collect::<Vec<_>>(),
                                            moves.iter().filter(|mv| mv.to() == dest_square && self.get_square(mv.from()).piece_type() == piece_type).collect::<Vec<_>>(), self)))
            }
                else {
                    Ok(filtered_moves[0].clone())
                }
    }
}


impl Board for SjadamBoard {
    type Move = SjadamMove;
    type ReverseMove = SjadamReverseMove;

    fn side_to_move(&self) -> Color {
        self.to_move
    }

    fn start_board() -> Self {
        Self::from_chess_board(&ChessBoard::start_board())
    }

    fn game_result(&self) -> Option<GameResult> {
        // In sjadam, king may be actually captured.
        // Check if the king is gone
        match (!self.get_piece(Piece::from_type_color(King, White)).is_empty(),
               !self.get_piece(Piece::from_type_color(King, Black)).is_empty()) {
            (true, false) => Some(GameResult::WhiteWin),
            (false, true) => Some(GameResult::BlackWin),
            (false, false) => panic!("Neither side has a king on the board:\n{:?}", self),
            (true, true) => {
                if self.half_move_clock >= 100 || self.repetitions >= 3 {
                    Some(GameResult::Draw)
                }
                else {
                    None
                }
            },
        }
    }

    fn do_move(&mut self, mv: Self::Move) -> Self::ReverseMove {
        let start_color = self.side_to_move();
        debug_assert_ne!(mv.from(), mv.to());
        debug_assert!(!self.is_empty(mv.from()),
                      "Tried to do move {} from empty square at \n{:?}", mv, self);



        let en_passant = mv.is_en_passant_move(self);
        let reverse_move = SjadamReverseMove {
            from: mv.from(), to: mv.to(),
            castling: mv.castling(), en_passant,
            capture: self.get_square(mv.to()).piece_type(),
            old_last_move: self.last_move.clone(),
            piece_moved: mv.piece_moved(),
            old_castling_en_passant: self.castling_en_passant,
            old_half_move_clock: self.half_move_clock,
            old_repetitions: self.repetitions,
            old_hash: self.hash
        };
        debug_assert!(mv.piece_moved() != PieceType::Empty,
                      "Empty piece_moved when doing {} on \n{:?}", mv, self);

        self.move_history.push(self.hash);
        debug_assert!(self.half_move_clock <= 100, "half_move_clock was {}", self.half_move_clock);
        self.half_move_clock += 1; // Gets zeroed later on on captures or promotions
        debug_assert!(self.half_move_clock <= 100, "half_move_clock was {}", self.half_move_clock);

        // Clear old en passant zobrist
        if let Some(en_passant_square) = self.en_passant_square() {
            self.hash ^= ZOBRIST_KEYS[769 + en_passant_square.file() as usize];
        }

        self.set_en_passant_square(None);

        // Remove castling priviledges on king moves
        if mv.piece_moved() == PieceType::King {
            let color = self.side_to_move();
            self.disable_castling(color);
        }

        // Remove castling privileges if anything moves from or to a corner
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
        if mv.piece_moved() == Pawn && mv.from().file() == mv.to().file()
            && i8::abs(mv.from().rank() as i8 - mv.to().rank() as i8) == 2
            && !self.piece_at_square(Piece::from_type_color(Pawn, start_color), mid_sq)
            && (start_color == White && mv.from().rank() == 1
            || start_color == Black && mv.from().rank() == 6)
            {
                self.set_en_passant_square(Some(mid_sq));
                self.hash ^= ZOBRIST_KEYS[769 + mid_sq.file() as usize];

            }

        self.move_piece(Piece::from_type_color(mv.piece_moved(), start_color), mv.from(), mv.to());

        if reverse_move.capture != Empty {
            self.clear_piece_at_square(Piece::from_type_color(reverse_move.capture, !start_color), mv.to());
            self.half_move_clock = 0;
        }

        if mv.castling() {
            // Move the rook too
            let (rook_from, rook_to) = if mv.to().file() == 6 { (7, 5) } else { (0, 3) };
            self.move_piece(Piece::from_type_color(Rook, start_color),
                            Square::from_ints(rook_from, mv.from().rank()),
                            Square::from_ints(rook_to, mv.from().rank()));
        }
            else if en_passant {
                // Remove the captured pawn
                let ep_square_rank = if start_color == Black { mv.to().rank() + 1 } else { mv.to().rank() - 1 };
                self.clear_piece_at_square(Piece::from_type_color(Pawn, !start_color),
                                           Square::from_ints(mv.to().file(), ep_square_rank));
            }
                else if (start_color == White && mv.to().rank() == 7)
                    || (start_color == Black && mv.to().rank() == 0)
                    {
                        // Promote to queen
                        debug_assert!(!self.is_empty(mv.to()));

                        if !(self.piece_at_square(Piece::from_type_color(King, start_color), mv.to())
                        || self.piece_at_square(Piece::from_type_color(Queen, start_color), mv.to())) {
                            self.clear_piece_at_square(Piece::from_type_color(mv.piece_moved(), start_color), mv.to());
                            self.set_piece_at_square(Piece::from_type_color(Queen, start_color), mv.to());
                            self.half_move_clock = 0;
                        }
                    }

        self.to_move = !self.side_to_move();
        self.hash ^= ZOBRIST_KEYS[768];

        // Last step: Detect repetitions
        // The hash is now fully updated, *except* for any possible repetition
        // Remove repetition from hash entirely
        self.hash ^= ZOBRIST_KEYS[792 + self.repetitions as usize];

        if self.half_move_clock >= 4 {
            for repetition in 1..=5 {
                let key = ZOBRIST_KEYS[792 + repetition];
                self.hash ^= key;

                if repetition == 5 {
                    self.repetitions = 5;
                }
                    else if self.move_history.iter().rev()
                        .take(self.half_move_clock as usize)
                        .skip(3)
                        .any(|&hash| hash == self.hash)
                        {
                            self.hash ^= key;
                        }
                        else {
                            self.repetitions = repetition as u8;
                            break;
                        }
            }
        }
            else {
                self.repetitions = 1;
                self.hash ^= ZOBRIST_KEYS[793];
            }

        debug_assert_ne!(start_color, self.side_to_move());
        debug_assert!(self.castling_en_passant & 15 <= reverse_move.old_castling_en_passant & 15);
        debug_assert_eq!(self.hash, self.hash_from_scratch(),
                         "Failed to create correct hash after {:?} on board\n{:?}", mv, self);
        self.last_move = Some(mv);
        reverse_move
    }

    fn reverse_move(&mut self, mv: Self::ReverseMove) {
        let start_color = !self.side_to_move();

        let old_hash = self.move_history.pop();
        debug_assert!(old_hash.is_some());

        if mv.piece_moved != Queen
            && self.piece_at_square(Piece::from_type_color(Queen, start_color), mv.to()) {
            self.clear_piece_at_square(Piece::from_type_color(Queen, start_color), mv.to());
            self.set_piece_at_square(Piece::from_type_color(mv.piece_moved, start_color), mv.from());
        }
            else { // If move was not promotion
                self.move_piece(Piece::from_type_color(mv.piece_moved, start_color), mv.to(), mv.from());
            }

        if mv.capture != Empty {
            self.set_piece_at_square(Piece::from_type_color(mv.capture, !start_color), mv.to());
        }

        if mv.castling() {
            // Move the rook too
            let (rook_from, rook_to) = if mv.to().file() == 6 { (7, 5) } else { (0, 3) };
            self.move_piece(Piece::from_type_color(Rook, start_color),
                            Square::from_ints(rook_to, mv.from().rank()),
                            Square::from_ints(rook_from, mv.from().rank()));
        }
            else if mv.en_passant() {
                // Replace the captured pawn
                let ep_square_rank = if start_color == Black { mv.to().rank() + 1 } else { mv.to().rank() - 1 };
                self.set_piece_at_square(Piece::from_type_color(Pawn, !start_color),
                                         Square::from_ints(mv.to().file(), ep_square_rank));
            }

        self.half_move_clock = mv.old_half_move_clock;
        self.castling_en_passant = mv.old_castling_en_passant;
        self.hash = mv.old_hash;
        self.repetitions = mv.old_repetitions;
        self.last_move = mv.old_last_move.clone();

        self.to_move = !self.side_to_move();
        debug_assert_ne!(!start_color, self.side_to_move());
        debug_assert!(!self.is_empty(mv.from()));
        debug_assert_eq!(self.hash, self.hash_from_scratch(),
                         "Failed to restore old hash after {:?} on board\n{:?}", mv, self);
        debug_assert_eq!(old_hash, Some(self.hash));
    }

    #[inline(never)]
    fn generate_moves(&self, moves: &mut Vec<Self::Move>) {
        debug_assert!(moves.is_empty());
        let (mut active_moves, mut inactive_moves) = move_gen::all_legal_moves(self);
        moves.append(&mut active_moves);
        moves.append(&mut inactive_moves);
    }
}

impl EvalBoard for SjadamBoard {
    #[inline(never)]
    fn static_eval(&self) -> f32 {
        self.static_eval_with_params(Self::PARAMS)
    }
}

impl ExtendedBoard for SjadamBoard {
    type ReverseNullMove = SjadamReverseNullMove;
    type HashBoard = u64;

    fn hash_board(&self) -> Self::HashBoard {
        self.hash
    }

    #[inline(never)]
    fn move_is_legal(&self, mv: Self::Move) -> bool {
        let piece_moved = self.get_square(mv.from());

        if piece_moved.color() != Some(self.side_to_move()) {
            return false;
        }

        let mut moves1 = vec![];
        let mut moves2 = vec![];
        let mut moves3 = vec![];

        move_gen::legal_moves_for_square(self, mv.from(), piece_moved.piece_type(),
                                                &mut moves1, &mut moves2, &mut moves3);

        if moves1.contains(&mv) || moves2.contains(&mv) || moves3.contains(&mv) {
            let mut moves = vec![];
            self.generate_moves(&mut moves);
            debug_assert!(moves.contains(&mv),
                          "Illegal move {:?} marked as legal on \n{:?}True legal moves: {:?}\nPiece moves: {:?}, {:?}, {:?}", mv, self, moves, moves1, moves2, moves3);
        }
        moves1.contains(&mv) || moves2.contains(&mv) || moves3.contains(&mv)
    }

    #[inline(never)]
    fn active_moves (&self, moves: &mut Vec<Self::Move>) {
        let (mut active_moves, _) = move_gen::all_legal_moves(self);
        moves.append(&mut active_moves);
    }

    fn null_move_is_available(&self) -> bool {
        true
    }

    fn do_null_move(&mut self) -> Self::ReverseNullMove {
        let reverse_move = Self::ReverseNullMove {
            old_last_move: self.last_move.clone(),
            old_castling_en_passant: self.castling_en_passant,
            old_hash: self.hash
        };

        self.move_history.push(self.hash);

        self.hash ^= self.castling_en_passant_key();

        self.set_en_passant_square(None);

        self.hash ^= self.castling_en_passant_key();

        self.to_move = !self.side_to_move();
        self.hash ^= ZOBRIST_KEYS[768];

        reverse_move
    }

    fn reverse_null_move(&mut self, mv: Self::ReverseNullMove) {
        let old_hash = self.move_history.pop();
        debug_assert!(old_hash.is_some());

        self.castling_en_passant = mv.old_castling_en_passant;
        self.hash = mv.old_hash;
        self.last_move = mv.old_last_move.clone();

        self.to_move = !self.side_to_move();
        debug_assert_eq!(self.hash, self.hash_from_scratch(),
                         "Failed to restore old hash after {:?} on board\n{:?}", mv, self);
        debug_assert_eq!(old_hash, Some(self.hash));
    }

    const BRANCH_FACTOR : u64 = 30;
}

impl TunableBoard for SjadamBoard {

    const PARAMS: &'static [f32] =
        &[0.95608354, 0.52998763,
            2.4566965, 0.34603548,
            2.257926, 0.3073387,
            3.090427, 0.16464132,
            4.094851, 0.18254907,
            0.7890315, -0.046917025,
            0.39141265,
            0.07361406, 0.09589645, 0.47790504, 0.31315705, 0.42919618,
            0.020580538, 0.034198467, 0.07280565, 0.18342502, 0.18807995,
            0.11247588,
            0.19844548, 0.12042952, 0.1471833, 0.1398165, 0.18280022, 0.030739496];

    fn static_eval_with_params (&self, params: &[f32]) -> f32 {
        debug_assert!(self.game_result() == None);
        let centre1 = BitBoard::from_u64(0b00000000_00000000_00000000_00011000_00011000_00000000_00000000_00000000);
        let centre2 = BitBoard::from_u64(0b00000000_00000000_00111100_00111100_00111100_00111100_00000000_00000000);
        let centre3 = BitBoard::from_u64(0b00000000_01111110_01111110_01111110_01111110_01111110_01111110_00000000);

        let centre_value = |bits: BitBoard| (bits & centre1).popcount() + (bits & centre2).popcount()
            + (bits & centre3).popcount();

        const I_PIECE_VALS: usize = 0;

        let pieces_value = |piece_ids: [usize; 6]| piece_ids.iter()
            .map(|i| {
                let piece_val = params[I_PIECE_VALS + (i / 2) * 2];
                let piece_center_val = params[I_PIECE_VALS + (i / 2) * 2 + 1];
                let piece_bitboard = self.bitboards[*i];
                piece_center_val * centre_value(piece_bitboard) as f32 +
                    piece_val * piece_bitboard.popcount() as f32
            })
            .sum();

        let white_val: f32 = pieces_value([0, 2, 4, 6, 8, 10]);
        let black_val: f32 = pieces_value([1, 3, 5, 7, 9, 11]);

        const I_TEMPO_BONUS: usize = 12;

        let tempo_bonus = match self.side_to_move() {
            White => params[I_TEMPO_BONUS],
            Black => -params[I_TEMPO_BONUS],
        };

        // Bonuses for king weaknesses which the opponents's pieces are
        // correctly positioned to take advantage of
        const I_KING_SAFETY: usize = 13;

        let pawn_val: f32 = params[I_KING_SAFETY];
        let knight_val: f32 = params[I_KING_SAFETY + 1];
        let bishop_val: f32 = params[I_KING_SAFETY + 2];
        let rook_val: f32 = params[I_KING_SAFETY + 3];
        let queen_val: f32 = params[I_KING_SAFETY + 4];

        // Bonuses for king weaknesses which the opponents's pieces are not
        // correctly positioned to take advantage of
        const I_WRONG_KING_SAFETY: usize = 18;

        let pawn_wrong_val: f32 = params[I_WRONG_KING_SAFETY];
        let knight_wrong_val: f32 = params[I_WRONG_KING_SAFETY + 1];
        let bishop_wrong_val: f32 = params[I_WRONG_KING_SAFETY + 2];
        let rook_wrong_val: f32 = params[I_WRONG_KING_SAFETY + 3];
        let queen_wrong_val: f32 = params[I_WRONG_KING_SAFETY + 4];


        let king_safety_penalties = [White, Black].iter().map(|&color| {

            let mut penalty = 0.0;
            let kings = self.bitboards[10 + color.disc()];
            let king_pos = kings.first_piece().unwrap();

            let friendly_pieces = if color == White {
                self.white_pieces()
            }
            else {
                self.black_pieces()
            };

            // Penalty for king being on same colored diagonal as enemy queens/bishops
            {
                let mut dia_penalty = 0.0;

                let colored_squares = if (kings & WHITE_SQUARES).is_empty() {
                    BLACK_SQUARES
                }
                else {
                    WHITE_SQUARES
                };

                let diagonal_neighbours = diagonal_neighbours(king_pos);

                let friendly_dia_neighbours = diagonal_neighbours & friendly_pieces;
                let open_dia_neighbours = diagonal_neighbours.popcount() - friendly_dia_neighbours.popcount();
                let bishops = self.bitboards[4 + (!color).disc()];
                let queens = self.bitboards[8 + (!color).disc()];

                dia_penalty -= bishop_val * (bishops & colored_squares).popcount() as f32;
                dia_penalty -= bishop_wrong_val * (bishops & !colored_squares).popcount() as f32;
                dia_penalty -= queen_val * (queens & colored_squares).popcount() as f32;
                dia_penalty -= queen_wrong_val * (queens & !colored_squares).popcount() as f32;

                dia_penalty *= open_dia_neighbours as f32;
                penalty += dia_penalty
            }

            {
                let mut orthogonal_penalty: f32 = 0.0;

                let neighbour_squares = orthogonal_neighbours(kings.first_piece().unwrap());
                let open_neighbours_squares = neighbour_squares & (!friendly_pieces);
                debug_assert!((open_neighbours_squares & friendly_pieces).is_empty());

                let rooks = self.bitboards[6 + (!color).disc()];
                let queens = self.bitboards[8 + (!color).disc()];

                for square in open_neighbours_squares {
                    orthogonal_penalty -= rook_val * (rooks & move_gen::possible_sjadam_squares(square)).popcount() as f32;
                    orthogonal_penalty -= rook_wrong_val * (rooks & !move_gen::possible_sjadam_squares(square)).popcount() as f32;
                    orthogonal_penalty -= queen_val * (queens & move_gen::possible_sjadam_squares(square)).popcount() as f32;
                    orthogonal_penalty -= queen_wrong_val * (queens & !move_gen::possible_sjadam_squares(square)).popcount() as f32;
                }

                penalty += orthogonal_penalty;

            }

            // Penalty when king has open knight-jump squares around it,
            // which a knight may be able to sjadam-jump to
            {
                let mut knight_penalty = 0.0;

                let open_neighbours_squares = move_gen::knight_moves(kings, friendly_pieces);
                debug_assert!((open_neighbours_squares & friendly_pieces).is_empty());

                let knights = self.bitboards[2 + (!color).disc()];

                for square in open_neighbours_squares {
                    let sjadam_squares = move_gen::possible_sjadam_squares(square);
                    knight_penalty -= knight_val * (knights & sjadam_squares).popcount() as f32;
                    knight_penalty -= knight_wrong_val * (knights & !sjadam_squares).popcount() as f32;
                }

                penalty += knight_penalty;
            }

            // Penalty when king can has open pawn-capture squares,
            // which a pawn may be able to sjadam-jump to
            {
                let mut pawn_penalty = 0.0;

                let open_neighbours_squares = pawn_attack_squares(kings.first_piece().unwrap(), !color) & !friendly_pieces;

                debug_assert!((open_neighbours_squares & friendly_pieces).is_empty());

                let pawns = self.bitboards[(!color).disc()];

                for square in open_neighbours_squares {
                    let sjadam_squares = move_gen::possible_sjadam_squares(square);
                    pawn_penalty -= pawn_val * (pawns & sjadam_squares).popcount() as f32;
                    pawn_penalty -= pawn_wrong_val * (pawns & !sjadam_squares).popcount() as f32;
                }

                penalty += pawn_penalty;
            }
            penalty
        })
            .collect::<Vec<_>>();

        const I_SPREAD: usize = 23;

        let spread = params[I_SPREAD];

        let white_spread_bonus = SQUARE_SQUARES.iter()
            .filter(|&&bitboard| !(bitboard & self.white_pieces).is_empty())
            .count() as f32 * spread;

        let black_spread_bonus = SQUARE_SQUARES.iter()
            .filter(|&&bitboard| !(bitboard & self.black_pieces).is_empty())
            .count() as f32 * spread;

        const I_SJADAM_MOBILITY: usize = 24;

        let get_sjadam_mobility_bonus = |piece: Piece| {
            let bitboard = self.get_piece(piece);
            let total_sjadam_squares: i32 = bitboard.into_iter()
                .map(|square| {
                    [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
                        .into_iter()
                        .filter(|(i, j)|
                            square.file() as i32 + i * 2 >= 0 && square.file() as i32 + i * 2 < 8
                                && square.rank() as i32 + j * 2 >= 0 && square.rank() as i32 + j * 2 < 8)
                        .map(|(i, j)| {
                            let jump_square = Square((square.0 as i32 + i + j * 8) as u8);
                            let dest_square = Square((square.0 as i32 + i * 2 + j * 16) as u8);
                            if self.all_pieces().get(jump_square)
                                && !self.all_pieces().get(dest_square)
                            {
                                1
                            }
                            else {
                                0
                            }
                        })
                        .sum::<i32>()
                })
                .sum();

            total_sjadam_squares as f32
                * params[I_SJADAM_MOBILITY + piece.piece_type() as u8 as usize - 1]
                * piece.color().unwrap().multiplier() as f32
        };

        let mut sjadam_mobility_bonus = 0.0;
        for i in 1..=6 {
            let piece_type = PieceType::from_disc(i).unwrap();
            for &color in [White, Black].iter() {
                let piece = Piece::from_type_color(piece_type, color);
                sjadam_mobility_bonus += get_sjadam_mobility_bonus(piece);
            }
        }

        white_val - black_val + tempo_bonus + white_spread_bonus - black_spread_bonus
            + king_safety_penalties[0] - king_safety_penalties[1]
            + sjadam_mobility_bonus
        /*
        TODO: Put pawn advancement eval back
        let pawn_val = match self.board[rank][file].piece_type() {
        Pawn => (rank as f32 - 3.5) * -0.1,
        _ => 0.0,
         */
    }
}

pub fn square_from_alg (alg : &str) -> Result<Square, pgn::Error> {
    if alg.len() != 2 { Err(pgn::Error::new(
        pgn::ErrorKind::ParseError,
        format!("Invalid square {}", alg))) }
    else {
        let (file, rank) = (alg.chars().nth(0).unwrap(), alg.chars().nth(1).unwrap());
        if file < 'a' || file > 'h' || rank < '1' || rank > '8' { Err(pgn::Error::new(
            pgn::ErrorKind::ParseError,
            format!("Invalid square {}", alg))) }
        else {
            let (file_u8, rank_u8) = (file as u8 - b'a',
                                      (rank as u8 - b'1'));

            let square = rank_u8 * 8 + file_u8;
            Ok(Square(square)) }
    }
}
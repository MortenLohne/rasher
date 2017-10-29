use board::sjadam_move::SjadamMove;
use board::std_board::BoardIter;
use board::std_board::ChessBoard;
use board::std_board::PieceType;
use board::std_board::Piece;
use board::std_board::Square;
use board::std_move::ChessMove;
use board::std_board::PieceType::*;

use search_algorithms::board::Color::*;
use search_algorithms::board::EvalBoard;
use board::sjadam_board::SjadamBoard;

use std::fmt;

const WHITE_SQUARES : BitBoard = BitBoard { board: 0b10101010_01010101_10101010_01010101_10101010_01010101_10101010_01010101 };
const BLACK_SQUARES : BitBoard = BitBoard{ board: !WHITE_SQUARES.board };

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

#[derive(PartialEq, Eq, Clone, Copy)]
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
    // Sets the square to false
    pub fn clear(&mut self, idx: Square) {
        let Square(i) = idx;
        debug_assert!(i < 64, format!("Tried to index pos {} on board{:?}!", idx, self));
        self.board &= !(1<<i);
    }

    pub fn popcount(&self) -> u32 {
        self.board.count_ones()
    }
    
    pub fn rank(self, rank: u8) -> u8 {
        (self.board >> (rank * 8)) as u8
    }
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
/*
/// Represents the positions of all the pieces
struct PieceBitBoards {
    bitboards: [[BitBoard; 2]; 6],
}

struct SjadamBitBoard {
    white_pieces: BitBoard,
    black_pieces: BitBoard,
    bitboards: [BitBoard; 12],
}
impl SjadamBitBoard {
    fn get_piece(&self, piece: Piece) -> BitBoard {
        self.bitboards[piece as u32 as usize - 2]
    }

    fn get_square(&self, square: Square) -> Piece {
        if self.white_pieces.get(square) {
            self.bitboards.iter()
                .take(6)
                .enumerate()
                .find(|&(_, bitboard)| bitboard.get(square))
                .map(|(i, _)| Piece::new(PieceType::from_disc(i as u32 + 1).unwrap(), White))
                .unwrap()
        }
        else if self.black_pieces.get(square) {
            self.bitboards.iter()
                .skip(6)
                .enumerate()
                .find(|&(_, bitboard)| bitboard.get(square))
                .map(|(i, _)| Piece::new(PieceType::from_disc(i as u32 + 1).unwrap(), Black))
                .unwrap()
        }
        else {
            Piece::empty()
        }
    }

    fn set_square(&mut self, square: Square, piece: Piece) {
        debug_assert!(!piece.is_empty());
        self.bitboards[piece as u8 as usize - 2].set(square);
    }
}
*/

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
    
    for square in BoardIter::new() {
        if !board.base_board[square].is_empty()
            && board.base_board[square].color().unwrap() == board.to_move()
        {
            legal_moves_for_square(board, square, friendly_pieces,
                                   opponent_pieces, &mut moves);
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

fn legal_moves_for_square(board: &SjadamBoard, square: Square, friendly_pieces: BitBoard,
                          opponent_pieces: BitBoard, bitboard_moves: &mut Vec<SjadamMove>) {
    let mut sjadam_squares = BitBoard::empty();
    sjadam_squares.set(square);
    
    let all_pieces = BitBoard::from_u64(opponent_pieces.board | friendly_pieces.board);
    
    sjadam_friendly_moves(&mut sjadam_squares, &friendly_pieces,
                          &all_pieces, square);

    sjadam_opponent_moves(&mut sjadam_squares, &opponent_pieces, &all_pieces);

    let moves : BitBoard = match board.base_board[square].piece_type() {
        PieceType::Rook => rook_moves(sjadam_squares, friendly_pieces, all_pieces),
        PieceType::Bishop => bishop_moves(sjadam_squares, friendly_pieces, all_pieces),
        PieceType::Queen => {
            let mut queen_moves = bishop_moves(sjadam_squares, friendly_pieces, all_pieces);
            queen_moves.board |= rook_moves(sjadam_squares, friendly_pieces, all_pieces).board;
            queen_moves
        },
        PieceType::Knight => knight_moves(sjadam_squares, friendly_pieces),
        PieceType::Pawn => {
            let mut all_pieces_pawns = all_pieces.clone();
            if let Some(ep_square) = board.base_board.en_passant_square() {
                all_pieces_pawns.set(ep_square);
            }
            if board.to_move() == White {
                pawn_moves_white(sjadam_squares, friendly_pieces, all_pieces_pawns)
            }
            else {
                pawn_moves_black(sjadam_squares, friendly_pieces, all_pieces_pawns)
            }
        },
        PieceType::King => king_moves(sjadam_squares, friendly_pieces),
        _ => BitBoard::empty(),
    };
    for i in 0..63 {
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
    let left_captures = ((sjadam_squares.board & RIGHT_MASK) >> 7) & opponent_pieces.board; // & LEFT_MASK;
    let right_captures = ((sjadam_squares.board & LEFT_MASK) >> 9) & opponent_pieces.board; // & RIGHT_MASK;
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

/// Adds all the legal moves for the piece in this position, to the input vector
/// Adds all moves, also those that put the player in check
#[inline(never)]
pub fn legal_moves_for_piece(board : &mut ChessBoard, square : Square, moves : &mut Vec<ChessMove>) {
    let piece = board[square].piece_type();
    
    match piece {
        King => legal_moves_for_king(board, square, moves),
        
        Queen =>  {
            add_moves_diagonally (board, square, moves);
            add_straight_moves(board, square, moves);
        },
        Rook => {
            add_straight_moves(board, square, moves);
        },
        Bishop => {
            add_moves_diagonally(board, square, moves);
        },
        Knight => legal_moves_for_knight(board, square, moves),
        
        Pawn => legal_moves_for_pawn(board, square, moves),
        
        Empty => (),
    }
}

#[inline(never)]
fn king_castling(board: &ChessBoard, square: Square) -> Vec<SjadamMove> {

    let file = (square.0 & 0b0000_0111) as i8;

    let mut moves = vec![];
    
    // Kingside castling
    if board.can_castle_kingside(board.to_move) {
        let mut can_castle_here = true; // !is_attacked(board, square);
        
        // Check that the two squares are empty and not in check
        for n in &[1, 2] {
            debug_assert_eq!(file, 4, "Error: King tried to castle from {} on:{}.",
                             square, board);
            let square_checked = Square(square.0 + n);
            if !board[square_checked].is_empty() {
                can_castle_here = false;
            }
        }
        if can_castle_here {
            moves.push(SjadamMove::new(square, Square(square.0 + 2), true));
        }
    }
    // Queenside castling
    if board.can_castle_queenside(board.to_move) {
        let mut can_castle_here = true; // !is_attacked(board, square);
        
        // Check that the two squares are empty and not in check
        for n in &[1, 2] {
            debug_assert_eq!(file, 4, "Error: File is {}.", file);
            let square_checked = Square(square.0 - n);
            if !board.piece_at(square_checked).is_empty() {
                can_castle_here = false;
            }
        }
        // Check that the knight-square is empty
        if !board.piece_at(Square(square.0 - 3)).is_empty() {
            can_castle_here = false;
        }
        if can_castle_here {
            moves.push(SjadamMove::new(square, Square(square.0 - 2), true));
        }
    }
    moves
}

#[inline(never)]
fn legal_moves_for_king(board : &mut ChessBoard, square : Square, moves : &mut Vec<ChessMove>) {
    
    let file = (square.0 & 0b0000_0111) as i8;
    let rank = (square.0 >> 3) as i8;
    // If the king and the two castling squares are not in check, castling is allowed
    // There must be no pieces between the castling pieces

    // Kingside castling
    if board.can_castle_kingside(board.to_move) {
        let mut can_castle_here = !is_attacked(board, square);
        
        // Check that the two squares are empty and not in check
        for n in &[1, 2] {
            debug_assert_eq!(file, 4, "Error: King tried to castle from {} on:{}.",
                             square, board);
            let square_checked = Square(square.0 + n);
            if !board[square_checked].is_empty()
                || is_attacked(board, square_checked) {
                    can_castle_here = false;
                }
            
        }
        if can_castle_here {
            moves.push(ChessMove::new(square, Square(square.0 + 2)));
        }
        
        
    }
    // Queenside castling
    if board.can_castle_queenside(board.to_move) {
        let mut can_castle_here = !is_attacked(board, square);
        
        // Check that the two squares are empty and not in check
        for n in &[1, 2] {
            debug_assert_eq!(file, 4, "Error: File is {}.", file);
            let square_checked = Square(square.0 - n);
            if !board.piece_at(square_checked).is_empty() ||
                is_attacked(board, square_checked)
                 {
                    can_castle_here = false;
                }
        }
        // Check that the knight-square is empty
        if !board.piece_at(Square(square.0 - 3)).is_empty() {
            can_castle_here = false;
        }
        if can_castle_here {
            moves.push(ChessMove::new(square, Square(square.0 - 2)));
        }
        
    }
    
    for i in -1..2 {
        for j in -1..2 {
            
            if file + i < 0 || file + i >= 8 ||
                rank + j < 0 || rank + j >= 8 ||
                (j == 0 && i == 0) {
                    continue;
                }
            let new_pos = Square(((rank + j) * 8 + file + i) as u8);

            // Check that the square is not occupied by a friendly piece
            let c_move = ChessMove::new(square, new_pos);

            if board[new_pos].is_empty() {
                let old_piece = board[square];
                board[square] = Piece::empty();
                moves.push(c_move);
                board[square] = old_piece;
            }
            else if board[new_pos].color().unwrap() != board.to_move {
                moves.push(c_move);
            }
        }
    }
}

#[inline(never)]
fn legal_moves_for_knight(board : &ChessBoard, square : Square, moves : &mut Vec<ChessMove>) {
    let file = (square.0 & 0b0000_0111) as i8;
    let rank = (square.0 >> 3) as i8;
    
    for &(i, j) in &[(-2, -1), (-2, 1), (-1, -2), (-1, 2),
                    (1, -2), (1, 2), (2, -1), (2, 1)] {
        if file + i < 0 || file + i >= 8 || rank + j < 0 || rank + j >= 8 {
            continue;
        }
        let new_pos = Square(((rank + j) * 8 + file + i) as u8);
        
        let c_move = ChessMove::new(square, new_pos);

        if board[new_pos].is_empty() || board[new_pos].color().unwrap() != board.to_move {
            //println!("Knight can move to {}, onto a {} {}, on: {}",
            //             Square(new_pos), color_to, piece_to, board);
            moves.push(c_move);
        }
    }
}

#[inline(never)]
fn legal_moves_for_pawn(board : &ChessBoard, square : Square, moves : &mut Vec<ChessMove>) {
    // Helper variables
    let file = (square.0 & 0b0000_0111) as i8;
    let rank = (square.0 >> 3) as i8;
    let pos = square.0 as i8;
    let (start_rank, prom_rank, back_rank, direction) =
        if board.to_move == White { (6, 1, 0, -1) } else { (1, 6, 7, 1) };

    if rank == back_rank { return }
    
    debug_assert!(rank > 0 && board.to_move == White || rank < 7 && board.to_move == Black);;
    // Checks if there are any pieces available for capture to the left,
    // including en passant capture
    if file > 0 {
        let take_square = Square((pos + direction * 8) as u8 - 1);
        if !board[take_square].is_empty() && board[take_square].color().unwrap() != board.to_move {
            if rank == prom_rank {
                for piece_type in &[Queen, Rook, Bishop, Knight] {
                    let c_move = ChessMove::new_prom(square, take_square, *piece_type);
                    moves.push(c_move);
                }
            }
            else {
                let c_move = ChessMove::new(square, take_square);
                moves.push(c_move);
            }
        }
        if board.en_passant_square().is_some()
            && take_square == board.en_passant_square().unwrap()
        {
            let c_move = ChessMove::new(square, take_square);
            moves.push(c_move);
        }
    }
    // Ditto, but to the right
    if file < 7 {
        let take_square = Square((pos + direction * 8) as u8 + 1);
        if !board[take_square].is_empty() && board[take_square].color().unwrap() != board.to_move {
            if rank == prom_rank {
                for piece_type in &[Queen, Rook, Bishop, Knight] {
                    let c_move = ChessMove::new_prom(square, take_square, *piece_type);
                    moves.push(c_move);
                }
            }
            else {
                let c_move = ChessMove::new(square, take_square);
                moves.push(c_move);
            }
        }
        if board.en_passant_square().is_some()
            && take_square == board.en_passant_square().unwrap()
        {
            let c_move = ChessMove::new(square, take_square);
            moves.push(c_move);
        }
    }

    //Checks if the pawn can walk forward one or two squares, promote
    let square_in_front = Square::from_ints(file as u8, (rank + direction) as u8);
    
    if board.piece_at(square_in_front).is_empty() {
        if rank == prom_rank {
            moves.push(ChessMove::new_prom(square, square_in_front, Queen));
        }
        else {
            let c_move = ChessMove::new(square, square_in_front);
            moves.push(c_move);
        }
        if rank == start_rank {
            let square_2_in_front = Square::from_ints(file as u8,
                                                      (rank + direction * 2) as u8);
            let c_move = ChessMove::new(square, square_2_in_front);
            if board.piece_at(square_2_in_front).is_empty() {
                moves.push(c_move);
            }
        }
    }
}

/// Returns whether a square is under attack by the side not to move
pub fn is_attacked(board : &ChessBoard, square : Square) -> bool {
    if !board.piece_at(square).is_empty() {
        debug_assert_eq!(board.to_move(), board.piece_at(square).color().unwrap(),
                         "{:?}\n Tried to check if {} {} at {} was attacked.",
                         board,  board.piece_at(square).color().unwrap(),
                         board.piece_at(square).piece_type(), square);
    }
    // Direction enemy pawns are coming from
    let pawn_direction = if board.to_move == White { -1 } else { 1 };
    let pos = square.0 as i8;
    let file = (square.0 & 0b0000_0111) as i8;
    let rank = (square.0 >> 3) as i8;

    if file > 0 {
        if (board.to_move == White && rank > 1) || (board.to_move == Black && rank < 6) {
            let pawn_square = Square((pos + pawn_direction * 8) as u8 - 1);
            
            if board[pawn_square] == Piece::new(Pawn, !board.to_move) { return true; }
        }
    }
    if file < 7 {
        if (board.to_move == White && rank > 1) || (board.to_move == Black && rank < 6) {
            let pawn_square = Square((pos + pawn_direction * 8) as u8 + 1);
            
            if board[pawn_square] == Piece::new(Pawn, !board.to_move) { return true; }
        }
    }

    for &(i, j) in &[(0, 1), (1, 0), (0, -1), (-1, 0)] {
        if check_threats_in_direction (i, j, board, square, &[Queen, Rook]) {
            return true;
        }
    }
    for &(i, j) in &[(1, 1), (1, -1), (-1, 1), (-1, -1)] {
        if check_threats_in_direction (i, j, board, square, &[Queen, Bishop]) {
            return true;
        }
    }
    
    for &(i, j) in &[(-2, -1), (-2, 1), (-1, -2), (-1, 2),
                    (1, -2), (1, 2), (2, -1), (2, 1)] {
        if file + i < 0 || file + i >= 8 || rank + j < 0 || rank + j >= 8 {
            continue;
        }
        let new_pos = Square(((rank + j) * 8 + file + i) as u8);
        
        if board[new_pos] == Piece::new(Knight, !board.to_move) {
            return true;
        }
    }
    for i in -1..2 {
        for j in -1..2 {
            if file + i < 0 || file + i >= 8 || rank + j < 0 || rank + j >= 8 ||
                (j == 0 && i == 0) {
                    continue;
                }
            let new_pos = Square(((rank + j) * 8 + file + i) as u8);

            // Check that there is no enemy king around
            if board[new_pos] == Piece::new(King, !board.to_move) {
                return true;
            }
        }
    }
    
    false
}

fn check_threats_in_direction (i : i8, j : i8, board : &ChessBoard, square : Square,
                               threats : &[PieceType]) -> bool {
    let mut file = (square.0 & 0b0000_0111) as i8;
    let mut rank = (square.0 >> 3) as i8;
    loop {
        file += i;
        rank += j;
        if file < 0 || rank < 0 || file >= 8 || rank >= 8 {
            return false;
        }
        let piece = board[Square::from_ints(file as u8, rank as u8)];
        if piece.is_empty() { continue }
        if piece.color().unwrap() == board.to_move { return false; }
        for threat in threats {
            if piece.piece_type() == *threat {
                return true;
            }
        }
        return false;
    }
}

fn add_moves_diagonally (board : &ChessBoard, square : Square, moves : &mut Vec<ChessMove>) {
    for &(i, j) in &[(1, 1), (1, -1), (-1, 1), (-1, -1)] {
        add_moves_in_direction(i, j, board, square, moves);
    }
}

fn add_straight_moves(board : &ChessBoard, square : Square, moves : &mut Vec<ChessMove>) {
    for &(i, j) in &[(0, 1), (1, 0), (0, -1), (-1, 0)] {
        add_moves_in_direction(i, j, board, square, moves);
    }
}
    
fn add_moves_in_direction (i : i8, j : i8, board : &ChessBoard, square : Square,
                           moves : &mut Vec<ChessMove>) {
    
    let mut file = (square.0 & 0b0000_0111) as i8;
    let mut rank = (square.0 >> 3) as i8;
    loop {
        file += i;
        rank += j;

        if file < 0 || rank < 0 || file >= 8 || rank >= 8 {
            break;
        }
        let target_square = Square::from_ints(file as u8, rank as u8);
        let piece_to = board[target_square];

        let c_move = ChessMove::new(square, target_square);
        if piece_to.is_empty() {
            moves.push(c_move);
            continue;
        }
        if piece_to.color().unwrap() != board.to_move {
            moves.push(c_move);
        }
        break;  // Break after finding a piece, friend or foe
        
    }
}

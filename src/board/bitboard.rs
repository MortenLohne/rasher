use std::iter::FromIterator;
use std::{fmt, ops};
use board::std_board::{Square, BoardIter, ChessBoard, Piece};

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct BitBoard {
    pub board: u64,
}

impl ops::BitOr for BitBoard {
    type Output = BitBoard;
    fn bitor(self, rhs: BitBoard) -> BitBoard {
        BitBoard::from_u64(self.board | rhs.board)
    }
}

impl ops::BitAnd for BitBoard {
    type Output = BitBoard;
    fn bitand(self, rhs: BitBoard) -> BitBoard {
        BitBoard::from_u64(self.board & rhs.board)
    }
}

impl ops::Not for BitBoard {
    type Output = BitBoard;
    fn not(self) -> BitBoard {
        BitBoard::from_u64(!self.board)
    }
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

    /// Returns a board with the diagonal neighbours squares (up to 4) set
    pub fn diagonal_neighbours(square: Square) -> BitBoard {
        DIAGONAL_NEIGHBOURS[square.0 as usize]
    }

    pub fn orthogonal_neighbours(square: Square) -> BitBoard {
        ORTHOGONAL_NEIGHBOURS[square.0 as usize]
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

    pub fn is_empty(&self) -> bool {
        self.board == 0
    }

    pub fn popcount(&self) -> u32 {
        self.board.count_ones()
    }

    /// Get a single rank
    /// Ranks are numbered from 0 (black's back rank) to 7 (white's back rank)
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

    pub fn first_piece(&self) -> Option<Square> {
        let square = self.board.trailing_zeros();
        if square == 64 {
            None
        }
        else {
            Some(Square(square as u8))
        }
    }
}

impl fmt::Debug for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for n in 0..8 {
            writeln!(f, "{:08b}", (self.board >> (n * 8)) as u8).unwrap();
        }
        Ok(())
    }
}

impl IntoIterator for BitBoard {
    type Item = Square;
    type IntoIter = BitBoardIterator;

    fn into_iter(self) -> Self::IntoIter {
        BitBoardIterator::new(self)
    }
}

impl FromIterator<Square> for BitBoard {
    fn from_iter<T: IntoIterator<Item=Square>>(iter: T) -> Self {
        let mut board = Self::empty();
        for square in iter {
            board.set(square);
        }
        board
    }
}

pub struct BitBoardIterator {
    board: BitBoard,
}

impl BitBoardIterator {
    fn new(board: BitBoard) -> Self {
        Self { board }
    }
}

impl Iterator for BitBoardIterator {
    type Item = Square;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.board.first_piece() {
            self.board.clear(next);
            Some(next)
        }
        else {
            None
        }
    }
}

lazy_static!(
    static ref DIAGONAL_NEIGHBOURS : [BitBoard; 64] = {
        let mut table : [BitBoard; 64] = [BitBoard::empty(); 64];

        for i in 0..64 {
            let mut board = BitBoard::empty();
            let (file, rank) = Square(i).file_rank();

            for &x in &[u8::overflowing_sub(file, 1).0, file + 1] {
                for &y in &[u8::overflowing_sub(rank, 1).0, rank + 1] {
                    if x < 8 && y < 8 {
                        board.set(Square::from_ints(x, y));
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
                board.set(Square::from_ints(file - 1, rank));
            }
            if file < 7 {
                board.set(Square::from_ints(file + 1, rank));
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
                board.set(Square::from_ints(file, rank - 1));
            }
            if rank < 7 {
                board.set(Square::from_ints(file, rank + 1));
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
);
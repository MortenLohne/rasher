use board::sjadam_board;
use board::bitboard::BitBoard;
use board::sjadam_board::SjadamBoard;
use board::std_board::{ChessBoard, BoardIter};
use board::std_board::Square;
use board::std_board::Piece;
use board::std_board::PieceType::*;
use search_algorithms::board::Color::*;
use search_algorithms::board::Board;
use pgn::PgnBoard;

#[test]
fn rotate() {
    let empty = BitBoard::empty();
    assert_eq!(empty, empty.rotate());

    let mut board = BitBoard::from_board(&ChessBoard::start_board(), |p| !p.is_empty());

    assert_eq!(board, board.rotate().rotate());
    
    assert_eq!(board, board.rotate().rotate().rotate().rotate());

    board = board.rotate();
    for rank in 0..8 {
        for &file in [0, 1, 6, 7].iter() {
            assert!(board.get(Square::from_ints(file, rank)));
        }
        for &file in [2, 3, 4, 5].iter() {
            assert!(!board.get(Square::from_ints(file, rank)),
            "Found piece in the middle of board \n{:?}", board);
        }
    }
}

impl BitBoard {
    /// Returns a bitboard representation of the board, with all bits set
    /// where f(piece) is true for the piece on the square
    #[allow(dead_code)]
    pub fn from_board<F: Fn(Piece) -> bool>(board: &ChessBoard, f: F) -> Self {
        let mut bit_board = BitBoard::empty();
        for square in BoardIter::new() {
            if f(board[square]) {
                bit_board = bit_board.set(square);
            }
        }
        bit_board
    }
    #[allow(dead_code)]
    pub fn all_from_board(board: &ChessBoard) -> Self {
        BitBoard::from_board(board, |piece| !piece.is_empty())
    }
}

use quickcheck::Arbitrary;
use quickcheck::Gen;

impl Arbitrary for BitBoard {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        BitBoard::from_u64(g.next_u64())
    }
}

impl Arbitrary for Square {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        Square((g.next_u64() % 64) as u8)
    }
}

quickcheck! {
    fn rotation_45_preserves_pieces(bitboard: BitBoard) -> bool {
        bitboard.rotate_45().board.count_ones() == bitboard.board.count_ones()
    }

    fn diagonals_preserve_pieces(bitboard: BitBoard) -> bool {
        (0..15).map(|n| bitboard.diagonal(n)).map(u8::count_ones).sum::<u32>() == bitboard.popcount()
    }

    fn rotation_315_preserves_pieces(bitboard: BitBoard) -> bool {
        bitboard.rotate_315().board.count_ones() == bitboard.board.count_ones()
    }

    fn antidiagonals_preserve_pieces(bitboard: BitBoard) -> bool {
        (-7..8).map(|n| bitboard.antidiagonal(n))
            .map(u8::count_ones)
            .sum::<u32>() == bitboard.popcount()
    }

    fn rotate_315_back(bitboard: BitBoard) -> bool {
        (0..8).fold(bitboard, |board, _| board.rotate_315()) == bitboard
    }

    fn rotate_45_back(bitboard: BitBoard) -> bool {
        (0..8).fold(bitboard, |board, _| board.rotate_45()) == bitboard
    }

    fn iterator_iterates_all(bitboard: BitBoard) -> bool {
        bitboard.into_iter().count() == bitboard.popcount() as usize
    }

    fn iterator(bitboard: BitBoard) -> bool {
        let mut board = BitBoard::empty();
        for square in bitboard {
            board = board.set(square);
        }
        board == bitboard
    }

    fn from_to_iterator(bitboard: BitBoard) -> bool {
        bitboard.into_iter().collect::<BitBoard>() == bitboard
    }
}

#[test]
fn rank() {
    let mut chess_board = ChessBoard::start_board();
    let bit_board = BitBoard::all_from_board(&chess_board);
    assert_eq!(bit_board.rank(0), 255);
    assert_eq!(bit_board.rank(1), 255);
    assert_eq!(bit_board.rank(2), 0);
    assert_eq!(bit_board.rank(6), 255);

    let mv = chess_board.move_from_san("e2e4").unwrap();
    chess_board.do_move(mv);
    assert_eq!(BitBoard::all_from_board(&chess_board).rank(4), 16);
}

#[test]
fn file() {
    let start_board = BitBoard::all_from_board(&ChessBoard::start_board());
    assert_eq!(start_board.file(0), 0b1100_0011);
    let mut board = ChessBoard::start_board();
    let mv = board.move_from_lan("a2a4").unwrap();
    board.do_move(mv);
    let bit_board = BitBoard::all_from_board(&board);
    assert_eq!(bit_board.file(0), 0b1100_1001);
}

#[test]
fn rotate_45() {
    let board = BitBoard::all_from_board(&ChessBoard::start_board());
    let board2 = (0..8).fold(board, |b, _| b.rotate_45());
    assert_eq!(board, board2);
}

#[test]
fn long_diagonal() {
    let board = BitBoard::all_from_board(&ChessBoard::start_board()).rotate_45();
    assert_eq!(board.diagonal(7), 0b1100_0011, "\nDiagonal: {:b}\n{:?}",
               board.diagonal(7), BitBoard::from_u64(board.board));
    assert_eq!(board.diagonal(8), 0b0100_0011, "\nDiagonal: {:b}\n{:?}",
               board.diagonal(8), BitBoard::from_u64(board.board));
    assert_eq!(board.diagonal(9), 0b11, "\n{:?}", board);
    assert_eq!(board.diagonal(13), 0b11, "\n{:?}", board);
    assert_eq!(board.diagonal(14), 1);
    assert_eq!(board.diagonal(0), 1);
    assert_eq!(board.diagonal(1), 0b11);
    assert_eq!(board.diagonal(2), 0b110);
}

#[test]
fn long_antidiagonal() {
    let board = BitBoard::all_from_board(&ChessBoard::start_board()).rotate_315();
    assert_eq!(board.antidiagonal(0), 0b1100_0011, "\nDiagonal: {:b}\n{:?}",
               board.antidiagonal(0), BitBoard::from_u64(board.board));
    assert_eq!(board.antidiagonal(1), 0b0110_0001, "\nDiagonal: {:b}\n{:?}",
               board.antidiagonal(1), board);
    assert_eq!(board.antidiagonal(2), 0b11_0000, "\n{:?}", board);
    assert_eq!(board.antidiagonal(6), 0b11, "\n{:?}", board);
    assert_eq!(board.antidiagonal(7), 1);
    assert_eq!(board.antidiagonal(-7), 1);
    assert_eq!(board.antidiagonal(-6), 0b11);
    assert_eq!(board.antidiagonal(-5), 0b011);
}

#[test]
fn diagonal_neighbours() {
    let a8_neighbours = BitBoard::diagonal_neighbours(Square(0));
    assert_eq!(a8_neighbours.popcount(), 1,
               "Neighbours of square 0:\n{:?}",
               BitBoard::diagonal_neighbours(Square(0)));
    assert_eq!(BitBoard::diagonal_neighbours(Square(8)).popcount(), 2);
    assert_eq!(BitBoard::diagonal_neighbours(Square(9)).popcount(), 4);

    let board = BitBoard::empty().set(Square(9));
    assert_eq!(BitBoard::from_u64(a8_neighbours.board & board.board).popcount(), 1);
}

#[test]
fn square_squares() {
    for bitboard in sjadam_board::SQUARE_SQUARES.iter() {
        assert_eq!(bitboard.popcount(), 4);
    }

    assert_eq!(sjadam_board::SQUARE_SQUARES.iter().fold(BitBoard::empty(), |b1, &b2| b1 | b2).popcount(), 64);
}

#[test]
fn first_piece() {
    let start_board = SjadamBoard::start_board();
    let white_kings = start_board.get_piece(Piece::new(King, White));
    let black_kings = start_board.get_piece(Piece::new(King, Black));
    
    assert_eq!(white_kings.first_piece(), Some(Square(60)));
    assert_eq!(black_kings.first_piece(), Some(Square(4)));
}

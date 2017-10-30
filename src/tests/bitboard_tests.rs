use board::sjadam_move_gen::BitBoard;
use board::std_board;
use board::std_board::ChessBoard;
use search_algorithms::board::EvalBoard;
use uci::UciBoard;

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
            assert!(board.get(std_board::Square::from_ints(file, rank)));
        }
        for &file in [2, 3, 4, 5].iter() {
            assert!(!board.get(std_board::Square::from_ints(file, rank)),
            "Found piece in the middle of board \n{:?}", board);
        }
    }
}

use quickcheck::Arbitrary;
use quickcheck::Gen;

impl Arbitrary for BitBoard {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        BitBoard::from_u64(g.next_u64())
    }
}

quickcheck! {
    fn rotation_45_preserves_pieces(bitboard: BitBoard) -> bool {
        bitboard.rotate_45().board.count_ones() == bitboard.board.count_ones()
    }
    fn diagonals_preserve_pieces(bitboard: BitBoard) -> bool {
        println!("Bitcounts: {}, {}",
                 (0..15).map(|n| bitboard.diagonal(n)).map(u8::count_ones).sum::<u32>(),
                 bitboard.popcount());
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
}

#[test]
fn rank() {
    let start_board = BitBoard::all_from_board(&ChessBoard::start_board());
    println!("{:?}", start_board);
    for rank in 0..8 {
        println!("{}", start_board.rank(rank));
    }
    assert_eq!(start_board.rank(0), 255);
    assert_eq!(start_board.rank(1), 255);
    assert_eq!(start_board.rank(2), 0);
    assert_eq!(start_board.rank(6), 255);
}

#[test]
fn file() {
    let start_board = BitBoard::all_from_board(&ChessBoard::start_board());
    assert_eq!(start_board.file(0), 0b1100_0011);
    let mut board = ChessBoard::start_board();
    let mv = board.from_alg("a2a4").unwrap();
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

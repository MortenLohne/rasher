use board::sjadam_move_gen::BitBoard;
use board::std_board;
use board::std_board::ChessBoard;
use board::std_move::ChessMove;
use search_algorithms::board::EvalBoard;
use search_algorithms::game_move::Move;

#[test]
fn rotate() {
    let start_board = BitBoard::from_board(&ChessBoard::start_board(), |p| !p.is_empty());
    let mut empty = BitBoard::empty();
    empty.rotate();
    assert_eq!(BitBoard::empty(), empty);
    let mut board = start_board.clone();
    board.rotate();
    println!("{:?}", board);
    board.rotate();
    assert_eq!(start_board, board);
    
    board.rotate();
    board.rotate();
    assert_eq!(start_board, board);

    board.rotate();
    for rank in 0..8 {
        for &file in [0, 1, 6, 7].iter() {
            assert!(board.get(std_board::Square::from_ints(file, rank)));
        }
        for &file in [2, 3, 4, 5].iter() {
            assert!(!board.get(std_board::Square::from_ints(file, rank)),
            "Found piece in the middle of board \n{:?}", board);
        }
    }

    board = BitBoard::from_u64(0b01000100_01001000_01010000_01010000_01110000_01000100_01000100_01111000);
    board.rotate();
    board.rotate();
    board.rotate();
    board.rotate();
    assert_eq!(board, BitBoard::from_u64(0b01000100_01001000_01010000_01010000_01110000_01000100_01000100_01111000));
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
    let mut chess_board = ChessBoard::start_board();
    chess_board.do_move(ChessMove::from_alg("a2a4").unwrap());
    let board = BitBoard::all_from_board(&chess_board);
    assert_eq!(board.file(0), 0b1100_1001);
}

#[test]
fn rotate_45() {
    let mut board = BitBoard::all_from_board(&ChessBoard::start_board());
    let board2 = BitBoard::all_from_board(&ChessBoard::start_board());
    for _ in 0..8 {
        board.rotate_45();
    }
    assert_eq!(board, board2);
}

use board::std_board::*;
use board::std_move::ChessMove;
use search_algorithms::board::Board;

use pgn::UciBoard;

use super::super::board::std_move_gen::move_gen;

use board::std_board::PieceType::*;
use search_algorithms::board::Color::{Black, White};

use tests::tactics_tests::basic_tactics_prop;

/// Tests that Board.piece_at() and Square::from_alg() work correctly
/// Also assumes that std_board::START_BOARD is correct
#[test]
fn test_piece_at () {
    let square = |str| Square::from_alg(str).unwrap();
    
    let start_board = ChessBoard::start_board();
    let d1 = square("d1");
    let e1 = square("e1");
    let a1 = square("a1");
    let h8 = square("h8");
    let e4 = square("e4");

    assert_eq!(start_board.piece_at(d1), Piece::new(Queen, White));
    assert_eq!(start_board.piece_at(e1), Piece::new(King, White));
    assert_eq!(start_board.piece_at(a1), Piece::new(Rook, White));
    assert_eq!(start_board.piece_at(h8), Piece::new(Rook, Black));
    assert_eq!(start_board.piece_at(e4), Piece::new(Empty, White));
}

#[test]
fn test_square () {
    assert_eq!(Square::from_alg("a3").unwrap(), Square::from_ints(0, 5));
    assert_eq!(Square::from_alg("e1").unwrap(), Square::from_ints(4, 7));
}

#[test]
fn respond_to_checks() {

    let mut legal_moves = vec![];

    let board1 = ChessBoard::from_fen("rnbqkbnr/ppNppppp/8/8/8/8/PPPPPPPP/R1BQKBNR b KQkq - 0 1")
        .unwrap();
    assert!(move_gen::is_attacked(&board1, board1.king_pos(board1.side_to_move())) == true,
            format!("Error: King should be under attack here:\n{}", board1));
    board1.generate_moves(&mut legal_moves);
    assert!(legal_moves.len() == 1,
            format!("Found {} legal moves, expected 1. Board:\n{}", legal_moves.len(), board1));
    legal_moves.clear();

    let board2 = ChessBoard::from_fen("r1bqkb1r/pppppppp/5N2/8/3n4/8/PPPPPPPP/R1BQKBNR b KQkq - 0 1")
        .unwrap();
    assert!(move_gen::is_attacked(&board2, board2.king_pos(board2.side_to_move())) == true,
            format!("Error: King should be under attack here:\n{}", board2));
    board2.generate_moves(&mut legal_moves);
    assert!(legal_moves.len() == 2,
            format!("Found {} legal moves, expected 2. Board:\n{}", legal_moves.len(), board2));
    legal_moves.clear();

    let board3 = ChessBoard::from_fen("r1bqkb1r/pppppppp/5N2/8/3n4/4P3/PPPP1PPP/R1BQKBNR b KQkq - 0 1")
        .unwrap();
    assert!(move_gen::is_attacked(&board3, board3.king_pos(board3.side_to_move())) == true,
            format!("Error: King should be under attack here:\n{}", board3));
    board3.generate_moves(&mut legal_moves);
    assert!(legal_moves.len() == 2,
            format!("Found {} legal moves, expected 2. Board:\n{}", legal_moves.len(), board3));
    legal_moves.clear();

    let board4 = ChessBoard::from_fen("r1bqkb1r/pppp1ppp/5p2/7Q/3n4/4P3/PPPP1PPP/R1B1KBNR b KQkq - 0 1")
        .unwrap();
    assert!(move_gen::is_attacked(&board4, board4.king_pos(board4.side_to_move())) == false,
            format!("Error: King should not be under attack here:\n{}", board4));
    board4.generate_moves(&mut legal_moves);
    assert!(legal_moves.len() == 29,
            format!("Found {} legal moves, expected 29. Board:\n{}", legal_moves.len(), board4));
    legal_moves.clear();

    let board5 = ChessBoard::from_fen("k7/Q6K/8/8/8/8/8/8 b - - 0 1").unwrap();
    assert!(move_gen::is_attacked(&board5, board5.king_pos(board5.side_to_move())) == true,
            format!("Error: King should be under attack here at {}:\n{}",
                    board5.king_pos(board5.side_to_move()), board5));
    board5.generate_moves(&mut legal_moves);
    assert!(legal_moves.len() == 1,
            format!("Found {} legal moves, expected 1. Board:\n{}", legal_moves.len(), board5));
    legal_moves.clear();

    let mut board6 = ChessBoard::from_fen("8/2p5/3p4/KP5r/1R3p1k/6P1/4P3/8 b - - 0 1").unwrap();
    assert!(move_gen::is_attacked(&board6, board6.king_pos(board6.side_to_move())) == true,
            format!("Error: King should be under attack here:\n{}", board5));
    legal_moves.clear();

    let mv = board6.from_alg("h4g4").unwrap();
    let mv2 = board6.from_alg("f4g3").unwrap();
    move_is_available_prop(&mut board6, mv);
    move_is_unavailable_prop(&mut board6, mv2);
}

#[test]
fn en_passant_test () {
    let mut board1 = ChessBoard::from_fen("7k/p7/8/1P6/8/8/8/7K b - - 0 1").unwrap();
    let move1 = board1.from_alg("a7a5").unwrap();
    let move2 = board1.from_alg("b5a6").unwrap();
    // Do a move that allows en passant
    board1.do_move(move1);

    // Check that en passant is the best move, and do it
    //move_is_available_prop(&mut board1, move2);
    basic_tactics_prop(&board1, move2);
    board1.do_move(move2);

    // The board that's expected
    let board_expected1 = ChessBoard::from_fen("7k/8/P7/8/8/8/8/7K b - - 0 3").unwrap();
    // Add the same moves, to ensure complete equality

    // Check that the boards are completely equal
    assert_eq!(board1, board_expected1, "Boards were not equal. Board1:\n{}\nBoard2:\n{}",
               board1, board_expected1);


    // Check that an en passant move is only available for one move after the pawn push
    let mut board2 = ChessBoard::from_fen("7k/p7/8/1P6/8/8/8/7K w - - 0 1").unwrap();
    let move1 = ChessMove::new(Square::from_alg("a7").unwrap(),
                          Square::from_alg("a5").unwrap());
    board2.do_move(move1);
    let move2 = ChessMove::new(Square::from_alg("h1").unwrap(),
                          Square::from_alg("h2").unwrap());
    board2.do_move(move2);
    let move3 = ChessMove::new(Square::from_alg("h8").unwrap(),
                          Square::from_alg("h7").unwrap());
    board2.do_move(move3);

    let mut moves = vec![];
    board2.generate_moves(&mut moves);
    assert_eq!(moves.len(), 6,
               "Only 6 moves should be available, board:\n{}", board2);
    
}

#[test]
fn is_pinned_to_piece_test() {
    let board1 = ChessBoard::from_fen("k7/ppp5/2P5/QK1B4/8/8/8/8 b - - 0 1").unwrap();
    is_pinned_prop(&board1, Square(0), Square(8), false);
    is_pinned_prop(&board1, Square(8), Square(0), true);
    is_pinned_prop(&board1, Square(9), Square(0), false);
    is_pinned_prop(&board1, Square(10), Square(0), false);
}

fn is_pinned_prop(board : &ChessBoard, pinee_pos : Square, pinner_pos : Square, is_pinned : bool) {
    if is_pinned {
        assert!(move_gen::is_pinned_to_piece(&board, pinee_pos, pinner_pos),
               format!("{} should be pinned to {}, but isn't. Board:\n{}",
                       board[pinee_pos].piece_type(), board[pinner_pos].piece_type(), board));
    }
    else {
        assert!(!move_gen::is_pinned_to_piece(&board, pinee_pos, pinner_pos),
               format!("{} should not be pinned to {}. Board:\n{}",
                       board[pinee_pos].piece_type(), board[pinner_pos].piece_type(), board));
    }
}

#[test]
fn king_in_check_test() {
    let board = ChessBoard::from_fen("8/1k5p/P7/8/8/8/8/7K b - -").unwrap();
    let mut moves = vec![];
    board.generate_moves(&mut moves);
    assert_eq!(moves.len(), 8, "{} \nExpected 8 moves in position, found {}; {:?}",
                                       board, moves.len(), moves);
}

#[test]
fn capture_from_check() {
    let board = ChessBoard::from_fen("7Q/5p1k/P5p1/5p2/7q/K7/8/8 b - - 1 41").unwrap();
    let mut moves = vec![];
    board.generate_moves(&mut moves);
    assert_eq!(moves, vec![board.from_alg("h7h8").unwrap()]);
}

#[test]
fn castling_test () {

    // Checks that white can't castle while in check
    let board2 = ChessBoard::from_fen("k3r3/8/8/8/8/8/3P1PPP/3RK2R w K - 0 1").unwrap();
    let mut moves = vec![];
    board2.generate_moves(&mut moves);
    assert!(moves.len() == 1,
            format!("Only 1 move should be available, board:\n{}", board2));

    // Checks that black queenside castling works in a tactic
    let board3 = ChessBoard::from_fen("r3k3/1R6/8/8/8/8/8/3K4 b q - 0 1").unwrap();
    let move3 = board3.from_alg("e8c8").unwrap();
    basic_tactics_prop(&board3, move3);

    // Positions with both castlings available
    let mut board4 = ChessBoard::from_fen(
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1").unwrap();
    let mv = board4.from_alg("e1c1").unwrap();
    let mv2 = board4.from_alg("e1g1").unwrap();
    let mv3 = board4.from_alg("e1h1").unwrap();
    move_is_available_prop(&mut board4, mv);
    move_is_available_prop(&mut board4, mv2);
    move_is_unavailable_prop(&mut board4, mv3);

    // Positions where castling is legal, but rook is attacked
    let mut board5 = ChessBoard::from_fen(
        "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();
    let mv4 = board5.from_alg("e1g1").unwrap();
    let mv5 = board5.from_alg("e1f1").unwrap();
    move_is_available_prop(&mut board5, mv4);
    move_is_available_prop(&mut board5, mv5);
}

fn move_is_available_prop(board : &mut ChessBoard, c_move : ChessMove) {
    let mut all_moves = vec![];
    board.generate_moves(&mut all_moves);
    assert!(all_moves.iter().find(|mv|c_move == **mv).is_some(),
            "{} should be legal here, board:{}Legal moves: {:?}",
            board.to_alg(&c_move), board, all_moves);
}

fn move_is_unavailable_prop(board : &mut ChessBoard, c_move : ChessMove) {
    let mut all_moves = vec![];
    board.generate_moves(&mut all_moves);
    assert!(!all_moves.iter().find(|mv|c_move == **mv).is_some(),
            "{} should not be legal here, board:{}Legal moves: {:?}",
            board.to_alg(&c_move), board, all_moves);
}

#[test]
fn starting_position_perf_test() {
    let mut board1 = ChessBoard::start_board().clone();
    assert_eq!(legal_moves_after_plies(&mut board1, 1), 20);
    assert_eq!(legal_moves_after_plies(&mut board1, 2), 400);
    assert_eq!(legal_moves_after_plies(&mut board1, 3), 8_902);
    assert_eq!(legal_moves_after_plies(&mut board1, 4), 197_281);
}

#[test]
#[ignore]
fn starting_position_perf_test_long() {
    let mut board1 =  ChessBoard::start_board().clone();
    assert_eq!(legal_moves_after_plies(&mut board1, 1), 20);
    assert_eq!(legal_moves_after_plies(&mut board1, 2), 400);
    assert_eq!(legal_moves_after_plies(&mut board1, 3), 8_902);
    assert_eq!(legal_moves_after_plies(&mut board1, 4), 197_281);
    assert_eq!(legal_moves_after_plies(&mut board1, 5), 4_865_617);
    assert_eq!(legal_moves_after_plies(&mut board1, 6), 119_060_679);
    assert_eq!(legal_moves_after_plies(&mut board1, 7), 3_195_913_043); //~10 min
    //assert_eq!(legal_moves_after_plies(&mut board, 8), 84_998_978_956);
}

#[test]
fn correct_move_gen_test1() {
    let mut board = ChessBoard::from_fen(
        "rnbqkbnr/pp2pppp/8/2pp4/3P4/5N2/PPP1PPPP/RNBQKB1R w KQkq - 0 1").unwrap();
    assert_eq!(legal_moves_after_plies(&mut board, 1), 30);
    assert_eq!(legal_moves_after_plies(&mut board, 2), 895);
}

#[test]
fn correct_move_gen_test2() {
    let mut board2 = ChessBoard::from_fen(
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1").unwrap();
    let mut moves = vec![];
    board2.generate_moves(&mut moves);
    assert_eq!(legal_moves_after_plies(&mut board2, 1), 48,
    "Expected 48 legal moves on:\n{}\nGot: {:?}", board2, moves);
    
    let mut temp_board = board2.clone();
    let mv = ChessMove::new(Square(48), Square(32));
    temp_board.do_move(mv);
    assert!(temp_board.en_passant_square().unwrap() == Square(40),
            "Error: En passant square was: {:?}", temp_board.en_passant_square());
    
    assert_eq!(legal_moves_after_plies(&mut board2, 2), 2_039);
    assert_eq!(legal_moves_after_plies(&mut board2, 3), 97_862);
}

#[test]
#[ignore]
fn correct_move_gen_test2_long() {
    let mut board2 = ChessBoard::from_fen(
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1").unwrap();
    assert_eq!(legal_moves_after_plies(&mut board2, 1), 48);
    assert_eq!(legal_moves_after_plies(&mut board2, 2), 2_039);
    assert_eq!(legal_moves_after_plies(&mut board2, 3), 97_862);
    assert_eq!(legal_moves_after_plies(&mut board2, 4), 4_085_604);
    assert_eq!(legal_moves_after_plies(&mut board2, 5), 193_690_734);
}

#[test]
fn correct_move_gen_test3() {
    let mut board3 = ChessBoard::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1").unwrap();
    assert_eq!(legal_moves_after_plies(&mut board3, 1), 14);
    assert_eq!(legal_moves_after_plies(&mut board3, 2), 191);
    assert_eq!(legal_moves_after_plies(&mut board3, 3), 2_812);
    assert_eq!(legal_moves_after_plies(&mut board3, 4), 43_238);
    assert_eq!(legal_moves_after_plies(&mut board3, 5), 674_641);
}

#[test]
#[ignore]
fn correct_move_gen_test3_long() {
    let mut board3 = ChessBoard::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1").unwrap();
    assert_eq!(legal_moves_after_plies(&mut board3, 1), 14);
    assert_eq!(legal_moves_after_plies(&mut board3, 2), 191);
    assert_eq!(legal_moves_after_plies(&mut board3, 3), 2_812);
    assert_eq!(legal_moves_after_plies(&mut board3, 4), 43_238);
    assert_eq!(legal_moves_after_plies(&mut board3, 5), 674_641);
    assert_eq!(legal_moves_after_plies(&mut board3, 6), 11_030_100);
    assert_eq!(legal_moves_after_plies(&mut board3, 7), 178_636_411);
}

#[test]
fn correct_move_gen_test4() {
    let mut board4 = ChessBoard::from_fen(
        "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1").unwrap();
    for (depth, nodes) in (1..6).zip(
        [6, 264, 9_467, 422_355].iter()) 
    {
        assert_eq!(legal_moves_after_plies(&mut board4, depth), *nodes);
    }
}

#[test]
#[ignore]
fn correct_move_gen_test4_long() {
    let mut board4 = ChessBoard::from_fen(
        "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1").unwrap();
    for (depth, nodes) in (1..7).zip(
        [6, 264, 9_467, 422_355, 15_833_319, 706_095_622].iter()) 
    {
        assert_eq!(legal_moves_after_plies(&mut board4, depth), *nodes);
    }
}

#[test]
fn correct_move_gen_test5() {
    let mut board5 = ChessBoard::from_fen(
        "rnbqkb1r/pp1p1ppp/2p5/4P3/2B5/8/PPP1NnPP/RNBQK2R w KQkq - 1 8").unwrap();
    for (depth, nodes) in (1..).zip([42, 1352, 53_392].iter()) 
    {
        assert_eq!(legal_moves_after_plies(&mut board5, depth), *nodes);
    }
}

#[test]
fn correct_move_gen_test6() {
    let mut board6 = ChessBoard::from_fen(
        "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10").unwrap();
    for (depth, nodes) in (1..5).zip(
        [46, 2_079, 89_890].iter()) {
        assert_eq!(legal_moves_after_plies(&mut board6, depth), *nodes);
    }
}

#[test]
#[ignore]
fn correct_move_gen_test6_long() {
    let mut board6 = ChessBoard::from_fen(
        "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10").unwrap();
    for (depth, nodes) in (1..7).zip(
        [46, 2_079, 89_890, 3_894_594, 164_075_551, 6_923_051_365].iter()) {
        assert_eq!(legal_moves_after_plies(&mut board6, depth), *nodes);
    }
}

#[test]
fn correct_move_gen_test7() {
    let mut board7 = ChessBoard::from_fen(
        "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();
    assert_eq!(legal_moves_after_plies(&mut board7, 3), 62_379);
}

use std::fmt;

/// Checks that the engine finds the total number of legal moves after n plies.
/// This provides a very strong indication that the move generator is correct
pub fn legal_moves_after_plies<B: Board + Eq + fmt::Debug + Clone>(board : &mut B, n : u8) -> u64
    where <B as Board>::Move: fmt::Display
{
    if n == 0 || board.game_result() != None { 1 }
    else {
        let mut total_moves = 0;
        let old_board = board.clone();
        let mut moves = vec![];
        board.generate_moves(&mut moves);
        for c_move in moves {
            let reverse_move = board.do_move(c_move.clone());
            total_moves += legal_moves_after_plies(board, n - 1);
            board.reverse_move(reverse_move);
            debug_assert_eq!(old_board, *board,
                             "Couldn't restore board after {}:\nOld:\n{:?}\nNew:\n{:?}",
                             c_move, old_board, board);
        }
        total_moves
    }
}

#[allow(dead_code)]
pub fn perf_prop<B: Board + Eq + fmt::Debug + Clone>(board: &mut B, n: u8) -> Vec<(B::Move, u64)>
    where <B as Board>::Move: fmt::Display
{
    let mut results = vec![];
    let old_board = board.clone();
    let mut moves = vec![];
    board.generate_moves(&mut moves);
    for c_move in moves {
        let reverse_move = board.do_move(c_move.clone());
        results.push((c_move.clone(), legal_moves_after_plies(board, n - 1)));
        board.reverse_move(reverse_move);
        debug_assert_eq!(old_board, *board,
                         "Couldn't restore board after {}:\nOld:\n{:?}\nNew:\n{:?}",
                         c_move, old_board, board);
    }
    results
}


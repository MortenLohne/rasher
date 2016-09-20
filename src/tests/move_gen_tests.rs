#[allow(unused_imports)]
use board::std_board;
use board::std_board::*;
use board::std_move::ChessMove;
use board::game_move::Move;
use board::board::Board;

use uci;
#[allow(unused_imports)]
use uci::UciBoard;

use alpha_beta;
use super::super::board::std_move_gen::move_gen;

extern crate time;

use std::sync::{Arc, Mutex};

#[allow(unused_imports)]
use board::std_board::PieceType::*;
#[allow(unused_imports)]
use board::board::Color::{Black, White};
#[allow(unused_imports)]
use ::Score::{Val, MateB, MateW};

/// Tests that Board.piece_at() and Square::from_alg() work correctly
/// Also assumes that std_board::START_BOARD is correct
#[test]
fn test_piece_at () {
    let square = |str| Square::from_alg(str).unwrap();
    
    let start_board = START_BOARD.clone();
    println!("{}", start_board);
    let d1 = square("d1");
    let e1 = square("e1");
    let a1 = square("a1");
    let h8 = square("h8");
    let e4 = square("e4");
    
    println!("e1: {}", e1);

    assert_eq!(start_board.piece_at(d1), Piece(Queen, White));
    assert_eq!(start_board.piece_at(e1), Piece(King, White));
    assert_eq!(start_board.piece_at(a1), Piece(Rook, White));
    assert_eq!(start_board.piece_at(h8), Piece(Rook, Black));
    assert_eq!(start_board.piece_at(e4), Piece(Empty, White));
}

#[test]
fn test_square () {
    assert_eq!(Square::from_alg("a3").unwrap(), Square::from_ints(0, 5));
    assert_eq!(Square::from_alg("e1").unwrap(), Square::from_ints(4, 7));
}

#[test]
fn test_score () {
    assert!(Val(0.0) > Val(-1.0));
    assert!(MateW(2) >= Val(100.0));
    assert!(MateW(3) < MateW(2));
    assert!(MateB(2) <= MateB(3));
    assert!(MateB(3) <= MateB(3));
    assert!(MateB(2) <= MateW(2));
}
/*
#[test]
fn test_is_attacked() {
    let board1 = Board::from_fen("rnbqkbnr/ppNppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1")
        .unwrap();
    let king_pos = board1.king_pos();
    assert_eq!(king_pos, Square(4));
    assert!(board1.is_attacked(&board1, king_pos) == true,
            format!("Error: King should be under attack here:\n{}", board1));

    let board2 = ChessBoard::from_fen("k7/1P6/8/8/8/8/8/6K1 b - - 0 1").unwrap();
    let king_pos = board2.king_pos();
    assert_eq!(king_pos, Square(0));
    assert!(move_gen::is_attacked(&board2, king_pos) == true,
               format!("Error: King should be under attack here:\n{}", board2));

    let board3 = ChessBoard::from_fen("2k5/2P5/8/8/8/8/8/5K2 b - - 0 1").unwrap();
    let king_pos = board3.king_pos();
    assert_eq!(king_pos, Square(2));
    assert!( move_gen::is_attacked(&board3, king_pos) == false,
               format!("Error: King should not be under attack here:\n{}", board3));
}
*/

#[test]
fn basic_movement_test() {
    let all_legal_moves = move_gen::all_legal_moves(&mut START_BOARD.clone());
    println!("All {} legal moves at start position: ", all_legal_moves.len());
    assert_eq!(all_legal_moves.len(), 20);
    for c_move in all_legal_moves {
        println!("{}", c_move);
    }
    

}

#[test]
fn respond_to_checks() {
    let mut board1 = ChessBoard::from_fen("rnbqkbnr/ppNppppp/8/8/8/8/PPPPPPPP/R1BQKBNR b KQkq - 0 1")
        .unwrap();
    assert!(move_gen::is_attacked(&board1, board1.king_pos()) == true,
            format!("Error: King should be under attack here:\n{}", board1));
    let legal_moves = move_gen::all_legal_moves(&mut board1);
    assert!(legal_moves.len() == 1,
            format!("Found {} legal moves, expected 1. Board:\n{}", legal_moves.len(), board1));

    let mut board2 = ChessBoard::from_fen("r1bqkb1r/pppppppp/5N2/8/3n4/8/PPPPPPPP/R1BQKBNR b KQkq - 0 1")
        .unwrap();
    assert!(move_gen::is_attacked(&board2, board2.king_pos()) == true,
            format!("Error: King should be under attack here:\n{}", board2));
    let legal_moves = move_gen::all_legal_moves(&mut board2);
    assert!(legal_moves.len() == 2,
            format!("Found {} legal moves, expected 2. Board:\n{}", legal_moves.len(), board2));

    let mut board3 = ChessBoard::from_fen("r1bqkb1r/pppppppp/5N2/8/3n4/4P3/PPPP1PPP/R1BQKBNR b KQkq - 0 1")
        .unwrap();
    assert!(move_gen::is_attacked(&board3, board3.king_pos()) == true,
            format!("Error: King should be under attack here:\n{}", board3));
    let legal_moves = move_gen::all_legal_moves(&mut board3);
    assert!(legal_moves.len() == 2,
            format!("Found {} legal moves, expected 2. Board:\n{}", legal_moves.len(), board3));

    let mut board4 = ChessBoard::from_fen("r1bqkb1r/pppp1ppp/5p2/7Q/3n4/4P3/PPPP1PPP/R1B1KBNR b KQkq - 0 1")
        .unwrap();
    assert!(move_gen::is_attacked(&board4, board4.king_pos()) == false,
            format!("Error: King should not be under attack here:\n{}", board4));
    let legal_moves = move_gen::all_legal_moves(&mut board4);
    assert!(legal_moves.len() == 29,
            format!("Found {} legal moves, expected 29. Board:\n{}", legal_moves.len(), board4));

    let mut board5 = ChessBoard::from_fen("k7/Q6K/8/8/8/8/8/8 b - - 0 1").unwrap();
    assert!(move_gen::is_attacked(&board5, board5.king_pos()) == true,
            format!("Error: King should be under attack here at {}:\n{}",
                    board5.king_pos(), board5));
    let legal_moves = move_gen::all_legal_moves(&mut board5);
    assert!(legal_moves.len() == 1,
            format!("Found {} legal moves, expected 1. Board:\n{}", legal_moves.len(), board5));

    let mut board6 = ChessBoard::from_fen("8/2p5/3p4/KP5r/1R3p1k/6P1/4P3/8 b - - 0 1").unwrap();
    assert!(move_gen::is_attacked(&board6, board6.king_pos()) == true,
            format!("Error: King should be under attack here:\n{}", board5));
    move_is_available_prop(&mut board6, ChessMove::from_alg("h4-g4").unwrap());
    move_is_unavailable_prop(&mut board6, ChessMove::from_alg("f4-g3").unwrap());
}

#[test]
fn basic_tactics_test() {
    // Basic knight fork
    let board1 = ChessBoard::from_fen("r3k3/2p5/8/3N4/1K5P/8/8/8 w - - 0 1").unwrap();
    let best_move1 = ChessMove::new( &board1, Square::from_alg("d5").unwrap(),
                                Square::from_alg("c7").unwrap());
    basic_tactics_prop(&board1, best_move1);

    // Checks that white can queen a pawn to mate
    let board2 = ChessBoard::from_fen("k7/p1P5/8/K7/8/8/8/8 w - - 0 1").unwrap();
    let best_move2 = ChessMove::new_prom( &board2, Square::from_alg("c7").unwrap(),
                                     Square::from_alg("c8").unwrap(), Piece(Queen, White) );
    basic_tactics_prop(&board2, best_move2);

    // Checks that black can underpromote a pawn to avoid mate
    let board3 = ChessBoard::from_fen("8/8/8/8/8/5K2/4p2R/5k2 b - - 0 1").unwrap();
    let best_move3 = ChessMove::new_prom( &board3, Square::from_alg("e2").unwrap(),
                                     Square::from_alg("e1").unwrap(), Piece(Knight, Black) );
    basic_tactics_prop(&board3, best_move3);

    // Checks that black can block a pawn to draw an endgame
    let board4 = ChessBoard::from_fen("1k6/2P5/3K4/8/8/8/8/8 b - - 0 1").unwrap();
    let best_move4 = ChessMove::new( &board4, Square::from_alg("b8").unwrap(),
                                Square::from_alg("c8").unwrap() );

    basic_tactics_prop(&board4, best_move4);

    let board5 = ChessBoard::from_fen("q6k/1P6/8/8/8/8/8/K7 w - - 0 1").unwrap();
    let best_move5 = ChessMove::from_alg("b7-a8Q").unwrap();
    basic_tactics_prop(&board5, best_move5);
}

#[allow(dead_code)]
/// Checks that the expected move is indeed played in the position
fn basic_tactics_prop(board : &ChessBoard, best_move : ChessMove) {
    let (score, tried_moves, node_count) : (_, Vec<ChessMove>,_) =
        alpha_beta::search_moves(board.clone(), Arc::new(Mutex::new(uci::EngineComm::new())), 
                                 uci::TimeRestriction::Depth(5),
                                 Arc::new(Mutex::new(None)));
    assert!(tried_moves[0].simple_eq(best_move),
            format!("Best move was {} with score {},\n ({}/{} internal/leaf nodes evaluated), 
expected {}, board:\n{}",
                    tried_moves[0], score, node_count.intern, node_count.leaf,
                    best_move, board));
}

#[test]
fn en_passant_test () {
    let mut board1 = ChessBoard::from_fen("7k/p7/8/1P6/8/8/8/7K b - - 0 1").unwrap();
    let move1 = ChessMove::from_alg("a7-a5").unwrap();
    let move2 = ChessMove::from_alg("b5-a6").unwrap();
    // Do a move that allows en passant
    board1.do_move(move1);

    // Check that en passant is the best move, and do it
    //move_is_available_prop(&mut board1, move2);
    basic_tactics_prop(&board1, move2);
    board1.do_move(move2);

    // The board that's expected
    let mut board_expected1 = ChessBoard::from_fen("7k/8/P7/8/8/8/8/7K b - - 0 3").unwrap();
    // Add the same moves, to ensure complete equality
    board_expected1.moves.push(move1);
    board_expected1.moves.push(move2);

    // Check that the boards are completely equal
    assert!(board1 == board_expected1, format!("Boards were not equal. Board1:\n{}\nBoard2:\n{}",
                                               board1, board_expected1));


    // Check that an en passant move is only available for one move after the pawn push
    let mut board2 = ChessBoard::from_fen("7k/p7/8/1P6/8/8/8/7K w - - 0 1").unwrap();
    let move1 = ChessMove::new(&board2, Square::from_alg("a7").unwrap(),
                          Square::from_alg("a5").unwrap());
    board2.do_move(move1);
    let move2 = ChessMove::new(&board2, Square::from_alg("h1").unwrap(),
                          Square::from_alg("h2").unwrap());
    board2.do_move(move2);
    let move3 = ChessMove::new(&board2, Square::from_alg("h8").unwrap(),
                          Square::from_alg("h7").unwrap());
    board2.do_move(move3);

    assert!(move_gen::all_legal_moves(&mut board2).len() == 6,
            format!("Only 6 moves should be available, board:\n{}", board2));
    
}

#[test]
fn is_pinned_to_piece_test() {
    let board1 = ChessBoard::from_fen("k7/ppp5/2P5/QK1B4/8/8/8/8 b - - 0 1").unwrap();
    is_pinned_prop(&board1, Square(0), Square(8), false);
    is_pinned_prop(&board1, Square(8), Square(0), true);
    is_pinned_prop(&board1, Square(9), Square(0), false);
    is_pinned_prop(&board1, Square(10), Square(0), false);
}

#[allow(dead_code)]
fn is_pinned_prop(board : &ChessBoard, pinee_pos : Square, pinner_pos : Square, is_pinned : bool) {
    if is_pinned {
        assert!(move_gen::is_pinned_to_piece(&board, pinee_pos, pinner_pos),
               format!("{} should be pinned to {}, but isn't. Board:\n{}",
                       board.piece_at(pinee_pos).0, board.piece_at(pinner_pos).0, board));
    }
    else {
        assert!(!move_gen::is_pinned_to_piece(&board, pinee_pos, pinner_pos),
               format!("{} should not be pinned to {}. Board:\n{}",
                       board.piece_at(pinee_pos).0, board.piece_at(pinner_pos).0, board));
    }
}

#[test]
fn castling_test () {
    // Checks that white kingside castle works
    let board1 = ChessBoard::from_fen("5k1r/6pp/4Q3/8/8/8/8/4K2R w K - 0 1").unwrap();
    let move1 = ChessMove::from_alg("e1-g1").unwrap();
    basic_tactics_prop(&board1, move1);

    // Checks that white can't castle while in check
    let mut board2 = ChessBoard::from_fen("k3r3/8/8/8/8/8/3P1PPP/3RK2R w K - 0 1").unwrap();
    assert!(move_gen::all_legal_moves(&mut board2).len() == 1,
            format!("Only 1 move should be available, board:\n{}", board2));

    // Checks that black queenside castling works in a tactic
    let board3 = ChessBoard::from_fen("r3k3/1R6/8/8/8/8/8/3K4 b q - 0 1").unwrap();
    let move3 = ChessMove::from_alg("e8-c8").unwrap();
    basic_tactics_prop(&board3, move3);

    // Positions with both castlings available
    let mut board4 = ChessBoard::from_fen(
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1").unwrap();
    move_is_available_prop(&mut board4, ChessMove::from_alg("e1-c1").unwrap());
    move_is_available_prop(&mut board4, ChessMove::from_alg("e1-g1").unwrap());
    move_is_unavailable_prop(&mut board4, ChessMove::from_alg("e1-h1").unwrap());

    // Positions were castling is legal, but rook is attacked
    let mut board5 = ChessBoard::from_fen(
        "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();
    move_is_available_prop(&mut board5, ChessMove::from_alg("e1-g1").unwrap());
    move_is_available_prop(&mut board5, ChessMove::from_alg("e1-f1").unwrap());
}

#[allow(dead_code)]
fn move_is_available_prop(board : &mut ChessBoard, c_move : ChessMove) {
    let all_moves = move_gen::all_legal_moves(board);
    assert!(all_moves.iter().find(|mv|c_move.simple_eq(**mv)).is_some(),
            "{} should be legal here, board:{}Legal moves: {:?}",
            c_move.to_alg(), board, all_moves);
}

#[allow(dead_code)]
fn move_is_unavailable_prop(board : &mut ChessBoard, c_move : ChessMove) {
    let all_moves = move_gen::all_legal_moves(board);
    assert!(!all_moves.iter().find(|mv|c_move.simple_eq(**mv)).is_some(),
            "{} should not be legal here, board:{}Legal moves: {:?}",
            c_move.to_alg(), board, all_moves);
}

#[test]
fn correct_move_gen_test1() {
    let mut board1 = std_board::START_BOARD.clone();
    println!("{}", board1);
    assert_eq!(legal_moves_after_plies(&mut board1, 1), 20);
    assert_eq!(legal_moves_after_plies(&mut board1, 2), 400);
    assert_eq!(legal_moves_after_plies(&mut board1, 3), 8_902);
    assert_eq!(legal_moves_after_plies(&mut board1, 4), 197_281);
    //assert_eq!(legal_moves_after_plies(&mut board1, 5), 4_865_609);
}

#[test]
#[ignore]
fn correct_move_gen_test1_long() {
    let mut board1 = std_board::START_BOARD.clone();
    assert_eq!(legal_moves_after_plies(&mut board1, 1), 20);
    assert_eq!(legal_moves_after_plies(&mut board1, 2), 400);
    assert_eq!(legal_moves_after_plies(&mut board1, 3), 8_902);
    assert_eq!(legal_moves_after_plies(&mut board1, 4), 197_281);
    assert_eq!(legal_moves_after_plies(&mut board1, 5), 4_865_609);
    assert_eq!(legal_moves_after_plies(&mut board1, 6), 119_060_324);
    assert_eq!(legal_moves_after_plies(&mut board1, 7), 3_195_901_860); //~10 min
    //assert_eq!(legal_moves_after_plies(&mut board, 8), 84_998_978_956);
}

#[test]
fn correct_move_gen_test2() {
    let mut board2 = ChessBoard::from_fen(
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1").unwrap();
    assert_eq!(legal_moves_after_plies(&mut board2, 1), 48);
    
    let mut temp_board = board2.clone();
    let mv = ChessMove::new(&temp_board, Square(48), Square(32));
    temp_board.do_move(mv);
    assert!(temp_board.en_passant_square().unwrap() == Square(40),
            "Error: En passant square was: {:?}", temp_board.en_passant_square());
    
    assert_eq!(legal_moves_after_plies(&mut board2, 2), 2_039);
    assert_eq!(legal_moves_after_plies(&mut board2, 3), 97_862);
    assert_eq!(legal_moves_after_plies(&mut board2, 4), 4_085_603);
}

#[test]
#[ignore]
fn correct_move_gen_test2_long() {
    let mut board2 = ChessBoard::from_fen(
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1").unwrap();
    assert_eq!(legal_moves_after_plies(&mut board2, 1), 48);
    assert_eq!(legal_moves_after_plies(&mut board2, 2), 2_039);
    assert_eq!(legal_moves_after_plies(&mut board2, 3), 97_862);
    assert_eq!(legal_moves_after_plies(&mut board2, 4), 4_085_603);
    assert_eq!(legal_moves_after_plies(&mut board2, 5), 193_690_690);
}

#[test]
fn correct_move_gen_test3() {
    let mut board3 = ChessBoard::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1").unwrap();
    assert_eq!(legal_moves_after_plies(&mut board3, 1), 14);
    assert_eq!(legal_moves_after_plies(&mut board3, 2), 191);
    assert_eq!(legal_moves_after_plies(&mut board3, 3), 2_812);
    assert_eq!(legal_moves_after_plies(&mut board3, 4), 43_238);
    assert_eq!(legal_moves_after_plies(&mut board3, 5), 674_624);
    assert_eq!(legal_moves_after_plies(&mut board3, 6), 11_030_083);
}

#[test]
#[ignore]
fn correct_move_gen_test3_long() {
    let mut board3 = ChessBoard::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1").unwrap();
    assert_eq!(legal_moves_after_plies(&mut board3, 1), 14);
    assert_eq!(legal_moves_after_plies(&mut board3, 2), 191);
    assert_eq!(legal_moves_after_plies(&mut board3, 3), 2_812);
    assert_eq!(legal_moves_after_plies(&mut board3, 4), 43_238);
    assert_eq!(legal_moves_after_plies(&mut board3, 5), 674_624);
    assert_eq!(legal_moves_after_plies(&mut board3, 6), 11_030_083);
    assert_eq!(legal_moves_after_plies(&mut board3, 7), 178_633_661);
}

#[test]
fn correct_move_gen_test4() {
    let mut board4 = ChessBoard::from_fen(
        "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1").unwrap();
    for (depth, nodes) in (1..6).zip(
        [6, 264, 9_467, 422_333, 15_833_292, 706_045_033].iter()) 
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
        [6, 264, 9_467, 422_333, 15_833_292, 706_045_033].iter()) 
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
        [46, 2_079, 89_890, 3_894_594, 164_075_551, 6_923_051_137].iter()) {
        assert_eq!(legal_moves_after_plies(&mut board6, depth), *nodes);
    }
}

#[test]
#[ignore]
fn correct_move_gen_test6_long() {
    let mut board6 = ChessBoard::from_fen(
        "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10").unwrap();
    for (depth, nodes) in (1..7).zip(
        [46, 2_079, 89_890, 3_894_594, 164_075_551, 6_923_051_137].iter()) {
        assert_eq!(legal_moves_after_plies(&mut board6, depth), *nodes);
    }
}

#[test]
fn correct_move_gen_test7() {
    let mut board7 = ChessBoard::from_fen(
        "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8").unwrap();
    assert_eq!(legal_moves_after_plies(&mut board7, 3), 62_379);
}

#[test]
fn correct_move_gen_test8() {
    let mut board = ChessBoard::from_fen(
        "rnbqkbnr/pp2pppp/8/2pp4/3P4/5N2/PPP1PPPP/RNBQKB1R w KQkq - 0 1").unwrap();
    println!("{}", board);
    assert_eq!(legal_moves_after_plies(&mut board, 1), 30);
    assert_eq!(legal_moves_after_plies(&mut board, 2), 895);
}

/// Checks that the engine finds the total number of legal moves after n plies.
/// This provides a very strong indication that the move generator is correct
#[allow(dead_code)]
fn legal_moves_after_plies(board : &mut ChessBoard, n : u8) -> u64 {
    if n == 0 { 1 }
    else {
        let mut total_moves = 0;
        for c_move in move_gen::all_legal_moves(board) {
            let mut old_board = board.clone();
            
            board.do_move(c_move);
            total_moves += legal_moves_after_plies(board, n - 1);
            board.undo_move(c_move);

            debug_assert!(&mut old_board == board,
                          format!("Board was not the same after undoing move {}:\nOld:{}New:{}",
                                  c_move, old_board, board));
        }
        total_moves
    }
}

/*
// Prevent the benchmark from running forever by interrupting it after 10 runs
#[allow(dead_code)]
static mut BENCHES_RUN : u32 = 0;

#[bench]
fn eval_start_pos_bench (bencher : &mut test::Bencher) {
    let start_time = time::get_time();
    let board = START_BOARD.clone();
    let mut total_nodes = ::NodeCount{ intern: 0, leaf: 0, total: 0 };
    bencher.iter(|| {
        unsafe {
            if BENCHES_RUN > 5 {
                return;
            }
        }
        let (_, _, node_counter) = alpha_beta::search_moves(
            board.clone(), Arc::new(Mutex::new(uci::EngineComm::new())), 
            uci::TimeRestriction::Depth(6), Arc::new(Mutex::new(None)));
        total_nodes.intern += node_counter.intern;
        total_nodes.leaf += node_counter.leaf;
        total_nodes.total += node_counter.total;
    });
    
    unsafe {
        if BENCHES_RUN > 5 {
            return;
        }
        BENCHES_RUN += 1;
    }
    let ms_taken = (time::get_time() - start_time).num_milliseconds();
    println!("{} nodes/s",
             (total_nodes.intern + total_nodes.leaf) as f32 / ms_taken as f32);
}
*/

use board::sjadam_board::SjadamBoard;
use uci::UciBoard;
use board::sjadam_move::SjadamMove;
use search_algorithms::game_move::Move;
use search_algorithms::board::{GameResult, EvalBoard};
use tests::move_gen_tests;

#[test]
fn correct_move_gen_start_pos() {
    let start_board = SjadamBoard::start_board().clone();
    let moves = start_board.all_legal_moves();
    assert_eq!(moves.len(), 146, "Found {} moves: {:?}, expected 146",
               moves.len(), moves);
}

/// Tests whether a pawn is allowed to sjadam over an opponent pawn
#[test]
fn jump_over_opponent() {
    let board = SjadamBoard::from_fen("8/8/8/7p/7P/8/8/K1k5 w - - 1 1").unwrap();
    assert_eq!(board.game_result(), None);
    let moves = board.all_legal_moves();
    assert_eq!(board.all_legal_moves().len(), 5,
               "Expected 5 legal moves, found {} moves: {:?}\n{:?}",
               moves.len(), moves, board);
}

#[test]
fn no_stalemate_test() {
    let board = SjadamBoard::from_fen("8/8/8/5p1k/3p1Q2/3P2R1/P3p2P/4K3 b - - 2 39").unwrap();
    assert_eq!(board.game_result(), None);
    let moves = board.all_legal_moves();
    // The exact number may be wrong due to other bugs
    // So don't trust this test
    assert_eq!(board.all_legal_moves().len(), 13,
               "Expected 13 legal moves, found {} moves: {:?}\n{:?}",
               moves.len(), moves, board);
}

#[test]
fn can_take_king_while_checked() {
    let board = SjadamBoard::from_fen("r2q2n1/pp4pp/6k1/2Nb4/8/5P2/P1P1PK2/2B3q1 w - - 0 1").unwrap();
    // Black is really sjadammated, so this is also a possible outcome
    if board.game_result() != Some(GameResult::WhiteWin) { 
        let moves = board.all_legal_moves();
        
        let correct_move = SjadamMove::from_alg("c5e5e5g6").unwrap();
        assert!(board.all_legal_moves().contains(&correct_move),
                "White couldn't take king on board:\n{:?}Moves: {:?}",
                board, moves);
    }
}

#[test]
fn sjadam_move_to_promote_pawn() {
    let board = SjadamBoard::from_fen("k7/5R2/5P2/8/8/8/8/7K w - - 0 1").unwrap();
    let moves = board.all_legal_moves();
        
    let correct_move = SjadamMove::from_alg("f6f8-").unwrap();
    assert!(board.all_legal_moves().contains(&correct_move),
            "White couldn't promote in sjadam on board:\n{:?}Moves: {:?}",
            board, moves);
}

#[test]
fn no_moves_on_back_rank() {
    let board = SjadamBoard::from_fen("8/K7/P7/8/8/8/8/7k w - - 0 1").unwrap();
    let moves = board.all_legal_moves();
    assert_eq!(moves.len(), 9, "Expected to find {} moves, found {}: {:?}", 9, moves.len(), moves);
}

#[test]
fn startpos_perf_test() {
    let mut board = SjadamBoard::start_board();
    for (n, &moves) in (1..4).zip([146, 21_469, 3_268_163].iter()) {
        assert_eq!(move_gen_tests::legal_moves_after_plies(&mut board, n), moves);
    }
}

#[test]
fn castling_en_passant_perf_test() {
    let mut board = SjadamBoard::start_board();
    board.do_move(SjadamMove::from_alg("g1e3-").unwrap());
    board.do_move(SjadamMove::from_alg("g8e6-").unwrap());
    board.do_move(SjadamMove::from_alg("f1d3d3c4").unwrap());
    board.do_move(SjadamMove::from_alg("e8g8-").unwrap());
    board.do_move(SjadamMove::from_alg("-c2c3").unwrap());
    board.do_move(SjadamMove::from_alg("-d7d5").unwrap());
    board.do_move(SjadamMove::from_alg("c1a3-").unwrap());
    board.do_move(SjadamMove::from_alg("-b7b5").unwrap());
    for (n, &moves) in (1..4).zip([160, 26_959, 4_259_773].iter()) {
        let result = move_gen_tests::legal_moves_after_plies(&mut board, n);
        assert_eq!(result, moves,
                   "Expected {} moves, found {} on board:\n{:?}.", moves, result, board);
    }
}

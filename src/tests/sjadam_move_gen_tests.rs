use board::sjadam_board::SjadamBoard;
use uci::UciBoard;
use board::sjadam_move::SjadamMove;
use board::std_board::*;
use board::std_move::ChessMove;
use search_algorithms::game_move::Move;
use search_algorithms::board::{GameResult, EvalBoard};

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
    assert_eq!(board.all_legal_moves().len(), 3,
               "Expected 3 legal moves, found {} moves: {:?}\n{:?}",
               moves.len(), moves, board);
}

#[test]
fn no_stalemate_test() {
    let board = SjadamBoard::from_fen("8/8/8/5p1k/3p1Q2/3P2R1/P3p2P/4K3 b - - 2 39").unwrap();
    assert_eq!(board.game_result(), None);
    let moves = board.all_legal_moves();
    // The exact number may be wrong due to other bugs
    // So don't trust this test
    assert_eq!(board.all_legal_moves().len(), 8,
               "Expected 8 legal moves, found {} moves: {:?}\n{:?}",
               moves.len(), moves, board);
}

#[test]
fn can_take_king_while_checked() {
    let board = SjadamBoard::from_fen("r2q2n1/pp4pp/6k1/2Nb4/8/5P2/P1P1PK2/2B3q1 w - - 0 1").unwrap();
    // Black is really sjadammated, so this is also a possible outcome
    if board.game_result() != Some(GameResult::WhiteWin) { 
        let moves = board.all_legal_moves();
        
        let mut correct_move = SjadamMove::from_sjadam_move(Square::from_alg("c5").unwrap(),
                                                        Square::from_alg("e5").unwrap(),
                                                        &board);
        correct_move.chess_move = Some(ChessMove::from_alg("e5g6").unwrap());
        assert!(board.all_legal_moves().contains(&correct_move),
                "White couldn't take king on board:\n{:?}Moves: {:?}",
                board, moves);
    }
}

/*
SjadamBoard { base_board: 
[ ][ ][ ][ ][ ][ ][ ][ ]
[ ][ ][ ][ ][ ][ ][ ][ ]
[ ][ ][ ][ ][ ][ ][ ][ ]
[ ][ ][ ][ ][ ][p][ ][k]
[ ][ ][ ][p][ ][Q][ ][ ]
[ ][ ][ ][P][ ][ ][R][ ]
[P][ ][ ][ ][p][ ][ ][P]
[ ][ ][ ][ ][K][ ][ ][ ]
To move: Black, move_number: 39, flags: 0, half_move_clock: 2

SjadamBoard { base_board: 
[ ][ ][ ][ ][ ][ ][ ][ ]
[ ][ ][ ][ ][ ][ ][ ][ ]
[ ][ ][ ][ ][ ][ ][ ][ ]
[ ][ ][ ][ ][ ][p][ ][k]
[ ][ ][ ][p][ ][Q][ ][ ]
[ ][ ][ ][P][ ][ ][R][ ]
[P][ ][ ][ ][p][ ][ ][P]
[ ][ ][ ][ ][K][ ][ ][ ]
To move: Black, move_number: 2, flags: 0, half_move_clock: 39
*/

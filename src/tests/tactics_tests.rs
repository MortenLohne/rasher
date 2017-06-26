use board::std_board::PieceType::*;
use search_algorithms::board::Color::{Black, White};
use search_algorithms::alpha_beta::Score::{Val, BlackWin, WhiteWin};

use board::std_board::*;
use board::std_move::ChessMove;
use search_algorithms::game_move::Move;
use search_algorithms::board::EvalBoard;
use search_algorithms::alpha_beta;

use uci;
use uci::UciBoard;

use std::sync;

#[allow(dead_code)]
/// Checks that the expected move is indeed played in the position
pub fn basic_tactics_prop(board : &ChessBoard, best_move : ChessMove) {
    let (handle, channel) = alpha_beta::start_uci_search(
        board.clone(), uci::TimeRestriction::Depth(5),
        uci::EngineOptions::new(),
        sync::Arc::new(sync::Mutex::new(uci::EngineComm::new())), None);
    
    let (score, move_str) = uci::get_uci_move_checked(handle, channel).unwrap();
    
    let game_move = ChessMove::from_alg(&move_str).unwrap();
    
    assert_eq!(game_move, best_move,
               "Best move was {:?} with score {}, expected {:?}, board:\n{}",
               game_move, score,
               best_move, board);
}

#[test]
fn basic_tactics_test() {
    // Basic knight fork
    let board1 = ChessBoard::from_fen("r3k3/2p5/8/3N4/1K5P/8/8/8 w - - 0 1").unwrap();
    let best_move1 = ChessMove::new(Square::from_alg("d5").unwrap(),
                                Square::from_alg("c7").unwrap());
    basic_tactics_prop(&board1, best_move1);

    // Checks that white can queen a pawn to mate
    let board2 = ChessBoard::from_fen("k7/p1P5/8/K7/8/8/8/8 w - - 0 1").unwrap();
    let best_move2 = ChessMove::new_prom(Square::from_alg("c7").unwrap(),
                                     Square::from_alg("c8").unwrap(), Queen );
    basic_tactics_prop(&board2, best_move2);

    // Checks that black can underpromote a pawn to avoid mate
    let board3 = ChessBoard::from_fen("8/8/8/8/8/5K2/4p2R/5k2 b - - 0 1").unwrap();
    let best_move3 = ChessMove::new_prom(Square::from_alg("e2").unwrap(),
                                     Square::from_alg("e1").unwrap(), Knight );
    basic_tactics_prop(&board3, best_move3);

    // Checks that black can block a pawn to draw an endgame
    let board4 = ChessBoard::from_fen("1k6/2P5/3K4/8/8/8/8/8 b - - 0 1").unwrap();
    let best_move4 = ChessMove::new(Square::from_alg("b8").unwrap(),
                                Square::from_alg("c8").unwrap() );

    basic_tactics_prop(&board4, best_move4);

    let board5 = ChessBoard::from_fen("q6k/1P6/8/8/8/8/8/K7 w - - 0 1").unwrap();
    let best_move5 = ChessMove::from_alg("b7a8Q").unwrap();
    basic_tactics_prop(&board5, best_move5);
}

#[test]
fn multipv_mates_test() {
    let board = ChessBoard::from_fen("NrB5/pp1R4/kp5R/3P2p1/K7/8/1P6/8 w - - 0 1").unwrap();
}

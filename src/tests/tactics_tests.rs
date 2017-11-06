use board::std_board::PieceType::*;
use search_algorithms::board::Color::{Black, White};
use search_algorithms::alpha_beta::Score::{Val, BlackWin, WhiteWin};

use board::std_board::*;
use board::std_move::ChessMove;
use search_algorithms::board::EvalBoard;
use search_algorithms::alpha_beta;
use search_algorithms::alpha_beta::Score;
use search_algorithms::alpha_beta::Score::*;

use uci;
use uci::UciBoard;

use std::sync;

#[allow(dead_code)]
/// Checks that the expected move is indeed played in the position
pub fn basic_tactics_prop(board : &ChessBoard, best_move : ChessMove) {
    let (handle, channel) = alpha_beta::start_uci_search(
        board.clone(), uci::TimeRestriction::Depth(4),
        uci::EngineOptions::new(),
        sync::Arc::new(sync::Mutex::new(uci::EngineComm::new())), None);
    
    let (score, move_str) = uci::get_uci_move(handle, channel).unwrap();
    
    let game_move = board.from_alg(&move_str).unwrap();
    
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

    // Check that white can capture a queen to promote to a winning position
    let board5 = ChessBoard::from_fen("q6k/1P6/8/8/8/8/8/K7 w - - 0 1").unwrap();
    let best_move5 = board5.from_alg("b7a8Q").unwrap();
    basic_tactics_prop(&board5, best_move5);
}

#[test]
fn mate_in_two_test() {
    let board = ChessBoard::from_fen("2krr3/pp3ppp/8/2b2P2/5B2/2P3NP/PP2pnP1/R3K2R b - - 4 4").unwrap();
    let best_move = board.from_alg("d8d1").unwrap();
    basic_tactics_prop(&board, best_move);
    
}

#[test]
fn multipv_mates_test() {
    let board = ChessBoard::from_fen("NrB5/pp1R4/kp5R/3P2p1/K7/8/1P6/8 w - - 0 1").unwrap();
    let mut options = uci::EngineOptions::new();
    options.multipv = 4;
    let (handle, channel) = alpha_beta::start_uci_search(
        board.clone(), uci::TimeRestriction::Mate(5),
        options, sync::Arc::new(sync::Mutex::new(uci::EngineComm::new())), None);
    
    let results = uci::get_uci_multipv(handle, channel, 4).unwrap();
    assert_eq!(results[0].0, Score::WhiteWin(1));
    assert_eq!(results[1].0, Score::WhiteWin(2));
    assert_eq!(results[2].0, Score::WhiteWin(3));
    assert_eq!(results[3].0, Score::WhiteWin(3));
}

#[test]
#[ignore]
fn multipv_mates_test_long() {
    let board = ChessBoard::from_fen("NrB5/pp1R4/kp5R/3P2p1/K7/8/1P6/8 w - - 0 1").unwrap();
    let mut options = uci::EngineOptions::new();
    options.multipv = 6;
    let (handle, channel) = alpha_beta::start_uci_search(
        board.clone(), uci::TimeRestriction::Mate(7),
        options, sync::Arc::new(sync::Mutex::new(uci::EngineComm::new())), None);
    
    let results = uci::get_uci_multipv(handle, channel, 6).unwrap();
    assert_eq!(results[0].0, Score::WhiteWin(1));
    assert_eq!(results[1].0, Score::WhiteWin(2));
    assert_eq!(results[2].0, Score::WhiteWin(3));
    assert_eq!(results[3].0, Score::WhiteWin(3));
    assert_eq!(results[4].0, Score::WhiteWin(4));
}

#[test]
fn multipv_mates_test2() {
    let board = ChessBoard::from_fen("8/8/8/8/4b3/7k/r4PNP/7K b - - 2 5").unwrap();
    let mut options = uci::EngineOptions::new();
    options.multipv = 4;
    let (handle, channel) = alpha_beta::start_uci_search(
        board.clone(), uci::TimeRestriction::Mate(5),
        options, sync::Arc::new(sync::Mutex::new(uci::EngineComm::new())), None);
    
    let results = uci::get_uci_multipv(handle, channel, 4).unwrap();
    assert_eq!(results[0].0, Score::BlackWin(1));
    assert_eq!(results[1].0, Score::BlackWin(2));
    assert_eq!(results[2].0, Score::BlackWin(3));
    assert_eq!(results[3].0, Score::BlackWin(3));
}

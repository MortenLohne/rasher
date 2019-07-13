use board::std_board::PieceType::*;
use board_game_traits::board::Color::{Black, White};
use search_algorithms::alpha_beta::Score::Val;

use board::std_board::*;
use board::std_move::ChessMove;
use board_game_traits::board::EvalBoard;
use search_algorithms::alpha_beta;
use search_algorithms::alpha_beta::Score;
use search_algorithms::alpha_beta::Score::*;
use uci_engine::UciEngine;
use uci;
use pgn_traits::pgn::PgnBoard;

use std::sync;
use search_algorithms::alpha_beta::AlphaBeta;
use uci_engine::UciOption;
use uci_engine::UciOptionType;

#[allow(dead_code)]
/// Checks that the expected move is indeed played in the position
pub fn basic_tactics_prop(board : &ChessBoard, best_move : ChessMove) {

    let engine = AlphaBeta::init();
    let (score, game_move) =
        engine.best_move(board.clone(),
                         uci::TimeRestriction::Depth(6),
                         None).unwrap();

    assert_eq!(game_move, best_move,
               "Best move was {:?} with score {:?}, expected {:?}, board:\n{}",
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
    let best_move5 = board5.move_from_lan("b7a8Q").unwrap();
    basic_tactics_prop(&board5, best_move5);
}

#[test]
fn mate_in_two_test() {
    let board = ChessBoard::from_fen("2krr3/pp3ppp/8/2b2P2/5B2/2P3NP/PP2pnP1/R3K2R b - - 4 4").unwrap();
    let best_move = board.move_from_lan("d8d1").unwrap();
    basic_tactics_prop(&board, best_move);
    
}

#[test]
fn multipv_mates_test() {
    let board = ChessBoard::from_fen("NrB5/pp1R4/kp5R/3P2p1/K7/8/1P6/8 w - - 0 1").unwrap();

    let mut engine = AlphaBeta::init();
    engine.set_uci_option(UciOption {
        name: "multipv".into(),
        option_type: UciOptionType::Spin(4, 1, 65536)
    });

    let results =
        engine.best_moves_multipv(board.clone(), uci::TimeRestriction::Mate(5), None).unwrap();

    assert_eq!(results[0].0.uci_string(White), "mate 1");
    assert_eq!(results[1].0.uci_string(White), "mate 2");
    assert_eq!(results[2].0.uci_string(White), "mate 3");
    assert_eq!(results[3].0.uci_string(White), "mate 3");
}

#[test]
#[ignore]
fn multipv_mates_test_long() {
    let board = ChessBoard::from_fen("NrB5/pp1R4/kp5R/3P2p1/K7/8/1P6/8 w - - 0 1").unwrap();

    let mut engine = AlphaBeta::init();
    engine.set_uci_option(UciOption {
        name: "multipv".into(),
        option_type: UciOptionType::Spin(5, 1, 65536)
    });

    let results =
        engine.best_moves_multipv(board.clone(), uci::TimeRestriction::Mate(7), None).unwrap();

    assert_eq!(results[0].0.uci_string(White), "mate 1");
    assert_eq!(results[1].0.uci_string(White), "mate 2");
    assert_eq!(results[2].0.uci_string(White), "mate 3");
    assert_eq!(results[3].0.uci_string(White), "mate 3");
    assert_eq!(results[4].0.uci_string(White), "mate 4");
}

#[test]
fn multipv_mates_test2() {
    let board = ChessBoard::from_fen("8/8/8/8/4b3/7k/r4PNP/7K b - - 2 5").unwrap();

    let mut engine = AlphaBeta::init();
    engine.set_uci_option(UciOption {
        name: "multipv".into(),
        option_type: UciOptionType::Spin(4, 1, 65536)
    });

    let results =
        engine.best_moves_multipv(board.clone(), uci::TimeRestriction::Mate(5), None).unwrap();

    assert_eq!(results[0].0.uci_string(Black), "mate -1");
    assert_eq!(results[1].0.uci_string(Black), "mate -2");
    assert_eq!(results[2].0.uci_string(Black), "mate -3");
    assert_eq!(results[3].0.uci_string(Black), "mate -3");
}

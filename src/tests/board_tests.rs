use board::chess::board::PieceType;
use board::chess::board::Square;
use board::chess::board::ChessBoard;
use pgn_traits::pgn::PgnBoard;
use board_game_traits::board::{Board, Color, GameResult};
use board::chess::mv::ChessMove;
use tests::tools;
use search_algorithms::alpha_beta::{AlphaBeta};
use uci;
use uci_engine::UciEngine;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hasher, Hash};

#[test]
fn repetitions_test1() {
    let mut board = ChessBoard::from_fen("r3k2r/1bp4p/p3p1N1/5pNQ/1p2p3/1P2P3/P1K2qPP/R7 w kq - 0 20").unwrap();
    for mv_str in "Kb1 Qe1+ Kc2 Qf2+ Kb1 Qe1+ Kb2".split_whitespace() {
        let mv = board.move_from_san(mv_str).unwrap();
        board.do_move(mv);
    }

    // Check that the engine does not play a move that allows a repetition
    let engine = AlphaBeta::init();
    let (_, mv) =
        engine.best_move(board.clone(),
                         uci::TimeRestriction::Depth(4),
                         None).unwrap();

    assert_ne!(mv, board.move_from_san("Qf2+").unwrap(),
    "{:?} was played which allows a draw, past hashes: {:?}",
    mv, board.past_board_hashes);

    board.do_move(board.move_from_san("Qf2+").unwrap());

    // Check that it plays the draw by repetition
    let engine = AlphaBeta::init();
    let (score, mv) =
        engine.best_move(board.clone(),
                         uci::TimeRestriction::Depth(4),
                         None).unwrap();

    assert_eq!(mv, board.move_from_san("Kb1").unwrap(),
               "{:?} was played which declines a draw, past hashes: {:?}",
               mv, board.past_board_hashes);
    assert_eq!(score.to_cp(Color::White), 0);

    board.do_move(board.move_from_san("Kb1").unwrap());

    assert_eq!(board.game_result(), Some(GameResult::Draw));
}

#[test]
fn repetitions_test2() {
    let mut board = ChessBoard::from_fen("1r2r3/Q4pkp/5bp1/3P1p2/2P5/1P6/P3NPPP/1R3RK1 b - - 0 24").unwrap();
    for mv_str in " Ra8 Qb7 Rab8 Qc7 Rbc8 Qb7 Rb8 Qc7 Rbc8".split_whitespace() {
        let mv = board.move_from_san(mv_str).unwrap();
        board.do_move(mv);
    }

    // Check that the engine does not play a move that allows a repetition
    let engine = AlphaBeta::init();
    let (_, mv) =
        engine.best_move(board.clone(),
                         uci::TimeRestriction::Depth(4),
                         None).unwrap();

    assert_ne!(mv, board.move_from_san("Qb7").unwrap(),
               "{:?} was played which allows a draw, past hashes: {:?}",
               mv, board.past_board_hashes);

    board.do_move(board.move_from_san("Qb7").unwrap());

    // Check that it plays the draw by repetition
    let engine = AlphaBeta::init();
    let (score, mv) =
        engine.best_move(board.clone(),
                         uci::TimeRestriction::Depth(4),
                         None).unwrap();

    let mut hasher = DefaultHasher::new();
    board.hash(&mut hasher);
    let hash = hasher.finish();

    assert_eq!(mv, board.move_from_san("Rb8").unwrap(),
               "{:?} was played which declines a draw, hash: {}, past hashes: {:?}",
               mv, hash, board.past_board_hashes);
    assert_eq!(score.to_cp(Color::Black), 0);

    board.do_move(board.move_from_san("Rb8").unwrap());

    assert_eq!(board.game_result(), Some(GameResult::Draw));
}

#[test]
fn from_alg_test() {
    assert_eq!(Square::from_alg("a8").unwrap(), Square(0));
    assert_eq!(Square::from_alg("b8").unwrap(), Square(1));
    assert_eq!(Square::from_alg("g1").unwrap(), Square(62));
}

#[test]
fn to_san_test() {
    let board = ChessBoard::start_board();
    assert_eq!(board.move_to_san(&ChessMove::new(Square::from_alg("e2").unwrap(),
                                                 Square::from_alg("e4").unwrap())), "e4");
    assert_eq!(board.move_to_san(&ChessMove::new(Square::from_alg("g1").unwrap(),
                                                 Square::from_alg("f3").unwrap())), "Nf3");
}

#[test]
fn to_san_capture_promote_check_test() {
    let board = ChessBoard::from_fen("q6k/1P6/8/8/8/8/8/K7 w - - 0 1").unwrap();
    assert_eq!(board.move_to_san(&ChessMove::new_prom(Square::from_alg("b7").unwrap(),
                                                      Square::from_alg("a8").unwrap(),
                                                      PieceType::Queen)),
               "bxa8=Q+");
    assert_eq!(board.move_to_san(&ChessMove::new(Square::from_alg("a1").unwrap(),
                                                 Square::from_alg("a2").unwrap())),
               "Ka2");
}

#[test]
fn to_san_castles_checkkmate_test() {
    let board = ChessBoard::from_fen("rn3r2/pbppq1p1/1p2pN2/8/3P2NP/6P1/PPP1BP1R/R3K1k1 w Q - 5 18").unwrap();
    assert_eq!(board.move_to_san(&ChessMove::new(Square::from_alg("e1").unwrap(),
                                                      Square::from_alg("c1").unwrap())),
               "0-0-0#");
}

#[test]
fn from_san_test() {
    let mut board = ChessBoard::start_board();
    let mut moves = vec![];

    for mv in ["e4", "d5", "Nc3", "Nf6", "Bb5", "Nbd7", "Nge2", "dxe4", "0-0"].iter() {
        board.generate_moves(&mut moves);
        for &mv in moves.iter() {
            assert_eq!(board.move_from_san(&board.move_to_san(&mv)).unwrap(), mv)
        }
        moves.clear();

        let mv = board.move_from_san(mv).unwrap();
        board.do_move(mv);
    };
}

#[test]
fn san_lan_test() {
    for _ in 0..10 {
        tools::test_san_lan_with_random_game(ChessBoard::start_board());
    }
}
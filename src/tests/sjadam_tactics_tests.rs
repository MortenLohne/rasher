use board::sjadam_board::SjadamBoard;
use uci::UciBoard;
use search_algorithms::alpha_beta;
use board::sjadam_move::SjadamMove;
use search_algorithms::board::Board;
use search_algorithms::board::Color;
use search_algorithms::board::Color::*;
use uci;
use std::sync;
use search_algorithms::alpha_beta::Score;
use search_algorithms::board::GameResult;

/// Promotes a pawn through a sjadam move, which mates
#[test]
fn promote_pawn_to_mate() {
    let board = SjadamBoard::from_fen("k7/5R2/5P2/8/8/8/8/7K w - - 0 1").unwrap();
    let correct_move = board.from_alg("f6f8").unwrap();
    basic_tactics_prop(&board, correct_move);
}

/// Checks that the expected move is indeed played in the position
fn basic_tactics_prop(board : &SjadamBoard, best_move : SjadamMove) {
    let (handle, channel) = alpha_beta::start_uci_search(
        board.clone(), uci::TimeRestriction::Depth(3),
        uci::EngineOptions::new(),
        sync::Arc::new(sync::Mutex::new(uci::EngineComm::new())), None);
    
    let (score, move_str) = uci::get_uci_move(handle, channel).unwrap();
    
    let game_move = board.from_alg(&move_str).unwrap();
    
    assert_eq!(game_move, best_move,
               "Best move was {:?} with score {:?}, expected {:?}, board:\n{:?}",
               game_move, score,
               best_move, board);
}

#[test]
fn sjadammate1() {
    let board = SjadamBoard::from_fen("r2q2n1/p5pp/2p3k1/1p1R4/8/5P2/P1P1P3/2B3K1 w - - 0 1").unwrap();
    let correct_move = board.from_alg("d5g7").unwrap();
    is_mate_in_one(&board, correct_move);
}

#[test]
fn sjadammate2() {
    let board = SjadamBoard::from_fen("4Q3/5p1p/3p4/q1p2rP1/p1k1b3/3Q2R1/2P4P/1K4N1 b - - 0 1").unwrap();
    let correct_move = board.from_alg("c4b4").unwrap();
    is_mate_in_one(&board, correct_move);
}

#[test]
fn sjadammate3() {
    let board = SjadamBoard::from_fen("r1bqkb1r/pp1ppppp/3n4/2p1P3/5P2/4N3/PPP2PPP/RNBQKB1R b KQkq - 0 1").unwrap();
    let correct_move = board.from_alg("d6f5").unwrap();
    is_mate_in_one(&board, correct_move);
}

#[test]
fn sjadammate6() {
    let board = SjadamBoard::from_fen("1b3rk1/pp4pp/3p3P/3p3B/6RK/2N2p1P/PP3R2/8 b - - 0 1").unwrap();
    let correct_move = board.from_alg("b8d8").unwrap();
    is_mate_in_one(&board, correct_move);
}

#[test]
fn sjadammate7() {
    let board = SjadamBoard::from_fen("3k3r/1pppppp1/8/p1P5/PP2PPn1/2b3Kp/3PR2R/8 w - - 0 1").unwrap();
    let correct_move = board.from_alg("f4c3").unwrap();
    is_mate_in_one(&board, correct_move);
}

#[test]
fn sjadammate8() {
    let board = SjadamBoard::from_fen("4r3/pr4pk/1pp3RP/4p2B/2P3K1/P4p1P/1NP2R2/4P3 w - - 0 1").unwrap();
    let correct_move = board.from_alg("g6g5").unwrap();
    is_mate_in_one(&board, correct_move);
}

#[test]
fn sjadammate9() {
    let board = SjadamBoard::from_fen("2kr1br1/5ppp/1pB1n3/2P2n2/4p3/2P3P1/PP1NNP2/R4R1K b - - 0 1").unwrap();
    let correct_move = board.from_alg("c8e7").unwrap();
    is_mate_in_one(&board, correct_move);
}

#[test]
fn sjadammate10() {
    let board = SjadamBoard::from_fen("r1b2bkr/ppp1p1pp/P1n2n2/8/4Pp1R/3NB3/2PP2PP/4RK2 w - - 0 1").unwrap();
    let correct_move = board.from_alg("d3f6").unwrap();
    is_mate_in_one(&board, correct_move);
}

#[test]
fn sjadammate11() {
    let board = SjadamBoard::from_fen("k7/p2b4/5p2/p2Kb3/1pB2p2/4p1P1/2P4P/3Q4 b - - 0 1").unwrap();
    let correct_move = board.from_alg("d7f5").unwrap();
    is_mate_in_one(&board, correct_move);
}

#[test]
fn sjadammate12() {
    let board = SjadamBoard::from_fen("rn1qk3/ppp1p1pp/3p1b2/6P1/2R1b3/3P4/PPpPPRP1/3QKB2 w q - 0 2").unwrap();
    let correct_move = board.from_alg("d1g4").unwrap();
    is_mate_in_one(&board, correct_move);
}

#[test]
fn sjadammate13() {
    let board = SjadamBoard::from_fen("r3k3/pppppppp/8/8/3K4/8/PPPPPPPP/R4BN1 b - - 0 1").unwrap();
    let correct_move = board.from_alg("d7d5").unwrap();
    is_mate_in_one(&board, correct_move);
}

#[test]
fn sjadammate14() {
    let board = SjadamBoard::from_fen("q2k1b1r/2p1pppp/1p6/1R6/2P5/3P1Q2/4PPPP/5KN1 w - - 0 1").unwrap();
    let correct_move = board.from_alg("f3c6").unwrap();
    is_mate_in_one(&board, correct_move);
}

#[test]
fn sjadammate15() {
    let board = SjadamBoard::from_fen("r1bqkb1r/2ppp1pp/p1n3n1/4P3/1NpP1p2/4P3/P2NP1PP/R1BQKB1R w KQkq - 0 1").unwrap();
    let correct_move = board.from_alg("d4f7").unwrap();
    is_mate_in_one(&board, correct_move);
}

#[test]
fn sjadammate16() {
    let board = SjadamBoard::from_fen("2b3k1/2pp1pp1/1n2p1p1/1rB5/4N3/1P2P3/3PB1PP/KR5R b - - 0 1").unwrap();
    let correct_move = board.from_alg("g6b1").unwrap();
    is_mate_in_one(&board, correct_move);
}

#[test]
fn sjadammate17() {
    let board = SjadamBoard::from_fen("1k5r/1bbp1pp1/pp6/3p1N2/1nPBP2p/1P3P2/2PP2R1/1K6 w - - 0 1").unwrap();
    let correct_move = board.from_alg("e4e5").unwrap();
    is_mate_in_one(&board, correct_move);
}

#[test]
fn sjadammate18() {
    let board = SjadamBoard::from_fen("7k/1pppp1pr/1pn3P1/8/1rNb2NP/2P1PPP1/1P1PQ3/R6K b - - 0 1").unwrap();
    let correct_move = board.from_alg("d7g6").unwrap();
    is_mate_in_one(&board, correct_move);
}

#[test]
fn repetitions_score_0() {
    let board = SjadamBoard::from_fen("q3pr1k/R7/K5R1/8/7P/8/8/7q w - -").unwrap();
    let engine_comm = sync::Arc::new(sync::Mutex::new(uci::EngineComm::new()));

    let (handle, channel) = alpha_beta::start_uci_search(
        board.clone(), uci::TimeRestriction::Infinite,
        uci::EngineOptions::new(),
        engine_comm.clone(), None);

    loop {
        let info = channel.recv().unwrap();
        if let Score::Draw(_) = info.pvs[0].0 {
            engine_comm.lock().unwrap().engine_should_stop = true;
            handle.join().unwrap();
            return;
        }
        else if info.nodes > 1_000_000 {
            panic!("Expected draw score, got: {}", info.to_info_string());
        }
    }
}

/// The engine should be able to return and make a move, even in drawn position
#[test]
fn can_move_in_draw_position() {
    let mut board = SjadamBoard::start_board();

    for mv_str in ["c1c3", "c8c6", "c3c1", "c6c8", "c1c3", "c8c6", "c3c1", "c6c8"].iter() {
        assert_eq!(board.game_result(), None, "Wrong game result for board:\n{:?}", board);
        let mv = board.from_alg(mv_str).unwrap();
        board.do_move(mv);
    }

    assert_eq!(board.game_result(), Some(GameResult::Draw), "Wrong game result for board:\n{:?}", board);

    let (handle, channel) = alpha_beta::start_uci_search(
        board.clone(), uci::TimeRestriction::Mate(3),
        uci::EngineOptions::new(),
        sync::Arc::new(sync::Mutex::new(uci::EngineComm::new())), None);

    let (_, move_str) = uci::get_uci_move(handle, channel).unwrap();

    let mv = board.from_alg(&move_str).unwrap();

    board.do_move(mv);

    assert_eq!(board.game_result(), Some(GameResult::Draw), "Wrong game result for board:\n{:?}", board);
}

fn is_mate_in_one(board: &SjadamBoard, best_move: SjadamMove) {
    let mut engine_options = uci::EngineOptions::new();
    engine_options.null_move_pruning = false;
    let (handle, channel) = alpha_beta::start_uci_search(
        board.clone(), uci::TimeRestriction::Mate(3),
        engine_options,
        sync::Arc::new(sync::Mutex::new(uci::EngineComm::new())), None);
    
    let (score, move_str) = uci::get_uci_move(handle, channel).unwrap();
    
    let game_move = board.from_alg(&move_str).unwrap();
    
    assert_eq!(game_move, best_move,
               "Best move was {:?} with score {:?}, expected {:?}, board:\n{:?}",
               game_move, score,
               best_move, board);
    match board.side_to_move() {
        Color::White => assert_eq!(score.uci_string(White), "mate 2"),
        Color::Black => assert_eq!(score.uci_string(Black), "mate -2"),
    }
}

use board::sjadam_board::SjadamBoard;
use uci::UciBoard;
use board::sjadam_move::SjadamMove;
use search_algorithms::alpha_beta;
use search_algorithms::game_move::Move;
use uci;
use std::sync;

/// Promotes a pawn through a sjadam move, which mates
#[test]
fn promote_pawn_to_mate() {
    let board = SjadamBoard::from_fen("k7/5R2/5P2/8/8/8/8/7K w - - 0 1").unwrap();
    let correct_move = SjadamMove::from_alg("f6f8-").unwrap();
    basic_tactics_prop(&board, correct_move);
}

/// Checks that the expected move is indeed played in the position
fn basic_tactics_prop(board : &SjadamBoard, best_move : SjadamMove) {
    let channel = alpha_beta::start_uci_search(board.clone(), uci::TimeRestriction::Depth(4),
                                               uci::EngineOptions::new(),
                                               sync::Arc::new(sync::Mutex::new(uci::EngineComm::new())), None);
    
    let (score, move_str) = uci::get_uci_move(channel);
    
    let game_move = SjadamMove::from_alg(&move_str).unwrap();
    
    assert_eq!(game_move, best_move,
               "Best move was {:?} with score {}, expected {:?}, board:\n{:?}",
               game_move, score,
               best_move, board);
}

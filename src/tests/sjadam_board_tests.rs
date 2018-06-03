use board::sjadam_board::SjadamBoard;
use search_algorithms::board::{GameResult, EvalBoard};
use uci::UciBoard;

use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;

#[test]
fn hash_stays_equal() {
    let mut board = SjadamBoard::start_board();

    let mut hasher = DefaultHasher::new();
    board.hash(&mut hasher);
    let start_hash = hasher.finish();

    for mv in board.all_legal_moves() {
        let undo_move = board.do_move(mv);
        board.undo_move(undo_move);

        hasher = DefaultHasher::new();
        board.hash(&mut hasher);
        let new_hash = hasher.finish();

        assert_eq!(start_hash, new_hash);

    }
}

#[test]
fn repetitions_do_not_preserve_hash() {
    let mut board = SjadamBoard::start_board();

    let mut hasher = DefaultHasher::new();
    board.hash(&mut hasher);
    let start_hash = hasher.finish();

    for mv_str in ["c1c3", "c8c6", "c3c1", "c6c8"].iter() {
        let mv = board.from_alg(mv_str).unwrap();
        board.do_move(mv);
    }

    hasher = DefaultHasher::new();
    board.hash(&mut hasher);
    let new_hash = hasher.finish();

    assert_ne!(start_hash, new_hash);
}

#[test]
fn repetitions_are_drawn() {
    let mut board = SjadamBoard::start_board();

    for mv_str in ["c1c3", "c8c6", "c3c1", "c6c8"].iter() {
        let mv = board.from_alg(mv_str).unwrap();
        board.do_move(mv);
    }

    assert_eq!(board.game_result(), None, "Wrong game result for board:\n{:?}", board);

    for mv_str in ["c1c3", "c8c6", "c3c1", "c6c8"].iter() {
        let mv = board.from_alg(mv_str).unwrap();
        board.do_move(mv);
    }

    assert_eq!(board.game_result(), Some(GameResult::Draw), "Wrong game result for board:\n{:?}", board);
}
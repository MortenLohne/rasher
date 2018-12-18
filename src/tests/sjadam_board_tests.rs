use board::sjadam_board::SjadamBoard;
use search_algorithms::board::{GameResult, Board};
use pgn::UciBoard;

use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;

#[test]
fn hash_stays_equal() {
    let mut board = SjadamBoard::start_board();

    let mut hasher = DefaultHasher::new();
    board.hash(&mut hasher);
    let start_hash = hasher.finish();

    let mut moves = vec![];
    board.generate_moves(&mut moves);

    for mv in moves {
        let reverse_move = board.do_move(mv.clone());
        board.reverse_move(reverse_move);

        hasher = DefaultHasher::new();
        board.hash(&mut hasher);
        let new_hash = hasher.finish();

        assert_eq!(start_hash, new_hash,
                   "\nHash was not preserved after undoing move {} on \n{:?}",
                   board.to_alg(&mv), board);

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

    for mv_str in ["c1c3", "c8c6", "c3c1", "c6c8", "c1c3", "c8c6", "c3c1", "c6c8"].iter() {
        assert_eq!(board.game_result(), None, "Wrong game result for board:\n{:?}", board);
        let mv = board.from_alg(mv_str).unwrap();
        board.do_move(mv);
    }

    assert_eq!(board.game_result(), Some(GameResult::Draw), "Wrong game result for board:\n{:?}", board);

    let mv = board.from_alg("g1e3").unwrap();
    board.do_move(mv);
    
    assert_eq!(board.game_result(), None, "Wrong game result for board:\n{:?}", board);
}

#[test]
fn pawn_moves_can_repeat() {
    let mut board = SjadamBoard::start_board();

    // There is no repetition from move 1, because of en passant
    for mv_str in ["e2e4", "e7e5", "c2e2", "c7e7", "e2c2", "e7c7", "c2e2", "c7e7", "e2c2", "e7c7", "c2e2"].iter() {
        assert_eq!(board.game_result(), None, "Wrong game result for board:\n{:?}", board);
        let mv = board.from_alg(mv_str).unwrap();
        board.do_move(mv);
    }

    assert_eq!(board.game_result(), Some(GameResult::Draw), "Wrong game result for board:\n{:?}", board);
}
use board_game_traits::board::Board;
use pgn::PgnBoard;
use board::crazyhouse_board::CrazyhouseBoard;

use tests::tools;

#[test]
fn available_moves_at_start() {
    let mut board = CrazyhouseBoard::start_board().clone();
    let mut moves = vec![];
    board.generate_moves(&mut moves);
    assert_eq!(moves.len(), 20,
               "Found {} legal moves in crazyhouse starting position, expected 20",
               moves.len());
    for mv in moves {
        let old_board = board.clone();
        let reverse_move = board.do_move(mv);
        let mut black_moves = vec![];
        board.generate_moves(&mut black_moves);
        assert_eq!(black_moves.len(), 20,
               "Found {} legal moves for black's first crazyhouse move, expected 20",
                   black_moves.len());
        board.reverse_move(reverse_move);
        assert_eq!(old_board, board);
    }
}

#[test]
fn starting_position_perf_test() {
    let mut board = CrazyhouseBoard::start_board().clone();

    tools::perft_check_answers(&mut board, &[1, 20, 400, 8_902, 197_281, 4_888_832]);
}

#[ignore]
#[test]
fn starting_position_perf_test_long() {
    let mut board = CrazyhouseBoard::start_board().clone();

    tools::perft_check_answers(&mut board, &[1, 20, 400, 8_902, 197_281, 4_888_832, 120_812_942]);
}

#[test]
fn block_check_with_crazyhouse_move_test() {
    let board = CrazyhouseBoard::from_fen("k1Q5/pp6/K7/8/8/8/8/8/rqp b - - 0 0").unwrap();
    let mut moves = vec![];
    board.generate_moves(&mut moves);
    assert_eq!(moves.len(), 2);
}

#[test]
fn crazyhouse_moves_test() {
    let board = CrazyhouseBoard::from_fen("k7/8/8/8/8/8/8/7K/QR w - - 0 0").unwrap();
    let mut moves = vec![];
    board.generate_moves(&mut moves);
    assert_eq!(moves.len(), 127);
}

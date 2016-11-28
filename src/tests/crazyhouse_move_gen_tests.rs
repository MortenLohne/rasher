use board::board::Board;
#[allow(unused_imports)]
use uci::UciBoard;
use board::crazyhouse_board::CrazyhouseBoard;

#[test]
fn available_moves_at_start() {
    let mut board = CrazyhouseBoard::start_board().clone();
    let moves = board.all_legal_moves();
    assert_eq!(moves.len(), 20,
               "Found {} legal moves in crazyhouse starting position, expected 20",
               moves.len());
    for mv in moves {
        let old_board = board.clone();
        let undo_move = board.do_move(mv);
        let black_moves = board.all_legal_moves();
        assert_eq!(black_moves.len(), 20,
               "Found {} legal moves for black's first crazyhouse move, expected 20",
                   black_moves.len());
        board.undo_move(undo_move);
        assert_eq!(old_board, board);
    }
}

#[test]
fn starting_position_perf_test() {
    let mut board = CrazyhouseBoard::start_board().clone();
    println!("{:?}", board);
    assert_eq!(legal_moves_after_plies(&mut board, 1), 20);
    assert_eq!(legal_moves_after_plies(&mut board, 2), 400);
    assert_eq!(legal_moves_after_plies(&mut board, 3), 8_902);
    assert_eq!(legal_moves_after_plies(&mut board, 4), 197_281);
}

#[test]
fn block_check_with_crazyhouse_move_test() {
    let board = CrazyhouseBoard::from_fen("k1Q5/pp6/K7/8/8/8/8/8/rqp b - - 0 0").unwrap();
    let moves = board.all_legal_moves();
    assert_eq!(moves.len(), 2);
}

#[test]
fn crazyhouse_moves_test() {
    let board = CrazyhouseBoard::from_fen("k7/8/8/8/8/8/8/7K/QR w - - 0 0").unwrap();
    let moves = board.all_legal_moves();
    assert_eq!(moves.len(), 127);
}

/// Checks that the engine finds the total number of legal moves after n plies.
/// This provides a very strong indication that the move generator is correct
#[allow(dead_code)]
fn legal_moves_after_plies<B: Board>(board : &mut B, n : u8) -> u64 {
    if n == 0 { 1 }
    else {
        let mut total_moves = 0;
        for c_move in board.all_legal_moves() {
            let old_board = board.clone();
            {
                let undo_move = board.do_move(c_move.clone());
                total_moves += legal_moves_after_plies(board, n - 1);
                board.undo_move(undo_move);
            }

            assert_eq!(&old_board, board,
                             "Board was not the same after undoing move {:?}:\nOld:{:?}New:{:?}",
                             c_move, old_board, board);
        }
        total_moves
    }
}
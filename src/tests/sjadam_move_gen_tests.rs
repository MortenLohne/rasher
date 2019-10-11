use board::sjadam::board::SjadamBoard;
use pgn_traits::pgn::PgnBoard;
use board_game_traits::board::{GameResult, Board};
use chess_bitboard::bitboard::BitBoard;
use chess_bitboard::bitboard::Square;
use board::sjadam::move_gen;
use tests::tools;

#[test]
fn possible_sjadam_squares() {
    let mut squares = BitBoard::empty();
    for square in [Square(0), Square(1), Square(8), Square(9)].iter().cloned() {
        let board = move_gen::possible_sjadam_squares(square);
        assert_eq!(board.popcount(), 16);
        squares = squares | board;
    }
    assert_eq!(squares.popcount(), 64);
}

#[test]
fn correct_move_gen_start_pos() {
    let start_board = SjadamBoard::start_board().clone();
    let mut moves = vec![];
    start_board.generate_moves(&mut moves);
    assert_eq!(moves.len(), 146, "{:?}Found {} moves: {:?}, expected 146",
               start_board, moves.len(), moves);
}

#[test]
fn bishop_moves() {
    let board = SjadamBoard::from_fen("k7/8/8/8/P7/8/KN6/BN6 w - - 1 1").unwrap();
    let mut moves = vec![];
    board.generate_moves(&mut moves);
    assert_eq!(moves.len(), 43,
               "Expected 43 legal moves, found {} moves: {:?}\n{:?}",
               moves.len(), moves, board);
}

/// Tests whether a pawn is allowed to sjadam over an opponent pawn
#[test]
fn jump_over_opponent() {
    let board = SjadamBoard::from_fen("8/8/8/7p/7P/8/8/K1k5 w - - 1 1").unwrap();
    assert_eq!(board.game_result(), None);
    let mut moves = vec![];
    board.generate_moves(&mut moves);
    assert_eq!(moves.len(), 5,
               "Expected 5 legal moves, found {} moves: {:?}\n{:?}",
               moves.len(), moves, board);
}

#[test]
fn no_stalemate_test() {
    let board = SjadamBoard::from_fen("8/8/8/5p1k/3p1Q2/3P2R1/P3p2P/4K3 b - - 2 39").unwrap();
    assert_eq!(board.game_result(), None);
    let mut moves = vec![];
    board.generate_moves(&mut moves);
    // The exact number may be wrong due to other bugs
    // So don't trust this test
    assert_eq!(moves.len(), 13,
               "Expected 13 legal moves, found {} moves: {:?}\n{:?}",
               moves.len(), moves, board);
}

#[test]
fn can_take_king_while_checked() {
    let board = SjadamBoard::from_fen("r2q2n1/pp4pp/6k1/2Nb4/8/5P2/P1P1PK2/2B3q1 w - - 0 1").unwrap();
    // Black is really sjadammated, so this is also a possible outcome
    if board.game_result() != Some(GameResult::WhiteWin) {
        let mut moves = vec![];
        board.generate_moves(&mut moves);
        
        let correct_move = board.move_from_lan("c5g6").unwrap();
        assert!(moves.contains(&correct_move),
                "White couldn't take king on board:\n{:?}Moves: {:?}",
                board, moves);
    }
}

#[test]
fn sjadam_move_to_promote_pawn() {
    let board = SjadamBoard::from_fen("k7/5R2/5P2/8/8/8/8/7K w - - 0 1").unwrap();
    let mut moves = vec![];
    board.generate_moves(&mut moves);
        
    let correct_move = board.move_from_lan("f6f8").unwrap();
    assert!(moves.contains(&correct_move),
            "White couldn't promote in sjadam on board:\n{:?}Moves: {:?}",
            board, moves);
}

#[test]
fn no_moves_on_back_rank() {
    let board = SjadamBoard::from_fen("8/K7/P7/8/8/8/8/7k w - - 0 1").unwrap();
    let mut moves = vec![];
    board.generate_moves(&mut moves);
    assert_eq!(moves.len(), 9, "Expected to find {} moves, found {}: {:?}\n{:?}",
               9, moves.len(), moves, board);
}

#[test]
fn startpos_perf_test() {
    let mut board = SjadamBoard::start_board();

    tools::perft_check_answers(&mut board, &[1, 146, 21_469, 3_260_755]);
}

#[test]
#[ignore]
fn startpos_perf_test_long() {
    let mut board = SjadamBoard::start_board();
    assert_eq!(tools::perft(&mut board, 4), 498_450_154);
    assert_eq!(tools::perft(&mut board, 5), 77_726_188_083);
    assert_eq!(tools::perft(&mut board, 6), 12_161_357_522_619);
}

#[test]
fn castling_en_passant_perf_test() {
    let mut board = SjadamBoard::start_board();
    let board2 = board.clone(); // From before non-lexical lifetimes
    board.do_move(board2.move_from_lan("g1e3").unwrap());
    board.do_move(board2.move_from_lan("g8e6").unwrap());
    board.do_move(board2.move_from_lan("f1c4").unwrap());
    board.do_move(board2.move_from_lan("e8g8").unwrap());
    board.do_move(board2.move_from_lan("c2c3").unwrap());
    board.do_move(board2.move_from_lan("d7d5").unwrap());
    board.do_move(board2.move_from_lan("c1a3").unwrap());
    board.do_move(board2.move_from_lan("b7b5").unwrap());
    for (n, &moves) in (1..4).zip([159, 26_638, 4_176_157].iter()) {
        let result = tools::perft(&mut board, n);
        let mut generated_moves = vec![];
        board.generate_moves(&mut generated_moves);
        assert_eq!(result, moves,
                   "Expected {} moves, found {} on board:\n{:?}\n{:?}.",
                   moves, result, board, generated_moves);
    }
}

#[test]
fn castling_en_passant_perf_test_2() {
    let mut board = SjadamBoard::start_board();

    let mut board2 = board.clone(); // From before non-lexical lifetimes
    board.do_move(board2.move_from_lan("g1e3").unwrap());
    board.do_move(board2.move_from_lan("g8e6").unwrap());
    board.do_move(board2.move_from_lan("f1c4").unwrap());
    board.do_move(board2.move_from_lan("e8g8").unwrap());
    board.do_move(board2.move_from_lan("c2c3").unwrap());
    board.do_move(board2.move_from_lan("d7d5").unwrap());
    board.do_move(board2.move_from_lan("c1a3").unwrap());
    board.do_move(board2.move_from_lan("b7b5").unwrap());
    board2 = board.clone();
    board.do_move(board2.move_from_lan("c3b6").unwrap());
    for (n, &moves) in (1..4).zip([176, 30_653, 5_116_686].iter()) {
        let result = tools::perft(&mut board, n);
        let mut generated_moves = vec![];
        board.generate_moves(&mut generated_moves);
        assert_eq!(result, moves,
                   "Expected {} moves, found {} on board:\n{:?}\n{:?}.",
                   moves, result, board, generated_moves);
    }
}

#[test]
fn perf_test_3() {
    let mut board = SjadamBoard::from_fen("1nbqkb1r/pppppppp/4r3/8/8/4N3/PPPPPPPP/RNBQ1RK1 b k - 0 1").unwrap();

    tools::perft_check_answers(&mut board, &[1, 143, 22_490, 3_319_855]);
}

#[test]
fn perf_test_4() {
    let mut board = SjadamBoard::from_fen("rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq - 1 1").unwrap();

    tools::perft_check_answers(&mut board, &[1, 144, 18_152, 2_743_499]);
}

#[test]
fn perf_test_5() {
    let mut board = SjadamBoard::from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 1 1").unwrap();

    tools::perft_check_answers(&mut board, &[1, 145, 25_337, 3_852_269]);
}

#[test]
fn perf_test_6() {
    let mut board = SjadamBoard::from_fen("rnbqkbnr/pppppppp/8/8/2K5/8/PPPPPPPP/RNBQ1BNR b kq - 0 1").unwrap();

    tools::perft_check_answers(&mut board, &[1, 140, 24_301, 3_626_924]);
}

#[test]
fn perf_test_7() {
    let mut board = SjadamBoard::from_fen("rnbqkbnr/pppppppp/1N6/8/8/8/PPPPPPPP/R1BQKBNR b KQkq - 0 1").unwrap();

    tools::perft_check_answers(&mut board, &[1, 145, 21_335, 3_184_704]);
}

#[test]
fn perf_test_8() {
    let mut board = SjadamBoard::from_fen("rnbqkbnr/pppppppp/8/8/6P1/8/PPPPPP1P/RNBQKBNR b KQkq - 0 1").unwrap();

    tools::perft_check_answers(&mut board, &[1, 146, 23_331, 3_578_045]);
}

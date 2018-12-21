use board::std_board::PieceType;
use board::std_board::Square;
use board::std_board::ChessBoard;
use pgn::UciBoard;
use search_algorithms::board::Board;
use board::std_move::ChessMove;
use rand::Rng;

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
            assert_eq!(board.mv_from_san(&board.move_to_san(&mv)).unwrap(), mv)
        }
        moves.clear();

        let mv = board.mv_from_san(mv).unwrap();
        board.do_move(mv);
    };
}

#[test]
fn san_lan_test() {
    for _ in 0..1000 {
        test_san_with_random_game(ChessBoard::start_board());
    }
}

fn test_san_with_random_game<B: UciBoard> (mut board: B) {
    let mut rng = rand::thread_rng();
    let mut moves = vec![];
    loop {
        board.generate_moves(&mut moves);
        for ref mv in moves.iter() {
            assert_eq!(board.mv_from_san(&board.move_to_san(mv)).unwrap(), **mv);
            assert_eq!(board.move_from_lan(&board.move_to_lan(mv)).unwrap(), **mv);
        }

        let movecount = moves.len();
        let mv = moves.remove(rng.gen_range(0, movecount - 1));
        board.do_move(mv);
        if board.game_result().is_some() {
            break;
        }
        moves.clear();
    }
}
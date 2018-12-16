use std::fmt;
use std::time;
use rand;

use search_algorithms::mcts;
use search_algorithms::mcts::MonteCarloTree;
use search_algorithms::board::EvalBoard;
use uci::UciBoard;

use board::std_board::ChessBoard;
use board::std_move::ChessMove;

use board::std_board::Square;
use board::std_board::PieceType::*;

#[ignore]
#[test]
fn knight_fork_test() {
// Basic knight fork
    let board1 = ChessBoard::from_fen("q3k3/2p5/8/3N4/1K5P/8/8/8 w - - 0 1").unwrap();
    let best_move1 = ChessMove::new(Square::from_alg("d5").unwrap(),
                                    Square::from_alg("c7").unwrap());
    basic_tactics_prop(&board1, best_move1);
}

// Commented because it takes a lot of time for little benefit
/*#[test]
fn queen_pawn_to_mate() {
    // Checks that white can queen a pawn to mate
    let board2 = ChessBoard::from_fen("k7/p1P5/8/K7/8/8/8/8 w - - 0 1").unwrap();
    let best_move2 = ChessMove::new_prom(Square::from_alg("c7").unwrap(),
                                     Square::from_alg("c8").unwrap(), Piece(Queen, White) );
    basic_tactics_prop(&board2, best_move2);
}
*/

#[ignore]
#[test]
fn underpromote() {
    // Checks that black can underpromote a pawn to avoid mate
    let board3 = ChessBoard::from_fen("8/8/8/8/8/5K2/4p2R/5k2 b - - 0 1").unwrap();
    let best_move3 = ChessMove::new_prom(Square::from_alg("e2").unwrap(),
                                         Square::from_alg("e1").unwrap(), Knight );
    basic_tactics_prop(&board3, best_move3);
}
#[ignore]
#[test]
fn block_pawn() {
    // Checks that black can block a pawn to draw an endgame
    let board4 = ChessBoard::from_fen("1k6/2P5/3K4/8/8/8/8/8 b - - 0 1").unwrap();
    let best_move4 = ChessMove::new(Square::from_alg("b8").unwrap(),
                                    Square::from_alg("c8").unwrap() );

    basic_tactics_prop(&board4, best_move4);
}
#[ignore]
#[test]
fn capture_to_promote() {
    let board5 = ChessBoard::from_fen("q6k/1P6/8/8/8/8/8/K7 w - - 0 1").unwrap();
    let best_move5 = board5.from_alg("b7a8Q").unwrap();
    basic_tactics_prop(&board5, best_move5);
}

/// Checks that the expected move is indeed played in the position
fn basic_tactics_prop<B: EvalBoard + fmt::Debug> (board : &B, best_move : B::Move) {
    let mut board = board.clone();
    let mut all_legal_moves = vec![];
    board.generate_moves(&mut all_legal_moves);

    let mut mc_tree = MonteCarloTree::new_root(&mut board);
    let mut searches = mc_tree.searches();
    let start_time = time::Instant::now();
    let mut rng = rand::weak_rng();
    
    while start_time.elapsed() < time::Duration::new(60, 0) {
        for _ in 0..10 {
            use std::ops::Add;
            mc_tree.select(&mut rng, searches, &mut mcts::SearchData::default());
            searches += 1;
            let searches_of_children = mc_tree.children.iter()
                .map(Option::as_ref).map(Option::unwrap)
                .map(|n| n.searches())
                .fold(0, u64::add);
            debug_assert!((searches as i64 - searches_of_children as i64).abs() <= 1,
                          format!("{} searches overall, but sum of searches of children is {}.",
                                  searches, searches_of_children));
        }
        
    }
    let engine_move = &all_legal_moves[mc_tree.best_child().unwrap()];
    let score = mc_tree.children[mc_tree.best_child().unwrap()].clone().unwrap().score();
    mc_tree.print_score(&board, &mut String::new());
    assert_eq!(engine_move, &best_move,
               "Best move was {:?} with score {}\nExpected move: {:?}, board:\n{:?}",
               engine_move, score, best_move, board);
}

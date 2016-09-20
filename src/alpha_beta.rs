use ::NodeCount;
use ::Score;
use ::Score::*;

use uci;

use board::board::Board;
use board::game_move::Move;
use board::board::Color::*;

extern crate time;
use std::sync::{Arc, Mutex};

pub fn search_moves<B: Board> (mut board : B, engine_comm : Arc<Mutex<uci::EngineComm>>,
                 time_restriction : uci::TimeRestriction,
                 mut log_writer : uci::SharableWriter) 
                     -> (Score, Vec<B::Move>, NodeCount) {
    
    engine_comm.lock().unwrap().engine_is_running = true;
    
    let max_depth : u8 = match time_restriction {
        uci::TimeRestriction::Depth(d) => d,
        uci::TimeRestriction::Mate(d) => d as u8,
        _ => 128,
    };;
    debug_assert!(max_depth > 1);
    
    let start_time = time::get_time();
    
    let (mut best_score, mut best_moves, mut best_node_count) = (None, None, None);

    for depth in 1..(max_depth + 1) {
        let (score, moves, node_count) =
            find_best_move_ab(&mut board, depth, &*engine_comm, time_restriction);
        best_score = Some(score); 
        best_moves = Some(moves.clone()); 
        best_node_count = Some(node_count.clone());

        let ms_taken = (time::get_time() - start_time).num_milliseconds();

        if moves.len() > 0 {
            engine_comm.lock().unwrap().best_move = Some(moves[0].to_alg());
        }
        else {
            uci::to_log("Warning: find_best_move_ab didn't return any moves", &mut log_writer);
            engine_comm.lock().unwrap().best_move = None;
        }

        uci::send_eval_to_gui(&log_writer, depth,
                         ms_taken, score, moves, node_count);
        match time_restriction {
            uci::TimeRestriction::GameTime(info) => { 
                if (board.to_move() == Black && 
                    ms_taken as u32 > info.black_inc / 5 + info.black_time / 50) ||
                    (board.to_move() == White && 
                     ms_taken as u32 > info.white_inc / 5 + info.white_time / 50)
                {
                    break;
                }
                
            },
            uci::TimeRestriction::MoveTime(time) => if ms_taken > time / 2 { break },
               
            _ => (),
        }
    }
    {
        let mut engine_comm = engine_comm.lock().unwrap();
        uci::uci_send(&format!("bestmove {}", engine_comm.best_move.clone().unwrap()), 
                      &mut log_writer);
        engine_comm.engine_is_running = false;
        
    }
    (best_score.unwrap(), best_moves.unwrap(), best_node_count.unwrap())
    
}

/// Returns a score, and a list of moves representing the moves it evaluated
fn find_best_move_ab<B: Board> (board : &mut B, depth : u8, 
                                engine_comm : &Mutex<uci::EngineComm>,
                                time_restriction : uci::TimeRestriction )
                                -> (Score, Vec<B::Move>, NodeCount) {

    fn find_best_move_ab_rec<B: Board> (board: &mut B, depth : u8, 
                                        mut alpha : Score, mut beta : Score,
                                        engine_comm : &Mutex<uci::EngineComm>,
                                        time_restriction : uci::TimeRestriction,
                                        node_counter : &mut NodeCount)
                                        -> (Score, Vec<B::Move>) {
        use uci::TimeRestriction::*;

        if node_counter.total % 1024 == 0 {
            let mut engine_comm = engine_comm.lock().unwrap();
            
            if engine_comm.engine_should_stop {
                engine_comm.engine_is_running = false;
                //return (::score_board(&board), vec![]);
                panic!();
            }
        } 
        if depth == 0 {
            node_counter.leaf += 1;
            node_counter.total += 1;
            return (board.score_board(), vec![]);
        }
        else {
            node_counter.intern += 1;
            node_counter.total += 1;
        }

        match time_restriction {
            GameTime(_) => (),
            Depth(_) => (),
            Nodes(n) => if node_counter.total > n {
                engine_comm.lock().unwrap().engine_is_running = false;
                // TODO: Actually stop here
                //return (::score_board(&board), vec![]);
            },
            Mate(_) => (),
            MoveTime(_) => (), //TODO: Might want to check the clock here, and not just on every depth increase
            Infinite => (),
        };
        
        /*if board.half_move_clock > 50 {
            return (Draw(0), vec![]);
        }*/
        // TODO: Add this back, preferably by adding is_game_over() to the trait
        
        // Helpful alias
        let color = board.to_move();
        let mut best_move = None;
        let mut best_line = vec![];
        
        let legal_moves = board.all_legal_moves();
        
        // Check if the player is checkmated or in stalemate
        if legal_moves.len() == 0 {
            return (board.is_mate_or_stalemate(), vec![])
            /*if move_gen::is_attacked(board, board.king_pos()) {
                if color == White {
                    return (MateB(0), vec![]);
                }
                else {
                    return (MateW(0), vec![]);
                }
            }
            else {
                return (Draw(0), vec![]);
            }*/
        }
        for c_move in legal_moves {
            // Score is greater than the minimizer will ever allow OR
            // Score is lower than the maximizer will ever allow
            if (color == White && alpha >= beta) ||
                (color == Black && beta <= alpha) {
                    break;
                }
            else {
                let undo_move : <B as Board>::UndoMove = board.do_move(c_move.clone());
                let (tried_score, tried_line) =
                    
                    find_best_move_ab_rec( board, depth - 1, alpha, beta,
                                            engine_comm, time_restriction, node_counter);
                board.undo_move(undo_move);
                
                if color == White && tried_score > alpha {
                    alpha = tried_score;
                    best_line = tried_line;
                    best_move = Some(c_move);
                }
                else if color == Black && tried_score < beta {
                    beta = tried_score;
                    best_line = tried_line;
                    best_move = Some(c_move);
                }
            }
        }
        match best_move {   
            Some(c_move) => best_line.push(c_move),
            None => (),
        }
        let score = if color == White { alpha } else { beta };
        (match score {
            MateB(i) => MateB(i + 1),
            MateW(i) => MateW(i + 1),
            Draw(i) => Draw(i + 1),
            Val(n) => Val(n), },
         best_line)
            
    };
    let mut node_counter = NodeCount { intern: 0, leaf: 0, total: 0 };
    let (score, mut moves) =
        find_best_move_ab_rec(board, depth, MateB(0), MateW(0),
                              engine_comm, time_restriction, &mut node_counter);
    moves.reverse();
    // println!("Evaluated {} internal nodes and {} leaves", node_counter.intern, node_counter.leaf);
    (score, moves, node_counter)
}   

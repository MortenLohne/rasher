use ::NodeCount;

use uci;
use std::fmt;
use std::cmp;
use std::cmp::Ordering;

use search_algorithms::board::GameResult;
use search_algorithms::board::EvalBoard;
use search_algorithms::game_move::Move;
use search_algorithms::board::Color::*;
use self::Score::*;

extern crate time;
use std::sync::{Arc, Mutex};

pub fn search_moves<B> (mut board : B, engine_comm : Arc<Mutex<uci::EngineComm>>,
                         time_restriction : uci::TimeRestriction,
                         mut log_writer : uci::SharableWriter) 
                         -> (Score, Vec<B::Move>, NodeCount)
    where B: EvalBoard + fmt::Debug
{
    {
        engine_comm.lock().unwrap().engine_is_running = true;
    }
    
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
                    ms_taken as u32 > info.black_inc / 5 + info.black_time / 100) ||
                    (board.to_move() == White && 
                     ms_taken as u32 > info.white_inc / 5 + info.white_time / 100)
                {
                    break;
                }
                
            },
            uci::TimeRestriction::MoveTime(time) => if ms_taken > time / 4 { break },
               
            _ => (),
        }
    }
    {
        println!("Unclocking best move");
        let mut engine_comm = engine_comm.lock().unwrap();
        println!("Sending best move");
        match engine_comm.best_move.clone() {
            Some(mv) => uci::uci_send(&format!("bestmove {}", mv), &mut log_writer),
            None => uci::uci_send(&format!("best score: {}", best_score.unwrap()), &mut log_writer),
        }
        engine_comm.engine_is_running = false;
        
    }
    (best_score.unwrap(), best_moves.unwrap(), best_node_count.unwrap())
    
}

/// Returns a score, and a list of moves representing the moves it evaluated
fn find_best_move_ab<B:> (board : &mut B, depth : u8, engine_comm : &Mutex<uci::EngineComm>,
                          time_restriction : uci::TimeRestriction )
                          -> (Score, Vec<B::Move>, NodeCount)
    where B: EvalBoard + fmt::Debug
{
    
    fn find_best_move_ab_rec<B:> (board: &mut B, depth : u8,
                                  mut alpha : Score, mut beta : Score,
                                  engine_comm : &Mutex<uci::EngineComm>,
                                  time_restriction : uci::TimeRestriction,
                                  node_counter : &mut NodeCount)
                                  -> (Score, Vec<B::Move>)
        where B: EvalBoard + fmt::Debug
    {
        use uci::TimeRestriction::*;
        
        // Check if the thread should stop
        if node_counter.total % 8096 == 0 {
            let should_stop : bool;
            {
                let mut engine_comm = engine_comm.lock().unwrap();
                should_stop = engine_comm.engine_should_stop;
                if should_stop {
                    engine_comm.engine_is_running = false;
                }
            }
            if should_stop { panic!("Engine was told to stop") }
        }
        match board.game_result() {
            Some(GameResult::WhiteWin) => return (BlackWin(0), vec![]),
            Some(GameResult::BlackWin) => return (WhiteWin(0), vec![]),
            Some(GameResult::Draw) => return (Draw(0), vec![]),
            None => (),
        }
        if depth == 0 {
            node_counter.leaf += 1;
            node_counter.total += 1;
            return (Val(board.eval_board()), vec![]);
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
        
        // If there is mate or stalemate on the board, we should already have returned
        assert!(legal_moves.len() > 0);
        
        for c_move in legal_moves {
            // Score is greater than the minimizer will ever allow OR
            // Score is lower than the maximizer will ever allow
            if (color == White && alpha >= beta) ||
                (color == Black && beta <= alpha) {
                    break;
                }
            else {
                let old_board = board.clone();
                let undo_move : <B as EvalBoard>::UndoMove = board.do_move(c_move.clone());
                let (tried_score, tried_line) =
                    
                    find_best_move_ab_rec( board, depth - 1, alpha, beta,
                                            engine_comm, time_restriction, node_counter);
                board.undo_move(undo_move);
                debug_assert_eq!(board, &old_board,
                                 "Failed to restore board after move {:?}", c_move);
                
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
            BlackWin(i) => BlackWin(i + 1),
            WhiteWin(i) => WhiteWin(i + 1),
            Draw(i) => Draw(i + 1),
            Val(n) => Val(n), },
         best_line)
            
    };
    let mut node_counter = NodeCount { intern: 0, leaf: 0, total: 0 };
    let (score, mut moves) =
        find_best_move_ab_rec(board, depth, BlackWin(0), WhiteWin(0),
                              engine_comm, time_restriction, &mut node_counter);
    moves.reverse();
    // println!("Evaluated {} internal nodes and {} leaves", node_counter.intern, node_counter.leaf);
    (score, moves, node_counter)
}   

#[derive(Clone, Copy, PartialEq)]
pub enum Score {
    Val(f32),
    Draw(u8),
    WhiteWin(u8),
    BlackWin(u8),
}

impl fmt::Display for Score {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Score::Val(f) => write!(fmt, "cp {}", (100.0 * f) as i16),
            Score::WhiteWin(n) => write!(fmt, "mate {}", n as i16 / 2),
            Score::BlackWin(n) => write!(fmt, "mate {}", n as i16 / -2),
            Score::Draw(_) => write!(fmt, "0"),
        }
    }
}

impl PartialOrd for Score {
    fn partial_cmp (&self, other: &Score) -> Option<cmp::Ordering> {
        match (*self, *other) {
            (WhiteWin(n1), WhiteWin(n2)) => Some((&n2).cmp(&n1)),
            (WhiteWin(_), _) => Some(Ordering::Greater),
            
            (Val(_), WhiteWin(_)) => Some(Ordering::Less),
            (Val(_), BlackWin(_)) => Some(Ordering::Greater),
            (Val(n1), Val(n2)) => (&n1).partial_cmp(&n2),
            (Val(n1), Draw(_)) => (&n1).partial_cmp(&0.0),

            (Draw(_), Val(n1)) => (&0.0).partial_cmp(&n1),
            (Draw(_), Draw(_)) => Some(Ordering::Equal),
            (Draw(_), WhiteWin(_)) => Some(Ordering::Less),
            (Draw(_), BlackWin(_)) => Some(Ordering::Greater),
            
            (BlackWin(n1), BlackWin(n2)) => Some(n1.cmp(&n2)),
            (BlackWin(_), _) => Some(Ordering::Less),
            
        }
    }
}
/*
impl Ord for Score {
    fn cmp (&self, other: &Score) -> cmp::Ordering {
        
    }
}
 */

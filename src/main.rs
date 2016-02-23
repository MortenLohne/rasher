#![feature(test)]
mod uci;
mod board;
mod tests;
mod move_gen;
mod monte_carlo;

extern crate time;

use Score::{Val, Draw, MateW, MateB};
use board::*;

use board::PieceType::*;
use board::Color::*;

use std::sync::{Arc, Mutex};
use std::io;

use std::fmt::Error;
use std::cmp::*;
use std::fmt::Display;
use std::fmt::Formatter;

use std::fs;
use std::path;

#[macro_use]
extern crate lazy_static;

fn main() {
    let reader = io::stdin();
    let mut input = "".to_string();
    loop {
        // println!("Enter input");
        reader.read_line(&mut input).unwrap();
        match &input[..] {
            "uci\n" => {
                // Prepare to create log file
                /*
                let mut options = fs::OpenOptions::new();
                options.write(true).append(true);
                let path = path::Path::new("mc_log.txt");

                let log_file = match fs::File::create(path) {
                    Ok(file) => file,
                    Err(err) => panic!(format!("Error creating log file at {:?} : {}", path, err)),
                };
                */
                let mut log_writer = Arc::new(Mutex::new(None));
                uci::to_log("Opened log file", &mut log_writer);
                
                match uci::connect_engine(&log_writer) {
                    Ok(_) => (),
                    Err(e) => {
                        uci::to_log(&e, &mut log_writer);
                        panic!(e);
                    },
                }
            }, 
            //"play_self\n" => play_game(&board::START_BOARD.clone()),
            //"play\n" => play_human(),
            s => println!("Unrecognized command \"{}\".", s),
        }
    }
    
    
    
    //play_game(&board::START_BOARD.clone());
    //play_human();
}

/*
#[allow(dead_code)]
fn play_game(board : &Board) {
    println!("Board:\n{}\nHalf move count: {}", board, (*board).half_move_clock);
    println!("\n");
    let engine_comm = Mutex::new(uci::EngineComm::new());
    engine_comm.lock().unwrap().engine_is_running = true;
    
    let (score, moves, _) = find_best_move_ab (&board, 5, &engine_comm, None);
    if moves.len() > 0 {
        println!("Found move with score {}.", score);
        play_game(&board.do_move(moves[0]));
    }
    else {
        match score {
            Val(_) => panic!("Found no moves for {}, but it was not mate! Board:\n{}",
                             board.to_move, board),
            MateW(_) => println!("White won at move! Board:\n{}", board),
            MateB(_) => println!("Black won! Board:\n{}", board),
            Draw(0) => println!("The game was drawn! Board:\n{}", board),
            Draw(n) => println!("Game was marked as drawn, but with {} moves left. Board:\n{}",
                                n, board),
        }
    }
}
*/

/*
#[allow(dead_code)]
fn play_human() {
    let mut board = board::START_BOARD.clone();
    loop {
        println!("Board:\n{}\nHalf move count: {}", board, board.half_move_clock);
        // If black, play as human
        if board.to_move == White {
            println!("Type your move as long algebraic notation (e2-e4):");

            let reader = io::stdin();
            let mut input_str = "".to_string();
            let legal_moves = move_gen::all_legal_moves(&board);
            // Loop until user enters a valid move
            loop {
                match Move::from_alg(&input_str) {
                    Ok(val) => {
                        let mut is_legal = false;
                        for c_move in legal_moves.iter() {
                            if *c_move == val { is_legal = true; }
                        }
                        if is_legal { break; }
                        println!("Move {} is illegal!", val);
                        for c_move in legal_moves.iter() {
                            println!("{}", c_move);
                        }
                    }
                       
                    Err(error) => {
                        println!("{}", error);
                    },
                }
                input_str = "".to_string();
                reader.read_line(&mut input_str).ok().expect("Failed to read line");
                input_str = input_str.trim().to_string();
                    
            }

            let c_move = Move::from_alg(&input_str).unwrap();
            println!("Doing move");
            board = board.do_move(c_move);
            
        }
        else {
            let engine_comm = Mutex::new(uci::EngineComm::new());
            engine_comm.lock().unwrap().engine_is_running = true;
            
            let (score, moves, _) = find_best_move_ab (&board, 3, &engine_comm, None);
            if moves.len() > 0 {
                println!("Found move with score {}.", score);
                board = board.do_move(moves[0]);
            }
            else {
                match score {
                    Val(_) => panic!("Found no moves for {}, 
but it was not mate or staltemate! Board:\n{}",
                                     board.to_move, board),
                    MateW(_) => { println!("White won at move! Board:\n{}", board); break },
                    MateB(_) => { println!("Black won! Board:\n{}", board); break },
                    Draw(0) => { println!("The game was drawn! Board:\n{}", board); break },
                    Draw(n) => println!(
                        "Game was marked as drawn, but with {} moves left. Board:\n{}",  n, board),
                }
            }
        }
    }
}
*/

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NodeCount {
    intern : u64,
    leaf : u64,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Score {
    Val(f32),
    Draw(u8),
    MateW(u8),
    MateB(u8),
}
impl Display for Score {
    fn fmt(&self, fmt : &mut Formatter) -> Result<(), Error> {
        let _ = match self {
            &Val(f) => fmt.write_str(&format!("cp {}", f).to_string()),
            &MateW(moves) => fmt.write_str(&format!("mate {}", moves)),
            &MateB(moves) => fmt.write_str(&format!("mate -{}", moves)),
            &Draw(_) => fmt.write_str("0.0 (forced draw)"),
        };
        Ok(())   
    }
}

impl PartialOrd for Score {
    fn partial_cmp (&self, other: &Score) -> Option<Ordering> {
        match (self, other) {
            (&MateW(n1), &MateW(n2)) => (&n2).partial_cmp(&n1),
            (&MateW(_), _) => Some(Ordering::Greater),
            
            (&Val(_), &MateW(_)) => Some(Ordering::Less),
            (&Val(_), &MateB(_)) => Some(Ordering::Greater),
            (&Val(n1), &Val(n2)) => (&n1).partial_cmp(&n2),
            (&Val(n1), &Draw(_)) => (&n1).partial_cmp(&0.0),

            (&Draw(_), &Val(n1)) => (&0.0).partial_cmp(&n1),
            (&Draw(_), &Draw(_)) => Some(Ordering::Equal),
            (&Draw(_), &MateW(_)) => Some(Ordering::Less),
            (&Draw(_), &MateB(_)) => Some(Ordering::Greater),
            
            (&MateB(n1), &MateB(n2)) => (&n1).partial_cmp(&n2),
            (&MateB(_), _) => Some(Ordering::Less),
            
        }
    }
}

fn score_board (board : &board::Board) -> Score {
    let center_proximity = |file, rank| {
        (3.5f32).powi(2) - ((3.5 - file as f32).powi(2) + (3.5 - rank as f32).powi(2)).sqrt()
    };
    let mut value = 0.0;
    for rank in 0..8 {
        for file in 0..8 {
            let piece_val = board.board[rank][file].value();
            let pos_val = center_proximity(file, rank) *
                match board.board[rank][file] {
                    Piece(Bishop, White) => 0.1,
                    Piece(Bishop, Black) => -0.1,
                    Piece(Knight, White) => 0.2,
                    Piece(Knight, Black) => -0.2,
                    Piece(Queen, White) => 0.2,
                    Piece(Queen, Black) => -0.2,
                    Piece(Pawn, White) => 0.05,
                    Piece(Pawn, Black) => -0.05,
                    _ => 0.0,
                };
            value += piece_val + pos_val;
            //println!("Value at {} is {}, {}, total: {}",
            //         Square::from_ints(file as u8, rank as u8), piece_val, pos_val, value);
        }
    }
    Val(value)
}

fn search_moves (board : Board, engine_comm : Arc<Mutex<uci::EngineComm>>,
                 time_restriction : uci::TimeRestriction,
                 mut log_writer : uci::SharableWriter) {
    
    engine_comm.lock().unwrap().engine_is_running = true;
    
    let max_depth : u8 = match time_restriction {
        uci::TimeRestriction::Depth(d) => d,
        uci::TimeRestriction::Mate(d) => d as u8,
        _ => 128,
    };;
    
    let start_time = time::get_time();
    
    for depth in 1..max_depth {
        let (score, moves, node_count) =
            find_best_move_ab(&board, depth, &*engine_comm, time_restriction);
        
        let ms_taken = (time::get_time() - start_time).num_milliseconds();

        if moves.len() > 0 {
            engine_comm.lock().unwrap().best_move = Some(moves[0]);
        }
        else {
            uci::to_log("Warning: find_best_move_ab didn't return any moves", &mut log_writer);
            engine_comm.lock().unwrap().best_move = None;
        }

        uci::send_eval_to_gui(&log_writer, depth,
                         ms_taken, score, moves, node_count);
        match time_restriction {
            uci::TimeRestriction::GameTime(info) => { 
                if (board.to_move == Black && 
                    ms_taken as u32 > info.black_inc / 5 + info.black_time / 50) ||
                    (board.to_move == White && 
                     ms_taken as u32 > info.white_inc / 5 + info.white_time / 50)
                {
                    break;
                }
                
            },
            uci::TimeRestriction::MoveTime(time) => if ms_taken > time / 2 { break },
               
            _ => (),
        }
    }
    uci::uci_send(&format!("bestmove {}", engine_comm.lock().unwrap().best_move.unwrap().to_alg()), 
             &mut log_writer);
    engine_comm.lock().unwrap().engine_is_running = false;
    
}

/// Returns a score, and a list of moves representing the moves it evaluated
fn find_best_move_ab (board : &Board, depth : u8, engine_comm : &Mutex<uci::EngineComm>,
                      time_restriction : uci::TimeRestriction )
                      -> (Score, Vec<Move>, NodeCount) {

    fn find_best_move_ab_rec (board: &Board, depth : u8, mut alpha : Score, mut beta : Score,
                              engine_comm : &Mutex<uci::EngineComm>,
                              time_restriction : uci::TimeRestriction,
                              node_counter : &mut NodeCount)
                              -> (Score, Vec<Move>) {
        use uci::TimeRestriction::*;
        {
            let mut engine_comm = engine_comm.lock().unwrap();
        
            if engine_comm.engine_should_stop {
                engine_comm.engine_is_running = true;
                panic!()
            }
        } 
        if depth == 0 {
            node_counter.leaf += 1;
            return (score_board(&board), vec![]);
        }
        else {
            node_counter.intern += 1;
        }

        match time_restriction {
            GameTime(_) => (),
            Depth(_) => (),
            Nodes(n) => if node_counter.leaf + node_counter.intern > n {
                engine_comm.lock().unwrap().engine_is_running = true;
                panic!();
            },
            Mate(_) => (),
            MoveTime(_) => (), //TODO: Might want to check the clock here, and not just on every depth increase
            Infinite => (),
        };
        
        if board.half_move_clock > 50 {
            return (Draw(0), vec![]);
        }
        
        // Helpful alias
        let color = board.to_move;
        let mut best_move = None;
        let mut best_line = vec![];
        
        let legal_moves = move_gen::all_legal_moves(board);
        
        // Check if the player is checkmated or in stalemate
        if legal_moves.len() == 0 {
            if move_gen::is_attacked(board, board.king_pos()) {
                if color == White {
                    return (MateB(0), vec![]);
                }
                else {
                    return (MateW(0), vec![]);
                }
            }
            else {
                return (Draw(0), vec![]);
            }
        }
        for c_move in legal_moves {
            // Score is greater than the minimizer will ever allow OR
            // Score is lower than the maximizer will ever allow
            if (color == White && alpha >= beta) ||
                (color == Black && beta <= alpha) {
                    break;
                }
            else {
                let tried_board = board.do_move(c_move);
                let (tried_score, tried_line) =
                    find_best_move_ab_rec( &tried_board, depth - 1, alpha, beta,
                                            engine_comm, time_restriction, node_counter);
                
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
    let mut node_counter = NodeCount { intern: 0, leaf: 0 };
    let (score, mut moves) =
        find_best_move_ab_rec(board, depth, MateB(0), MateW(0),
                              engine_comm, time_restriction, &mut node_counter);
    moves.reverse();
    // println!("Evaluated {} internal nodes and {} leaves", node_counter.intern, node_counter.leaf);
    (score, moves, node_counter)
}   


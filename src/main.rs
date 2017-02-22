mod uci;
mod board;
mod tests;
mod search_algorithms;

extern crate time;
extern crate rand;
extern crate ordered_float;

use search_algorithms::alpha_beta::Score;
use search_algorithms::alpha_beta::Score::*;

use std::sync::{Arc, Mutex};
use std::io;

use std::fmt;

use search_algorithms::board::EvalBoard;
use board::std_board::ChessBoard;
use board::crazyhouse_board::CrazyhouseBoard;

#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate itertools;
fn main() {
    
    loop {
        let reader = io::stdin();
        let mut input = "".to_string();
        reader.read_line(&mut input).unwrap();
	let tokens : Vec<&str> = input.split_whitespace().collect();
        let mut log_writer = Arc::new(Mutex::new(None));
        match tokens[0] {
            "uci" => {
                match uci::choose_variant(&log_writer, &mut io::BufReader::new(reader)) {
                    Ok(_) => (),
                    Err(e) => {
                        uci::to_log(&format!("Error: Engine failed with {}", e), &mut log_writer);
                        panic!(e);
                    },
                }
            },
            "isready" => {
                uci::open_log_file(&log_writer);
                uci::to_log("received isready from GUI", &log_writer);
                println!("readyok");
            }, 
            
            "mem usage" => {
                use std::mem;
                println!("Standard board: {}", mem::size_of::<board::std_board::ChessBoard>());
                println!("Standard move: {}", mem::size_of::<board::std_move::ChessMove>());
                println!("Standard piece: {}", mem::size_of::<board::std_board::Piece>());
                println!("Board score: {}", mem::size_of::<Score>());
                
            },
            //"play_self" => play_game(&board::START_BOARD.clone()),
            "play" => {
                if tokens.len() == 1 || tokens[1] == "standard" {
                    let board = ChessBoard::start_board().clone();
                    play_game(board, log_writer)
                }
                else {
                    match tokens[1] {
                        "crazyhouse" => {
                            let board = CrazyhouseBoard::start_board().clone();
                            play_game(board, log_writer)
                        },
                        s => println!("Unrecognized variant {}.", s),
                    }
                }
            },
            "mcts" => {
                let mut board = ChessBoard::start_board().clone();
                let mut mc_tree = search_algorithms::mcts::MonteCarloTree::new_root(&mut board);
                let mut searches = mc_tree.searches;
                let start_time = time::get_time();
                let mut rng = rand::weak_rng();
                
                while time::get_time() < start_time + time::Duration::seconds(10800) {
                    for _ in 1..10 {
                        use std::ops::Add;
                        mc_tree.select(&mut board, searches, &mut rng);
                        searches += 1;
                        let searches_of_children = mc_tree.children.iter()
                            .map(Option::as_ref).map(Option::unwrap)
                            .map(|n| n.searches)
                            .fold(0, u64::add);
                        debug_assert!((searches as i64 - searches_of_children as i64).abs() <= 1,
                                      format!("{} searches overall, but sum of searches of children is {}.",
                                              searches, searches_of_children));
                        if searches % 4096 == 0 {
                            mc_tree.print_score(&board, &mut String::new());
                        }
                    }
                    
                }
                mc_tree.print_score(&board, &mut String::new());
                
                
            }
            s => uci::to_log(&format!("Unrecognized command \"{}\".", s), &log_writer),
        }
    }
    
    
    
    //play_game(&board::START_BOARD.clone());
    //play_human();
}

#[allow(dead_code)]
fn play_game<B> (mut board : B, log_writer : uci::SharableWriter) 
    where B: EvalBoard + uci::UciBoard + fmt::Debug {
    println!("Board:\n{:?}", board);
    println!("\n");
    let engine_comm = Arc::new(Mutex::new(uci::EngineComm::new()));
    engine_comm.lock().unwrap().engine_is_running = true;
    
    let (score, moves, _) = search_algorithms::alpha_beta::search_moves(
        board.clone(), engine_comm.clone(), uci::TimeRestriction::MoveTime(5000), log_writer.clone());
    if moves.len() > 0 {
        println!("Found move with score {}.", score);
        board.do_move(moves[0].clone());
        play_game(board, log_writer);
    }
    else {
        match score {
            Val(_) => panic!("Found no moves for {}, but it was not mate! Board:\n{:?}",
                             board.to_move(), board),
            WhiteWin(_) => println!("White won at move! Board:\n{:?}", board),
            BlackWin(_) => println!("Black won! Board:\n{:?}", board),
            Draw(0) => println!("The game was drawn! Board:\n{:?}", board),
            Draw(n) => println!("Game was marked as drawn, but with {} moves left. Board:\n{:?}",
                                n, board),
        }
    }
}


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
                match ChessMove::from_alg(&input_str) {
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

            let c_move = ChessMove::from_alg(&input_str).unwrap();
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
                    WhiteWin(_) => { println!("White won at move! Board:\n{}", board); break },
                    BlackWin(_) => { println!("Black won! Board:\n{}", board); break },
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
    intern: u64,
    leaf: u64,
    total: u64,
}

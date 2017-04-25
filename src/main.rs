mod uci;
mod board;
mod tests;
mod search_algorithms;

extern crate time;
extern crate rand;
extern crate ordered_float;
extern crate rayon;

use search_algorithms::alpha_beta;
use search_algorithms::alpha_beta::Score;
use search_algorithms::alpha_beta::Score::*;
use search_algorithms::game_move::Move;
use search_algorithms::mcts;

use std::sync::{Arc, Mutex};
use std::io;
use std::fmt;


use search_algorithms::board::EvalBoard;
use search_algorithms::board::GameResult;
use board::std_board::ChessBoard;
use board::crazyhouse_board::CrazyhouseBoard;
use board::sjadam_board::SjadamBoard;

#[macro_use]
extern crate lazy_static;
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
            
            "mem_usage" => {
                use std::mem;
                println!("Standard board: {}", mem::size_of::<board::std_board::ChessBoard>());
                println!("Standard move: {}", mem::size_of::<board::std_move::ChessMove>());
                println!("Standard piece: {}", mem::size_of::<board::std_board::Piece>());
                println!("Board score: {}", mem::size_of::<Score>());
                println!("Size of chess mcts node: {}",
                         mem::size_of::<mcts::MonteCarloTree<ChessBoard>>());
                
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
                        "sjadam" => {
                            let board = SjadamBoard::start_board().clone();
                            play_game(board, log_writer)
                        }
                        s => println!("Unrecognized variant {}.", s),
                    }
                }
            },
            "mcts" => {
                
                if tokens.len() == 1 || tokens[1] == "standard" {
                    let mut board = ChessBoard::start_board().clone();
                    search_algorithms::mcts::search_position(&mut board);
                }
                else {
                    match tokens[1] {
                        "crazyhouse" => {
                            let mut board = CrazyhouseBoard::start_board().clone();
                            search_algorithms::mcts::search_position(&mut board);
                        },
                        "sjadam" => {
                            let mut board = SjadamBoard::start_board().clone();
                            search_algorithms::mcts::search_position(&mut board);
                        }
                        s => println!("Unrecognized variant {}.", s),
                    }
                }
            }
            s => uci::to_log(&format!("Unrecognized command \"{}\".", s), &log_writer),
        }
    }
}

/// Makes the engine play a game against itself
fn play_game<B> (mut board : B, log_writer : uci::SharableWriter) 
    where B: EvalBoard + uci::UciBoard + fmt::Debug + Send + 'static, <B as EvalBoard>::Move: Sync {
    println!("Board:\n{:?}", board);
    println!("\n");
    match board.game_result() {
        None => {
            let channel = alpha_beta::start_uci_search(board.clone(), uci::TimeRestriction::MoveTime(5000),
                                                       uci::EngineOptions::new());
            let (score, move_str) = uci::get_uci_move(channel);
            println!("Found move {} with score {}.", move_str, score);
            board.do_move(B::Move::from_alg(&move_str).unwrap());
            play_game(board, log_writer);
        }
        Some(GameResult::WhiteWin) => println!("White won at move! Board:\n{:?}", board),
        Some(GameResult::BlackWin) => println!("Black won! Board:\n{:?}", board),
        Some(GameResult::Draw) => println!("The game was drawn! Board:\n{:?}", board),
    }
}
/*
fn play_human<B>(mut board : B, log_writer : uci::SharableWriter)
    where B: EvalBoard + uci::UciBoard + fmt::Debug {

    use search_algorithms::board::Color::*;
    loop {
        println!("Board:\n{}\nHalf move count: {}", board, board.half_move_clock);
        // If black, play as human
        if board.to_move == White {
            println!("Type your move as long algebraic notation (e2-e4):");

            let reader = io::stdin();
            let mut input_str = "".to_string();
            let legal_moves = board.all_legal_moves();
            // Loop until user enters a valid move
            loop {
                match B::Move::from_alg(&input_str) {
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

            let c_move =  B::Move::from_alg(&input_str).unwrap();
            println!("Doing move");
            board = board.do_move(c_move);
            
        }
        else {
            let engine_comm = Mutex::new(uci::EngineComm::new());
            engine_comm.lock().unwrap().engine_is_running = true;
            
            let (score, moves, _) = alpha_beta:: (&board, 3, &engine_comm, None);
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

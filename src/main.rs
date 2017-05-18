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
    let mut stdin = io::BufReader::new(io::stdin());

    loop {
        let mut log_writer = Arc::new(Mutex::new(None));
        uci::open_log_file(&log_writer);
        
        if let Ok(input) = uci::get_engine_input(&log_writer, &mut stdin) {
            
	    let tokens : Vec<&str> = input.split_whitespace().collect();

            match tokens[0] {
                "uci" => {
                    match uci::choose_variant(&log_writer, &mut stdin) {
                        Ok(_) => (),
                        Err(e) => {
                            uci::to_log(&format!("Engine front terminated unexpectedly with \"{}\"", e),
                                        &mut log_writer);
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
                "play_self" => {
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
                "play" => {
                    if tokens.len() == 1 || tokens[1] == "standard" {
                        let board = ChessBoard::start_board().clone();
                        play_human(board, log_writer)
                    }
                    else {
                        match tokens[1] {
                            "crazyhouse" => {
                                let board = CrazyhouseBoard::start_board().clone();
                                play_human(board, log_writer)
                            },
                            "sjadam" => {
                                let board = SjadamBoard::start_board().clone();
                                play_human(board, log_writer)
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
        else {
            return;
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
                                                       uci::EngineOptions::new(),
                                                       Arc::new(Mutex::new(uci::EngineComm::new())));
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
/// Play a game against the engine through stdin 
fn play_human<B>(mut board : B, log_writer : uci::SharableWriter)
    where B: EvalBoard + 'static + uci::UciBoard + fmt::Debug + Send, <B as EvalBoard>::Move: Sync
{
    match board.game_result() {
        None => {
            use search_algorithms::board::Color::*;
            println!("Board:\n{:?}", board);
            // If black, play as human
            if board.to_move() == White {
                println!("Type your move as long algebraic notation (e2e4):");

                let reader = io::stdin();
                let mut input_str = "".to_string();
                let legal_moves = board.all_legal_moves();
                // Loop until user enters a valid move
                loop {
                    input_str.clear();
                    reader.read_line(&mut input_str).ok().expect("Failed to read line");
                    
                    match B::Move::from_alg(input_str.trim()) {
                        Ok(val) => {
                            if legal_moves.contains(&val) { break; }
                            println!("Move {:?} is illegal! Legal moves: {:?}", val, legal_moves);
                            println!("Try again: ");
                        }
                        
                        Err(error) => {
                            println!("{}, try again.", error);
                        },
                    }
                }
                let c_move =  B::Move::from_alg(input_str.trim()).unwrap();
                board.do_move(c_move);
                play_human(board, log_writer);
            }
            else {
                let channel = alpha_beta::start_uci_search(
                    board.clone(), uci::TimeRestriction::MoveTime(5000), uci::EngineOptions::new(),
                    Arc::new(Mutex::new(uci::EngineComm::new())));
                let (score, move_str) = uci::get_uci_move(channel);
                let best_move = <B as EvalBoard>::Move::from_alg(&move_str);
                println!("Computer played {:?} with score {}", best_move, score);
                board.do_move(best_move.unwrap());
                play_human(board, log_writer);
            }
        }
    
        Some(GameResult::WhiteWin) => println!("White won at move! Board:\n{:?}", board),
        Some(GameResult::BlackWin) => println!("Black won! Board:\n{:?}", board),
        Some(GameResult::Draw) => println!("The game was drawn! Board:\n{:?}", board),
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NodeCount {
    intern: u64,
    leaf: u64,
    total: u64,
}

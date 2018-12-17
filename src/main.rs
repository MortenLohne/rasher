mod uci;
pub mod board;
mod tests;
pub mod search_algorithms;

#[cfg(test)]
#[macro_use]
extern crate quickcheck;
#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;
extern crate itertools;
extern crate rand;
extern crate ordered_float;
extern crate rayon;
#[cfg(feature = "logging")]
extern crate log4rs;
#[cfg(feature = "profile")]
extern crate cpuprofiler;

use std::sync::{Arc, Mutex};
use std::io;
use std::fmt;
use std::hash::Hash;
use std::time;

use search_algorithms::alpha_beta;
use search_algorithms::alpha_beta::Score;
use search_algorithms::mcts;
use search_algorithms::board::GameResult;
use uci::UciBoard;

use board::std_board::ChessBoard;
use board::crazyhouse_board::CrazyhouseBoard;
use board::sjadam_board::SjadamBoard;

#[cfg(feature = "logging")] 
use log4rs::config::{Appender, Config, Root};
#[cfg(feature = "logging")]
use std::io::Write;

#[cfg(feature = "profile")]
use cpuprofiler::PROFILER;
use search_algorithms::board::Board;
use search_algorithms::board::ExtendedBoard;

#[cfg(feature = "logging")]
fn init_log() -> Result<(), Box<std::error::Error>> {
    let appender = log4rs::append::file::FileAppender::builder().append(true).build("rasher.log")?;
    
    let config = Config::builder()
        .appender(Appender::builder().build("uci", Box::new(appender)))
        .build(Root::builder().appender("uci").build(log::LogLevelFilter::Debug))?;
    log4rs::init_config(config)?;
    Ok(())
}

fn main() {
    #[cfg(feature = "logging")]
    init_log().unwrap_or_else(|err| {
        let _ = writeln!(&mut io::stderr(), "Failed to open log file: {}", err);
    });

    #[cfg(feature = "profile")]
    PROFILER.lock().unwrap().start("./my-prof.profile").unwrap();

    let mut stdin = io::BufReader::new(io::stdin());
    info!("Opened log");
    loop {
        if let Ok(input) = uci::get_engine_input(&mut stdin) {
            
	    let tokens : Vec<&str> = input.split_whitespace().collect();

            match tokens[0] {
                "uci" => {
                    uci::connect_engine(&mut stdin).unwrap();
                    return;
                },
                "isready" => {
                    info!("received isready from GUI");
                    uci::uci_send("readyok");
                }, 
                
                "mem_usage" => {
                    use std::mem;
                    println!("Standard board: {}", mem::size_of::<board::std_board::ChessBoard>());
                    println!("Standard move: {}", mem::size_of::<board::std_move::ChessMove>());
                    println!("Standard piece: {}", mem::size_of::<board::std_board::Piece>());
                    println!("Sjadam board: {}", mem::size_of::<SjadamBoard>());
                    println!("Sjadam move: {}", mem::size_of::<board::sjadam_move::SjadamMove>());
                    println!("Board score: {}", mem::size_of::<Score>());
                    println!("Size of chess mcts node: {}",
                             mem::size_of::<mcts::MonteCarloTree<ChessBoard>>());
                    
                },
                //"play_self" => play_game(&board::START_BOARD.clone()),
                "play_self" => {
                    if tokens.len() == 1 || tokens[1] == "standard" {
                        let board = ChessBoard::start_board().clone();
                        play_game(board)
                    }
                    else {
                        match tokens[1] {
                            "crazyhouse" => {
                                let board = CrazyhouseBoard::start_board().clone();
                                play_game(board)
                            },
                            "sjadam" => {
                                let board = SjadamBoard::start_board().clone();
                                play_game(board)
                            }
                            s => println!("Unrecognized variant {}.", s),
                        }
                    }
                },
                "play" => {
                    if tokens.len() == 1 || tokens[1] == "standard" {
                        let board = ChessBoard::start_board().clone();
                        play_human(board)
                    }
                    else {
                        match tokens[1] {
                            "crazyhouse" => {
                                let board = CrazyhouseBoard::start_board().clone();
                                play_human(board)
                            },
                            "sjadam" => {
                                let board = SjadamBoard::start_board().clone();
                                play_human(board)
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
                "mcts_debug" => mcts::play_human(ChessBoard::start_board()),
                s => warn!("Unrecognized command \"{}\".", s),
            }
        }
        else {
            return;
        }
    }
}

/// Makes the engine play a game against itself
fn play_game<B> (mut board : B) 
    where B: UciBoard + ExtendedBoard + fmt::Debug + Send + 'static + Hash + Eq + Clone,
<B as Board>::Move: Sync + Send {
    println!("Board:\n{:?}", board);
    println!("\n");
    match board.game_result() {
        None => {
            let (handle, channel) = alpha_beta::start_uci_search(
                board.clone(), uci::TimeRestriction::MoveTime(time::Duration::from_secs(5)),
                uci::EngineOptions::new(),
                Arc::new(Mutex::new(uci::EngineComm::new())), None);
            
            let (score, move_str) = uci::get_uci_move(handle, channel).unwrap();
            println!("Found move {} with score {}.", move_str, score.uci_string(board.side_to_move()));
            let mv = board.from_alg(&move_str).unwrap();
            board.do_move(mv);
            play_game(board);
        }
        Some(GameResult::WhiteWin) => println!("White won at move! Board:\n{:?}", board),
        Some(GameResult::BlackWin) => println!("Black won! Board:\n{:?}", board),
        Some(GameResult::Draw) => println!("The game was drawn! Board:\n{:?}", board),
    }
}
/// Play a game against the engine through stdin 
fn play_human<B>(mut board : B)
    where B: 'static + UciBoard + ExtendedBoard + fmt::Debug + Send + Hash + Eq,
          <B as Board>::Move: Sync + Send
{
    match board.game_result() {
        None => {
            use search_algorithms::board::Color::*;
            println!("Board:\n{:?}", board);
            // If black, play as human
            if board.side_to_move() == White {
                println!("Type your move as long algebraic notation (e2e4):");

                let reader = io::stdin();
                let mut input_str = "".to_string();
                let mut legal_moves = vec![];
                board.generate_moves(&mut legal_moves);
                // Loop until user enters a valid move
                loop {
                    input_str.clear();
                    reader.read_line(&mut input_str).expect("Failed to read line");
                    
                    match board.from_alg(input_str.trim()) {
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
                let c_move = board.from_alg(input_str.trim()).unwrap();
                board.do_move(c_move);
                play_human(board);
            }
            else {
                let (handle, channel) = alpha_beta::start_uci_search(
                    board.clone(), uci::TimeRestriction::MoveTime(time::Duration::from_secs(5)),
                    uci::EngineOptions::new(),
                    Arc::new(Mutex::new(uci::EngineComm::new())), None);
                
                let (score, move_str) = uci::get_uci_move(handle, channel).unwrap();
                let best_move = board.from_alg(&move_str);
                println!("Computer played {:?} with score {}",
                         best_move, score.uci_string(board.side_to_move()));
                board.do_move(best_move.unwrap());
                play_human(board);
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
}

impl NodeCount {
    fn new() -> Self {
        NodeCount { intern: 0, leaf: 0 }
    }
    fn total(&self) -> u64 {
        self.intern + self.leaf
    }
}

impl std::ops::Add for NodeCount {
    type Output = NodeCount;
    fn add(self, other: NodeCount) -> Self {
        NodeCount { intern: self.intern + other.intern, leaf: self.leaf + other.leaf }
    }
}

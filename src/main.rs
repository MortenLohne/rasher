mod uci;
pub mod board;
mod tests;
pub mod search_algorithms;
pub mod pgn;
pub mod uci_engine;

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

use std::io;
use std::fmt;
use std::hash::Hash;
use std::time;

use search_algorithms::alpha_beta::Score;
use search_algorithms::mcts;
use search_algorithms::board::GameResult;
use pgn::PgnBoard;

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
use search_algorithms::alpha_beta::AlphaBeta;
use uci_engine::UciEngine;
use search_algorithms::monte_carlo::MonteCarlo;
use std::sync::Arc;
use std::sync::Mutex;
use search_algorithms::monte_carlo;

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

    for &cp in [-3.0, -2.0, -1.0, 0.0, 1.0, 2.0, 3.0].iter() {
        let win_pct = monte_carlo::eval_to_win_pct(cp);
        println!("{} eval: {}", cp, win_pct);
        println!("{} win percentage: {} eval", win_pct, monte_carlo::win_pct_to_eval(win_pct))
    }

    let monte_carlo: MonteCarlo<ChessBoard> = MonteCarlo::init();
    let mut board = ChessBoard::start_board();
    for uci_info in monte_carlo.search(board.clone(),
                                       uci::TimeRestriction::Infinite,
                                       Arc::new(Mutex::new(uci::EngineComm::new())),
                                       None) {
        println!("{}", uci_info.to_info_string(&mut board));
    }

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
    where B: PgnBoard + ExtendedBoard + fmt::Debug + Send + 'static + Hash + Eq + Clone,
<B as Board>::Move: Sync + Send {
    println!("Board:\n{:?}", board);
    println!("\n");
    match board.game_result() {
        None => {
            let mut engine = AlphaBeta::init();
            let (score, mv) =
                engine.best_move(board.clone(),
                                 uci::TimeRestriction::MoveTime(time::Duration::from_secs(5)),
                                 None).unwrap();

            println!("Found move {:?} with score {}.", mv, score.uci_string(board.side_to_move()));
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
    where B: 'static + PgnBoard + ExtendedBoard + fmt::Debug + Send + Hash + Eq,
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
                    
                    match board.move_from_lan(input_str.trim()) {
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
                let c_move = board.move_from_lan(input_str.trim()).unwrap();
                board.do_move(c_move);
                play_human(board);
            }
            else {

                let mut engine = AlphaBeta::init();
                let (score, best_move) =
                    engine.best_move(board.clone(),
                                     uci::TimeRestriction::MoveTime(time::Duration::from_secs(5)),
                                     None).unwrap();

                println!("Computer played {:?} with score {}",
                         best_move, score.uci_string(board.side_to_move()));
                board.do_move(best_move);
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
    qsearch: u64,
    hash_full_hits: u64, // TT hit causes a cutoff
    hash_partial_hits: u64, // TT hit, hash move is used, but no cutoff
    hash_misses: u64, // No TT hit
    hash_move_cutoffs: u64, // Hash move causes a cutoff before other moves are tried
    null_move_cutoffs: u64,
    null_move_skips: u64,
}

impl NodeCount {
    fn new() -> Self {
        NodeCount { intern: 0, leaf: 0, qsearch: 0, null_move_cutoffs: 0,
            null_move_skips: 0, hash_full_hits: 0,
            hash_partial_hits: 0, hash_misses: 0, hash_move_cutoffs: 0 }
    }
    fn total(&self) -> u64 {
        self.intern + self.leaf + self.qsearch
    }
}

impl std::ops::Add for NodeCount {
    type Output = NodeCount;
    fn add(self, other: NodeCount) -> Self {
        NodeCount {
            intern: self.intern + other.intern, leaf: self.leaf + other.leaf,
            qsearch: self.qsearch + other.qsearch,
            null_move_cutoffs: self.null_move_cutoffs + other.null_move_cutoffs,
            null_move_skips: self.null_move_skips + other.null_move_skips,
            hash_full_hits: self.hash_full_hits + other.hash_full_hits,
            hash_partial_hits: self.hash_partial_hits + other.hash_partial_hits,
            hash_misses: self.hash_misses + other.hash_misses,
            hash_move_cutoffs: self.hash_move_cutoffs + other.hash_move_cutoffs

        }
    }
}

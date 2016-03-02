#![feature(test)]
mod uci;
mod board;
mod tests;
mod move_gen;
mod monte_carlo;
mod alpha_beta;

extern crate time;

use Score::*;
use board::std_board::*;

use board::std_board::PieceType::*;
use board::std_board::Color::*;

use std::sync::{Arc, Mutex};
use std::io;

use std::fmt::Error;
use std::cmp::*;
use std::fmt::Display;
use std::fmt::Formatter;

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
                let mut log_writer = Arc::new(Mutex::new(None));

                match uci::connect_engine(&log_writer) {
                    Ok(_) => (),
                    Err(e) => {
                        uci::to_log(&e, &mut log_writer);
                        panic!(e);
                    },
                }
            }, 
            
            "mem usage\n" => {
                use std::mem;
                println!("Standard board: {}", mem::size_of::<Board>());
                println!("Standard move: {}", mem::size_of::<board::std_move::Move>());
                println!("Standard piece: {}", mem::size_of::<board::std_board::Piece>());
                println!("Board score: {}", mem::size_of::<Score>());
                
            },
            //"play_self\n" => play_game(&board::START_BOARD.clone()),
            //"play\n" => play_human(),
            s => println!("Unrecognized command \"{}\".", s),
        }
        input.clear();
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
    intern: u64,
    leaf: u64,
    total: u64,
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
#[inline(never)]
fn score_board (board : &Board) -> Score {
    const POS_VALS : [[u8; 8]; 8] = 
        [[0, 0, 0, 0, 0, 0, 0, 0],
         [0, 1, 1, 1, 1, 1, 1, 0],
         [0, 1, 2, 2, 2, 2, 1, 0],
         [0, 1, 2, 3, 3, 2, 1, 0],
         [0, 1, 2, 3, 3, 2, 1, 0],
         [0, 1, 2, 2, 2, 2, 1, 0],
         [0, 1, 1, 1, 1, 1, 1, 0],
         [0, 0, 0, 0, 0, 0, 0, 0]];
    /*let center_proximity = |file, rank| {
    (3.5f32).powi(2) - ((3.5 - file as f32).powi(2) + (3.5 - rank as f32).powi(2)).sqrt()
    };*/
    let mut value = 0.0;
    for rank in 0..8 {
        for file in 0..8 {
            let piece_val = board.board[rank][file].value();
            let pos_val = POS_VALS[rank][file] as f32 *
                match board.board[rank][file] {
                    Piece(Bishop, White) => 0.15,
                    Piece(Bishop, Black) => -0.15,
                    Piece(Knight, White) => 0.3,
                    Piece(Knight, Black) => -0.3,
                    Piece(Queen, White) => 0.3,
                    Piece(Queen, Black) => -0.3,
                    Piece(Pawn, White) => 0.00,
                    Piece(Pawn, Black) => -0.00,
                    _ => 0.0,
                };
            let pawn_val = match board.board[rank][file] {
                Piece(Pawn, _) => (rank as f32 - 3.5) * -0.1,
                _ => 0.0,
            };
            value += piece_val + pos_val + pawn_val;
            //println!("Value at {} is {}, {}, total: {}",
            //         Square::from_ints(file as u8, rank as u8), piece_val, pos_val, value);
        }
    }
    Val(value)
}


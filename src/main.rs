#![feature(core)]
#![feature(test)]
mod uci;
mod board;
mod tests;
mod move_gen;
mod monte_carlo;

use Score::{Val, Draw, MateW, MateB};
use board::PieceType;
use board::PieceType::*;
use board::Color::*;
use board::Square;
use board::Piece;
use board::Board;
use board::Move;

use std::sync::{Arc, Mutex};
use std::io;

use std::fmt::Error;
use std::cmp::*;
use std::fmt::Display;
use std::fmt::Formatter;

#[macro_use]
extern crate lazy_static;

fn main() {
    let mut reader = io::stdin();
    let mut input = "".to_string();
    loop {
        println!("Enter input");
        reader.read_line(&mut input).unwrap();
        match (&input[..]) {
            "uci\n" => uci::connect_engine(),
            "play_self\n" => play_game(&board::START_BOARD.clone()),
            "play\n" => play_human(),
            s => println!("Unrecognized command \"{}\".", s),
        }
    }
    
    
    
    //play_game(&board::START_BOARD.clone());
    //play_human();
}

#[allow(dead_code)]
fn play_game(board : &Board) {
    println!("Board:\n{}\nHalf move count: {}", board, (*board).half_move_clock);
    println!("\n");
    let (score, moves, _) = find_best_move_ab (&board, 5, None);
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
#[allow(dead_code)]
fn play_human() {
    let mut board = board::START_BOARD.clone();
    loop {
        println!("Board:\n{}\nHalf move count: {}", board, board.half_move_clock);
        // If black, play as human
        if board.to_move == White {
            println!("Type your move as long algebraic notation (e2-e4):");

            let mut reader = io::stdin();
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
            input_str = "".to_string();
            println!("Doing move");
            board = board.do_move(c_move);
            
        }
        else {
            let (score, moves, _) = find_best_move_ab (&board, 3, None);
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

#[derive(Clone, Debug, Eq, PartialEq)]
struct NodeCount {
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

#[allow(dead_code)]
fn find_best_move (board : &Board, depth : u8) -> (Score, Option<Move>) {
    if depth == 0 {
        return (score_board(&board), None);
    }
    if board.half_move_clock > 50 {
        return (Draw(0), None);
    }
    let mut best_score = if board.to_move == White { MateB(0) } else { MateW(0) };
    let mut best_move : Option<Move> = None;

    let legal_moves = move_gen::all_legal_moves(&board);
    if legal_moves.len() == 0 {
        if move_gen::is_attacked(board, board.king_pos()) {
            if board.to_move == White {
                return (MateB(0), None);
            }
            else {
                return (MateW(0), None);
            }
        }
        else {
            return (Draw(0), None);
        }
    }
    for c_move in legal_moves.iter() {

        let tried_board = board.do_move(*c_move);
        
        let (tried_score, _) = find_best_move(&tried_board, depth - 1);

        if board.to_move == White && tried_score == MateB(0) && tried_score < best_score
             || board.to_move == Black && tried_score == MateW(0) && tried_score > best_score {
            panic!(format!("{} has best score {}, tried score {}.",
                           board.to_move, best_score, tried_score));
        }
        if board.to_move == White && tried_score > best_score
            || board.to_move == Black && tried_score < best_score {
                best_score = tried_score;
                best_move = Some(*c_move);
            }
    }
    (match best_score {
        MateB(i) => MateB(i + 1),
        MateW(i) => MateW(i + 1),
        Draw(i) => Draw(i + 1),
        Val(n) => Val(n), },
     best_move)
}

/// Returns a score, and a list of moves representing the moves it evaluated
fn find_best_move_ab (board : &Board, depth : u8, stop_signal : Option<Arc<Mutex<Box<bool>>>>)
                      -> (Score, Vec<Move>, NodeCount) {

    fn find_best_move_ab_rec (board: &Board, depth : u8, mut alpha : Score, mut beta : Score,
                              stop_signal : Option<Arc<Mutex<Box<bool>>>>,
                              node_counter : &mut NodeCount)
                              -> (Score, Vec<Move>) {

        let should_abort = match stop_signal.clone() {
            Some(mutex) => {
                let stop_signal = mutex.lock().unwrap();
                *(stop_signal.clone())
            },
            None => false,
        };
        
        if should_abort { panic!() }
        
        if depth == 0 {
            node_counter.leaf += 1;
            return (score_board(&board), vec![]);
        }
        else {
            node_counter.intern += 1;
        }
        if board.half_move_clock > 50 {
            return (Draw(0), vec![]);
        }
        
        // Helpful alias
        let color = board.to_move;
        let mut best_move = None;
        let mut best_line = vec![];
        
        let legal_moves = move_gen::all_legal_moves(board);
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
                let (tried_score, tried_line) = find_best_move_ab_rec(
                    &tried_board, depth - 1, alpha, beta, stop_signal.clone(), node_counter);
                
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
    let (score, mut moves) = find_best_move_ab_rec(board, depth, MateB(0), MateW(0),
                                                   stop_signal, &mut node_counter);
    moves.reverse();
    // println!("Evaluated {} internal nodes and {} leaves", node_counter.intern, node_counter.leaf);
    (score, moves, node_counter)
}   


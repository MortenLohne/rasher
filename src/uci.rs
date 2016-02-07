#[allow(unused_imports)]
use board::PieceType::{Pawn, Knight, Bishop, Rook, Queen, King, Empty};
use board::Piece;
use board::Color::{Black, White};
use board::Move;
use board::Board;
use board;
use ::Score;
use std::thread;

extern crate time;

use std::sync::{Mutex, Arc};
use std::io;
use std::io::Write;
use std::fs;
use std::path;

/// Connects the engine to a GUI using UCI. 
/// Assumes "uci has already been sent"
pub fn connect_engine() {

    // Prepare to create log file
    let mut options = fs::OpenOptions::new();
    options.write(true).append(true);
    let path = path::Path::new("log.txt");

    let log_file = match fs::File::create(path) {
        Ok(file) => file,
        Err(err) => panic!(format!("Error creating log file at {:?} : {}", path, err)),
    };
    let mut writer = Arc::new(Mutex::new(io::BufWriter::new(log_file)));
    to_log("Opened log file", &mut writer);

    // Do the standard handshake with the GUI
    to_log("Received uci command from GUI", &mut writer);
    uci_send("id name morten_chess", &mut writer);
    uci_send("uciok", &mut writer);
    

    // The engine would send any customizable options here. For now, there are none.
    
    let mut board = None;
    let stop_signal = Arc::new(Mutex::new(Box::new(false)));
    let best_move = Arc::new(Mutex::new(None));
    
    loop {
        let input = get_engine_input(&mut writer);
        let tokens = input.split_whitespace().collect::<Vec<_>>();
        if tokens.len() == 0 {
            to_log(&format!("Unrecognized input \"{}\"", input), &mut writer);
            continue;
        }
        match tokens[0] {
            "isready" => uci_send("readyok", &mut writer),
            "quit" => panic!(),
            "ucinewgame" => (), // Ignore this for now
            "position" => {
                board = match parse_position(&input, &mut writer) {
                    Ok(b) => Some(b),
                    Err(err) => {
                        to_log(&err, &mut writer);
                        panic!(err);
                    },
                };
            },
            "stop" => {
                let mut stop_signal = stop_signal.lock().unwrap();
                *stop_signal = Box::new(true);
                let best_move : board::Move = match best_move.lock().unwrap().clone() {
                    Some(mv) => mv,
                    None => {
                        to_log(&"Haven't found a move yet: ignoring stop command.", &mut writer);
                        continue;
                    },
                };
                uci_send(&format!("bestmove {}", best_move.to_alg()), &mut writer);
            },
            "go" => {
                match board {
                    Some(ref board) => parse_go(&input, board, writer.clone(),
                                                stop_signal.clone(), best_move.clone()),
                    None => {
                        to_log(&"Received go command with receiving a position first. Exiting...",
                               &mut writer);
                        panic!();
                    },
                }
            },
            _ => { // TODO: If receiving unrecognied token, parse the next one as usual
                to_log(&format!("Unrecognized input \"{}\". Ignoring.", input), &mut writer);
            },
        }
    }
}
    
fn parse_go (input : &String, board : &Board,
             mut writer : Arc<Mutex<io::BufWriter<fs::File>>>,
             stop_signal : Arc<Mutex<Box<bool>>>, best_move : Arc<Mutex<Option<board::Move>>>) {

    let mut tokens_it = input.split_whitespace().skip(1);
    let mut move_time = None;

    // Parses a string from the iterator to return a u32
    fn parse_int (next_token : Option<&str>, writer : &mut Arc<Mutex<io::BufWriter<fs::File>>> )
                  -> Option<u32>
    {
        match next_token {
            Some (token) => match u32::from_str_radix(token, 10) {
                Ok(n) => Some(n),
                Err(_) =>  {
                    to_log("Error: \"movetime\" token formatted incorrectly", writer);
                    None
                },
            },
            None => {
                to_log("Error: Expected int after \"movetime\" token", writer);
                None
            },
        }
    }
    
    loop {
        match tokens_it.next() {
            Some("movetime") =>
                move_time = parse_int(tokens_it.next(), &mut writer),
            Some("btime") => {
                if board.to_move == Black {
                    move_time = parse_int(tokens_it.next(), &mut writer).map(|i| i / 15);
                }
            },
            Some("wtime") => {
                if board.to_move == White {
                    move_time = parse_int(tokens_it.next(), &mut writer).map(|i| i / 15);
                }
            },
            Some (s) => to_log(&format!("Unknown token \"{}\" in go command", s), &mut writer),
            None => break,
        }
    }
    
    {
        let mut stop_signal = stop_signal.lock().unwrap();
        *stop_signal = Box::new(false);
    }
    calc_from_pos(board.clone(), writer.clone(), stop_signal.clone(), best_move.clone());
    
    match move_time {
        Some(n) => {
            thread::spawn(move || {
                thread::sleep_ms(n - 20);
                let mut stop_signal = stop_signal.lock().unwrap();
                *stop_signal = Box::new(true);
                let best_move : board::Move = match best_move.lock().unwrap().clone() {
                    Some(mv) => mv,
                    None => {
                        to_log(&"Error: Didn't find a move in time", &mut writer);
                        return;
                    },
                };
                uci_send(&format!("bestmove {}", best_move.to_alg()), &mut writer); 
            });
        },
        None => (),
    }
}

/// Calulates progressively deeper from a certain position,
/// printing the results to standard output in a UCI compatible way
fn calc_from_pos (board : Board, mut log_writer : Arc<Mutex<io::BufWriter<fs::File>>>,
                  stop_calc : Arc<Mutex<Box<bool>>>,
                  best_move : Arc<Mutex<Option<board::Move>>>) {

    thread::spawn(move || {
        let start_time = time::get_time();
        for depth in 1.. {
            let (score, moves, node_count) = ::find_best_move_ab(&board, depth,
                                                                 Some(stop_calc.clone()));
            if moves.len() > 0 {
                let mut best_move = best_move.lock().unwrap();
                *best_move = Some(moves[0]);
            }

            let eng_score = match score {
                Score::Val(f) => "cp ".to_string() + &((100.0 * f) as i16).to_string(),
                Score::MateW(n) => "mate ".to_string() + &(n as i16 / 2).to_string(),
                Score::MateB(n) => "mate ".to_string() + &(n as i16 / -2).to_string(),
                Score::Draw(_) => "0 ".to_string(),
                
            };
            
            let mut inf_str = "info ".to_string();
            inf_str.push_str(&format!("depth {} ", depth));
            // inf_str.push_str(&format!("seldepth {} ", depth));
            inf_str.push_str(&format!("score {} ", eng_score));
            
            let total_nodes = node_count.intern + node_count.leaf;
            let ms_taken = (time::get_time() - start_time).num_milliseconds();
            
            inf_str.push_str(&format!("nodes {} ", total_nodes));
            inf_str.push_str(&format!("time {} ", ms_taken));
            if ms_taken > 0 {   
                inf_str.push_str(&format!("nps {} ", total_nodes * 1000 / ms_taken as u64));
            }
            if moves.len() > 0 {
                inf_str.push_str("pv ");
            }
            for c_move in moves {
                inf_str.push_str(&format!("{} ", c_move.to_alg()));
            }
            uci_send(&inf_str, &mut log_writer);
        }
    });
}
#[allow(unused_must_use)]
/// Simple helper method to write a line to the log
pub fn to_log (message : &str, log_writer : &mut Arc<Mutex<io::BufWriter<fs::File>>>) {
    let mut log_writer = log_writer.lock().unwrap();

    let time = format!("[{}:{}]", time::get_time().sec % 3600, time::get_time().nsec / 1000000);
    log_writer.write(&format!("{} Log: {}\n", time, message).bytes().collect::<Vec<u8>>());
    log_writer.flush();
}

/// Prints the input to stdout (Where it can be read by the GUI), and also writes it
/// to the log file. Should always be used instead of a pure println!()
#[allow(unused_must_use)]
pub fn uci_send (message : &str, log_writer : &mut Arc<Mutex<io::BufWriter<fs::File>>>) {
    let mut log_writer = log_writer.lock().unwrap();

    let time = format!("[{}:{}]", time::get_time().sec % 3600, time::get_time().nsec / 1000000);
    log_writer.write(&format!("{} Engine: {}\n", time, message)
                     .bytes().collect::<Vec<u8>>());
    log_writer.flush();
    println!("{}", message);
}

/// Waits for input from the GUI, and writes the input to the log
#[allow(unused_must_use)]
fn get_engine_input(log_writer : &mut Arc<Mutex<io::BufWriter<fs::File>>>) -> String {

    let mut reader = io::stdin();
    let mut input = "".to_string();
    reader.read_line(&mut input).unwrap();
    
    let mut log_writer = log_writer.lock().unwrap();

    let time = format!("[{}:{}]", time::get_time().sec % 3600, time::get_time().nsec / 1000000);
    log_writer.write(&format!("{} GUI: {}", time, input).bytes().collect::<Vec<u8>>());
    log_writer.flush();
    
    input
}

/// Turns the whole position string from the GU (Like "position startpos moves e2e4")
/// into an internal board representation
fn parse_position(input : &String, log_writer : &mut Arc<Mutex<io::BufWriter<fs::File>>>)
                  -> Result<board::Board, String> {
    
    let words : Vec<&str> = input.split_whitespace().collect();
    if words.len() < 2 || words[0] != "position" {
        Err(format!("Illegal position string: had length {}", words.len()))
    }
    else {
        // moves_pos is the position on the input string where the token "moves" is expected
        let (mut board, moves_pos) =
            if words[1] == "startpos" {
                (board::START_BOARD.clone(), 2)
            }
        else if words[1] == "fen" {
            let mut fen_string : String = "".to_string();
            for token in words.iter().skip(2).take(6) {
                fen_string.push_str(token);
                fen_string.push(' ');
            }
            fen_string = fen_string.trim().to_string();
            match parse_fen(&fen_string) {
                Ok(b) => (b, 8),
                Err(err) => return Err(err),
            }
        }
        else {
            return Err(format!("Illegally formatted position string, 
2nd token is {} and string has {} tokens", words[1], words.len()))
        };
        if words.len() > moves_pos  {
            if words[moves_pos] == "moves" {
                for c_move_str in words.iter().skip(moves_pos + 1) {
                    let c_move = match Move::from_short_alg(c_move_str) {
                        Ok(m) => m,
                        Err(err) => {
                            to_log(&err, log_writer);
                            return Err(format!("{}", err));
                        },
                    };
                    board = board.do_move(c_move);
                }
            }
        
            else {
                return Err(format!("Illegally formatted position string: 
Expected words.len() to be {} if no moves are included, was {}", moves_pos, words.len()))
            }
        }
        Ok(board)
    }
}
    
pub fn parse_fen (fen : &str) -> Result<board::Board, String> {
    let mut board = [[Piece(Empty, White); 8]; 8];
    let fen_split : Vec<&str> = fen.split(" ").collect();
    if fen_split.len() != 6 {
        return Err(format!("Invalid FEN string \"{}\": Had {} fields instead of 6",
                           fen, fen_split.len()));
    }
    let ranks : Vec<&str> = fen_split[0].split("/").collect();
    if ranks.len() != 8 {
        return Err(format!("Invalid FEN string \"{}\": Had {} ranks instead of 8",
                           fen, ranks.len()));
    }
    for i in 0..8 {
        let mut cur_rank : Vec<Piece> = Vec::new();
        for c in ranks[i].chars() {
            match board::CHAR_PIECE_MAP.get(&c) {
                Some(piece) => cur_rank.push(*piece),
                None => match c.to_digit(10) {
                    Some(mut i) => {
                        while i > 0 {
                            cur_rank.push(Piece(Empty, White));
                            i -= 1;
                        }
                    },
                    None => return Err(format!("Invalid FEN string: Illegal character {}", c)),
                },
            }   
        }
        if cur_rank.len() != 8 {
            return Err(format!("Invalid FEN string: Specified {} pieces on rank {}.",
                               cur_rank.len(), i))
        }
        else {
            for j in 0..8 {
                board[i][j] = *cur_rank.get(j).unwrap();
            }
        }
    }
    if fen_split[1].len() != 1 {
        return Err("Invalid FEN string: Error in side to move-field".to_string());
    }
    let char_to_move = fen_split[1].chars().collect::<Vec<_>>()[0];
    let to_move = if char_to_move == 'w' { White }
    else if char_to_move == 'b' { Black }
    else { return Err("Invalid FEN string: Error in side to move-field".to_string()) };

    let mut castling_rights = [false; 4];
    for c in fen_split[2].chars() {
        match c {
            '-' => break,
            'K' => castling_rights[0] = true,
            'Q' => castling_rights[1] = true,
            'k' => castling_rights[2] = true,
            'q' => castling_rights[3] = true,
            _ => return Err("Invalid FEN string: Error in castling field.".to_string()),
        }
    }
    let en_passant = if fen_split[3] == "-" { None }
    else {
        match board::Square::from_alg(fen_split[3]) {
            Some(square) => Some(square),
            None => return Err(format!("Invalid en passant square {}.", fen_split[3])),
        }
    };

    let (half_clock, move_num) : (u16, u16) =
        match ((fen_split[4]).parse(), (fen_split[5]).parse()) {
            (Ok(n1), Ok(n2)) => (n1, n2),
            _ => return Err("Invalid halfmove clock or move num".to_string()),
        };
    
    Ok(board::Board{ board: board, to_move: to_move, castling: castling_rights,
                     en_passant: en_passant, half_move_clock: half_clock,
                     move_num: move_num, moves: vec![]})
}

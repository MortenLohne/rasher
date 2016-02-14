#[allow(unused_imports)]
use board::PieceType::{Pawn, Knight, Bishop, Rook, Queen, King, Empty};
use board::Color::{Black, White};
use board::*;
use board;
use ::Score;
use std::thread;

extern crate time;

use std::sync::{Mutex, Arc};
use std::io;
use std::io::Write;
use std::fs;
use std::path;
use std::process;

pub type SharableWriter = Option<Arc<Mutex<io::BufWriter<fs::File>>>>;

/// Connects the engine to a GUI using UCI. 
/// Assumes "uci has already been sent"
pub fn connect_engine(mut writer : SharableWriter) -> Result<(),String> {

    // Do the standard handshake with the GUI
    to_log("Received uci command from GUI", &writer);
    uci_send("id name morten_chess", &writer);
    uci_send("uciok", &writer);
    
    // The engine would send any customizable options here. For now, there are none.
    
    let mut board = None;
    let engine_comm = Arc::new(Mutex::new(EngineComm::new()));

    // Listen to commands from GUI forever
    loop {
        let input = get_engine_input(&mut writer);
        let tokens = input.split_whitespace().collect::<Vec<_>>();
        if tokens.len() == 0 {
            to_log(&format!("Unrecognized input \"{}\"", input), &mut writer);
            continue;
        }
        match tokens[0] { // Parse the first word of the input
            "isready" => uci_send("readyok", &mut writer),
            "quit" => { to_log("Quitting...", &mut writer); process::exit(0); },
            "ucinewgame" => (), // Ignore this for now
            "position" => board = Some(try!(parse_position(&input, &mut writer))), 
            
            "stop" => {
                let mut engine_comm = engine_comm.lock().unwrap();
                engine_comm.engine_should_stop = true;
                let best_move : board::Move = match engine_comm.best_move.clone() {
                    Some(mv) => mv,
                    None => {
                        to_log(&"Haven't found a move yet: ignoring stop command.", &mut writer);
                        continue;
                    },
                };
                uci_send(&format!("bestmove {}", best_move.to_alg()), &mut writer);
            },
            "go" => {
                let board = try!(board.clone().ok_or("Received go command without receiving a position first. Exiting..."));
                let time_restriction = try!(parse_go(&input, writer.clone()));
                
                start_engine(board.clone(), writer.clone(), time_restriction, engine_comm.clone());
            },
            _ => { // TODO: If receiving unrecognied token, parse the next one as usual
                to_log(&format!("Unrecognized input \"{}\". Ignoring.", input), &mut writer);
            },
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum TimeRestriction {
    GameTime(TimeInfo),
    Depth(u8),
    Nodes(u64),
    Mate(u16),
    MoveTime(i64),
    Infinite,   
}

/// Struct for communicating with the engine from another thread
#[derive(PartialEq, Eq, Clone)]
pub struct EngineComm {
    pub engine_should_stop : bool,
    pub engine_is_running : bool,
    pub best_move : Option<Move>,
}

impl EngineComm {
    pub fn new() -> Self {
        EngineComm{ engine_should_stop: false, engine_is_running: false, best_move : None }
    }
}

fn start_engine (board : Board, log_writer : SharableWriter,
                 time_restriction : TimeRestriction, engine_comm : Arc<Mutex<EngineComm>>) {
    engine_comm.lock().unwrap().engine_is_running = true;

    thread::spawn (move || {
        ::search_moves(board, engine_comm,
                       time_restriction, log_writer);
    });
}

fn parse_go (input : &String, mut writer : SharableWriter)
             -> Result<TimeRestriction, String> {

    // Parses an optional string to return a u32
    fn parse_int (next_token : Option<&str>) -> Result<u32, String> {
        
        match next_token {
            Some(token) => u32::from_str_radix(token, 10).map_err(|_| String::from("Error: \"movetime\" token formatted incorrectly")),
            None => Err(String::from("Error: Expected int after \"movetime\" token, but it was the last token in the command")),
        }
    }        

    match input.split_whitespace().nth(1) {
        Some("infinite") => return Ok(TimeRestriction::Infinite),
        Some("movetime") => return Ok(TimeRestriction::MoveTime(
            try!(parse_int(input.split_whitespace().nth(2))
                 ) as i64)),
        None => { to_log("Received no restriction \"go\" command, assuming \"infinite\"",
                         &mut writer);
        },
        Some(_) => (),
    }
    
    let mut tokens_it = input.split_whitespace().skip(1);
    
    let (mut white_time, mut black_time, mut white_inc, mut black_inc, mut moves_to_go) =
        (None, None, None, None, None);
    
    loop {
        match tokens_it.next() {
            Some("moves_to_go") => moves_to_go = Some(try!(parse_int(tokens_it.next()))),
            
            Some("btime") => black_time = Some(try!(parse_int(tokens_it.next()))),
            Some("wtime") => {
                white_time = Some(try!(parse_int(tokens_it.next())));
            },
            Some("winc") => white_inc = Some(try!(parse_int(tokens_it.next()))),
            Some("binc") => black_inc = Some(try!(parse_int(tokens_it.next()))),
            Some (s) => return Err(format!("Unknown token \"{}\" in go command", s)),
            None => break,
        }
    }
    if white_time == None || black_time == None {
        return Err(String::from("Did not receive time left for white and black, nor other restrictions on \"go\" command."))
    }
    if white_inc == None {
        to_log("Did not receive white increment times, assuming 0", &mut writer);
        white_inc = Some(0);
    }
    if black_inc == None {
        to_log("Did not receive black increment times, assuming 0", &mut writer);
        black_inc = Some(0);
    }
    let time_info = TimeInfo {white_time: white_time.unwrap(), black_time: black_time.unwrap(),
                              white_inc: white_inc.unwrap(), black_inc: black_inc.unwrap(),
                              moves_to_go: moves_to_go.map(|v| v as u16 )};
    Ok(TimeRestriction::GameTime(time_info))
}

/// Sends the engine's evaluation to the GUI via uci, along with other data
/// like node count, time taken, etc
pub fn send_eval_to_gui (mut log_writer : SharableWriter, depth : u8,
                         ms_taken : i64, 
                         score : Score, moves : Vec<Move>, node_count : ::NodeCount) {
    let eng_score = match score {
        Score::Val(f) => "cp ".to_string() + &((100.0 * f) as i16).to_string(),
        Score::MateW(n) => "mate ".to_string() + &(n as i16 / 2).to_string(),
        Score::MateB(n) => "mate ".to_string() + &(n as i16 / -2).to_string(),
        Score::Draw(_) => "0 ".to_string(),
        
    };
    
    let mut inf_str = "info ".to_string();
    inf_str.push_str(&format!("depth {} ", depth));
    // inf_str.push_str(&format!("selDepth {} ", depth));
    inf_str.push_str(&format!("score {} ", eng_score));
    
    let total_nodes = node_count.intern + node_count.leaf;
    
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

#[allow(unused_must_use)]
/// Simple helper method to write a line to the log
pub fn to_log (message : &str, log_writer : &SharableWriter) {

    match log_writer{
        &Some(ref writer) => {
            
            let mut log_writer = writer.lock().unwrap();

            let time = format!("[{}:{}]", time::get_time().sec % 3600, 
                               time::get_time().nsec / 1000000);
            log_writer.write(&format!("{} {}\n", time, message)
                             .bytes().collect::<Vec<u8>>());
            log_writer.flush();
        },
        &None => (),
    }
}

/// Prints the input to stdout (Where it can be read by the GUI), and also writes it
/// to the log file, if it is present
#[allow(unused_must_use)]
pub fn uci_send (message : &str, log_writer : &SharableWriter) {

    let log_message = format!("Engine: {}", message);
    to_log(&log_message, log_writer);
    println!("{}", message);
}

/// Waits for input from the GUI, and writes the input to the log and returns it
#[allow(unused_must_use)]
fn get_engine_input(log_writer : &SharableWriter) -> String {

    let reader = io::stdin();
    let mut input = "".to_string();
    reader.read_line(&mut input).unwrap();
    
    to_log(&input, log_writer);

    input
}

/// Turns the whole position string from the GU (Like "position startpos moves e2e4")
/// into an internal board representation
fn parse_position(input : &String, log_writer : &SharableWriter)
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
            return Err(format!("Illegally formatted position string: \"{}\": 2nd token is {} and string has {} tokens",
                               input, words[1], words.len()))
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
    if fen_split.len() < 4 || fen_split.len() > 6 {
        return Err(format!("Invalid FEN string \"{}\": Had {} fields instead of [4, 5, 6]",
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
        if fen_split.len() > 4 {
            (try!(fen_split[4].parse().map_err(|_|"Invalid half_move number in FEN string")),
             try!(fen_split[5].parse().map_err(|_|"Invalid half_move number in FEN string")))
        }
    else {
        (0, 0)
    };
    
    Ok(board::Board{ board: board, to_move: to_move, castling: castling_rights,
                     en_passant: en_passant, half_move_clock: half_clock,
                     move_num: move_num, moves: vec![]})
}

use board::std_board::ChessBoard;
use board::sjadam_board::SjadamBoard;
use board::std_board::TimeInfo;

use board::crazyhouse_board::CrazyhouseBoard;

use search_algorithms::game_move;
use search_algorithms::board;
use search_algorithms::alpha_beta;
use search_algorithms::mcts;
use search_algorithms::alpha_beta::Score;

extern crate time;

use std::fmt;
use std::thread;
use std::sync::{Mutex, Arc};
use std::io;
use std::io::Write;
use std::io::Read;
use std::{fs, process};
use std::str::FromStr;

pub type SharableWriter = Arc<Mutex<Option<io::BufWriter<fs::File>>>>;

pub trait UciBoard : Sized {
    fn from_fen(&str) -> Result<Self, String>;
    fn to_fen(&self) -> String;
}

pub trait UciMove : Sized {
    fn from_alg(&str) -> Result<Self, String>;
    fn to_alg(&self) -> String;
}

pub fn choose_variant(log_writer : &SharableWriter, stdin : &mut io::BufRead) -> Result<(), String> {

    open_log_file(log_writer);

    // Do the standard handshake with the GUI
    to_log("Received uci command from GUI", log_writer);
    uci_send("id name rasher", log_writer);

    uci_send("option name Write Debug Log type check default true", log_writer);
    uci_send("option name Variant type string", log_writer);
    uci_send("option name Hash type spin default 64 min 16 max 32768", log_writer);
    uci_send("option name Threads type spin default 2 min 1 max 16", log_writer);
    
    uci_send("uciok", log_writer);
    
    //uci_send("variants standard crazyhouse", log_writer);

    let input : String = get_engine_input(log_writer, stdin);
    let tokens = input.split_whitespace().collect::<Vec<_>>();
    if tokens.len() < 1 {
        to_log(&format!("Unrecognized variant string \"{}\", continuing normally", input), log_writer);
        //continue; TODO: Go to connect_engine
    }
    if tokens[0] == "variant" {
        match tokens[1] {
            "standard" => connect_engine::<ChessBoard>(log_writer, stdin),
            "crazyhouse" => connect_engine::<CrazyhouseBoard>(log_writer, stdin),
            "sjadam" => connect_engine::<SjadamBoard>(log_writer, stdin),
            _ => Err(format!("Unrecognized chess variant {}.", tokens[1])),
        }
    }
    else {
        let mut input_with_newline = input.clone();
        input_with_newline.push_str("\n");
        connect_engine::<ChessBoard>(log_writer,
                                     &mut io::Cursor::new(input_with_newline)
                                     .chain(stdin))
    }
}

/// Connects the engine to a GUI using UCI. 
/// Assumes "uci has already been sent"
pub fn connect_engine<Board>(log_writer : &SharableWriter,stdin : &mut io::BufRead)
                             -> Result<(), String> 
    where Board : 'static + board::EvalBoard + UciBoard + Send + fmt::Debug,
<Board as board::EvalBoard>::Move: Sync
{
    
    let mut board : Option<Board> = None;
    let mut engine_string = "minimax".to_string();
    let engine_comm = Arc::new(Mutex::new(EngineComm::new()));
    let mut engine_options = EngineOptions::new();
    
    // Listen to commands from GUI forever
    loop {
        let input = get_engine_input(log_writer, stdin);
        let tokens = input.split_whitespace().collect::<Vec<_>>();
        if tokens.len() == 0 {
            to_log(&format!("Unrecognized input \"{}\"", input), log_writer);
            continue;
        }
        match tokens[0] { // Parse the first word of the input
            "isready" => uci_send("readyok", log_writer),
            "quit" => { to_log("Quitting...", log_writer); process::exit(0); },
            "ucinewgame" => (), // Ignore this for now
            "position" => board = Some(try!(parse_position::<Board>(&input, log_writer))),
            "setoption" => { 
                // If the board has not been sent, we are in init mode,
                // which means init options can still be changed
                if board == None { 
                    try!(parse_setoption_init(&input, &mut engine_options, log_writer))
                }
                else {
                    try!(parse_setoption(&input, log_writer))
                }
            },
            "engine" => engine_string = tokens[1].to_string(),
            "stop" => {
                let best_move : String;
                {
                    let mut engine_comm = try!(engine_comm.lock().map_err(|err| err.to_string()));
                    engine_comm.engine_should_stop = true;
                    best_move = match engine_comm.best_move.clone() {
                        Some(mv) => mv,
                        None => {
                            to_log(&"Haven't found a move yet: sending a bogus move", log_writer);
                            "e2e4".to_string() // Send a bogus move
                        },
                    };
                }
                uci_send(&format!("bestmove {}", best_move), log_writer);
            },
            "go" => {
                loop {
                    let engine_is_running = {
                        let mut engine_comm = try!(engine_comm.lock().map_err(|err| err.to_string()));                          
                        if !engine_comm.engine_is_running {
                            engine_comm.engine_should_stop = false;
                            break;
                        }
                        engine_comm.engine_is_running
                    };
                    if engine_is_running {
                        to_log("Engine was already running, cannot start new. Trying again later", log_writer);
                        thread::sleep(::std::time::Duration::from_millis(100));
                        
                    }
                    
                }
                let board = try!(board.clone()
                                 .ok_or("Received go without receiving a position first. Exiting..."));
                let time_restriction = try!(parse_go(&input, log_writer));
                match engine_string.as_str() {
                    "minimax" => 
                        start_engine(board, log_writer.clone(),
                                     time_restriction, engine_comm.clone()),
                    "mcts" => {
                        let rx = start_mcts_engine(board, time_restriction,
                                                   engine_options.clone());
                        thread::spawn(move || {
                            loop {
                                match rx.recv() {
                                    Ok(uci_info) => println!("info {}", uci_info.to_info_string()),
                                    Err(_) => break,
                                }
                            }
                        });
                    },
                    _ => (),
                }
            },
            _ => { // TODO: If receiving unrecognied token, parse the next one as usual
                to_log(&format!("Unrecognized input \"{}\". Ignoring.", input), log_writer);
            },
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum TimeRestriction {
    GameTime(TimeInfo),
    Depth(u8),
    Nodes(u64),
    Mate(u16),
    MoveTime(i64),
    Infinite,
}

/// Struct for communicating with the engine from another thread
pub struct EngineComm {
    pub engine_should_stop : bool,
    pub engine_is_running : bool,
    pub best_move : Option<String>,
}

impl EngineComm {
    pub fn new() -> Self {
        EngineComm{ engine_should_stop: false, engine_is_running: false, best_move : None }
    }
}

use std::sync::mpsc;

fn start_mcts_engine<B>(board: B, time_limit: TimeRestriction,
                        options: EngineOptions) -> mpsc::Receiver<UciInfo>
    where B: 'static + board::EvalBoard + fmt::Debug + Send, <B as board::EvalBoard>::Move: Sync
{
    let (tx, rx) = mpsc::channel();
    thread::spawn(move ||
                  mcts::uci_search(board, time_limit, options, tx));
    rx
}

fn start_engine<B> (board : B, log_writer : SharableWriter,
                                            time_restriction : TimeRestriction, 
                      engine_comm : Arc<Mutex<EngineComm>>)
    where B: 'static + board::EvalBoard + Send + fmt::Debug
{
    {
        engine_comm.lock().unwrap().engine_is_running = true;
    }
    let cloned_board = board.clone();
    let cloned_log_writer = log_writer.clone();
    
    let thread_result = thread::Builder::new()
        .name("alpha_beta_thread".to_string())
        .spawn (move || {
            alpha_beta::search_moves(cloned_board, engine_comm,
                                     time_restriction, cloned_log_writer);
        });
    match thread_result {
        Ok(handle) => {
            thread::spawn(move || match handle.join() {
                Ok(_) => (),
                Err(panic) =>
                    to_log(&format!("Alpha_beta thread stopped by: \"{:?}\"", panic), &log_writer),
            });
        },
        Err(err) => println!("Couldn't create alpha_beta thread, error: \"{}\"", err),
    }
    
}

pub fn open_log_file (log_writer : &SharableWriter) {
    let mut inner_writer = log_writer.lock().unwrap();
    match *inner_writer {
        Some(_) => (), // Log is already open, do nothing
        None => match fs::File::create("mc_log.txt") {
            Ok(log_file) => { 
                *inner_writer = Some(io::BufWriter::new(log_file));
            },
            Err(_) => (), //panic!(err), // TODO: Panic here for debugging purposes. This error can be ignored, and the engine can run without logging
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum ChessVariant {
    Standard,
    Crazyhouse,
}

// Options set in the UCI engine
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct EngineOptions {
    pub variant: ChessVariant,
    pub threads: u32,
    pub hash_memory: u32, // In megabytes
    pub multipv: u32,
}

impl EngineOptions {
    fn new() -> EngineOptions {
        EngineOptions { variant: ChessVariant::Standard, threads: 2, hash_memory: 64, multipv: 4 }
    }
}

// Parse "setoption" strings from the GUI. Some options (# of cores etc) can only
// be changed during engine initialization, these should be set here
fn parse_setoption_init (input: &str, options: &mut EngineOptions, log_writer : &SharableWriter)
                             -> Result<(), String> {

    let (option_name, value) = try!(parse_setoption_data(input));

    match &option_name.to_lowercase()[..] {
        "Variant" => match &value.to_lowercase()[..] {
            "standard" => options.variant = ChessVariant::Standard,
            "crazyhouse" => options.variant = ChessVariant::Crazyhouse,
            _ => return Err(format!("Error: Unknown chess variant \"{}\"", value)),
        },
        "Threads" => {
            let threads = try!(u32::from_str(&value).map_err(|e|format!("{}", e)));
            options.threads = threads;
        }

        "Hash" => {
            let hash = try!(u32::from_str(&value).map_err(|e|format!("{}", e)));
            options.hash_memory = hash;
        }
        // If no init-only option matches, try the other ones
        s => try!(parse_setoption(s, log_writer)),
    }
    Ok(())
}

// Parse "setoption" strings from the GUI. Most options (# of cores etc) can only
// be changed during engine initialization.
// A few, like debug logs, can be toggled while the engine is running
fn parse_setoption (input : &str, log_writer : &SharableWriter) -> Result<(), String> {
    
    let (option_name, value) = try!(parse_setoption_data(input));

    match &option_name.to_lowercase()[..] {
        "write debug log" => {
            match &value.to_lowercase()[..] {
                "true" => open_log_file(log_writer),
                "false" => {
                    let mut inner_writer = log_writer.lock().unwrap();
                    *inner_writer = None;
                },
                _ => to_log(&format!("Unrecognized value {} in option {}, ignoring...", 
                                     value, option_name), log_writer),
            }
        },
        _ => to_log(&format!("Unrecognized option {} with value {}, ignoring...", 
                             option_name, value), log_writer),
    }
    Ok(())
}

///Helper method for parsing a setoption string into the option's name and value
fn parse_setoption_data(input : &str) -> Result<(String, String), String> {
    if !(input.contains("name") && input.contains("value")) {
        return Err("Setoption string did not include name and value".to_string());
    }
    let mut input_iter = input.split_whitespace();
    if input_iter.next() != Some("setoption") || input_iter.next() != Some("name") {
        return Err("Setoption string did not start with \"setoption name\"".to_string())
    }
    let option_name : String = input.split_whitespace()
        .skip(2)
        .take_while(|token| token != &"value")
        .collect();
    
    let value : String = input.split_whitespace().skip_while(|token| token != &"value").collect();
    
    Ok((option_name, value))    
}

pub fn parse_go (input : &str, log_writer : &SharableWriter)
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
        Some("depth") => return Ok(TimeRestriction::Depth(
            try!(parse_int(input.split_whitespace().nth(2))
            ) as u8)),
        Some("mate") => return Ok(TimeRestriction::Mate(
            try!(parse_int(input.split_whitespace().nth(2))
            ) as u16)),
        Some("nodes") => return Ok(TimeRestriction::Nodes(
            try!(parse_int(input.split_whitespace().nth(2))
            ) as u64)),
        None => return Ok(TimeRestriction::Infinite),
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
        to_log("Did not receive white increment times, assuming 0", log_writer);
        white_inc = Some(0);
    }
    if black_inc == None {
        to_log("Did not receive black increment times, assuming 0", log_writer);
        black_inc = Some(0);
    }
    let time_info = TimeInfo {white_time: white_time.unwrap(), black_time: black_time.unwrap(),
                              white_inc: white_inc.unwrap(), black_inc: black_inc.unwrap(),
                              moves_to_go: moves_to_go.map(|v| v as u16 )};
    Ok(TimeRestriction::GameTime(time_info))
}

/// Sends the engine's evaluation to the GUI via uci, along with other data
/// like node count, time taken, etc
pub fn send_eval_to_gui<M> (log_writer : &SharableWriter, depth : u8,
                         ms_taken : i64, 
                              score : Score, moves : Vec<M>, node_count : ::NodeCount)
    where M: game_move::Move
{
    let eng_score = score.to_string();
    
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
    uci_send(&inf_str, log_writer);
}

/// Simple helper method to write a line to the log
pub fn to_log (message : &str, log_writer : &SharableWriter) {

    let mut inner_writer = log_writer.lock().unwrap();

    match *inner_writer {
        Some(ref mut writer) => {
            
            let time = format!("[{}:{}]", time::get_time().sec % 3600, 
                               time::get_time().nsec / 1000000);
            writer.write(&format!("{} {}\n", time, message)
                             .bytes().collect::<Vec<u8>>()).unwrap();
            writer.flush().unwrap();
        },
        None => (),
    }
}

/// Prints the input to stdout (Where it can be read by the GUI), and also writes it
/// to the log file, if it is present
pub fn uci_send (message : &str, log_writer : &SharableWriter) {

    let log_message = format!("Engine: {}", message);
    to_log(&log_message, log_writer);
    println!("{}", message);
}

/// Waits for input from the GUI, and writes the input to the log and returns it
fn get_engine_input(log_writer : &SharableWriter, stdin : &mut io::BufRead) -> String {

    let mut input = "".to_string();
    stdin.read_line(&mut input).unwrap();
    input = input.trim().to_string();
    to_log(&format!("GUI: {}", input), log_writer);

    input
}

/// Turns the whole position string from the GUI (Like "position startpos moves e2e4")
/// into an internal board representation
fn parse_position<Board> (input : &String, log_writer : &SharableWriter) -> Result<Board, String>
    where Board: 'static + board::EvalBoard + UciBoard{
    
    let words : Vec<&str> = input.split_whitespace().collect();
    if words.len() < 2 || words[0] != "position" {
        return Err(format!("Illegal position string: had length {}", words.len()));
    }
    
    // moves_pos is the position on the input string where the token "moves" is expected
    let (mut board, moves_pos) =
        if words[1] == "startpos" {
            (Board::start_board().clone(), 2)
        }
    else if words[1] == "fen" {
        let mut fen_string : String = "".to_string();
        for token in words.iter().skip(2).take_while(|&s|*s != "moves") {
            fen_string.push_str(token);
            fen_string.push(' ');
        }
        fen_string = fen_string.trim().to_string();
        match Board::from_fen(&fen_string) {
            Ok(b) => (b, 8),
            Err(err) => return Err(err),
        }
    }
    else {
        return Err(format!("Illegally formatted position string: \"{}\": 2nd token is {} and string has {} tokens",
                           input, words[1], words.len()))
    };
    use search_algorithms::game_move::Move;
    if words.len() > moves_pos  {
        if words[moves_pos] == "moves" {
            for c_move_str in words.iter().skip(moves_pos + 1) {
                let c_move = match Board::Move::from_alg(c_move_str) {
                    Ok(m) => m,
                    Err(err) => {
                        to_log(&err, log_writer);
                        return Err(format!("{}", err));
                    },
                };
                board.do_move(c_move);
            }
        }
        
        else {
            return Err(format!("Illegally formatted position string: 
Expected words.len() to be {} if no moves are included, was {}", moves_pos, words.len()))
        }
    }
    Ok(board)
}

/// Trait representing an algorithm returning uci-compatible output
pub struct UciInfo {
    pub depth: u16,
    pub seldepth: u16,
    pub time: i64,
    pub nodes: u64,
    pub hashfull: f64,
    pub pvs: Vec<(Score, String)>,
}
    
impl UciInfo {
    pub fn to_info_string(&self) -> String {
        use fmt::Write;
        let mut string = String::new();
        if self.pvs.len() == 1 {
            write!(string, "info depth {} seldepth {} score {} nodes {} time {} nps {} pv {}\n",
                   self.depth, self.seldepth, self.pvs[0].0, self.nodes,
                   self.time, (1000 * self.nodes) as i64 / self.time, self.pvs[0].1).unwrap();
        }
        else {
            for (n, &(ref score, ref moves)) in self.pvs.iter().enumerate() {
                write!(string, "info depth {} seldepth {} multipv {} score {} nodes {} time {} nps {} pv {}\n",
                       self.depth, self.seldepth, n, score, self.nodes,
                       self.time, (1000 * self.nodes) as i64 / self.time, moves).unwrap();
            }
        }
        string
    }
}

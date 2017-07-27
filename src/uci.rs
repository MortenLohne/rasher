use board::std_board::ChessBoard;
use board::sjadam_board::SjadamBoard;
use board::crazyhouse_board::CrazyhouseBoard;
use board::std_board::TimeInfo;

use search_algorithms::board;
use search_algorithms::game_move::Move;
use search_algorithms::alpha_beta;
use search_algorithms::mcts;
use search_algorithms::alpha_beta::Score;

extern crate time;

use std::fmt;
use std::thread;
use std::sync::{Mutex, Arc};
use std::io;
use std::str::FromStr;

pub trait UciBoard : Sized {
    fn from_fen(&str) -> Result<Self, String>;
    fn to_fen(&self) -> String;
}

pub trait UciMove : Sized {
    fn from_alg(&str) -> Result<Self, String>;
    fn to_alg(&self) -> String;
}

/// Connects the engine to a GUI using UCI. 
/// Assumes "uci has already been sent"
pub fn connect_engine(stdin : &mut io::BufRead) -> Result<(), String> {

    // Do the standard handshake with the GUI
    info!("Received uci command from GUI");
    uci_send("id name rasher");

    //uci_send("option name Write Debug Log type check default true");
    uci_send("option name Hash type spin default 128 min 0 max 32768");
    uci_send("option name Threads type spin default 1 min 1 max 16");
    uci_send("option name MultiPV type spin default 1 min 1 max 64");
    uci_send("option name UCI_Variant type combo default Chess var Chess var Sjadam");
    
    uci_send("uciok");
    
    let mut board_string : String = "position startpos".to_string();
    let mut engine_string = "minimax".to_string();
    let engine_comm = Arc::new(Mutex::new(EngineComm::new()));
    let mut engine_options = EngineOptions::new();
    
    // Listen to commands from GUI forever
    loop {
        let input = get_engine_input(stdin)?;
        let tokens = input.split_whitespace().collect::<Vec<_>>();
        if tokens.is_empty() {
            warn!("Unrecognized input \"{}\"", input);
            continue;
        }
        match tokens[0] { // Parse the first word of the input
            "isready" => uci_send("readyok"),
            "quit" => { info!("Quitting..."); return Ok(()); },
            "ucinewgame" => (), // Ignore this for now
            "position" => board_string = input.to_string(),
            "setoption" => parse_setoption(&input, &mut engine_options)?,
            "engine" =>
                if tokens.len() >= 2 {
                    engine_string = tokens[1].to_string(); }
                else { },
            "stop" => {
                let best_move : String;
                {
                    let mut engine_comm = try!(engine_comm.lock().map_err(|err| err.to_string()));
                    engine_comm.engine_should_stop = true;
                    best_move = match engine_comm.best_move.clone() {
                        Some(mv) => mv,
                        None => {
                            warn!("Haven't found a move yet: sending a bogus move");
                            "e2e4".to_string() // Send a bogus move
                        },
                    };
                }
                uci_send(&format!("bestmove {}", best_move));
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
                        warn!("Engine was already running, cannot start new. Trying again later");
                        thread::sleep(::std::time::Duration::from_millis(100));
                        
                    }
                    
                }
                let (time_restriction, searchmoves_input) = parse_go(&input)?;
                
                let (_, rx) = match engine_options.variant {
                    ChessVariant::Standard => {
                        match engine_string.as_str() {
                            "minimax" => alpha_beta::start_uci_search(
                                parse_position::<ChessBoard>(&board_string)?,
                                time_restriction,
                                engine_options.clone(),
                                engine_comm.clone(), searchmoves_input
                                    .clone()
                                    .map(|moves|
                                         moves.iter()
                                         .map(|move_string| <<ChessBoard as board::EvalBoard>::Move>::from_alg(move_string))
                                         .map(Result::unwrap)
                                         .collect::<Vec<_>>()
                                         )),
                            "mcts" => start_mcts_engine(
                                parse_position::<ChessBoard>(&board_string)?,
                                time_restriction, engine_options.clone(),
                                engine_comm.clone()),
                            _ => panic!("Unknown engine {}", engine_string),
                        }
                    },
                    ChessVariant::Crazyhouse => {
                        match engine_string.as_str() {
                            "minimax" => alpha_beta::start_uci_search(
                                parse_position::<CrazyhouseBoard>(&board_string)?,
                                time_restriction,
                                engine_options.clone(),
                                engine_comm.clone(), searchmoves_input
                                    .clone()
                                    .map(|moves|
                                         moves.iter()
                                         .map(|move_string| <<CrazyhouseBoard as board::EvalBoard>::Move>::from_alg(move_string))
                                         .map(Result::unwrap)
                                         .collect::<Vec<_>>()
                                         )),
                            "mcts" => start_mcts_engine(
                                parse_position::<CrazyhouseBoard>(&board_string)?,
                                time_restriction, engine_options.clone(),
                                engine_comm.clone()),
                            _ => panic!("Unknown engine {}", engine_string),
                        }
                    },
                    ChessVariant::Sjadam => {
                        match engine_string.as_str() {
                            "minimax" => alpha_beta::start_uci_search(
                                parse_position::<SjadamBoard>(&board_string)?,
                                time_restriction,
                                engine_options.clone(),
                                engine_comm.clone(), searchmoves_input
                                    .clone()
                                    .map(|moves|
                                         moves.iter()
                                         .map(|move_string| <<SjadamBoard as board::EvalBoard>::Move>::from_alg(move_string))
                                         .map(Result::unwrap)
                                         .collect::<Vec<_>>()
                                         )),
                            "mcts" => start_mcts_engine(
                                parse_position::<SjadamBoard>(&board_string)?,
                                time_restriction, engine_options.clone(),
                                engine_comm.clone()),
                            _ => panic!("Unknown engine {}", engine_string),
                        }
                    }
                };
                thread::spawn(move || {
                    // Last info that has been received, if any
                    let mut last_info = None;
                    loop {
                        match rx.recv() {
                            Ok(uci_info) => {
                                uci_send(&uci_info.to_info_string());
                                last_info = Some(uci_info);
                            },
                            Err(_) => {
                                // If the channel has hung up,
                                // send final data as well as bestmove command
                                match last_info {
                                    None => uci_send("bestmove none"),
                                    Some(uci_info) => {
                                        uci_send(&uci_info.to_info_string());
                                        assert!(!uci_info.pvs.is_empty());
                                        let (_, ref moves_string) = uci_info.pvs[0];
                                        println!("bestmove {}",
                                                 moves_string
                                                 .split_whitespace()
                                                 .next().unwrap_or("none"));
                                    },
                                };
                                return;
                            },
                        }
                    }
                });
            }
            _ => { // TODO: If receiving unrecognied token, parse the next one as usual
                warn!("Unrecognized input \"{}\". Ignoring.", input);
            },
        }
    }
}

/// Given a receiving channel for uci info and an engine thread, wait until the transmitter closes the handle and the thread exits
/// Then return the best score and move
/// For the function to return ok, the engine must have terminated successfully
/// If the engine crashes, the error value will contain the last move the engine
/// sent, if any.
pub fn get_uci_move (handle: thread::JoinHandle<()>, rx: mpsc::Receiver<UciInfo>)
                             -> Result<(Score, String), (Option<(Score, String)>, String)> {
    let mut last_info = None;
    loop { // The channel will return error when closed
        match rx.recv() {
            Ok(uci_info) => last_info = Some(uci_info),
            Err(_) => {
                if let Some(uci_info) = last_info {
                    if uci_info.pvs.is_empty() {
                        return Err((None, "Engine returned 0 moves".to_string()));
                    }
                    let (score, ref moves_string) = uci_info.pvs[0];
                    let pv_string = moves_string
                        .split_whitespace()
                        .next().unwrap().to_string();
                    if let Err(err) = handle.join() {
                        return Err((Some((score, pv_string)), format!("{:?}", err)))
                    }
                    return Ok((score, pv_string))
                }
                else {
                    return Err((None, "Engine returned no output".to_string()));
                }
            },
        }
    }
}

#[allow(dead_code)]
pub fn get_uci_multipv (handle: thread::JoinHandle<()>, rx: mpsc::Receiver<UciInfo>,
                                multipv: u32)
                             -> Result<Vec<(Score, String)>, (Option<Vec<(Score, String)>>, String)> {
    let mut last_info = None;
    loop { // The channel will return error when closed
        match rx.recv() {
            Ok(uci_info) => last_info = Some(uci_info),
            Err(_) => {
                if let Some(uci_info) = last_info {
                    if uci_info.pvs.is_empty() {
                        return Err((None, "Engine returned 0 moves".to_string()));
                    }
                    let results = uci_info.pvs.iter()
                        .map(|&(score, ref moves_string)| {
                            let pv_string = moves_string
                                .split_whitespace()
                                .next().unwrap().to_string();
                            (score, pv_string)
                        })
                        .collect::<Vec<_>>();
                    if results.len() as u32 != multipv {
                        return Err((Some(results.clone()),
                                    format!("Engine returned {} moves, expected {}",
                                            results.len() as u32, multipv)))
                    }
                    if let Err(err) = handle.join() {
                        return Err((Some(results), format!("{:?}", err)))
                    }
                    return Ok(results)
                }
                else {
                    return Err((None, "Engine returned no output".to_string()));
                }
            },
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum TimeRestriction {
    GameTime(TimeInfo),
    Depth(u16),
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
                        options: EngineOptions, engine_comm: Arc<Mutex<EngineComm>>)
                        -> (thread::JoinHandle<()>, mpsc::Receiver<UciInfo>)
    where B: 'static + board::EvalBoard + fmt::Debug + Send, <B as board::EvalBoard>::Move: Sync
{
    mcts::start_uci_search(board, time_limit, options, engine_comm)
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum ChessVariant {
    Standard,
    Crazyhouse,
    Sjadam,
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
    pub fn new() -> EngineOptions {
        EngineOptions { variant: ChessVariant::Standard, threads: 1, hash_memory: 128, multipv: 1 }
    }
}

// Parse "setoption" strings from the GUI. 
fn parse_setoption (input: &str, options: &mut EngineOptions) -> Result<(), String> {

    let (option_name, value) = try!(parse_setoption_data(input));

    match &option_name.to_lowercase()[..] {
        "variant" | "uci_variant" => match &value.to_lowercase()[..] {
            "standard" => options.variant = ChessVariant::Standard,
            "crazyhouse" => options.variant = ChessVariant::Crazyhouse,
            "sjadam" => options.variant = ChessVariant::Sjadam,
            _ => return Err(format!("Error: Unknown chess variant \"{}\"", value)),
        },
        "uci_standard" => options.variant = ChessVariant::Standard,
        "uci_crazyhouse" => options.variant = ChessVariant::Crazyhouse,
        "uci_sjadam" => options.variant = ChessVariant::Sjadam,
        "threads" => {
            let threads = try!(u32::from_str(&value).map_err(|e|format!("{}", e)));
            options.threads = threads;
        }

        "hash" => {
            let hash = try!(u32::from_str(&value).map_err(|e|format!("{}", e)));
            options.hash_memory = hash;
        }
        "multipv" => {
            let multipv = u32::from_str(&value).map_err(|e|format!("{:?}", e))?;
            options.multipv = multipv;
        }
        "write debug log" => {
            match &value.to_lowercase()[..] {
                "true" => (),
                "false" => (),
                _ => warn!("Unrecognized value {} in option {}, ignoring...", 
                                     value, option_name),
            }
        },
        _ => warn!("Unrecognized option {} with value {}, ignoring...", 
                             option_name, value),
    }
    Ok(())
}

///Helper method for parsing a setoption string into the option's name and value
fn parse_setoption_data(input : &str) -> Result<(String, String), String> {
    if !(input.contains("name") && input.contains("value")) {
        panic!(format!("setoption string \"{}\" did not include name and value", input));
    }
    let mut input_iter = input.split_whitespace();
    if input_iter.next() != Some("setoption") || input_iter.next() != Some("name") {
        return Err("Setoption string did not start with \"setoption name\"".to_string())
    }
    let option_name : String = input_iter
        .by_ref()
        .take_while(|token| token != &"value")
        .collect(); // TODO: Will not correctly concatinate multi-word options
    
    let value : String = input_iter.collect();
    
    Ok((option_name, value.trim().to_string()))    
}

/// Parses a go command, returning a time restriction and the moves to search
/// If moves to search is none, search all moves
pub fn parse_go (input : &str)
                 -> Result<(TimeRestriction, Option<Vec<String>>), String> {

    // Parses an optional string to return a u32
    fn parse_int (next_token : Option<&str>) -> Result<u32, String> {
        
        match next_token {
            Some(token) => u32::from_str_radix(token, 10).map_err(|_| String::from("Error: \"movetime\" token formatted incorrectly")),
            None => Err(String::from("Error: Expected int after \"movetime\" token, but it was the last token in the command")),
        }
    }        

    match input.split_whitespace().nth(1) {
        Some("infinite") => return Ok((TimeRestriction::Infinite, None)),
        Some("movetime") => return Ok((TimeRestriction::MoveTime(
            try!(parse_int(input.split_whitespace().nth(2))
                 ) as i64), None)),
        Some("depth") => return Ok((TimeRestriction::Depth(
            try!(parse_int(input.split_whitespace().nth(2))
            ) as u16), None)),
        Some("mate") => return Ok((TimeRestriction::Mate(
            try!(parse_int(input.split_whitespace().nth(2))
            ) as u16), None)),
        Some("nodes") => return Ok((TimeRestriction::Nodes(
            try!(parse_int(input.split_whitespace().nth(2))
            ) as u64), None)),
        Some("searchmoves") => return Ok((TimeRestriction::Infinite, Some(input.split_whitespace().skip(2).map(|s|s.to_string()).collect::<Vec<_>>()))),
        None => return Ok((TimeRestriction::Infinite, None)),
        Some(_) => (),
    }
    
    let mut tokens_it = input.split_whitespace().skip(1);
    
    let (mut white_time, mut black_time, mut white_inc, mut black_inc, mut moves_to_go) =
        (None, None, None, None, None);
    
    loop {
        match tokens_it.next() {
            Some("moves_to_go") => moves_to_go = Some(try!(parse_int(tokens_it.next()))),
            Some("movestogo") => moves_to_go = Some(try!(parse_int(tokens_it.next()))),
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
        warn!("Did not receive white increment times, assuming 0");
        white_inc = Some(0);
    }
    if black_inc == None {
        warn!("Did not receive black increment times, assuming 0");
        black_inc = Some(0);
    }
    let time_info = TimeInfo {white_time: white_time.unwrap(), black_time: black_time.unwrap(),
                              white_inc: white_inc.unwrap(), black_inc: black_inc.unwrap(),
                              moves_to_go: moves_to_go.map(|v| v as u16 )};
    Ok((TimeRestriction::GameTime(time_info), None))
}

/// Prints the input to stdout (Where it can be read by the GUI), and also writes it
/// to the log file, if it is present
pub fn uci_send (message : &str) {
    info!("Engine: {}", message);
    println!("{}", message);
}

/// Waits for input from the GUI, and writes the input to the log and returns it
/// Returns error if stdin is closed, or on any io error
pub fn get_engine_input(stdin : &mut io::BufRead) -> Result<String, String> {

    let mut input = "".to_string();
    match stdin.read_line(&mut input) {
        Ok(0) => Err("Got empty input from stdin, presumably the handle is closed".to_string()),
        Ok(_) => {
            input = input.trim().to_string();
            info!("GUI: {}", input);
            Ok(input)
        },
        Err(err) => Err(err.to_string()),
    }
    
}

/// Turns the whole position string from the GUI (Like "position startpos moves e2e4")
/// into an internal board representation
fn parse_position<Board> (input : &String) -> Result<Board, String>
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
                        return Err(err.to_string());
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
#[derive(Debug, PartialEq, Clone)]
pub struct UciInfo {
    pub depth: u16,
    pub seldepth: u16,
    pub time: i64,
    pub nodes: u64,
    pub hashfull: f64,
    pub pvs: Vec<(Score, String)>, // One or more principal variations, sorted from best to worst
}
    
impl UciInfo {
    pub fn to_info_string(&self) -> String {
        use fmt::Write;
        let mut string = String::new();
        if self.pvs.len() == 1 {
            write!(string, "info depth {} seldepth {} score {} nodes {} hashfull {} time {} nps {} pv {}\n",
                   self.depth, self.seldepth, self.pvs[0].0, self.nodes,
                   (self.hashfull * 1000.0) as i64,
                   self.time, (1000 * self.nodes) as i64 / (self.time + 1), self.pvs[0].1).unwrap();
        } // Add one to self.time to avoid division by zero
        else {
            for (n, &(ref score, ref moves)) in self.pvs.iter().enumerate() {
                write!(string, "info depth {} seldepth {} multipv {} score {} nodes {} hashfull {} time {} nps {} pv {}\n",
                       self.depth, self.seldepth, n + 1, score, self.nodes,
                       (self.hashfull * 1000.0) as i64,
                       self.time, (1000 * self.nodes) as i64 / (self.time + 1), moves).unwrap();
            }
        }
        string
    }
}

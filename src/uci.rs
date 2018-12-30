use board::std_board::ChessBoard;
use board::sjadam_board::SjadamBoard;
use board::crazyhouse_board::CrazyhouseBoard;
use tests::tools;
use search_algorithms::board;
use search_algorithms::alpha_beta;
use search_algorithms::mcts;
use search_algorithms::alpha_beta::Score;
use search_algorithms::board::EvalBoard;

use std::error;
use std::fmt;
use std::thread;
use std::sync::{Mutex, Arc};
use std::io;
use std::str::FromStr;
use std::time;
use std::hash::Hash;
use search_algorithms::board::ExtendedBoard;

/// Connects the engine to a GUI using UCI. 
/// Assumes "uci has already been sent"
pub fn connect_engine(stdin : &mut io::BufRead) -> Result<(), Box<error::Error>> {

    // Do the standard handshake with the GUI
    info!("Received uci command from GUI");
    uci_send("id name rasher");

    //uci_send("option name Write Debug Log type check default true");
    uci_send("option name Hash type spin default 256 min 0 max 32768");
    uci_send("option name Threads type spin default 1 min 1 max 128");
    uci_send("option name MultiPV type spin default 1 min 1 max 128");
    uci_send("option name DebugInfo type check default false");
    uci_send("option name UCI_Variant type combo default Chess var Chess var Sjadam var Crazyhouse");
    
    uci_send("uciok");
    
    let mut board_string : String = "position startpos".to_string();
    let mut engine_string = "minimax".to_string();
    let engine_comm = Arc::new(Mutex::new(EngineComm::new()));
    let mut engine_options = EngineOptions::new();
    let mut search_thread : Option<thread::JoinHandle<()>> = None;
    
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
            "perft" => {
                if let Some(depth) = tokens.get(1)
                    .and_then(|depth| depth.parse::<u16>().ok()) {
                    let result = match engine_options.variant {
                        ChessVariant::Standard => {
                            let mut board = parse_position::<ChessBoard>(&board_string)?;
                            tools::perft(&mut board, depth)
                        }
                        ChessVariant::Sjadam => {
                            let mut board = parse_position::<SjadamBoard>(&board_string)?;
                            tools::perft(&mut board, depth)
                        }
                        ChessVariant::Crazyhouse => {
                            let mut board = parse_position::<CrazyhouseBoard>(&board_string)?;
                            tools::perft(&mut board, depth)
                        }
                    };
                    println!("{:?}", result);
                }
            },
            "eval" => {
                match engine_options.variant {
                    ChessVariant::Standard => {
                        let board = parse_position::<ChessBoard>(&board_string)?;
                        uci_send(&format!("Eval: {}", board.static_eval()));
                    }
                    ChessVariant::Sjadam => {
                        let board = parse_position::<SjadamBoard>(&board_string)?;
                        uci_send(&format!("Eval: {}", board.static_eval()));
                    }
                    ChessVariant::Crazyhouse => {
                        let board = parse_position::<CrazyhouseBoard>(&board_string)?;
                        uci_send(&format!("Eval: {}", board.static_eval()));
                    }
                };
            }
            "fen" => {
                match engine_options.variant {
                    ChessVariant::Standard => {
                        let board = parse_position::<ChessBoard>(&board_string)?;
                        uci_send(&format!("Fen: {}", board.to_fen()));
                    }
                    ChessVariant::Sjadam => {
                        let board = parse_position::<SjadamBoard>(&board_string)?;
                        uci_send(&format!("Fen: {}", board.to_fen()));
                    }
                    ChessVariant::Crazyhouse => {
                        let board = parse_position::<CrazyhouseBoard>(&board_string)?;
                        uci_send(&format!("Fen: {}", board.to_fen()));
                    }
                };
            }
            "engine" =>
                if tokens.len() >= 2 {
                    engine_string = tokens[1].to_string();
                },
            "stop" => {
                {
                    let mut engine_comm = engine_comm.lock().map_err(|err| err.to_string())?;
                    engine_comm.engine_should_stop = true;
                }
                // Block until engine has shut down
                if let Some(handle) = search_thread.take() {
                    if let Err(err) = handle.join() {
                        error!("Search thread crashed.\n{:?}", err);
                    };
                };
            },
            "go" => {
                // If engine is running, block until engine has shut down
                {
                    let mut engine_comm = engine_comm.lock().map_err(|err| err.to_string())?;
                    engine_comm.engine_should_stop = true;
                }
                
                if let Some(handle) = search_thread.take() {
                    if let Err(err) = handle.join() {
                        error!("Search thread crashed.\n{:?}", err);
                    };
                };
                {
                    let mut engine_comm = engine_comm.lock().map_err(|err| err.to_string())?;
                    engine_comm.engine_should_stop = false;
                }
                let (time_restriction, searchmoves_input) = parse_go(&input)?;


                let handle =
                match engine_options.variant {
                    ChessVariant::Standard =>
                        start_correct_engine::<ChessBoard>(&mut board_string, &mut engine_string,
                                                           engine_comm.clone(), engine_options,
                                                           time_restriction, searchmoves_input)?,
                    ChessVariant::Sjadam =>
                        start_correct_engine::<SjadamBoard>(&mut board_string, &mut engine_string,
                                                            engine_comm.clone(), engine_options,
                                                            time_restriction, searchmoves_input)?,
                    ChessVariant::Crazyhouse =>
                        start_correct_engine::<CrazyhouseBoard>(&mut board_string, &mut engine_string,
                                                                engine_comm.clone(), engine_options,
                                                                time_restriction, searchmoves_input)?,
            };
                search_thread = Some(handle);
            }
            "eval_game" => {
                match engine_options.variant {
                    ChessVariant::Standard => {
                        let board = parse_position::<ChessBoard>(&board_string)?;
                        eval_game(board, &tokens[1..]);
                    }
                    ChessVariant::Sjadam => {
                        let board = parse_position::<SjadamBoard>(&board_string)?;
                        eval_game(board, &tokens[1..]);
                    }
                    ChessVariant::Crazyhouse => {
                        let board = parse_position::<CrazyhouseBoard>(&board_string)?;
                        eval_game(board, &tokens[1..]);
                    }
                };
            }
            "sjadam" => engine_options.variant = ChessVariant::Sjadam,
            "crazyhouse" => engine_options.variant = ChessVariant::Crazyhouse,
            "hash1G" => engine_options.hash_memory = 1024,
            "multipv5" => engine_options.multipv = 5,
            _ => { // TODO: If receiving unrecognied token, parse the next one as usual
                warn!("Unrecognized input \"{}\". Ignoring.", input);
            },
        }
    }
}

fn start_correct_engine<B>(board_string: &mut String, engine_string: &mut String,
                           engine_comm: Arc<Mutex<EngineComm>>, engine_options: EngineOptions,
                           time_restriction: TimeRestriction,
                           searchmoves_input: Option<Vec<String>>) -> Result<thread::JoinHandle<()>, Box<error::Error>>
where B: ExtendedBoard + PgnBoard + fmt::Debug + Send + Sync + Hash + Eq + 'static, <B as Board>::Move: Send + Sync {
    let mut board = parse_position::<B>(&board_string)?;
    let (handle, rx) =
        match engine_string.as_str() {
            "minimax" => alpha_beta::start_uci_search(
                board.clone(),
                time_restriction,
                engine_options,
                engine_comm.clone(), searchmoves_input
                    .clone()
                    .map(|moves|
                        moves.iter()
                            .map(|move_string| board.move_from_lan(move_string))
                            .map(Result::unwrap)
                            .collect::<Vec<_>>()
                    )),
            "mcts" => start_mcts_engine(
                board.clone(),
                time_restriction, engine_options,
                engine_comm.clone()),
            _ => panic!("Unknown engine {}", engine_string),
        };
    thread::spawn(move || {
        // Last info that has been received, if any
        let mut last_info = None;
        loop {
            match rx.recv() {
                Ok(uci_info) => {
                    uci_send(&uci_info.to_info_string(&mut board));
                    last_info = Some(uci_info);
                },
                Err(_) => {
                    // If the channel has hung up, send bestmove command
                    match last_info {
                        None => uci_send("bestmove null"),
                        Some(ref uci_info) if uci_info.pvs.is_empty() || uci_info.pvs[0].1.is_empty() => {
                            uci_send(&format!("UCI info {:?} was sent with no PVs",
                                              uci_info));
                            uci_send("bestmove null");
                        },
                        Some(uci_info) => {
                            let mv = uci_info.pvs[0].1[0].clone();
                            uci_send(
                                &format!("bestmove {}", board.move_to_lan(&mv)));
                        },
                    };
                    return;
                },
            }
        }
    });
    Ok(handle)
}

/// Given a receiving channel for uci info and an engine thread, wait until the transmitter closes the handle and the thread exits
/// Then return the best score and move
/// For the function to return ok, the engine must have terminated successfully
/// If the engine crashes, the error value will contain the last move the engine
/// sent, if any.
pub fn get_uci_move<B: Board> (handle: thread::JoinHandle<()>, rx: mpsc::Receiver<UciInfo<B>>)
                             -> Result<(Score, B::Move), (Option<(Score, B::Move)>, String)> {
    let mut last_info = None;
    loop { // The channel will return error when closed
        match rx.recv() {
            Ok(uci_info) => last_info = Some(uci_info),
            Err(_) => {
                if let Some(uci_info) = last_info {
                    if uci_info.pvs.is_empty() {
                        return Err((None, "Engine returned 0 moves".to_string()));
                    }
                    let (score, ref moves) = uci_info.pvs[0];

                    if let Err(err) = handle.join() {
                        return Err((Some((score, moves[0].clone())), format!("{:?}", err)))
                    }
                    return Ok((score, moves[0].clone()))
                }
                else {
                    return Err((None, "Engine returned no output".to_string()));
                }
            },
        }
    }
}

#[allow(dead_code)]
pub fn get_uci_multipv<B: Board> (handle: thread::JoinHandle<()>, rx: mpsc::Receiver<UciInfo<B>>)
                             -> Result<Vec<(Score, B::Move)>, (Option<Vec<(Score, B::Move)>>, String)> {
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
                        .map(|&(score, ref moves)| {
                            (score, moves[0].clone())
                        })
                        .collect::<Vec<_>>();
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

// Stores time information for the game, in milliseconds
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct TimeInfo {
    pub white_time : time::Duration,
    pub black_time : time::Duration,
    pub white_inc : time::Duration,
    pub black_inc : time::Duration,
    pub moves_to_go : Option<u16>, // Number of moves to the next time control
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum TimeRestriction {
    GameTime(TimeInfo),
    Depth(u16),
    Nodes(u64),
    Mate(u16),
    MoveTime(time::Duration),
    Infinite,
}

/// Struct for communicating with the engine from another thread
pub struct EngineComm {
    pub engine_should_stop : bool,
}

impl EngineComm {
    pub fn new() -> Self {
        EngineComm{ engine_should_stop: false }
    }
}

use std::sync::mpsc;
use pgn::PgnBoard;
use search_algorithms::board::Board;

fn start_mcts_engine<B>(board: B, time_limit: TimeRestriction,
                        options: EngineOptions, engine_comm: Arc<Mutex<EngineComm>>)
                        -> (thread::JoinHandle<()>, mpsc::Receiver<UciInfo<B>>)
    where B: 'static + PgnBoard + fmt::Debug + Send + Clone + PartialEq,
<B as board::Board>::Move: Sync + Send
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
    pub null_move_pruning: bool,
    pub debug_info: bool,
}

impl EngineOptions {
    pub fn new() -> EngineOptions {
        EngineOptions { variant: ChessVariant::Standard, threads: 1,
            hash_memory: 256, multipv: 1, null_move_pruning: true, debug_info: false}
    }
}

// Parse "setoption" strings from the GUI. 
fn parse_setoption (input: &str, options: &mut EngineOptions) -> Result<(), String> {

    let (option_name, value) = try!(parse_setoption_data(input));

    match &option_name.to_lowercase()[..] {
        "variant" | "uci_variant" => match value.to_lowercase().as_str() {
            "standard" => options.variant = ChessVariant::Standard,
            "crazyhouse" => options.variant = ChessVariant::Crazyhouse,
            "sjadam" => options.variant = ChessVariant::Sjadam,
            _ => return Err(format!("Error: Unknown chess variant \"{}\"", value)),
        },

        "debuginfo" => match value.to_lowercase().as_str() {
            "true" => options.debug_info = true,
            "false" => options.debug_info = false,
            _ => return Err(format!("Invalid DebugInfo value {}", value)),
        }

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
                "true" | "false" => (),
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
    fn parse_int (next_token : Option<&str>) -> Result<u64, String> {
        
        match next_token {
            Some(token) => u64::from_str_radix(token, 10).map_err(|_| String::from("Error: \"movetime\" token formatted incorrectly")),
            None => Err(String::from("Error: Expected int after \"movetime\" token, but it was the last token in the command")),
        }
    }

    fn parse_dur(next_token: Option<&str>) -> Result<time::Duration, String> {
        parse_int(next_token).map(time::Duration::from_millis)
    }

    match input.split_whitespace().nth(1) {
        Some("movetime") => return Ok((TimeRestriction::MoveTime(
            try!(parse_dur(input.split_whitespace().nth(2))
                 )), None)),
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
        Some("infinite") | None => return Ok((TimeRestriction::Infinite, None)),
        Some(_) => (),
    }
    
    let mut tokens_it = input.split_whitespace().skip(1);
    
    let (mut white_time, mut black_time, mut white_inc, mut black_inc, mut moves_to_go) =
        (None, None, None, None, None);
    
    loop {
        match tokens_it.next() {
            Some("moves_to_go") | Some("movestogo")
                => moves_to_go = Some(parse_int(tokens_it.next())?),
            Some("btime") => black_time = Some(parse_dur(tokens_it.next())?),
            Some("wtime") => white_time = Some(parse_dur(tokens_it.next())?),
            Some("winc") => white_inc = Some(parse_dur(tokens_it.next())?),
            Some("binc") => black_inc = Some(parse_dur(tokens_it.next())?),
            Some (s) => return Err(format!("Unknown token \"{}\" in go command", s)),
            None => break,
        }
    }
    if white_time == None || black_time == None {
        return Err(String::from("Did not receive time left for white and black, nor other restrictions on \"go\" command."))
    }
    if white_inc == None {
        warn!("Did not receive white increment times, assuming 0");
        white_inc = Some(time::Duration::new(0, 0));
    }
    if black_inc == None {
        warn!("Did not receive black increment times, assuming 0");
        black_inc = Some(time::Duration::new(0, 0));
    }
    let time_info = TimeInfo {white_time: white_time.unwrap(), black_time: black_time.unwrap(),
                              white_inc: white_inc.unwrap(), black_inc: black_inc.unwrap(),
                              moves_to_go: moves_to_go.map(|v| v as u16 )};
    Ok((TimeRestriction::GameTime(time_info), None))
}

pub fn eval_game<Board: EvalBoard>(mut board: Board, moves: &[&str])
    where Board: 'static + PgnBoard + ExtendedBoard + fmt::Debug + Send + Hash + Eq,
<Board as board::Board>::Move: Send + Sync
{

    let (handle, channel) = alpha_beta::start_uci_search(
        board.clone(), TimeRestriction::MoveTime(time::Duration::from_millis(5_000)),
        EngineOptions::new(), Arc::new(Mutex::new(EngineComm::new())),
        None);


    let (eval, mut last_correct_move) = get_uci_move(handle, channel).unwrap();
    let mut last_eval = eval;
    
    for mv_str in moves {
        let mv = board.move_from_lan(mv_str).unwrap();
        {
            let mut moves = vec![];
            board.generate_moves(&mut moves);
            assert!(moves.contains(&mv));
        }
        board.do_move(mv.clone());
        
        let (handle, channel) = alpha_beta::start_uci_search(
            board.clone(), TimeRestriction::MoveTime(time::Duration::from_millis(5_000)),
            EngineOptions::new(), Arc::new(Mutex::new(EngineComm::new())),
            None);

        let (eval, best_move) = get_uci_move(handle, channel).unwrap();

        let delta_score = eval.to_cp(board.side_to_move()) - last_eval.to_cp(board.side_to_move());
        
        if best_move != mv && delta_score > 0 {
            let verdict = match delta_score {
                0 ... 100 => "Inaccuracy",
                100 ... 300 => "Mistake",
                _ => "Blunder",
            };
            uci_send(&format!(
                "{} ({}): {}, {} was better. ({} vs {}, delta={})",
                mv_str, !board.side_to_move(), verdict,
                board.move_to_lan(&last_correct_move),
                eval.uci_string(board.side_to_move()), last_eval.uci_string(board.side_to_move()),
                delta_score));
        }
        
        else {
            uci_send(&format!("{} ({}): {}",
                              mv_str, board.side_to_move(), eval.uci_string(board.side_to_move())));
        }
        
        last_eval = eval;
        last_correct_move = best_move;
    }
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
fn parse_position<Board> (input : &str) -> Result<Board, Box<error::Error>>
    where Board: 'static + PgnBoard {
    
    let words : Vec<&str> = input.split_whitespace().collect();
    if words.len() < 2 || words[0] != "position" {
        return Err(format!("Illegal position string: had length {}", words.len()).into());
    }
    
    // moves_pos is the position on the input string where the token "moves" is expected
    let (mut board, moves_pos) =
        if words[1] == "startpos" {
            (Board::start_board(), 2)
        }
    else if words[1] == "fen" {
        let mut fen_string : String = "".to_string();
        for token in words.iter().skip(2).take_while(|&s|*s != "moves") {
            fen_string.push_str(token);
            fen_string.push(' ');
        }
        fen_string = fen_string.trim().to_string();
        (Board::from_fen(&fen_string)?, 8)
    }
    else {
        return Err(format!(
            "Illegally formatted position string: \"{}\": 2nd token is {} and string has {} tokens",
            input, words[1], words.len()).into())
    };
    if words.len() > moves_pos  {
        if words[moves_pos] == "moves" {
            for c_move_str in words.iter().skip(moves_pos + 1) {
                let c_move =
                    board.move_from_lan(c_move_str)?;
                board.do_move(c_move);
            }
        }
        
        else {
            return Err(format!("Illegally formatted position string: 
Expected words.len() to be {} if no moves are included, was {}", moves_pos, words.len()).into())
        }
    }
    Ok(board)
}

/// Trait representing an algorithm returning uci-compatible output
#[derive(Debug, PartialEq, Clone)]
pub struct UciInfo<B: Board> {
    pub color: board::Color,
    pub depth: u16,
    pub seldepth: u16,
    pub time: i64,
    pub nodes: u64,
    pub hashfull: f64,
    pub pvs: Vec<(Score, Vec<B::Move>)>, // One or more principal variations, sorted from best to worst
}
    
impl<B: PgnBoard> UciInfo<B> {
    pub fn to_info_string(&self, board: &mut B) -> String {
        use fmt::Write;
        let mut string = String::new();
        for (n, &(ref score, ref moves)) in self.pvs.iter().enumerate() {
            write!(string, "info depth {} seldepth {} {}score {} nodes {} hashfull {} time {} nps {} pv ",
                   self.depth, self.seldepth,
                   if self.pvs.len() == 1 { "".to_string() } else { format!("multipv {} ", n + 1) },
                   score.uci_string(self.color),
                   self.nodes, (self.hashfull * 1000.0) as i64,
                   self.time, (1000 * self.nodes) as i64 / (self.time + 1))
                .unwrap();

            let mut reverse_moves = vec![];

            for mv in moves.iter() {
                write!(string, "{} ", board.move_to_lan(mv)).unwrap();
                reverse_moves.push(board.do_move(mv.clone()))
            }
            for reverse_move in reverse_moves {
                board.reverse_move(reverse_move);
            }
            if n < self.pvs.len() - 1 {
                writeln!(string, "").unwrap()
            }
        }
        string
    }
}

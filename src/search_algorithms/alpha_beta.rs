use ::NodeCount;

use std::sync::mpsc;
use uci;
use std::fmt;
use std::cmp;
use std::cmp::PartialOrd;
use std::collections::HashMap;
use std::hash::Hash;
use std::cmp::Ordering;

use search_algorithms::board::GameResult;
use search_algorithms::board::EvalBoard;
use search_algorithms::game_move::Move;
use search_algorithms::board::Color::*;
use self::Score::*;
use std::thread;

extern crate time;
use std::sync::{Arc, Mutex};

/// Start a standard uci search, sending the results through a channel
pub fn start_uci_search<B> (board: B, time_limit: uci::TimeRestriction,
                            options: uci::EngineOptions, engine_comm: Arc<Mutex<uci::EngineComm>>,
                            move_list: Option<Vec<B::Move>>)
                            -> (thread::JoinHandle<()>, mpsc::Receiver<uci::UciInfo>)
    where B: EvalBoard + fmt::Debug + Send + 'static + Hash + Eq, <B as EvalBoard>::Move: Sync + Send
{
    let (sender, receiver) = mpsc::channel();
    let thread = thread::spawn(move || uci_search(board, time_limit, options, sender, engine_comm, &move_list));
    (thread, receiver)
}


pub fn uci_search<B>(board: B, time_limit: uci::TimeRestriction,
                     options: uci::EngineOptions, channel: mpsc::Sender<uci::UciInfo>,
                     engine_comm: Arc<Mutex<uci::EngineComm>>, move_list: &Option<Vec<B::Move>>)
    where B: EvalBoard + fmt::Debug + Send + Hash + Eq, <B as EvalBoard>::Move: Sync
{
    search_moves(board, engine_comm, time_limit, options, channel, move_list);
}

pub fn search_moves<B> (mut board: B, engine_comm: Arc<Mutex<uci::EngineComm>>,
                        time_restriction: uci::TimeRestriction,
                        options: uci::EngineOptions,
                        channel: mpsc::Sender<uci::UciInfo>,
                        move_list: &Option<Vec<B::Move>>) 
                         -> (Score, Vec<B::Move>, NodeCount)
    where B: EvalBoard + fmt::Debug + Hash + Eq
{
    {
        engine_comm.lock().unwrap().engine_is_running = true;
    }
    
    let max_depth : u16 = match time_restriction {
        uci::TimeRestriction::Depth(d) => d,
        uci::TimeRestriction::Mate(d) => d as u16,
        _ => 128,
    };
    debug_assert!(max_depth > 1);
    
    let start_time = time::get_time();
    let mut total_node_count = NodeCount::new();
    
    let (mut best_score, mut best_moves, mut best_node_count) = (None, None, None);

    let mut table = HashMap::new();
    
    for depth in 1..(max_depth + 1) {
        
        let mut pvs = vec![];
        let mut pv_moves = vec![];
        for _ in 0..options.multipv {
            
            let mut moves_to_search = if move_list.is_some() {
                move_list.clone().unwrap()
            }
            else {
                board.all_legal_moves()
            };
            moves_to_search.retain(|mv| !pv_moves.iter().any(|mv2| mv == mv2));
            if moves_to_search.is_empty() {
                continue;
            }
            let (score, moves, node_count) =
            // If all moves are preserved, send None to the function
            // This means the root position will still be hashed correctly
                if moves_to_search.len() == board.all_legal_moves().len() {
                    find_best_move_ab(&mut board, depth, &*engine_comm, time_restriction,
                                      None, &mut table)
                }
            else {
                table.remove(&board);
                find_best_move_ab(&mut board, depth, &*engine_comm, time_restriction,
                                  Some(moves_to_search), &mut table)
            };
            best_score = Some(score); 
            best_moves = Some(moves.clone()); 
            best_node_count = Some(node_count.clone());

            if !moves.is_empty() {
                engine_comm.lock().unwrap().best_move = Some(moves[0].to_alg());
            }
            else {
                warn!("Find_best_move_ab didn't return any moves");
                engine_comm.lock().unwrap().best_move = None;
                continue;
            }
            let pv_str = moves.iter()
                .map(Move::to_alg)
                .collect::<Vec<_>>()
                .join(" ");
            pv_moves.push(moves[0].clone());
            pvs.push((score, pv_str));
            total_node_count = total_node_count + node_count;
        }
        let ms_taken = (time::get_time() - start_time).num_milliseconds();
        let uci_info = uci::UciInfo { depth: depth, seldepth: depth, time: ms_taken,
                                      nodes: total_node_count.total, hashfull: 0.0,
                                      pvs: pvs };
        channel.send(uci_info).unwrap();

        match time_restriction {
            uci::TimeRestriction::GameTime(info) => { 
                if (board.to_move() == Black && 
                    ms_taken as u32 > info.black_inc / 5 + info.black_time / 100) ||
                    (board.to_move() == White && 
                     ms_taken as u32 > info.white_inc / 5 + info.white_time / 100)
                {
                    break;
                }
                
            },
            uci::TimeRestriction::MoveTime(time) => if ms_taken > time / 4 { break },
            
            _ => (),
            
        }
    }
    {
        // TODO: Makes this always get executed when the engine shuts down
        let mut engine_comm = engine_comm.lock().unwrap();
        engine_comm.engine_is_running = false;
    }
    (best_score.unwrap(), best_moves.unwrap(), best_node_count.unwrap())
    
}

type Table<B, M> = HashMap<B, HashEntry<M>>;

/// Returns a score, and a list of moves representing the best line it found
fn find_best_move_ab<B:> (board : &mut B, depth : u16, engine_comm : &Mutex<uci::EngineComm>,
                          time_restriction: uci::TimeRestriction,
                          move_list: Option<Vec<B::Move>>, table: &mut Table<B, B::Move>)
                          -> (Score, Vec<B::Move>, NodeCount)
    where B: EvalBoard + fmt::Debug + Hash + Eq
{
    
    fn find_best_move_ab_rec<B:> (board: &mut B, depth : u16,
                                  mut alpha: Score, mut beta : Score,
                                  engine_comm: &Mutex<uci::EngineComm>,
                                  time_restriction: uci::TimeRestriction,
                                  node_counter: &mut NodeCount,
                                  mut move_list: Option<Vec<B::Move>>,
                                  table: &mut Table<B, B::Move>)
                                  -> (Score, Vec<B::Move>)
        where B: EvalBoard + fmt::Debug + Hash + Eq
    {
        use uci::TimeRestriction::*;
        let first_candidate =
            if let Some(&HashEntry{ref best_move, score: (ordering, score), depth: entry_depth })
            = table.get(board) {
                let maximizing = board.to_move() == White;
                if entry_depth >= depth && (
                    ordering == Ordering::Equal || alpha.partial_cmp(&score) != Some(ordering))
                {
                    /*debug_assert_eq!(score, find_best_move_ab_rec(
                        &mut board.clone(), entry_depth, BlackWin(0), WhiteWin(0),
                        engine_comm, time_restriction.clone(),
                        &mut NodeCount::new(), None, &mut HashMap::new()).0,
                                     "Position:\n{:?}", board);*/
                    return (score, match *best_move {
                        Some(ref mv) => vec![mv.clone()],
                        None => vec![]
                    })
                }
                else {
                    best_move.clone()
                }
            }
        else {
            None
        };
        // Check if the thread should stop
        if node_counter.total % 8096 == 0 {
            let should_stop : bool;
            {
                let mut engine_comm = engine_comm.lock().unwrap();
                should_stop = engine_comm.engine_should_stop;
                if should_stop {
                    engine_comm.engine_is_running = false;
                }
            }
            if should_stop { panic!("Engine was told to stop") }
        }
        match board.game_result() {
            Some(GameResult::WhiteWin) => return (WhiteWin(0), vec![]),
            Some(GameResult::BlackWin) => return (BlackWin(0), vec![]),
            Some(GameResult::Draw) => return (Draw(0), vec![]),
            None => (),
        }
        if depth == 0 {
            node_counter.leaf += 1;
            node_counter.total += 1;
            let eval = board.eval_board();
            //let hash_entry = HashEntry::new_bottom_move(eval);
            //table.insert(board.clone(), hash_entry);
            return (Val(eval), vec![]);
        }
        else {
            node_counter.intern += 1;
            node_counter.total += 1;
        }

        match time_restriction {
            GameTime(_) => (),
            Depth(_) => (),
            Nodes(n) => if node_counter.total > n {
                engine_comm.lock().unwrap().engine_is_running = false;
                // TODO: Actually stop here
                //return (::score_board(&board), vec![]);
            },
            Mate(_) => (),
            MoveTime(_) => (), //TODO: Might want to check the clock here, and not just on every depth increase
            Infinite => (),
        };
        
        /*if board.half_move_clock > 50 {
            return (Draw(0), vec![]);
        }*/
        // TODO: Add this back, preferably by adding is_game_over() to the trait
        
        // Helpful alias
        let color = board.to_move();
        let mut best_move = None;
        let mut best_line = vec![];
        
        let mut legal_moves = if move_list.is_some() {
            move_list.take().unwrap()
        }
        else {
            board.all_legal_moves()
        };
        
        // If there is mate or stalemate on the board, we should already have returned
        assert!(!legal_moves.is_empty(), "Found 0 legal moves, but game result was {:?} on \n{:?}",
                board.game_result(), board);
        if let Some(mv) = first_candidate {
            // May fail if search is restricted to only certain moves
            if let Some(position) = legal_moves.iter().position(|e| *e == mv) {
                legal_moves.swap(0, position);
            }
        }
        
        for c_move in legal_moves {
            // Score is greater than the minimizer will ever allow OR
            // Score is lower than the maximizer will ever allow
            if (color == White && alpha >= beta) ||
                (color == Black && alpha >= beta) {
                    break;
                }
            else {
                let old_board = board.clone();
                let undo_move : <B as EvalBoard>::UndoMove = board.do_move(c_move.clone());
                let (tried_score, tried_line) =
                    
                    find_best_move_ab_rec( board, depth - 1, alpha, beta, engine_comm,
                                           time_restriction, node_counter, None, table);
                board.undo_move(undo_move);
                debug_assert_eq!(board, &old_board,
                                 "Failed to restore board after move {:?}", c_move);
                
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
        if let Some(ref c_move) = best_move {
            best_line.push(c_move.clone());
        }
        let score = if color == White { alpha } else { beta };
        let final_score = match score {
            BlackWin(i) => BlackWin(i + 1),
            WhiteWin(i) => WhiteWin(i + 1),
            Draw(i) => Draw(i + 1),
            Val(n) => if depth == 1 {
                let n2 = board.eval_board();
                Val((n + n2) / 2.0)
            }
            else {
                Val(n)
            }, };

        if move_list == None {
            // When doing multipv search, the position may already be in the hash
            // In that case, do not overwrite it
            let ordering = match board.to_move() {
                White => Ordering::Greater,
                Black => Ordering::Less,
            };
            table.insert(board.clone(), HashEntry {
                best_move: best_move, score: (ordering, final_score), depth: depth
            });
        }
         (final_score, best_line)
            
    };
    let mut node_counter = NodeCount { intern: 0, leaf: 0, total: 0 };
    let (score, mut moves) =
        find_best_move_ab_rec(board, depth, BlackWin(0), WhiteWin(0), engine_comm,
                              time_restriction, &mut node_counter, move_list, table);
    moves.reverse();
    (score, moves, node_counter)
}

struct HashEntry<M> {
    best_move: Option<M>, // The best reply. May be none if the position is game over, or if if child positions have not been evaluated yet
    score: (Ordering, Score),
    depth: u16,
}

/// The evaluation of a position. May be extact (A player wins in n moves) or an approximate evaluation. 
#[derive(Clone, Copy, PartialEq)]
pub enum Score {
    Val(f32),
    Draw(u8),
    WhiteWin(u8),
    BlackWin(u8),
}

impl fmt::Display for Score {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Score::Val(f) => write!(fmt, "cp {}", (100.0 * f) as i16),
            Score::WhiteWin(n) => write!(fmt, "mate {}", n as i16 / 2),
            Score::BlackWin(n) => write!(fmt, "mate {}", n as i16 / -2),
            Score::Draw(_) => write!(fmt, "0"),
        }
    }
}

impl fmt::Debug for Score {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Display::fmt(self, fmt)
    }
}

impl PartialOrd for Score {
    fn partial_cmp (&self, other: &Score) -> Option<cmp::Ordering> {
        match (*self, *other) {
            (WhiteWin(n1), WhiteWin(n2)) => Some((&n2).cmp(&n1)),
            (WhiteWin(_), _) => Some(Ordering::Greater),
            
            (Val(_), WhiteWin(_)) => Some(Ordering::Less),
            (Val(_), BlackWin(_)) => Some(Ordering::Greater),
            (Val(n1), Val(n2)) => (&n1).partial_cmp(&n2),
            (Val(n1), Draw(_)) => (&n1).partial_cmp(&0.0),

            (Draw(_), Val(n1)) => (&0.0).partial_cmp(&n1),
            (Draw(_), Draw(_)) => Some(Ordering::Equal),
            (Draw(_), WhiteWin(_)) => Some(Ordering::Less),
            (Draw(_), BlackWin(_)) => Some(Ordering::Greater),
            
            (BlackWin(n1), BlackWin(n2)) => Some(n1.cmp(&n2)),
            (BlackWin(_), _) => Some(Ordering::Less),
            
        }
    }
}

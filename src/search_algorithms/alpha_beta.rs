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
use uci::UciMove;
use search_algorithms::board::Color::*;
use self::Score::*;
use std::thread;
use std::mem;

extern crate time;
use std::sync::{Arc, Mutex};

/// Start a standard uci search, sending the results through a channel
pub fn start_uci_search<B> (board: B, time_limit: uci::TimeRestriction,
                            options: uci::EngineOptions, engine_comm: Arc<Mutex<uci::EngineComm>>,
                            move_list: Option<Vec<B::Move>>)
                            -> (thread::JoinHandle<()>, mpsc::Receiver<uci::UciInfo>)
    where B: EvalBoard + fmt::Debug + Send + 'static + Hash + Eq,
<B as EvalBoard>::Move: Sync + Send + UciMove
{
    let (sender, receiver) = mpsc::channel();
    let thread = thread::spawn(move || uci_search(board, time_limit, options, sender, engine_comm, &move_list));
    (thread, receiver)
}


pub fn uci_search<B>(board: B, time_limit: uci::TimeRestriction,
                     options: uci::EngineOptions, channel: mpsc::Sender<uci::UciInfo>,
                     engine_comm: Arc<Mutex<uci::EngineComm>>, move_list: &Option<Vec<B::Move>>)
    where B: EvalBoard + fmt::Debug + Send + Hash + Eq,
<B as EvalBoard>::Move: Sync + UciMove
{
    search_moves(board, engine_comm, time_limit, options, channel, move_list);
}

pub fn search_moves<B> (mut board: B, engine_comm: Arc<Mutex<uci::EngineComm>>,
                        time_restriction: uci::TimeRestriction,
                        options: uci::EngineOptions,
                        channel: mpsc::Sender<uci::UciInfo>,
                        move_list: &Option<Vec<B::Move>>) 
                         -> (Score, Vec<B::Move>, NodeCount)
    where B: EvalBoard + fmt::Debug + Hash + Eq, <B as EvalBoard>::Move: UciMove
{
    {
        engine_comm.lock().unwrap().engine_is_running = true;
    }
    
    let max_depth : u16 = match time_restriction {
        uci::TimeRestriction::Depth(d) => d,
        uci::TimeRestriction::Mate(d) => d,
        _ => 128,
    };
    debug_assert!(max_depth > 1);
    
    let start_time = time::get_time();
    let mut total_node_count = NodeCount::new();
    
    let (mut best_score, mut best_moves, mut best_node_count) = (None, None, None);

    let mut table = Table::new(options.hash_memory as usize * 1024 * 1024);
    
    for depth in 1..(max_depth + 1) {
        
        let mut pvs : Vec<(Score, String)> = vec![]; // Scores and pv strings of searched moves
        let mut pv_moves : Vec<B::Move> = vec![]; // Moves that have been searched
        for _ in 0..options.multipv {
            
            let mut moves_to_search = if move_list.is_some() {
                move_list.clone().unwrap()
            }
            else {
                board.all_legal_moves()
            };
            moves_to_search.retain(|mv| pv_moves.iter().all(|mv2| mv != mv2));
            if moves_to_search.is_empty() {
                continue;
            }

            let (score, mut moves, node_count) =
            // If all moves are preserved, send None to the function
            // This means the root position will still be hashed correctly
                if moves_to_search.len() == board.all_legal_moves().len() {
                    find_best_move_ab(&mut board, depth, &*engine_comm, time_restriction,
                                      None, &mut table)
                }
            else {
                // TODO: Clearing the hash table loses a ton of performance in multipv mode
                table.clear();
                //table.remove(&board);
                find_best_move_ab(&mut board, depth, &*engine_comm, time_restriction,
                                  Some(moves_to_search.clone()), &mut table)
            };
            // If no moves were found, return a random move
            // This usually means that the position on the board is won, or already a draw
            if moves.is_empty() {
                moves.push(moves_to_search[0].clone());
            }
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
                .map(UciMove::to_alg)
                .collect::<Vec<_>>()
                .join(" ");
            pv_moves.push(moves[0].clone());
            pvs.push((score, pv_str));
            total_node_count = total_node_count + node_count;
        }
        let ms_taken = (time::get_time() - start_time).num_milliseconds();
        let uci_info = uci::UciInfo {
            depth: depth, seldepth: depth, time: ms_taken, nodes: total_node_count.total,
            hashfull: table.mem_usage as f64 / (table.max_memory + 1) as f64 ,
            pvs: pvs };
        channel.send(uci_info).unwrap();
        match time_restriction {
            uci::TimeRestriction::GameTime(info) => {
                let (time, inc) = match board.to_move() {
                    White => (info.white_time, info.white_inc),
                    Black => (info.black_time, info.black_inc),
                };
                
                if ms_taken as u32 > inc / (B::branch_factor() as u32 / 5)
                    + time / (B::branch_factor() as u32 * 5) {
                        break;
                    }
            }
            
            uci::TimeRestriction::MoveTime(time) => if ms_taken > time / (B::branch_factor() as i64 / 5) { break },
            
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

/// Returns a score, and a list of moves representing the best line it found
fn find_best_move_ab<B> (board : &mut B, depth : u16, engine_comm : &Mutex<uci::EngineComm>,
                          time_restriction: uci::TimeRestriction,
                          move_list: Option<Vec<B::Move>>, table: &mut Table<B, B::Move>)
                          -> (Score, Vec<B::Move>, NodeCount)
    where B: EvalBoard + fmt::Debug + Hash + Eq, <B as EvalBoard>::Move: UciMove
{
    
    fn find_best_move_ab_rec<B> (board: &mut B, depth : u16,
                                  mut alpha: Score, mut beta : Score,
                                  engine_comm: &Mutex<uci::EngineComm>,
                                  time_restriction: uci::TimeRestriction,
                                  node_counter: &mut NodeCount,
                                  mut move_list: Option<Vec<B::Move>>,
                                  table: &mut Table<B, B::Move>)
                                  -> (Score, Vec<B::Move>)
        where B: EvalBoard + fmt::Debug + Hash + Eq, <B as EvalBoard>::Move: UciMove
    {
        debug_assert!(alpha <= beta, "alpha={}, beta={}, depth={}, board:\n{:?}",
                      alpha, beta, depth, board); 
        use uci::TimeRestriction::*;
        let first_candidate =
            if let Some(&HashEntry{ref best_reply, score: (ordering, score), depth: entry_depth })
            = table.get(board) {
                if entry_depth >= depth && (
                    ordering == Ordering::Equal || alpha.partial_cmp(&score) != Some(ordering))
                {
                    return (score, match best_reply.clone() { Some(v) => vec![v], None => vec![], })
                }
                else {
                    best_reply.clone()
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
        
        // Helpful alias
        let color = board.to_move();
        let mut best_line = vec![];
        let mut best_score = None;
        
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
            // Process candidate move first for better pruning
            if let Some(position) = legal_moves.iter().position(|e| *e == mv) {
                legal_moves.swap(0, position);
            }
        }
        debug_assert!(!legal_moves.is_empty());
        for c_move in legal_moves {
            // Score is greater than the minimizer will ever allow OR
            // Score is lower than the maximizer will ever allow
            
            let old_board = board.clone();
            let undo_move : <B as EvalBoard>::UndoMove = board.do_move(c_move.clone());
            let new_alpha = decrement_score(alpha, board);
            let new_beta = decrement_score(beta, board);

            let (tried_score, tried_line) =
                find_best_move_ab_rec(board, depth - 1,
                                      new_alpha,
                                      new_beta,
                                      engine_comm,
                                      time_restriction, node_counter, None, table);
            board.undo_move(undo_move);
            debug_assert_eq!(board, &old_board,
                             "Failed to restore board after move {:?}", c_move);
            if best_score.is_none() {
                best_score = Some(tried_score);
                best_line = tried_line.clone();
                best_line.push(c_move.clone());
            }
            if color == White && tried_score > alpha {
                alpha = tried_score;
                best_line = tried_line;
                best_line.push(c_move.clone());
                best_score = Some(tried_score);
            }
            else if color == Black && tried_score < beta {
                beta = tried_score;
                best_line = tried_line;
                best_line.push(c_move.clone());
                best_score = Some(tried_score);
            }
            if alpha >= beta {
                break; 
            }
        }

        let score = increment_score(best_score.unwrap(), board);
        
        if move_list == None {
            // When doing multipv search, the position may already be in the hash
            // In that case, do not overwrite it
            let ordering = match board.to_move() {
                White => Ordering::Greater,
                Black => Ordering::Less,
            };

            table.insert(board.clone(), HashEntry {
                best_reply: best_line.get(0).map(Clone::clone), score: (ordering, score), depth: depth
            });
        }
        (score, best_line)
            
    };
    let mut node_counter = NodeCount { intern: 0, leaf: 0, total: 0 };
    let (score, mut moves) =
        find_best_move_ab_rec(board, depth, BlackWin(0), WhiteWin(0), engine_comm,
                              time_restriction, &mut node_counter, move_list, table);
    moves.reverse();
    (score, moves, node_counter)
}

fn increment_score<B: EvalBoard>(score: Score, board: &B) -> Score {
    match score {
        BlackWin(i) if board.to_move() == Black => BlackWin(i + 1),
        BlackWin(i) => BlackWin(i),
        WhiteWin(i) if board.to_move() == White => WhiteWin(i + 1),
        WhiteWin(i) => WhiteWin(i),
        Draw(i) => Draw(i + 1),
        Val(n) => Val(n),
    }
}

fn decrement_score<B: EvalBoard>(score: Score, board: &B) -> Score {
    match score {
        BlackWin(i) if board.to_move() == Black && i > 0 => BlackWin(i - 1),
        BlackWin(i) if i > 0 => BlackWin(i),
        BlackWin(i) => BlackWin(i),
        WhiteWin(i) if board.to_move() == White && i > 0 => WhiteWin(i - 1),
        WhiteWin(i) if i > 0 => WhiteWin(i),
        WhiteWin(i) => WhiteWin(i),
        Draw(i) if i > 0 => Draw(i - 1),
        Draw(i) => Draw(i),
        Val(n) => Val(n),
    }
}

struct HashEntry<M> {
    best_reply: Option<M>,
    score: (Ordering, Score),
    depth: u16,
}


/// A transposition table for storing known positions, which only grows to a certain
/// size in memory. 
struct Table<B, M> {
    hash_table: HashMap<B, HashEntry<M>>,
    hits: u64, // Total hits in table
    lookups: u64, // Total lookups in table
    mem_usage: usize,
    max_memory: usize,
}

impl<B: EvalBoard + Eq + Hash, M: UciMove> Table<B, M> {
    pub fn new(max_memory: usize) -> Table<B, M> {
        Table { hash_table: HashMap::with_capacity(6 * max_memory / (10 * Self::value_mem_usage())),
                hits: 0, lookups: 0,
                mem_usage: 0, max_memory: max_memory }
    }
        
    pub fn get(&mut self, key: &B) -> Option<&HashEntry<M>> {
        self.lookups += 1;
        let result = self.hash_table.get(key);
        if result.is_some() {
            self.hits += 1;
        }
        result
    }

    pub fn insert(&mut self, key: B, value: HashEntry<M>) {
        let extra_mem = Self::value_mem_usage();
        if self.mem_usage + extra_mem < 6 * self.max_memory / 10 {
            self.mem_usage += extra_mem;
            self.hash_table.insert(key, value);
        }
    }

    #[allow(dead_code)]
    pub fn remove(&mut self, key: &B) -> Option<HashEntry<M>> {
        self.hash_table.remove(key).map(|value| {
            self.mem_usage -= Self::value_mem_usage();
            value
        })
    }

    pub fn clear(&mut self) {
        self.hash_table.clear();
        self.mem_usage = 0;
    }

    fn value_mem_usage() -> usize {
        mem::size_of::<HashEntry<M>>() + mem::size_of::<B>() + mem::size_of::<u64>()
    }
}

/// The evaluation of a position. May be extact (A player wins in n moves) or an approximate evaluation. 
#[derive(Clone, Copy, PartialEq)]
pub enum Score {
    Val(f32),
    Draw(u16),
    WhiteWin(u16),
    BlackWin(u16),
}

impl fmt::Display for Score {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Score::Val(f) => write!(fmt, "cp {}", (100.0 * f) as i16),
            Score::WhiteWin(n) => write!(fmt, "mate {}", n as i16),
            Score::BlackWin(n) => write!(fmt, "mate {}", n as i16 * -1),
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

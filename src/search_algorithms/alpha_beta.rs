use ::NodeCount;

use std::sync::mpsc;
use uci;
use std::fmt;
use std::cmp;
use std::cmp::PartialOrd;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::hash::Hash;
use std::cmp::Ordering;
use std::f32;

use search_algorithms::board::GameResult;
use search_algorithms::board::EvalBoard;
use uci::UciBoard;
use search_algorithms::board::Color::*;
use self::Score::*;
use std::thread;
use std::mem;
use std::time;
use std::sync::{Arc, Mutex};

/// Start a standard uci search, sending the results through a channel
pub fn start_uci_search<B> (board: B, time_limit: uci::TimeRestriction,
                            options: uci::EngineOptions, engine_comm: Arc<Mutex<uci::EngineComm>>,
                            move_list: Option<Vec<B::Move>>)
                            -> (thread::JoinHandle<()>, mpsc::Receiver<uci::UciInfo>)
    where B: UciBoard + fmt::Debug + Send + 'static + Hash + Eq,
<B as EvalBoard>::Move: Sync + Send
{
    let (sender, receiver) = mpsc::channel();
    let thread = thread::spawn(move || uci_search(board, time_limit, options, sender, engine_comm, &move_list));
    (thread, receiver)
}


pub fn uci_search<B>(board: B, time_limit: uci::TimeRestriction,
                     options: uci::EngineOptions, channel: mpsc::Sender<uci::UciInfo>,
                     engine_comm: Arc<Mutex<uci::EngineComm>>, move_list: &Option<Vec<B::Move>>)
    where B: UciBoard + fmt::Debug + Send + Hash + Eq,
<B as EvalBoard>::Move: Sync
{
    search_moves(board, engine_comm, time_limit, options, channel, move_list);
}

pub fn search_moves<B> (mut board: B, engine_comm: Arc<Mutex<uci::EngineComm>>,
                        time_restriction: uci::TimeRestriction,
                        options: uci::EngineOptions,
                        channel: mpsc::Sender<uci::UciInfo>,
                        move_list: &Option<Vec<B::Move>>) 
    where B: UciBoard + fmt::Debug + Hash + Eq
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
    
    let mut total_node_count = NodeCount::new();
    let start_time = time::Instant::now();
    
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

            if let Some((score, moves, node_count)) =
            // If all moves are preserved, send None to the function
            // This means the root position will still be hashed correctly
                if moves_to_search.len() == board.all_legal_moves().len() {
                    find_best_move_ab(&mut board, depth, &*engine_comm, time_restriction,
                                      options, start_time, None, &mut table)
                }
            else {
                // TODO: Clearing the hash table loses a ton of performance in multipv mode
                table.clear();
                //table.remove(&board);
                find_best_move_ab(&mut board, depth, &*engine_comm, time_restriction, options,
                                  start_time, Some(moves_to_search.clone()), &mut table)
            }
            {
                let mut pv_str = String::new();
                
                let mut pv_board = board.clone();

                for mv in moves.iter() {
                    pv_str.push_str(&pv_board.to_alg(&mv));
                    pv_str.push(' ');
                    debug_assert!(pv_board.all_legal_moves().contains(&mv),
                                  "Move {:?} from pv {:?} was illegal on \n{:?}\nStart board:\n{:?}",
                                  mv, moves, pv_board, board);
                    pv_board.do_move(mv.clone());
                }

                pv_moves.push(moves[0].clone());
                pvs.push((score, pv_str));
                total_node_count = total_node_count + node_count;
            }
            else {
                return; // The search has been stopped. Do not send any more data.
            }
        
        }
        let time_taken = time::Instant::now() - start_time;
        let ms_taken = time_taken.as_secs() as u32 * 1000 + time_taken.subsec_nanos() / 1000_000;
        
        let uci_info = uci::UciInfo {
            depth: depth, seldepth: depth, time: ms_taken as i64, nodes: total_node_count.total,
            hashfull: table.mem_usage as f64 / (table.max_memory + 1) as f64 ,
            pvs: pvs };
        channel.send(uci_info).unwrap();
        
        match time_restriction {
            uci::TimeRestriction::GameTime(info) => {

                let time_taken = time::Instant::now() - start_time;
                let (time, inc) = match board.to_move() {
                    White => (info.white_time, info.white_inc),
                    Black => (info.black_time, info.black_inc),
                };
                
                if time_taken > inc / (B::branch_factor() as u32 / 5)
                    + time / (B::branch_factor() as u32 * 5) {
                        break;
                    }
            }
            
            uci::TimeRestriction::MoveTime(time) =>
                if time_taken > time / (B::branch_factor() as u32 / 5) {
                    break
                },
            
            _ => (),
            
        }
    }
    {
        // TODO: Makes this always get executed when the engine shuts down
        let mut engine_comm = engine_comm.lock().unwrap();
        engine_comm.engine_is_running = false;
    }
    
}

/// Returns a score, and a list of moves representing the best line it found
fn find_best_move_ab<B> (board : &mut B, depth : u16, engine_comm : &Mutex<uci::EngineComm>,
                         time_restriction: uci::TimeRestriction, options: uci::EngineOptions,
                         start_time: time::Instant, move_list: Option<Vec<B::Move>>,
                         table: &mut Table<B, B::Move>)
                          -> Option<(Score, Vec<B::Move>, NodeCount)>
    where B: UciBoard + fmt::Debug + Hash + Eq
{
    
    fn find_best_move_ab_rec<B> (board: &mut B, depth : u16,
                                 mut alpha: Score, mut beta : Score,
                                 engine_comm: &Mutex<uci::EngineComm>,
                                 time_restriction: uci::TimeRestriction,
                                 options: uci::EngineOptions,
                                 start_time: time::Instant,
                                 node_counter: &mut NodeCount,
                                 mut move_list: Option<Vec<B::Move>>,
                                 table: &mut Table<B, B::Move>)
                                 -> Option<(Score, Vec<B::Move>)>
        where B: UciBoard + fmt::Debug + Hash + Eq
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
                        return Some((score, best_reply.iter().cloned().collect()))
                }
                else {
                    best_reply.clone()
                }
            }
        else {
            None
        };
        // Check if the thread should stop
        if node_counter.total % 1024 == 0 {
            {
                let mut engine_comm = engine_comm.lock().unwrap();
                if engine_comm.engine_should_stop {
                    engine_comm.engine_is_running = false;
                    return None // "Engine was told to stop"
                }
            }
            // If we've spent more than half of our time, abort immediately
            if let uci::TimeRestriction::GameTime(info) = time_restriction {

                let time_taken = time::Instant::now() - start_time;
                let time_left = match board.to_move() {
                    White => info.white_time,
                    Black => info.black_time,
                };
                
                if time_taken > time_left / 2 {
                    return None;
                }
            }
        }
        
        match board.game_result() {
            Some(GameResult::WhiteWin) => return Some((WhiteWin(0), vec![])),
            Some(GameResult::BlackWin) => return Some((BlackWin(0), vec![])),
            Some(GameResult::Draw) => return Some((Draw(0), vec![])),
            None => (),
        }
        if depth == 0 {
            node_counter.leaf += 1;
            node_counter.total += 1;
            
            if let Mate(_) = time_restriction {
                return Some((Val(board.eval_board()), vec![]));
            }
             
            let score = quiescence_search(board, node_counter, alpha, beta);
            
            let ordering = match board.to_move() {
                White => Ordering::Greater,
                Black => Ordering::Less,
            };
            
            table.insert(board.clone(), HashEntry {
                best_reply: None, score: (ordering, score), depth: depth
            });
            return Some((score, vec![]));
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

        let old_eval = board.eval_board();
        let mut move_searched = false; // ensure at least one move gets searched in zugswang
        
        for c_move in legal_moves {
            // Score is greater than the minimizer will ever allow OR
            // Score is lower than the maximizer will ever allow
            
            let old_board = board.clone();
            let undo_move : <B as EvalBoard>::UndoMove = board.do_move(c_move.clone());

            match (alpha, beta, color) {
                // Do not prune if you are getting mated
                (_, Score::WhiteWin(_), Black) |
                (Score::BlackWin(_), _, White) => (),
                _ if depth < 3 && options.null_move_pruning
                    && move_searched && !board.game_result().is_some() =>
                {
                    let eval = board.eval_board();
                    if !board.to_move() == White && eval < old_eval {
                        board.undo_move(undo_move);
                        continue;
                    }
                    else if !board.to_move() == Black && eval > old_eval {
                        board.undo_move(undo_move);
                        continue;
                    }
                }
                _ => (),
            }
            move_searched = true;
            
            let new_alpha = decrement_score(alpha);
            let new_beta = decrement_score(beta);


            let (tried_score, tried_line) =
                find_best_move_ab_rec(board, depth - 1,
                                      new_alpha, new_beta,
                                      engine_comm, time_restriction, options,
                                      start_time, node_counter, None, table)?;
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

        let score = increment_score(best_score.unwrap());
        
        if move_list == None {
            // When doing multipv search, the position may already be in the hash
            // In that case, do not overwrite it
            let ordering = match board.to_move() {
                White => Ordering::Greater,
                Black => Ordering::Less,
            };
            table.insert(board.clone(), HashEntry {
                best_reply: best_line.last().map(Clone::clone),
                score: (ordering, score), depth: depth
            });
        }
        Some((score, best_line))
            
    };
    let mut node_counter = NodeCount { intern: 0, leaf: 0, total: 0 };
    if let Some((score, mut moves)) =
        find_best_move_ab_rec(board, depth, BlackWin(0), WhiteWin(0), engine_comm,
                              time_restriction, options, start_time,
                              &mut node_counter, move_list, table)
    {
        moves.reverse();
        Some((score, moves, node_counter))
    }
    else {
        None
    }
}

#[inline(never)]
pub fn quiescence_search<B>(board: &mut B, node_counter: &mut NodeCount,
                        mut alpha: Score, mut beta: Score) -> Score
    where B: EvalBoard + fmt::Debug {
    node_counter.intern += 1;
    node_counter.total += 1;

    let stand_pat = match board.game_result() {
        Some(result) => return Score::from_game_result(result),
        None => Val(board.eval_board()),
    };

    let mut best_score = stand_pat;
    let color = board.to_move();

    let active_moves = board.active_moves();
    
    for mv in active_moves.iter() {
        let undo_move = board.do_move(mv.clone());
        let score = quiescence_search(board, node_counter, alpha, beta);
        board.undo_move(undo_move);
        if (board.to_move() == Black && score < best_score)
            || (board.to_move() == White && score > best_score)
        {
            best_score = score;
        }

        if color == White && score > alpha {
            alpha = score;
            best_score = score;
        }
        
        else if color == Black && score < beta {
            beta = score;
            best_score = score;
        }
        if alpha >= beta {
            break; 
        }
    }

    // If all active moves lead to being mated, re-examine with all moves
    // TODO: Avoid re-computing move list
    match (best_score, color) {
        (Score::WhiteWin(_), Black) | (Score::BlackWin(_), White) => {
            for mv in board.all_legal_moves().iter()
                .filter(|mv| !active_moves.contains(mv)){
                let undo_move = board.do_move(mv.clone());
                let score = quiescence_search(board, node_counter, alpha, beta);
                board.undo_move(undo_move);
                if (board.to_move() == Black && score < best_score)
                    || (board.to_move() == White && score > best_score)
                {
                    best_score = score;
                }

                if color == White && score > alpha {
                    alpha = score;
                    best_score = score;
                }
                
                else if color == Black && score < beta {
                    beta = score;
                    best_score = score;
                }
                if alpha >= beta {
                    break; 
                }
            }
            increment_score(best_score)

        },
        _ => increment_score(best_score),
    }
}
    
fn increment_score(score: Score) -> Score {
    match score {
        BlackWin(i) => BlackWin(i + 1),
        WhiteWin(i) => WhiteWin(i + 1),
        Draw(i) => Draw(i + 1),
        Val(n) => Val(n),
    }
}

fn decrement_score(score: Score) -> Score {
    match score {
        BlackWin(i) => BlackWin(u16::saturating_sub(i, 1)),
        WhiteWin(i) => WhiteWin(u16::saturating_sub(i, 1)),
        Draw(i) => Draw(u16::saturating_sub(i, 1)),
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

impl<B: EvalBoard + Eq + Hash, M> Table<B, M> {
    
    #[inline(never)]
    pub fn new(max_memory: usize) -> Table<B, M> {
        Table { hash_table: HashMap::with_capacity(6 * max_memory / (10 * Self::value_mem_usage())),
                hits: 0, lookups: 0,
                mem_usage: 0, max_memory: max_memory }
    }

    #[inline(never)]
    pub fn get(&mut self, key: &B) -> Option<&HashEntry<M>> {
        self.lookups += 1;
        let result = self.hash_table.get(key);
        if result.is_some() {
            self.hits += 1;
        }
        result
    }
    
    #[inline(never)]
    pub fn insert(&mut self, key: B, value: HashEntry<M>) {
        let extra_mem = Self::value_mem_usage();
        match self.hash_table.entry(key) {
            Entry::Occupied(mut entry) => *entry.get_mut() = value,
            Entry::Vacant(entry) => if self.mem_usage + extra_mem < 6 * self.max_memory / 10 {
                self.mem_usage += extra_mem;
                entry.insert(value);
            },
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

impl Score {
    pub fn from_game_result(result: GameResult) -> Self {
        match result {
            GameResult::WhiteWin => WhiteWin(0),
            GameResult::BlackWin => BlackWin(0),
            GameResult::Draw => Draw(0),
        }
    }
    #[allow(dead_code)]
    pub fn to_cp(self) -> i16 {
        match self {
            Val(val) => (val * 100.0) as i16,
            Draw(_) => 0,
            WhiteWin(n) => 12000 - n as i16,
            BlackWin(n) => -12000 + n as i16,
        }
    }
}

impl fmt::Display for Score {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Score::Val(f) => write!(fmt, "cp {}", (100.0 * f) as i16),
            Score::WhiteWin(n) => write!(fmt, "mate {}", (1 + n as i16) / 2),
            Score::BlackWin(n) => write!(fmt, "mate {}", ((1 + n as i16) / 2) * -1),
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

impl Ord for Score {
    fn cmp (&self, other: &Score) -> cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Eq for Score {}

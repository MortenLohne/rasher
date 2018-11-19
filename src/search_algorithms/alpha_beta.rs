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
use std::ops;

use search_algorithms::board::GameResult;
use search_algorithms::board::EvalBoard;
use uci::UciBoard;
use search_algorithms::board::Color;
use search_algorithms::board::Color::*;
use self::Score::*;
use std::thread;
use std::mem;
use std::time;
use std::sync::{Arc, Mutex};
use uci::TimeRestriction::{Nodes, Mate, GameTime};

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
    
    let max_depth : u16 = match time_restriction {
        uci::TimeRestriction::Depth(d) | uci::TimeRestriction::Mate(d) => d,
        _ => 128,
    };
    debug_assert!(max_depth > 1);

    // Normally, the root position should never already be decided
    // If the root position is drawn *anyway*, try to make a move that preserves the draw
    // If no move is found, return null move
    match board.game_result() {
        Some(GameResult::Draw) => {
            for mv in board.all_legal_moves() {
                let undo_move = board.do_move(mv.clone());

                if board.game_result() == Some(GameResult::Draw) {
                    board.undo_move(undo_move);
                    let uci_info = uci::UciInfo {
                        depth: 1, seldepth: 1, time: 0, nodes: 1,
                        hashfull: 0.0,
                        pvs: vec![(Score::Draw(0), board.to_alg(&mv))], color: board.to_move() };
                    channel.send(uci_info).unwrap();
                    return;
                }

                board.undo_move(undo_move);
            }

            let uci_info = uci::UciInfo {
                depth: 1, seldepth: 1, time: 0, nodes: 1,
                hashfull: 0.0,
                pvs: vec![(Score::Draw(0), "null".to_string())], color: board.to_move() };
            channel.send(uci_info).unwrap();

            return;
        },
        _ => (),
    }
    
    let mut total_node_count = NodeCount::new();
    let start_time = time::Instant::now();
    
    let mut table = Table::new(options.hash_memory as usize * 1024 * 1024);

    'depth_loop: for depth in 1..=max_depth {
        
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
                // If all moves are to be searched, send None to the function
                // This way, the root position will still be hashed correctly
                if moves_to_search.len() == board.all_legal_moves().len() {
                    find_best_move_ab(&mut board, depth, &*engine_comm, time_restriction,
                                      options, start_time, None, &mut table)
                }
                else {
                    // In multipv search, the hash table needs to be cleared for correct behaviour. Don't know why.
                    // TODO: Fix this. Clearing the hash table loses a ton of performance in multipv mode
                    table.clear();
                    //table.remove(&board);
                    find_best_move_ab(&mut board, depth, &*engine_comm, time_restriction, options,
                                      start_time, Some(moves_to_search.clone()), &mut table)
                }
            {
                let mut pv_str = String::new();

                let mut pv_board = board.clone();

                for mv in &moves {
                    pv_str.push_str(&pv_board.to_alg(mv));
                    pv_str.push(' ');
                    debug_assert!(pv_board.all_legal_moves().contains(mv),
                                  "Move {:?} from pv {:?} was illegal on \n{:?}\nStart board:\n{:?}",
                                  mv, moves, pv_board, board);
                    pv_board.do_move(mv.clone());
                }
                pv_moves.push(moves[0].clone());
                pvs.push((score, pv_str));
                total_node_count = total_node_count + node_count;
            }
            else {
                break 'depth_loop; // The search has been stopped. Do not send any more data.
            }
        
        }
        let time_taken = time::Instant::now() - start_time;
        let ms_taken = time_taken.as_secs() as u32 * 1000 + time_taken.subsec_nanos() / 1_000_000;
        
        let uci_info = uci::UciInfo {
            depth: depth, seldepth: depth, time: ms_taken as i64, nodes: total_node_count.total(),
            hashfull: table.mem_usage as f64 / (table.max_memory + 1) as f64 ,
            pvs: pvs, color: board.to_move() };
        channel.send(uci_info).unwrap();

        // If we're playing a time control, don't start searching deeper
        // if we have little time left
        match time_restriction {
            uci::TimeRestriction::GameTime(info) => {

                let time_taken = time::Instant::now() - start_time;
                let (time, inc) = match board.to_move() {
                    White => (info.white_time, info.white_inc),
                    Black => (info.black_time, info.black_inc),
                };
                
                if time_taken > inc / (B::BRANCH_FACTOR as u32 / 5)
                    + time / (B::BRANCH_FACTOR as u32 * 5) {
                        break;
                    }
            }
            
            uci::TimeRestriction::MoveTime(time) =>
                if time_taken > time / (B::BRANCH_FACTOR as u32 / 10) {
                    break
                },
            
            _ => (),
            
        }
    }
}

/// Returns a score, and a list of moves representing the best line it found
fn find_best_move_ab<B> (board : &mut B, depth : u16, engine_comm : &Mutex<uci::EngineComm>,
                         time_restriction: uci::TimeRestriction, options: uci::EngineOptions,
                         start_time: time::Instant, move_list: Option<Vec<B::Move>>,
                         table: &mut Table<B::HashBoard, B::Move>)
                          -> Option<(Score, Vec<B::Move>, NodeCount)>
    where B: UciBoard + fmt::Debug + Hash + Eq
{
    
    fn find_best_move_ab_rec<B> (board: &mut B, depth : u16,
                                 mut alpha: Score, beta : Score,
                                 engine_comm: &Mutex<uci::EngineComm>,
                                 time_restriction: uci::TimeRestriction,
                                 options: uci::EngineOptions,
                                 start_time: time::Instant,
                                 node_counter: &mut NodeCount,
                                 mut move_list: Option<Vec<B::Move>>,
                                 killer_moves: &[Option<B::Move>; 2],
                                 table: &mut Table<B::HashBoard, B::Move>)
                                 -> Option<(Score, Option<B::Move>, Vec<B::Move>)>
        where B: UciBoard + fmt::Debug + Hash + Eq
    {
        debug_assert!(alpha <= beta, "alpha={:?}, beta={:?}, depth={}, board:\n{:?}",
                      alpha, beta, depth, board);
        let first_candidate =
            if let Some(&HashEntry{ref best_reply, score: (ordering, score), depth: entry_depth })
            = table.get(&board.hash_board()) {
                if entry_depth >= depth {
                    match ordering {
                        Ordering::Equal =>
                            return Some((score, None,
                                         best_reply.iter().cloned().collect())),
                        Ordering::Less if score < alpha =>
                            return Some((score, None,
                                         best_reply.iter().cloned().collect())),
                        Ordering::Greater if score > beta =>
                            return Some((score, None,
                                         best_reply.iter().cloned().collect())),
                        _ => best_reply.clone()
                    }
                }
                else {
                    best_reply.clone()
                }
            }
        else {
            None
        };
        // Check if the thread should stop
        if node_counter.total() % 2048 == 0 {
            abort_search_check(engine_comm, start_time, time_restriction,
                               node_counter, board.to_move())?;
        }

        if let Some(result) = board.game_result() {
            return Some((Score::from_game_result(result, board.to_move()),
                         None, Vec::new()))
        }

        // Helpful alias
        let color = board.to_move();
        let mut best_line = vec![];

        if depth == 0 { // Quiescence search

            // If searching for mate, don't do quiescence search
            if let Mate(_) = time_restriction {
                return Some((Val(board.eval_board() * color.multiplier() as f32),
                             None, vec![]));
            }

            node_counter.intern += 1;

            let stand_pat = Val(board.eval_board() * color.multiplier() as f32);

            if stand_pat >= beta {
                return Some((beta, None, vec![]));
            }

            alpha = alpha.max(stand_pat);
            let mut node_type = if alpha == Loss(0) && beta == Win(0) {
                Ordering::Equal // Pv-node
            }
            else {
                Ordering::Less // All-node: True value is < score
            };

            let active_moves = board.active_moves();

            for mv in active_moves {
                let undo_move = board.do_move(mv.clone());
                
                let (mut score, _, best_moves) = find_best_move_ab_rec(
                    board, depth, !decrement_score(beta), !decrement_score(alpha),
                    engine_comm, time_restriction, options,
                    start_time, node_counter, None, &[None, None], table)?;

                score = !score;
                board.undo_move(undo_move);
                
                if score >= beta {
                    node_type = Ordering::Greater; // Cute-node: True value is >= beta
                    alpha = score; // TODO: Could be beta as well. Test.
                    best_line = best_moves;
                    best_line.push(mv.clone());
                    break;
                }
                if score > alpha {
                    alpha = score;
                    best_line = best_moves;
                    best_line.push(mv.clone());
                }
            }

            let killer_move = if node_type == Ordering::Greater {
                best_line.last().cloned()
            }
            else {
                None
            };

            let score = increment_score(alpha);
            
            table.insert(board.hash_board(), HashEntry {
                best_reply: best_line.last().cloned(), score: (node_type, score), depth
            });
            return Some((score, killer_move, best_line));  
        }
        debug_assert!(depth > 0);
        
        node_counter.intern += 1;

        //let mut best_score = None;
    
        let mut legal_moves = if move_list.is_some() {
            move_list.take().unwrap()
        }
        else {
            board.all_legal_moves()
        };
        
        // If there is mate or stalemate on the board, we should already have returned
        debug_assert!(!legal_moves.is_empty(),
                       "Found 0 legal moves, but game result was {:?} on \n{:?}",
                       board.game_result(), board);

        let mut moves = vec![];

        if let Some(mv) = first_candidate {
            moves.push(mv);
        }
        moves.extend(killer_moves.iter()
                     .flat_map(|mv| mv.iter())
                     .cloned()
                     .filter(|mv| board.move_is_legal(mv.clone())));
        moves.append(&mut legal_moves);

        let old_eval = board.eval_board() * color.multiplier() as f32;
        let mut move_searched = false; // ensure not all moves are pruned as null moves

        let mut node_type = if alpha == Loss(0) && beta == Win(0) {
            Ordering::Equal // It is currently a pv node
        }
        else {
            Ordering::Less // All-node: True value is < score
        };

        let mut child_killer_moves = [None, None];
        
        for c_move in moves {

            #[cfg(debug_assertions)]
            let old_board = board.clone();
            let undo_move = board.do_move(c_move.clone());

            match alpha {
                // Do null-move pruning
                // Do not prune if you are getting mated
                // TODO: Verify sanity
                Loss(_) => (),
                _ if depth < 3 && options.null_move_pruning
                    && move_searched && !board.game_result().is_some() =>
                {
                    let eval = board.eval_board() * color.multiplier() as f32;
                    if eval < old_eval {
                        board.undo_move(undo_move);
                        continue;
                    }
                }
                _ => (),
            }
            
            let (mut tried_score, child_killer_move, tried_line) =
                find_best_move_ab_rec(board, depth - 1,
                                      !decrement_score(beta), !decrement_score(alpha),
                                      engine_comm, time_restriction, options,
                                      start_time, node_counter, None, &child_killer_moves, table)?;

            if let Some(mv) = child_killer_move {
                insert_killer_move(&mut child_killer_moves, mv);
            }
            
            tried_score = !tried_score;
            
            board.undo_move(undo_move);

            #[cfg(debug_assertions)]
            debug_assert_eq!(board, &old_board,
                            "Failed to restore board after move {:?}", c_move);

            if !move_searched {
                alpha = alpha.max(tried_score);
                best_line = tried_line.clone();
                best_line.push(c_move.clone());
                move_searched = true;
            }
            
            if tried_score >= beta {
                node_type = Ordering::Greater; // True value is >= beta
                alpha = tried_score;
                best_line = tried_line.clone();
                best_line.push(c_move.clone());
                break;
            }
            
            if tried_score > alpha {
                alpha = tried_score;
                best_line = tried_line;
                best_line.push(c_move.clone());
            }
        }

        let score = increment_score(alpha);
        
        if move_list == None {
            // When doing multipv search, the position may already be in the hash
            // In that case, do not overwrite it
            table.insert(board.hash_board(), HashEntry {
                best_reply: best_line.last().cloned(),
                score: (node_type, score), depth
            });
        }
        let killer_move = if node_type == Ordering::Greater {
            best_line.last().cloned()
        }
        else {
            None
        };
        Some((score, killer_move, best_line))
    }
            
    let mut node_counter = NodeCount::new();
    if let Some((score, _, mut moves)) =
        find_best_move_ab_rec(board, depth, Loss(0), Win(0), engine_comm,
                              time_restriction, options, start_time,
                              &mut node_counter, move_list, &[None, None], table)
    {
        debug_assert!(!moves.is_empty(),
                      "Found empty pv at depth {}, score was {:?}",
                      depth, score);
        moves.reverse();
        Some((score, moves, node_counter))
    }
    else {
        None
    }
}

fn abort_search_check(engine_comm: &Mutex<uci::EngineComm>, start_time: time::Instant,
                      time_restriction: uci::TimeRestriction, node_counter: &NodeCount,
                      to_move: Color) -> Option<()> {
    {
        let engine_comm = engine_comm.lock().unwrap();
        if engine_comm.engine_should_stop {
            return None // "Engine was told to stop"
        }
    }
    let time_taken = time::Instant::now() - start_time;

    // If we've spent more than half of our time, abort immediately
    match time_restriction {
        Nodes(n) if node_counter.total() > n
        => return None,
        GameTime(info) => {
            let time_left = match to_move {
                White => info.white_time,
                Black => info.black_time,
            };

            if time_taken > time_left / 2 {
                None
            }
            else {
                Some(())
            }
        }
        // If on movetime restriction, abort if we are getting close to our time limit
        uci::TimeRestriction::MoveTime(time)
        if time_taken > time - time::Duration::from_millis(10) => {
            None
        },
        _ => Some(()),
    }
}

fn insert_killer_move<T: Eq>(moves: &mut[Option<T>; 2], new_move: T) {
    if moves[0].is_none() {
        moves[0] = Some(new_move);
    }
    else if moves[1].is_none() && &new_move != moves[0].as_ref().unwrap() {
        moves[1] = Some(new_move)
    }
    else if moves[0].as_ref().unwrap() != &new_move {
        moves.swap(1, 0);
        moves[0] = Some(new_move);
    }
}

fn increment_score(score: Score) -> Score {
    match score {
        Loss(i) => Loss(i + 1),
        Win(i) => Win(i + 1),
        Draw(i) => Draw(i + 1),
        Val(n) => Val(n),
    }
}

fn decrement_score(score: Score) -> Score {
    match score {
        Loss(i) => Loss(u16::saturating_sub(i, 1)),
        Win(i) => Win(u16::saturating_sub(i, 1)),
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

impl<B: Eq + Hash, M> Table<B, M> {
    #[inline(never)]
    pub fn new(max_memory: usize) -> Table<B, M> {

        Table { hash_table: HashMap::with_capacity(
            6 * max_memory / (10 * Self::value_mem_usage())),
                hits: 0, lookups: 0,
                mem_usage: 0, max_memory
        }
    }

    pub fn clear(&mut self) {
        self.hash_table.clear();
        self.mem_usage = 0;
    }

    fn value_mem_usage() -> usize {
        mem::size_of::<HashEntry<M>>() + mem::size_of::<u64>() + mem::size_of::<u64>()
    }
 
    #[inline(never)]
    pub fn get(&mut self, key: &B) -> Option<&HashEntry<M>>
        where B: Eq + Hash {
        self.lookups += 1;
        let result = self.hash_table.get(&key);
        if result.is_some() {
            self.hits += 1;
        }
        result
    }
    
    #[inline(never)]
    pub fn insert(&mut self, key: B, value: HashEntry<M>)
        where B: Eq + Hash {
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
    pub fn remove(&mut self, key: &B) -> Option<HashEntry<M>>
        where B: EvalBoard + Eq + Hash {
        //key.hash(&mut self.hasher);
        self.hash_table.remove(key).map(|value| {
            self.mem_usage -= Self::value_mem_usage();
            value
        })
    }
}

/// The evaluation of a position. May be extact (A player wins in n moves) or an approximate evaluation. 
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Score {
    Val(f32),
    Draw(u16),
    Win(u16),
    Loss(u16),
}

impl Score {
    pub fn from_game_result(result: GameResult, to_move: Color) -> Self {
        match (result, to_move) {
            (GameResult::WhiteWin, White) => Win(0),
            (GameResult::WhiteWin, Black) => Loss(0),
            (GameResult::BlackWin, Black) => Win(0),
            (GameResult::BlackWin, White) => Loss(0),
            (GameResult::Draw, _) => Draw(0),
        }
    }
    #[allow(dead_code)]
    pub fn to_cp(self, to_move: Color) -> i16 {
        match self {
            Val(val) => (val * 100.0) as i16 * to_move.multiplier() as i16,
            Draw(_) => 0,
            Win(n) => (12_000 - n as i16) * to_move.multiplier() as i16,
            Loss(n) => (-12_000 + n as i16) * to_move.multiplier() as i16,
        }
    }

    pub fn uci_string(mut self, to_move: Color) -> String {
        if to_move == Black {
            self = !self;
        }
        match self {
            Val(f) => format!("cp {}", (100.0 * f) as i16),
            Win(n) => format!("mate {}", (1 + n as i16) / 2),
            Loss(n) => format!("mate -{}", (1 + n as i16) / 2),
            Draw(_) => format!("0"),
        }
    }
}

impl ops::Not for Score {
    type Output = Self;
    fn not(self) -> Self::Output {
        match self {
            Val(val) => Val(-val),
            Draw(n) => Draw(n),
            Win(n) => Loss(n),
            Loss(n) => Win(n),
        }
    }
}

impl PartialOrd for Score {
    fn partial_cmp (&self, other: &Score) -> Option<cmp::Ordering> {
        match (*self, *other) {
            (Win(n1), Win(n2)) => Some((&n2).cmp(&n1)),
            (Win(_), _) => Some(Ordering::Greater),
            
            (Val(_), Win(_)) => Some(Ordering::Less),
            (Val(_), Loss(_)) => Some(Ordering::Greater),
            (Val(n1), Val(n2)) => (&n1).partial_cmp(&n2),
            (Val(n1), Draw(_)) => (&n1).partial_cmp(&0.0),

            (Draw(_), Val(n1)) => (&0.0).partial_cmp(&n1),
            (Draw(_), Draw(_)) => Some(Ordering::Equal),
            (Draw(_), Win(_)) => Some(Ordering::Less),
            (Draw(_), Loss(_)) => Some(Ordering::Greater),
            
            (Loss(n1), Loss(n2)) => Some(n1.cmp(&n2)),
            (Loss(_), _) => Some(Ordering::Less),
            
        }
    }
}

impl Ord for Score {
    fn cmp (&self, other: &Score) -> cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Eq for Score {}

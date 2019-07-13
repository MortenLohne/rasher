use ::NodeCount;

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

use board_game_traits::board::GameResult;
use board_game_traits::board::ExtendedBoard;
use pgn_traits::pgn::PgnBoard;
use board_game_traits::board::Color;
use board_game_traits::board::Color::*;
use self::Score::*;
use std::mem;
use std::time;
use std::sync::{Arc, Mutex};
use uci::TimeRestriction::{Nodes, Mate, GameTime};
use board_game_traits::board::Board;
use uci_engine;
use uci_engine::UciOption;
use search_algorithms::alpha_beta::Score::Loss;
use search_algorithms::alpha_beta::Score::Win;
use search_algorithms::alpha_beta::Score::Val;
use uci::EngineComm;
use uci_engine::UciOptionType;
use uci::ChessVariant;
use uci::UciInfo;
use std::io;
use std::io::Write;

type Depth = u16;

pub struct AlphaBeta<B: ExtendedBoard> {
    time_limit: uci::TimeRestriction,
    options: uci::EngineOptions,
    start_time: time::Instant,
    node_counter: NodeCount,
    root_move_list: Option<Vec<B::Move>>,
    table: Table<B::HashBoard, B::Move>,
    engine_comm : Arc<Mutex<uci::EngineComm>>,
}

impl<B> uci_engine::UciEngine<B> for AlphaBeta<B>
where B: ExtendedBoard + PgnBoard + fmt::Debug + Hash + Eq + 'static {
    fn init() -> Self {
        AlphaBeta {
            time_limit: uci::TimeRestriction::Infinite,
            options: uci::EngineOptions::new(),
            start_time: time::Instant::now(),
            node_counter: NodeCount::new(),
            root_move_list: None,
            table: Table::new(0),
            engine_comm: Arc::new(Mutex::new(uci::EngineComm::new())),
        }
    }

    fn uci_options(&mut self) -> Vec<UciOption> {
        vec![
            UciOption { name: "DebugInfo".to_string(), option_type: UciOptionType::Check(false) },
            UciOption { name: "hash".to_string(), option_type: UciOptionType::Spin(256, 0, 65536) },
            UciOption { name: "threads".to_string(), option_type: UciOptionType::Spin(1, 1, 1) },
            UciOption { name: "multipv".to_string(), option_type: UciOptionType::Spin(1, 1, 65536) },
            UciOption {
                name: "UCI_Variant".to_string(),
                option_type: UciOptionType::Combo(
                    "standard".to_string(),
                    vec!["standard".to_string(), "sjadam".to_string(), "crazyhouse".to_string()])
            }
        ]
    }

    fn set_uci_option(&mut self, uci_option: UciOption) {
        match (uci_option.name.as_str(), uci_option.option_type) {
            ("DebugInfo", UciOptionType::Check(val)) =>
                self.options.debug_info = val,
            ("hash", UciOptionType::Spin(hash, _, _)) =>
                self.options.hash_memory = hash as u32,
            ("threads", UciOptionType::Spin(threads, _, _)) =>
                self.options.threads = threads as u32,
            ("multipv", UciOptionType::Spin(multipv, _, _)) =>
                self.options.multipv = multipv as u32,
            ("UCI_Variant", UciOptionType::Combo(variant, _)) =>
                match variant.as_str() {
                    "standard" => self.options.variant = ChessVariant::Standard,
                    "sjadam" => self.options.variant = ChessVariant::Sjadam,
                    "crazyhouse" => self.options.variant = ChessVariant::Crazyhouse,
                    variant => panic!("Unknown game/chess variant {}", variant),
                },
            (name, option_type) => {
                let message = format!("Unknown option {} of type {:?}", name, option_type);
                warn!("{}", message);
                io::stderr().write( message.as_bytes()).unwrap();
                io::stderr().write( b"\n").unwrap();
            },
        }
    }

    fn search(self, mut board: B, time_limit: uci::TimeRestriction,
              engine_comm: Arc<Mutex<EngineComm>>, move_list: Option<Vec<B::Move>>)
        -> Box<Iterator<Item=UciInfo<B>>> {
        let max_depth: Depth = match time_limit {
            uci::TimeRestriction::Depth(d) | uci::TimeRestriction::Mate(d) => d,
            _ => 128,
        };
        debug_assert!(max_depth > 1);

        // If the root position is drawn, instamove another drawing move
        if board.game_result() == Some(GameResult::Draw) {
            return Box::new(self.preserve_draw(&mut board).into_iter());
        }

        Box::new(DepthIterator::new(self, board, time_limit, engine_comm, move_list))
    }
}

impl<B> AlphaBeta<B>
    where B: ExtendedBoard + PgnBoard + fmt::Debug + Hash + Eq {

    fn init_new_search(&mut self, time_limit: uci::TimeRestriction,
                       engine_comm: Arc<Mutex<EngineComm>>, move_list: Option<Vec<B::Move>>) {
        self.engine_comm = engine_comm;
        self.start_time = time::Instant::now();
        self.table = Table::new(self.options.hash_memory as usize * 1024 * 1024);
        self.node_counter = NodeCount::new();
        self.time_limit = time_limit;
        self.root_move_list = move_list;
    }

    fn search_depth(&mut self, board: &mut B, depth: Depth)
        -> Option<UciInfo<B>> {

        let mut pvs: Vec<(Score, Vec<B::Move>)> = vec![]; // Scores and principal variations of searched moves
        let mut pv_moves: Vec<B::Move> = vec![]; // Moves that have already been returned, for multipv mode
        for _ in 0..self.options.multipv {

            // In multipv search, the root position needs to be cleared from the hash for correct behaviour.
            self.table.remove(&board.hash_board());

            let mut moves_to_search = vec![];
            board.generate_moves(&mut moves_to_search);
            let total_move_count = moves_to_search.len();

            moves_to_search.retain(|mv|
                !pv_moves.contains(mv) &&
                (self.root_move_list.as_ref()
                    .map(|moves| moves.contains(mv))
                    .unwrap_or(true)));
            if moves_to_search.is_empty() {
                continue;
            }

            if let Some((score, moves)) =
            // If all moves are to be searched, send None to the function
            // This way, the root position will still be hashed correctly
            if moves_to_search.len() == total_move_count {
                self.find_best_move_ab(board, depth, None)
            } else {
                self.find_best_move_ab(board, depth, Some(moves_to_search))
            }
            {
                if moves.is_empty() {
                    error!("No moves returned when searching at depth {}. Board:\n{:?}", depth, board);
                }
                else {
                    pv_moves.push(moves[0].clone());
                }
                pvs.push((score, moves));
            } else {
                return None; // The search has been stopped. Do not send any more data.
            }
        }
        let time_taken = time::Instant::now() - self.start_time;
        let ms_taken = time_taken.as_secs() as u32 * 1000 + time_taken.subsec_nanos() / 1_000_000;

        let uci_info = uci::UciInfo {
            depth: depth,
            seldepth: depth,
            time: ms_taken as i64,
            nodes: self.node_counter.total(),
            hashfull: self.table.mem_usage as f64 / (self.table.max_memory + 1) as f64,
            pvs: pvs,
            color: board.side_to_move()
        };
        if self.options.debug_info {
            println!("leaf nodes: {}, internal nodes: {}, qsearch nodes: {}, null move cutoffs: {}, null_move_skips: {}, full hash hits:  {}, partial hash hits: {}, hash misses: {}, hash_move_cutoffs: {}",
                     self.node_counter.leaf, self.node_counter.intern, self.node_counter.qsearch,
                     self.node_counter.null_move_cutoffs, self.node_counter.null_move_skips,
                     self.node_counter.hash_full_hits, self.node_counter.hash_partial_hits,
                     self.node_counter.hash_misses, self.node_counter.hash_move_cutoffs)
        }
        Some(uci_info)
    }

    /// Returns a score, and a list of moves representing the best line it found
    fn find_best_move_ab (&mut self, board : &mut B, depth : Depth, move_list: Option<Vec<B::Move>>)
                             -> Option<(Score, Vec<B::Move>)>
        where B: PgnBoard + ExtendedBoard + fmt::Debug + Hash + Eq
    {
        if let Some((score, _, mut moves)) =
        self.find_best_move_ab_rec(board, depth, move_list,
                                   Loss(0), Win(0), &[None, None], true)
        {
            debug_assert!(!moves.is_empty(),
                          "Found empty pv at depth {}, score was {:?}",
                          depth, score);
            moves.reverse();
            Some((score, moves))
        }
        else {
            None
        }
    }

    fn find_best_move_ab_rec (&mut self, board: &mut B, depth : Depth, mut move_list: Option<Vec<B::Move>>,
                              mut alpha: Score, beta : Score,
                              killer_moves: &[Option<B::Move>; 2], allow_null_moves: bool)
                              -> Option<(Score, Option<B::Move>, Vec<B::Move>)>
        where B: PgnBoard + ExtendedBoard + fmt::Debug + Hash + Eq
    {
        debug_assert!(alpha <= beta, "alpha <= beta. alpha={:?}, beta={:?}, depth={}, board:\n{:?}",
                      alpha, beta, depth, board);
        let first_candidate =
            if let Some(&HashEntry{ref best_reply, score: (ordering, score), depth: entry_depth })
            = self.table.get(&board.hash_board()) {
                if entry_depth >= depth {
                    match ordering {
                        Ordering::Equal => {
                            self.node_counter.hash_full_hits += 1;
                            return Some((score, None,
                                         best_reply.iter().cloned().collect()))
                        },
                        Ordering::Less if score < alpha => {
                            self.node_counter.hash_full_hits += 1;
                            return Some((score, None,
                                         best_reply.iter().cloned().collect()))
                        },
                        Ordering::Greater if score > beta => {
                            self.node_counter.hash_full_hits += 1;
                            return Some((score, None,
                                         best_reply.iter().cloned().collect()))
                        },
                        _ => {
                            self.node_counter.hash_partial_hits += 1;
                            best_reply.clone()
                        }
                    }
                }
                else {
                    self.node_counter.hash_partial_hits += 1;
                    best_reply.clone()
                }
            }
            else {
                self.node_counter.hash_misses += 1;
                None
            };
        // Check if the thread should stop
        if self.node_counter.total() % 2048 == 0 {
            self.abort_search_check(board.side_to_move())?;
        }

        // Helpful alias
        let color = board.side_to_move();
        let mut best_line = vec![];

        if depth == 0 {
            if let Some((score, killer_move, best_move, node_type)) =
            self.qsearch(board, depth, alpha, beta, first_candidate) {

                self.table.insert(board.hash_board(), HashEntry {
                    best_reply: best_move.clone(),
                    score: (node_type, score), depth
                });

                if let Some(mv) = best_move {
                    return Some((score, killer_move, vec![mv]));
                }
                else {
                    return Some((score, killer_move, vec![]));
                }
            }
            else {
                return None;
            }
        }

        debug_assert!(depth > 0);

        if let Some(result) = board.game_result() {
            return Some((Score::from_game_result(result, color),
                         None, Vec::new()))
        }

        const R : Depth = 2;

        if alpha > Loss(Depth::max_value()) && depth >= 2 && allow_null_moves && board.null_move_is_available() {
            let reverse_null_move = board.do_null_move();
            let (tried_score, _, _) =
                self.find_best_move_ab_rec(board, (depth - 1).saturating_sub(R), None,
                                      !decrement_score(beta),
                                      !decrement_score(alpha),  &[None, None],
                                      false)?;
            board.reverse_null_move(reverse_null_move);
            if !tried_score >= beta {
                self.node_counter.null_move_cutoffs += 1;
                // Return immediately for null move cutoffs.
                // Do not store hash entries or killer moves.
                return Some((increment_score(!tried_score), None, vec![]));
            }
            else {
                self.node_counter.null_move_skips += 1;
            }
        }

        self.node_counter.intern += 1;

        let mut legal_moves = if move_list.is_some() {
            move_list.take().unwrap()
        }
        else {
            let mut moves = vec![];
            board.generate_moves(&mut moves);
            moves
        };

        // If there is mate or stalemate on the board, we should already have returned
        debug_assert!(!legal_moves.is_empty(),
                      "Found 0 legal moves, but game result was {:?} on \n{:?}",
                      board.game_result(), board);

        let mut moves = vec![];

        if let Some(mv) = first_candidate.as_ref() {
            moves.push(mv.clone());
        }
        moves.extend(killer_moves.iter()
            .flat_map(|mv| mv.iter())
            .cloned()
            .filter(|mv| board.move_is_legal(mv.clone())));
        moves.append(&mut legal_moves);

        let old_eval = board.static_eval() * color.multiplier() as f32;
        let mut move_searched = false; // ensure not all moves are pruned as null moves

        let mut node_type = if alpha == Loss(0) && beta == Win(0) {
            Ordering::Equal // It is currently a pv node
        }
        else {
            Ordering::Less // All-node: True value is < score
        };

        let mut child_killer_moves = [None, None];

        for (i, c_move) in moves.iter().enumerate() {
            #[cfg(debug_assertions)]
                let old_board = board.clone();
            let reverse_move = board.do_move(c_move.clone());

            match alpha {
                // Do null-move pruning
                // Do not prune if you are getting mated
                // TODO: Verify sanity
                Loss(_) => (),
                _ if depth < 3 && self.options.null_move_pruning
                    && move_searched && !board.game_result().is_some() =>
                    {
                        let eval = board.static_eval() * color.multiplier() as f32;
                        if eval < old_eval {
                            board.reverse_move(reverse_move);
                            continue;
                        }
                    }
                _ => (),
            }


            let (mut tried_score, child_killer_move, tried_line) =
                self.find_best_move_ab_rec(board, depth - 1, None,
                                      !decrement_score(beta), !decrement_score(alpha),
                                           &child_killer_moves, true)?;

            if let Some(mv) = child_killer_move {
                insert_killer_move(&mut child_killer_moves, mv);
            }

            tried_score = !tried_score;

            board.reverse_move(reverse_move);

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
                if i == 0 && Some(c_move) == first_candidate.as_ref() {
                    self.node_counter.hash_move_cutoffs += 1;
                }
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
            self.table.insert(board.hash_board(), HashEntry {
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

    fn qsearch(&mut self, board: &mut B, depth: Depth, mut alpha: Score, beta: Score,
               hash_move: Option<<B as Board>:: Move>)
        -> Option<(Score, Option<B::Move>, Option<B::Move>, Ordering)>
        where <B as Board>::Move: PartialEq + fmt::Debug {

        let color = board.side_to_move();

        if let Some(result) = board.game_result() {
            self.node_counter.leaf += 1;
            return Some((Score::from_game_result(result, color),
                         None, None, Ordering::Equal))
        }

        // If searching for mate, don't do quiescence search
        if let Mate(_) = self.time_limit {
            return Some((Val(board.static_eval() * color.multiplier() as f32),
                         None, None, Ordering::Equal));
        }

        let stand_pat = Val(board.static_eval() * color.multiplier() as f32);

        if stand_pat >= beta {
            self.node_counter.leaf += 1;
            return Some((beta, None, None, Ordering::Equal));
        }

        alpha = alpha.max(stand_pat);
        let mut node_type = if alpha == Loss(0) && beta == Win(0) {
            Ordering::Equal // Pv-node
        } else {
            Ordering::Less // All-node: True value is < score
        };

        let mut active_moves = vec![];
        board.active_moves(&mut active_moves);
        if let Some(ref mv) = hash_move {
            // If active moves don't contain the move, search it anyway
            if let Some(index) = active_moves.iter().position(|mv2| mv == mv2) {
                active_moves.swap(0, index);
            }
            else {
                active_moves.insert(0, mv.clone());
            }
        }

        let mut best_move = None;

        if active_moves.is_empty() {
            self.node_counter.leaf += 1;
        }
        else {
            self.node_counter.qsearch += 1;
        }

        for (i, mv) in active_moves.iter().enumerate() {
            let reverse_move = board.do_move(mv.clone());

            let (mut score, _, _, _) = self.qsearch(
                board, depth, !decrement_score(beta), !decrement_score(alpha), None)?;

            score = !score;
            board.reverse_move(reverse_move);

            if score >= beta {
                node_type = Ordering::Greater; // Cute-node: True value is >= beta
                alpha = score; // TODO: Could be beta as well. Test.
                best_move = Some(mv.clone());
                if i == 0 && hash_move.is_some() {
                    self.node_counter.hash_move_cutoffs += 1;
                }
                break;
            }
            if score > alpha {
                alpha = score;
                best_move = Some(mv.clone());
            }
        }

        let killer_move = if node_type == Ordering::Greater {
            best_move.clone()
        } else {
            None
        };

        let score = increment_score(alpha);

        return Some((score, killer_move, best_move, node_type));

    }

    fn abort_search_check(&mut self, to_move: Color) -> Option<()> {
        {
            let engine_comm = self.engine_comm.lock().unwrap();
            if engine_comm.engine_should_stop {
                return None // "Engine was told to stop"
            }
        }
        let time_taken = time::Instant::now() - self.start_time;

        // If we've spent more than half of our time, abort immediately
        match self.time_limit {
            Nodes(n) if self.node_counter.total() > n
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

    /// A bugged GUI may not correctly adjudicate draws
    /// Therefore, if the root position is in fact already a draw, make any move that preserve the draw
    fn preserve_draw(&self, board: &mut B) -> Option<uci::UciInfo<B>> {
        let mut moves = vec![];
        board.generate_moves(&mut moves);
        for mv in moves {
            let reverse_move = board.do_move(mv.clone());

            if board.game_result() == Some(GameResult::Draw) {
                board.reverse_move(reverse_move);
                let uci_info = uci::UciInfo {
                    depth: 1,
                    seldepth: 1,
                    time: 0,
                    nodes: 1,
                    hashfull: 0.0,
                    pvs: vec![(Score::Draw(0), vec![mv.clone()])],
                    color: board.side_to_move()
                };
                return Some(uci_info);
            }
            board.reverse_move(reverse_move);
        }
        None
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
        Loss(i) => Loss(Depth::saturating_sub(i, 1)),
        Win(i) => Win(Depth::saturating_sub(i, 1)),
        Draw(i) => Draw(Depth::saturating_sub(i, 1)),
        Val(n) => Val(n),
    }
}

struct DepthIterator<B: ExtendedBoard> {
    depth: Depth,
    engine: AlphaBeta<B>,
    board: B,
}

impl<B> DepthIterator<B>
    where B: PgnBoard + ExtendedBoard + fmt::Debug + Hash + Eq + 'static {
    fn new(engine: AlphaBeta<B>, board: B, time_limit: uci::TimeRestriction, engine_comm: Arc<Mutex<EngineComm>>,
           move_list: Option<Vec<B::Move>>) -> DepthIterator<B> {
        let mut iter = DepthIterator {
            depth: 0,
            engine: engine,
            board: board,
        };
        iter.engine.init_new_search(time_limit, engine_comm, move_list);

        iter
    }
}

impl<B: ExtendedBoard> Iterator for DepthIterator<B>
where B: PgnBoard + ExtendedBoard + fmt::Debug + Hash + Eq {
    type Item = UciInfo<B>;
    fn next(&mut self) -> Option<Self::Item> {

        self.depth += 1;

        // If we're playing a time control, don't start searching deeper
        // if we have little time left
        let time_taken = time::Instant::now() - self.engine.start_time;

        match self.engine.time_limit {
            uci::TimeRestriction::GameTime(info) => {
                let time_taken = time::Instant::now() - self.engine.start_time;
                let (time, inc) = match self.board.side_to_move() {
                    White => (info.white_time, info.white_inc),
                    Black => (info.black_time, info.black_inc),
                };

                if time_taken > inc / (B::BRANCH_FACTOR as u32 / 5)
                    + time / (B::BRANCH_FACTOR as u32 * 5) {
                    return None;
                }
            },
            uci::TimeRestriction::MoveTime(time) =>
                if time_taken > time / (B::BRANCH_FACTOR as u32 / 10) {
                    return None;
                },

            uci::TimeRestriction::Depth(d) | uci::TimeRestriction::Mate(d) =>
                if self.depth > d {
                    return None;
                },

            _ => (),
        }

        self.engine.search_depth(&mut self.board, self.depth)
    }
}

struct HashEntry<M> {
    best_reply: Option<M>,
    score: (Ordering, Score),
    depth: Depth,
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
        where B: Eq + Hash {
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
    Draw(Depth),
    Win(Depth),
    Loss(Depth),
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

    pub fn to_value(self, to_move: Color) -> f32 {
        match self {
            Val(val) => val,
            Draw(_) => 0.0,
            Win(_) => 100.0 * to_move.multiplier() as f32,
            Loss(_) => -100.0 * to_move.multiplier() as f32,
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
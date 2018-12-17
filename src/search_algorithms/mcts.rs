use search_algorithms::board::GameResult;
use search_algorithms::board::GameResult::*;
use search_algorithms::board::Board;
use uci::UciBoard;
use search_algorithms::board::Color;
use search_algorithms::alpha_beta;
use uci::TimeRestriction::*;
use search_algorithms::board::Color::*;

use rand;
use ordered_float::NotNaN;
use itertools::Itertools;
use std::f64;
use std::io;
use std::fmt;
use std::io::Write;
use std::marker::{Send, Sync};
use std::sync::{Arc, Mutex, mpsc};
use uci;
use std::time;

use rayon::prelude::*;

pub fn play_human<B: fmt::Debug + UciBoard>(mut board: B) {
    let stdin = io::stdin();
    while board.game_result() == None {
        println!("{:?}", board);
        loop {
            print!("Enter move: ");
            io::stdout().flush().unwrap();
            let mut input = String::new();
            stdin.read_line(&mut input).unwrap();
            input.trim();
            while input.ends_with('\n') {
                input.pop();
            }
            if let Ok(human_move) = board.from_alg(&input) {
                let mut moves = vec![];
                board.generate_moves(&mut moves);
                if moves.contains(&human_move) {
                    board.do_move(human_move);
                    break;
                }
                else {
                    println!("Illegal move {:?}, try again.", human_move);
                }
            }
            else {
                println!("Couldn't parse move string '{}', try again.", input);
            }   
        }
        if board.game_result() != None {
            break;
        }
        println!("{:?}\nOpponent's turn.", board);
        let start_time = time::Instant::now();
        
        let mut mctree = MonteCarloTree::new_root(&mut board);
        let mut searches = mctree.searches();
        let mut rng = rand::weak_rng();
        
        while start_time.elapsed() < time::Duration::from_secs(20) {
            use std::ops::Add;
            mctree.select(&mut rng, searches, &mut SearchData::default());
            searches += 1;
            let searches_of_children = mctree.children.iter()
                .map(Option::as_ref).map(Option::unwrap)
                .map(|n| n.searches())
                .fold(0, u64::add);
            debug_assert!((searches as i64 - searches_of_children as i64).abs() <= 1,
                          format!("{} searches overall, but sum of searches of children is {} {}.",
                                  searches, searches_of_children, mctree.is_fully_expanded));
            
        }
        mctree.print_score(&board, &mut String::new());
        
        let best_move_index = mctree.best_child().unwrap();
        let mut moves = vec![];
        board.generate_moves(&mut moves);
        let best_move = moves[best_move_index].clone();
        let best_node = mctree.children[best_move_index].take().unwrap();
        println!("{:?}\nAI played {:?} after {}/{} searches, with {:.2}% winning chance",
                 board, best_move,
                 best_node.searches(),
                 searches,
                 100.0 * *best_node.score());
        board.do_move(best_move);
    }
    println!("{:?}\n{:?} won!", board, board.game_result());
}

use std::thread;

pub fn start_uci_search<B> (board: B, time_limit: uci::TimeRestriction,
                            options: uci::EngineOptions, engine_comm: Arc<Mutex<uci::EngineComm>>)
                            -> (thread::JoinHandle<()>, mpsc::Receiver<uci::UciInfo>)
    where B: UciBoard + fmt::Debug + Send + 'static, <B as Board>::Move: Sync
{
    let (sender, receiver) = mpsc::channel();
    
    let thread = thread::spawn(move || uci_search(board, time_limit, options, engine_comm, sender));
    (thread, receiver)
}

/// The standard way to use this module
/// Searches until time restriction is reached, continually sending results through a channel
pub fn uci_search<B>(mut board: B, time_limit: uci::TimeRestriction,
                     options: uci::EngineOptions,  engine_comm: Arc<Mutex<uci::EngineComm>>,
                     channel: mpsc::Sender<uci::UciInfo>)
    where B: UciBoard + fmt::Debug + Send + Clone, <B as Board>::Move: Sync
{
    let mut mc_tree = MonteCarloTree::new_root(&mut board);
    let start_time = time::Instant::now();
    let mut rng = rand::weak_rng();
    for n in 1.. {
        for _ in 0..100 * (n as f64).sqrt() as usize {
            use std::ops::Add;
            let searches = mc_tree.searches();
            //searches_last_print = mc_tree.searches;
            let mut search_data = SearchData::default();
            mc_tree.select_parallel(&mut rng, searches, &mut search_data, 1);
            // mc_tree.select(&mut rng, searches, &mut search_data);
            let searches_of_children = mc_tree.children.iter()
                .map(Option::as_ref).map(Option::unwrap)
                .map(|n| n.searches())
                .fold(0, u64::add);
            debug_assert_eq!(mc_tree.searches(), searches_of_children,
                          "{} searches overall, but sum of searches of children is {}.\n{:?}",
                                  mc_tree.searches(), searches_of_children, mc_tree);
        }
        
        let time_taken = time::Instant::now() - start_time;
        
        match time_limit {
            GameTime(info) => { 
                if (board.side_to_move() == Black &&
                    time_taken > info.black_inc + info.black_time / 20) ||
                    (board.side_to_move() == White &&
                     time_taken > info.white_inc / 5 + info.white_time / 20)
                {
                    break;
                }
            },
            Depth(n) | Mate(n)
                if mc_tree.searches() >= (10 as u64).pow(n as u32)
                => break,
            Nodes(n) => if mc_tree.searches() >= n { break } else { },
            MoveTime(time) => {
                if time_taken > time {
                    break;
                }
                else { () }
            },
            Infinite | Depth (_) | Mate(_) => (),
        }
        if engine_comm.lock().unwrap().engine_should_stop {
            break;
        }
        send_uci_info(&mc_tree, &mut board, start_time, &options, &channel);
    }
    send_uci_info(&mc_tree, &mut board, start_time, &options, &channel);
}

fn send_uci_info<B>(mc_tree: &MonteCarloTree<B>,
                    board: &mut B, start_time: time::Instant,
                    options: &uci::EngineOptions, channel: &mpsc::Sender<uci::UciInfo>)
    where B: UciBoard + fmt::Debug
{
    debug_assert_eq!(mc_tree.board, *board);

    let time_taken = time::Instant::now() - start_time;
    let ms_taken = time_taken.as_secs() as u32 * 1000 + time_taken.subsec_nanos() / 1_000_000;
    let mut pvs = vec![];
    
    for &(node, ref go_move) in mc_tree.children.iter()
        .map(Option::as_ref)
        .filter(Option::is_some)
        .map(Option::unwrap)
        .zip({
            let mut moves = vec![];
            board.generate_moves(&mut moves);
            moves
        })
        .sorted_by(|&(node1, _), &(node2, _)| {
            let cmp = node1.score().cmp(&node2.score());
            if mc_tree.maximizing { cmp.reverse() }
            else { cmp }
        })
        .iter().take(options.multipv as usize)
        
    {
        let undo_move = board.do_move(go_move.clone());
        let mut pv = board.to_alg(go_move);
        pv.push(' ');
        pv.push_str(&node.pv(board).iter()
                    .map(|mv| board.to_alg(mv))
                    .collect::<Vec<_>>()
                    .join(" "));
        let score = alpha_beta::Score::Val((node.score().into_inner() as f32 - 0.5) * 20.0);
        pvs.push((score, pv));
        board.undo_move(undo_move);
    }

    let depth = pvs[0].1.split_whitespace().count() as u16;
    let uci_info = uci::UciInfo { depth: depth, seldepth: depth, time: ms_taken as i64,
                                  nodes: mc_tree.searches(), hashfull: 0.0, pvs: pvs,
                                  color: board.side_to_move() };
    channel.send(uci_info).unwrap();
}

/// Scecial non-uci search that gives additional debug information
pub fn search_position<B>(board: &mut B)
    where B: Board + fmt::Debug + Send + Clone + PartialEq, <B as Board>::Move: Sync
{
    let mut mc_tree = MonteCarloTree::new_root(board);
    let start_time = time::Instant::now();
    
    let mut rng = rand::weak_rng();
    let mut total_depth : u64 = 0;
    let mut searches_last_print = 0;
    
    for n in 1.. {
        for _ in 0..((5*n) as f64).sqrt() as usize {
            use std::ops::Add;
            let searches = mc_tree.searches();
            //searches_last_print = mc_tree.searches;
            let mut search_data = SearchData::default();
            mc_tree.select_parallel(&mut rng, searches, &mut search_data, 1);
            //mc_tree.select(board, &mut rng, searches, &mut search_data)
            total_depth += search_data.total_depth as u64;
            let searches_of_children = mc_tree.children.iter()
                .map(Option::as_ref).map(Option::unwrap)
                .map(|n| n.searches())
                .fold(0, u64::add);
            debug_assert!((mc_tree.searches() as i64 - searches_of_children as i64).abs() <= 1,
                          format!("{} searches overall, but sum of searches of children is {}.\n{:?}",
                                  mc_tree.searches(), searches_of_children, mc_tree));
            if mc_tree.searches() - searches_last_print > 4096 {
                searches_last_print = mc_tree.searches();
                
                let elapsed_time = start_time.elapsed();
                let elapsed_seconds = elapsed_time.as_secs() as f32
                    + elapsed_time.subsec_nanos() as f32 / 1000_0000_0000.0;
                    
                println!("{} total searches at {}nps, t={}s, {:.2}% draws, average depth {}.",
                         mc_tree.searches(),
                         (mc_tree.searches() as f32 / elapsed_seconds) as u64,
                         elapsed_seconds,
                         100.0 * mc_tree.score.draws as f64 / mc_tree.searches() as f64,
                         total_depth / mc_tree.searches());
                mc_tree.print_score(board, &mut String::new());
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MonteCarloTree<B> {
    // All the node's child nodes. 
    pub children: Vec<Option<Box<MonteCarloTree<B>>>>,
    board: B,
    pub score: Score,
    game_result: Option<GameResult>,
    is_fully_expanded: bool,
    maximizing: bool,
}

impl<B> MonteCarloTree<B> {
    pub fn value(&self) -> f64 {
        return self.score.black_wins as f64 * 1.0 + self.score.draws as f64 * 0.5;
    }

    pub fn searches(&self) -> u64 {
        return self.score.black_wins + self.score.white_wins + self.score.draws;
    }
}

impl<B: Board + fmt::Debug + Clone + PartialEq> MonteCarloTree<B> {

    pub fn new_root(board : &mut B) -> Self {
        let mut moves = vec![];
        board.generate_moves(&mut moves);
        let mut root = MonteCarloTree { children: vec![None; moves.len()],
                                        board: board.clone(),
                                        score: Score::new(),
                                        game_result: board.game_result(),
                                        is_fully_expanded: false,
                                        maximizing: board.side_to_move() == Color::Black
        };
        let mut rng = rand::weak_rng();
        while !root.is_fully_expanded {
            root.expand(&mut rng, &mut SearchData::default());
            if root.searches() > 10_000 {
                panic!("Failed to fully expand root node after 10000 searches")
            }
        }        
        root
    }

    pub fn new_child(&self, board: B, num_of_children: usize) -> Box<Self> {
        Box::new(MonteCarloTree { children: vec![None; num_of_children],
                                  game_result: board.game_result(),
                                  board: board,
                                  score: Score::new(),
                                  is_fully_expanded: false, 
                                  maximizing: !self.maximizing })
    }

    pub fn score(&self) -> NotNaN<f64> {
        NotNaN::new(self.value() / self.searches() as f64).unwrap_or_else(|_|NotNaN::new(0.5).unwrap())
    }

    /// Increments the search counter for the node, and adds 1.0, 0.5 or 0.0 to the value,
    /// depending on the given result
    fn add_value(&mut self, result: GameResult) {
        // TODO: Maybe do not count a draw as a search
        match result {
            BlackWin => {
                self.score.black_wins += 1;
            },
            WhiteWin => {
                self.score.white_wins += 1;
            },
            Draw => {
                self.score.draws += 1;
            }
        }
    }

    /// Returns the index of the best child node
    pub fn best_child(&self) -> Option<usize> {
        let iter = self.children.iter()
            .map(Option::as_ref)
            .enumerate()
            .filter(|&(_, opt)| opt.is_some())
            .map(|(n, opt)| (n, opt.unwrap()));
        if self.maximizing {
            iter.max_by_key(|&(_, child)| child.score())
                .map(|(n, _)| n)
        }
        else {
            iter.min_by_key(|&(_, child)| child.score())
                .map(|(n, _)| n)
        }
    }

    pub fn pv(&self, board: &mut B) -> Vec<<B as Board>::Move> {
        debug_assert_eq!(*board, self.board);
        match self.best_child() {
            None => vec![],
            Some(index) => {
                //println!("Found best child {:?}", self);
                let child_node = self.children[index].as_ref()
                    .expect(&format!("No index {} in {:?}", index, self.children));
                let mut moves = vec![];
                board.generate_moves(&mut moves);
                assert_eq!(moves.len(), self.children.len(),
                           "{:?} has {} moves but node has {} children: {:?}\n{:?}\nNode board: {:?}\nChild node board:{:?}",
                           board, moves.len(), self.children.len(), self.children,
                           moves, self.board, child_node.board);

                if index >= moves.len() {
                    println!("{:?}\n{:?}\n{:?}", board, moves, self);
                }

                for (i, ref mv) in moves.iter().enumerate() {
                    let old_board = board.clone();
                    let undo_move = board.do_move((*mv).clone());
                    if let Some(ref child) = self.children[i] {
                        debug_assert_eq!(*board, child.board,
                                         "\nAfter doing {:?}, boards were mismatched. Old board:{:?}\nParent node board:{:?}\n",
                                         mv, old_board, self.board);
                    }
                    board.undo_move(undo_move);
                }

                let game_move = moves[index].clone();
                let old_board = board.clone();
                let undo_move = board.do_move(game_move.clone());
                debug_assert_eq!(*board, child_node.board,
                                 "\nAfter doing {:?}, boards were mismatched. Old board:\n{:?}\n",
                                 game_move, old_board);
                let mut result = child_node.pv(board);
                board.undo_move(undo_move);
                debug_assert_eq!(*board, old_board);
                result.insert(0, game_move);
                result
            }
        }
    }

    pub fn print_score(&self, board: &B, padding: &mut String) {

        for (node, go_move) in self.children.iter()
            .map(Option::as_ref)
            .filter(Option::is_some)
            .map(Option::unwrap)
            .zip({
                let mut moves = vec![];
                board.generate_moves(&mut moves);
                moves
            })
            
            .sorted_by(|&(node1, _), &(node2, _)| {
                let cmp = node1.score().cmp(&node2.score());
                if self.maximizing { cmp.reverse() }
                else { cmp }
            })
        {
            let mut board_after_move = board.clone();
            board_after_move.do_move(go_move.clone());
            let mut moves = vec![];
            board.generate_moves(&mut moves);
            if let Some(best_reply_index) = node.best_child() {
                let best_reply = moves[best_reply_index].clone();
                if let Some(ref best_reply_node) = node.children[best_reply_index] {
                    
                    println!("{}Move {:?} scores {:.2}% ({}/{}/{}), n={}, best reply {:?} with {:.2}% ({}/{}/{}), n={}", 
                             padding, go_move, 100.0 * *node.score(),
                             node.score.white_wins, node.score.draws, node.score.black_wins,
                             node.searches(),
                             best_reply, 100.0 * *best_reply_node.score(),
                             best_reply_node.score.white_wins, best_reply_node.score.draws,
                             best_reply_node.score.black_wins, best_reply_node.searches());
                    if node.searches() > 10_000 {
                        println!("Children//:");
                        padding.push_str("  ");
                        node.print_score(&board_after_move, padding);
                        let len = padding.len(); 
                        padding.truncate(len - 2);
                        println!("//children");
                    }                 
                    continue;
                }
            }
            if node.searches() >= 10 {
                println!("{}Move {:?} scores {:.2}%, {} searches, best reply not found", 
                         padding, go_move, 100.0 * *node.score(), node.searches() );
            }
        }
    }

    /// Returns a measure of how attractive this node is to explore further
    /// Nodes with good scores but less searches are more attractive
    /// Higher is always better, even when minimzing
    pub fn move_selection_value(&self, total_searches: u64) -> NotNaN<f64> {
        if self.searches() == 0 {
            if self.maximizing { NotNaN::new(f64::MAX).unwrap() }
            else { NotNaN::new(f64::MIN).unwrap() }
        }
        else {
            let adjusted_score = if self.maximizing { 1.0 - *self.score() } else { *self.score() };
            NotNaN::new(adjusted_score + 
                        (2.0f64).sqrt() * ((total_searches as f64).ln() / self.searches() as f64).sqrt())
                .unwrap()
        }
    }
    
    // Select the node. If the selected child is not fully expanded, expand it
    pub fn select<R> (&mut self, rng: &mut R, total_searches: u64,
                      search_data: &mut SearchData) -> GameResult
        where R: rand::Rng
    {
        //println!("Selecting serially");
        search_data.selection_depth += 1;
        search_data.total_depth += 1;
        assert!(self.is_fully_expanded, "Tried to select node that wasn't fully expanded");
        if let Some(result) = self.game_result {
            self.add_value(result);
            return result
        }
        use std::ops::Add;
        let searches_of_children = self.children.iter()
            .map(Option::as_ref).map(Option::unwrap)
            .map(|n| n.searches())
            .fold(0, u64::add);
        debug_assert!((self.searches() as i64 - searches_of_children as i64).abs() <= 1,
                      format!("{} searches in total, but sum of searches of children is {}\n{:?}",
                              self.searches(), searches_of_children, self.children));
        
        //let moves = board.generate_moves();
        // Find the index of the child node with highest selection value
        let child_index : usize = self.children.iter_mut()
            .map(Option::as_mut).map(Option::unwrap)
            .enumerate()
            .map(|(n, child)| (n, child.move_selection_value(total_searches), child))
            .max_by_key(|&(_, move_value, _)| move_value)
            .map(|(n, _, _)| n).unwrap();

        let value;
        {
            let child : &mut MonteCarloTree<_> = self.children[child_index].as_mut().unwrap();
            
            if child.is_fully_expanded {
                value = child.select(rng, total_searches, search_data);
            }
            else {
                value = child.expand(rng, search_data);
            }
        }
        self.add_value(value);
        value
    }

    // Expand this node, creating a new child
    pub fn expand <R> (&mut self, rng: &mut R,
                          search_data: &mut SearchData) -> GameResult
        where R: rand::Rng
    {
        //println!("Expanding");
        match self.game_result {
            None => {
                let mut moves = vec![];
                self.board.generate_moves(&mut moves);
                debug_assert!(self.children.contains(&None));
                debug_assert_eq!(moves.len(), self.children.len(),
                                 "{} moves available, but tree has {} children\n{:?}",
                                 moves.len(),
                                 self.children.len(), self.board);
                
                let mut random_index = rng.gen_range(0, self.children.len());
                // Pick random child nodes until we find an unexpanded one
                // TODO: This takes a long time with few unexplored nodes. Optimize.
                loop {
                    if self.children[random_index] == None {
                        let mut board = self.board.clone();
                        let mut moves = vec![];
                        board.generate_moves(&mut moves);
                        board.do_move(moves[random_index].clone());

                        let value;
                        let num_child_moves = if board.game_result() == None {
                            moves.clear();
                            board.generate_moves(&mut moves);
                            moves.len()
                        }
                        else { 0 };
                        self.children[random_index] = Some(
                            self.new_child(board.clone(), num_child_moves));
                        if !self.children.contains(&None) {
                            self.is_fully_expanded = true;
                        }
                        {
                            let new_child = &mut self.children[random_index].as_mut().unwrap();
                            value = Self::simulate(board, rng, search_data);
                            
                            new_child.add_value(value);
                        }
                        self.add_value(value);
                        return value;
                    }
                    else {
                        random_index += 1;
                        random_index %= self.children.len();
                    }
                }
            },
            Some(result) => {
                self.is_fully_expanded = true;
                self.add_value(result);
                result
            }
        }
    }

    fn simulate <R> (mut board: B, rng: &mut R, search_data: &mut SearchData) -> GameResult
        where R: rand::Rng
    {
        search_data.total_depth += 1;
        if let Some(result) = board.game_result() {
            return result
        }

        let mut moves = vec![];
        board.generate_moves(&mut moves);
        board.do_move(moves[rng.gen_range(0, moves.len())].clone());

        Self::simulate(board, rng, search_data)
    }    
}

impl<B: Board + fmt::Debug + Send + Clone + PartialEq> MonteCarloTree<B> {
// Select the node. If the selected child is not fully expanded, expand it
    pub fn select_parallel<Ra> (&mut self, rng: &mut Ra,
                                  total_searches: u64, search_data: &mut SearchData,
                                  depth: u16) -> Score
        where Ra: rand::Rng + rand::Rand + Send + Clone + Sync,
    <B as Board>::Move: Sync
    {
        //println!("Selecting in parallel");
        
        assert!(self.is_fully_expanded, "Tried to select node that wasn't fully expanded");
        if let Some(result) = self.game_result {
            self.add_value(result);
            search_data.selection_depth += 1;
            search_data.total_depth += 1;
            return Score::from_game_result(&result)
        }
    
        if self.children.len() == 1 {
            let child = self.children[0].as_mut().unwrap();

            let value =
                if child.is_fully_expanded {
                    (*child).select_parallel(rng, total_searches,
                                                    search_data, depth + 1)
                }
                    else {
                        Score::from_game_result(&child.expand(rng, search_data))
                    };
            self.score.add_score(&value);
            return value;
        }
        // Find the index of the child node with highest selection value
        let child_indices : Vec<usize>;
        {
            let mut move_selection_values : Vec<_> = self.children.iter()
                .map(Option::as_ref).map(Option::unwrap)
                .enumerate()
                .map(|(n, child)| (n, child.move_selection_value(total_searches), child))
            .collect();
            move_selection_values.sort_by_key(|&(_, n, _)| n);
            move_selection_values.reverse();
            child_indices = move_selection_values.iter()
            // TODO: Sort so that highest value is taken
                .take(2)
                .map(|&(n, _, _)| n).collect();
        }
        let (child1, child2) : (&mut Box<MonteCarloTree<_>>, &mut Box<MonteCarloTree<_>>)
            = get_two_mut(&mut self.children[0..], child_indices[0], child_indices[1]);
        
        let value = [(child1, rng.clone()),
                     (child2, rng.clone())].into_par_iter()
            .map(|&mut(ref mut child, ref mut new_rng)| {

                if child.is_fully_expanded {
                    if depth > 4 {
                        Score::from_game_result(&child.select(new_rng,
                                                              total_searches, &mut search_data.clone()))
                    }
                    else {
                        (*child).select_parallel(new_rng, total_searches,
                                                 &mut search_data.clone(), depth + 1)
                    }
                }
                else {
                    Score::from_game_result(&child.expand(new_rng,
                                                          &mut search_data.clone()))
                }
            })
            .reduce(Score::new, |acc, score| { let mut a2 = acc.clone(); a2.add_score(&score); a2 });

        self.score.add_score(&value);
        value
            
    }    
}

#[derive(PartialEq, Clone, Debug, Default)]
pub struct SearchData {
    selection_depth: u16,
    total_depth: u16,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Score {
    white_wins : u64,
    black_wins : u64,
    draws : u64,
}

impl Score {
    fn new() -> Self {
        Score {white_wins: 0, black_wins: 0, draws: 0 }
    }

    /// Creates a score object containing a single result
    fn from_game_result(result: &GameResult) -> Self {
        let mut score = Score::new();
        match *result {
            WhiteWin => score.white_wins += 1,
            BlackWin => score.black_wins += 1,
            Draw => score.draws += 1,
        }
        score
    }
    
    fn add_score(&mut self, other: &Score) {
        self.white_wins += other.white_wins;
        self.black_wins += other.black_wins;
        self.draws += other.draws;
    }
}

/// Returns mutable references to two elements in a slice
/// Panics if either index is out of bounds, or the indices are not distinct
fn get_two_mut<T>(slice: &mut[Option<T>], index1: usize, index2: usize) 
    -> (&mut T, &mut T) {
    
    if index1 >= slice.len() || index2 >= slice.len()
        || index1 == index2 {
            panic!();
    }
    else {
        let e1_mut = (slice[index1].as_mut().unwrap() as *const T) as *mut T;
        let e2_mut = (slice[index2].as_mut().unwrap() as *const T) as *mut T;
        unsafe {
            (&mut *e1_mut, &mut *e2_mut)
        }
    }
}

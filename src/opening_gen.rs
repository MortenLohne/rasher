use search_algorithms::board::EvalBoard;
use search_algorithms::alpha_beta::Score;
use uci;
use uci::UciBoard;
use uci::TimeRestriction;
use uci::EngineOptions;
use uci::EngineComm;
use search_algorithms::alpha_beta;

use std::fmt::Debug;
use std::hash::Hash;
use std::sync::{Arc, Mutex};
use std::collections::HashSet;
use std::time;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::thread;
use std::time::Duration;
use board::sjadam_board::SjadamBoard;

use rayon::prelude::*;
use std::io;
use board::sjadam_move::SjadamMove;
use std::process;
use std::fs;

type Depth = u32;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Transposition {
    Searching,
    Searched(Score, Depth),
}

type Table<B> = Arc<Mutex<HashMap<<B as EvalBoard>::HashBoard, Transposition>>>;

const SEARCH_TIME_MS : u64 = 750;

const MIN_SCORE : Score = Score::Val(-2.5);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OpeningTree<B: EvalBoard> {
    pub mv: Option<B::Move>,
    pub eval: Score, // From the perspective of the side to move
    pub children: Option<Vec<OpeningTree<B>>>,
}

impl<B: EvalBoard> OpeningTree<B>
    where B: EvalBoard + UciBoard + Debug + Sync + Send + Hash + Eq + 'static,
<B as EvalBoard>::Move: Sync + Send {

    pub fn size(&self) -> usize {
        1 + self.children.as_ref().unwrap_or(&vec![]).iter().map(Self::size).sum::<usize>()
        
    }
    
    pub fn new_root() -> Self {
        OpeningTree { mv: None, eval: Score::Draw(0), children: None }
    }

    pub fn sort_tree(&mut self, board: &mut B) {
        if let Some(ref mut children) = self.children {
            children.sort_by(|child1, child2| {
                child1.eval.partial_cmp(&child2.eval).unwrap()
            });

            assert_eq!(Some(!self.eval), children.get(0).map(|ch| ch.eval),
                       "self-eval was {:?} after {:?}, child-evals were {:?}", self.eval, self.mv, children);

            for child in children {
                if let Some(mv) = child.mv.clone() {
                    let undo_move = board.do_move(mv);
                    child.sort_tree(board);
                    board.undo_move(undo_move);
                }
            }
        }
    }

    pub fn prune(&mut self, board: &mut B, transpositions: &mut HashSet<B>) {
        if let Some(ref mut children) = self.children {

            children.retain(|child| {
                
                let undo_move = board.do_move(child.mv.clone().unwrap());
                
                let result = !transpositions.contains(&board);
                transpositions.insert(board.clone());

                board.undo_move(undo_move);

                result && child.eval.to_cp(board.to_move()).abs() < 100
                
            });
            for child in children {
                let undo_move = board.do_move(child.mv.clone().unwrap());

                child.prune(board, transpositions);

                board.undo_move(undo_move);
            }
        }
    }

    pub fn print_epd<T: io::Write>(&self, board: &mut B, pv: &mut Vec<String>,
                                   depth: Depth, sink: &mut T) {
        let undo_move =
            if let Some(mv) = self.mv.as_ref() {
                pv.push(board.to_alg(mv));
                Some(board.do_move(mv.clone()))
            }
        else {
            None
        };
        let fen = board.to_fen().split_whitespace().take(4).collect::<Vec<&str>>().join(" ");
        
        write!(sink, "{} ce {}; res {}; fmvn {}; id \"{}\"\n",
               fen, self.eval.to_cp(board.to_move()),
               self.children.as_ref().map(|chs| chs.len()).unwrap_or(0),
               1 + depth / 2, pv.join(" ")).unwrap();

        for child in self.children.as_ref()
            .unwrap_or(&vec![]) {
            child.print_epd(board, pv, depth + 1, sink);
        }

        if let Some(undo_move) = undo_move {
            pv.pop();
            board.undo_move(undo_move);
        }
    }

    pub fn print_opening<T: io::Write>(&self, board: &B, sink: &mut T) {
        for child in self.children.as_ref().unwrap_or(&vec![]) {
            write!(sink, "Child eval for {:?}: {}\n",
                     child.mv, child.eval.to_cp(!board.to_move())).unwrap();

            for grandchild in child.children.as_ref().unwrap_or(&vec![]) {
                write!(sink, "\tGrandchild eval for {:?}: {}, best reply {:?}\n",
                         grandchild.mv, grandchild.eval.to_cp(board.to_move()),
                         grandchild.children.as_ref().and_then(|chs| chs.get(0).and_then(|ch| ch.mv.clone() ))).unwrap();
            }
        }
    }

    pub fn eval(&mut self, depth: Depth, mut board: B, table: &mut Table<B>) -> Option<Score> {
        if depth == 0 {
            return None
        }

        // Look for position in transposition table
        // If the position is being searched by another thread, wait until it's finished
        // If we want to actually search this position, always write a 'Searching' entry
        for i in 1.. {
            match table.lock() {
                Ok(mut table) => {
                    match table.entry(board.hash_board()) {
                        Entry::Occupied(mut entry) => match *entry.get() {
                            Transposition::Searched(score, entry_depth) => {
                                if entry_depth >= depth {
                                    debug!("Found entry at depth {} while searching at depth {} after {:?}, skipping search",
                                           entry_depth, depth, self.mv);
                                    self.eval = score;
                                    return Some(!score)
                                }
                                else {
                                    info!("Overriding old entry at depth {} with new entry at depth {} after {:?}",
                                           entry_depth, depth, self.mv);
                                    entry.insert(Transposition::Searching);
                                    break;
                                }
                            },
                            Transposition::Searching => (),
                        },
                        Entry::Vacant(entry) => {
                            entry.insert(Transposition::Searching);
                            break
                        },
                    }
                }
                Err(err) => {
                    error!("Poisoned transposition table mutex: {}", err)
                }
            }
            debug!("Move {:?} at depth {} is currently being searched, {}th attempt", self.mv, depth, i);
            thread::sleep(Duration::from_millis(i));
        }


        let multipv = if depth > 1 { 16 } else { 1 };

        let duration = time::Duration::from_millis(SEARCH_TIME_MS * (depth * depth) as u64);

        let mut options = EngineOptions::new();
        options.multipv = multipv;
        options.hash_memory = 32;
        
        let (handle, move_channel) =
            alpha_beta::start_uci_search(
                board.clone(),
                TimeRestriction::MoveTime(duration),
                options,
                Arc::new(Mutex::new(EngineComm::new())),
                None);
        
        if let Ok(mut results) = uci::get_uci_multipv(handle, move_channel) {

            results.sort_by(|&(score1, _), &(score2, _)|
                score1.partial_cmp(&score2).unwrap());
            results.reverse(); // Best score first

            if results.is_empty() {
                return None
            }

            debug_assert!(self.children.is_none());
            self.children = Some(vec![]);

            //let mut smallest = Score::Win(0);
            let mut biggest = Score::Loss(0);
            
            for &(ref score, ref mv_str) in results.iter() {

                let children : &mut Vec<Self> = self.children.as_mut().unwrap();
                let mv = board.from_alg(&mv_str).unwrap();

                let child = OpeningTree { mv: Some(mv.clone()),
                                          eval: !*score,
                                          children: None };
                children.push(child);

                // If score for this move is too low, do not evaluate further
                // Instead, just use whatever score was returned by the shallower search
                if *score > MIN_SCORE {
                    let undo_move = board.do_move(mv);

                    let result =
                        if depth > 1 {
                            children.last_mut().unwrap().eval(depth - 1, board.clone(), table)
                        } else {
                            Some(*score)
                        };

                    board.undo_move(undo_move);
                    if let Some(val) = result {
                        if val > biggest { biggest = val }
                    }
                }
                else {
                    if *score > biggest {
                        biggest = *score;
                    }
                }
            }

            match table.lock() {
                Ok(mut table) => {
                    match table.entry(board.hash_board()) {
                        Entry::Occupied(mut entry) => match *entry.get() {
                            Transposition::Searched(score, entry_depth) => {
                                if entry_depth > depth { // If it has been searched deeper in the meantime
                                    info!("Position after {:?} at depth {} in thread {:?} has been serched to {} by another thread",
                                    self.mv, depth, thread::current().name(), entry_depth);
                                    self.eval = score;
                                    return Some(!score);
                                }
                                else {
                                    entry.insert(Transposition::Searched(biggest, depth));
                                }
                            },
                            Transposition::Searching => {
                                entry.insert(Transposition::Searched(biggest, depth));
                            }
                        },
                        Entry::Vacant(_) => {
                            error!("Found vacant entry after search. Should have been Occupied, but marked as searching")
                        },
                    }
                }
                Err(err) => {
                    error!("Poisoned transposition table mutex: {}", err)
                }
            }

            self.eval = biggest;
            Some(!biggest)
        }
        
        else {
            None
        }
    }
}

pub fn gen_sjadam_openings() {

    const DEPTH : Depth = 3;

    let moves = ["c1a7","c1c3","f1f3","h2h3","a2a3","b1b3","f1h7",
        "g1f3","c1a3","b1d3","h2h4","g1e3","g1g3","c1e3","f1d3","b1c3","h1h7",
        "f1h3","g1h3","c1a5","g1h5","b1a3","a2a4","b1a5","a1a7","f2f3","g1g4",
        "c1b4","c2c3","a1f3","f1h5","f1g4","b1b4","h1f3","a1c3","e2e3","h1c3",
        "a1a3","a1b3","h1b3","a1g3","h1g3","a1a5","h1h3","e2e4","c2c4","a1h3",
        "h1h5","h1a3","f1c4", "d2d3", "d2d4", "f2f4"];

    let table = Arc::new(Mutex::new(HashMap::new()));
    let mut board = SjadamBoard::start_board();

    let trees = moves.par_iter()
        .map(|mv| board.from_alg(mv).unwrap())
        .map(|mv: SjadamMove|
            {
                let mut table = table.clone();
                let mut tree = OpeningTree::new_root();
                tree.mv = Some(mv.clone());
                let mut board = SjadamBoard::start_board();
                board.do_move(mv.clone());

                println!("Started generating openings for {:?}", board.to_alg(&mv));

                let root_eval = tree.eval(DEPTH, board.clone(), &mut table);
                tree.sort_tree(&mut board);
                println!("Finished root eval for {:?}: {}",
                         board.to_alg(&mv), root_eval.unwrap().to_cp(!board.to_move()));

                assert_eq!(root_eval.unwrap(), tree.children.as_ref().unwrap()[0].eval,
                           "root-eval was {:?}, child-evals were {:?}",
                           root_eval, tree.children.as_ref().unwrap().iter().map(|ch| (ch.mv.clone(), ch.eval)).collect::<Vec<_>>());

                tree
    })
        .collect::<Vec<_>>();

    println!("Finished all trees");

    let mut final_tree = OpeningTree::new_root();

    final_tree.eval = !trees.iter().min_by_key(|ch: &&OpeningTree<SjadamBoard>| ch.eval).unwrap().eval;
    final_tree.children = Some(trees);
    final_tree.mv = None;

    final_tree.sort_tree(&mut board);

    let mut tree_file = fs::File::create(format!("opening_d{}_{}ms.txt", DEPTH, SEARCH_TIME_MS)).unwrap();
    final_tree.print_opening(&board, &mut tree_file);

    let mut epd_file = fs::File::create(format!("opening_d{}_{}ms.epd", DEPTH, SEARCH_TIME_MS)).unwrap();
    let mut pruned_epd_file = fs::File::create(format!("opening_pruned_d{}_{}ms.epd", DEPTH, SEARCH_TIME_MS)).unwrap();

    println!("Final eval: {:?}", final_tree.eval);
    final_tree.print_epd(&mut board, &mut vec![], 0, &mut epd_file);

    final_tree.prune(&mut board, &mut HashSet::new());
    final_tree.print_epd(&mut board, &mut vec![], 0, &mut pruned_epd_file);

    process::exit(0);

}
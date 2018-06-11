use search_algorithms::board::EvalBoard;
use search_algorithms::board::Color::*;
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

const SEARCH_TIME_MS : u64 = 1000;

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

    pub fn print_opening(&self, board: &mut B, pv: &mut Vec<String>) {
        let undo_move =
            if let Some(mv) = self.mv.as_ref() {
                pv.push(board.to_alg(mv));
                Some(board.do_move(mv.clone()))
            }
        else {
            None
        };
        let fen = board.to_fen().split_whitespace().take(4).collect::<Vec<&str>>().join(" ");
        
        println!("{} ce {}; id \"{}\"",
                 fen, self.eval.to_cp(board.to_move()), pv.join(" "));
        for child in self.children.as_ref()
            .unwrap_or(&vec![]) {
            child.print_opening(board, pv);
        }

        if let Some(undo_move) = undo_move {
            pv.pop();
            board.undo_move(undo_move);
        }
        
    }

    pub fn eval(&mut self, depth: u8, mut board: B) -> Option<Score> {
        if depth == 0 {
            return None
        }

        let multipv = if depth > 1 { 16 } else { 1 };

        let duration = time::Duration::from_millis(SEARCH_TIME_MS * (multipv as f64).sqrt() as u64);

        let mut options = EngineOptions::new();
        options.multipv = multipv;
        options.hash_memory = 128;
        
        let (handle, move_channel) =
            alpha_beta::start_uci_search(
                board.clone(),
                TimeRestriction::MoveTime(duration),
                options,
                Arc::new(Mutex::new(EngineComm::new())),
                None);
        
        if let Ok(mut results) = uci::get_uci_multipv(handle, move_channel) {

            results = results.iter()
                .cloned()
                .filter(|&(score, _)|
                    score > Score::Val(-2.0))
                .collect();
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
                
                let undo_move = board.do_move(mv);

                let result =
                    if depth > 1 {
                        children.last_mut().unwrap().eval(depth - 1, board.clone())
                    }
                else {
                    Some(*score)
                };
                board.undo_move(undo_move);
                if let Some(val) = result {
                    if val > biggest { biggest = val }
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

pub fn gen_from_startpos<B>(depth: u8)
    where B: EvalBoard + UciBoard + Debug + Sync + Send + Hash + Eq + 'static,
<B as EvalBoard>::Move: Sync + Send {
    gen_openings(B::start_board(), depth, "", &mut HashSet::new());
}

pub fn gen_openings<B>(mut board: B, depth: u8, pv: &str, transpositions: &mut HashSet<B>)
    where B: EvalBoard + UciBoard + Debug + Sync + Send + Hash + Eq + 'static,
<B as EvalBoard>::Move: Sync + Send {

    if depth == 0 {
        return;
    }
    let multipv = if depth > 2 { 24 } else { 1 };
    
    let mut options = EngineOptions::new();
    options.multipv = multipv;
    options.hash_memory = 1024;
    
    let (handle, move_channel) =
        alpha_beta::start_uci_search(board.clone(),
                                     TimeRestriction::MoveTime(
                                         time::Duration::from_millis(SEARCH_TIME_MS * multipv as u64)),
                                     options,
                                     Arc::new(Mutex::new(EngineComm::new())),
                                     None);
    
    match uci::get_uci_multipv(handle, move_channel) {
        Ok(mut results) => {
            
            match board.to_move() {
                White => results = results.iter()
                    .cloned()
                    .filter(|&(score, _)| score.to_cp(White) > i16::min(-100, -50 * depth as i16))
                    .collect(),
                Black => results = results.iter()
                    .cloned()
                    .filter(|&(score, _)| score.to_cp(Black) < i16::max(100, 50 * depth as i16))
                    .collect(),
            }
            
            for &(ref score, ref mv_str) in results.iter() {
                
                let mv = board.from_alg(&mv_str).unwrap();
                let mut new_pv = pv.to_string();
                new_pv.push(' ');
                new_pv.push_str(mv_str);
                let undo_move = board.do_move(mv);
                if !transpositions.contains(&board) {
                    transpositions.insert(board.clone());
                    println!("{}; ce {}; {}",
                             board.to_fen(), score.to_cp(board.to_move()), new_pv);
                    gen_openings(board.clone(), depth - 1, &new_pv, transpositions);
                }
                board.undo_move(undo_move);
            }
        },
        Err(err) => {
            println!("Got no results: {:?}", err);
        },
    }
}
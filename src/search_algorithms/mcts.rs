use search_algorithms::board::GameResult;
use search_algorithms::board::GameResult::*;
use search_algorithms::board::EvalBoard;
use search_algorithms::game_move::Move;
use search_algorithms::board::Color;

use rand;
use ordered_float::NotNaN;
use time;
use itertools::Itertools;
use std::f64;
use std::io;
use std::fmt;
use std::io::Write;

pub fn play_human<B: EvalBoard + fmt::Debug>(mut board: B) {
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
            if let Ok(human_move) = B::Move::from_alg(&input) {
                if board.all_legal_moves().contains(&human_move) {
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
        let start_time = time::get_time();
        
        let mut mctree = MonteCarloTree::new_root(&mut board);
        let mut searches = mctree.searches;
        let mut rng = rand::weak_rng();
        
        while time::get_time() < start_time + time::Duration::seconds(20) {
            use std::ops::Add;
            mctree.select(&mut board, searches, &mut rng);
            searches += 1;
            let searches_of_children = mctree.children.iter()
                .map(Option::as_ref).map(Option::unwrap)
                .map(|n| n.searches)
                .fold(0, u64::add);
            debug_assert!((searches as i64 - searches_of_children as i64).abs() <= 1,
                          format!("{} searches overall, but sum of searches of children is {} {}.",
                                  searches, searches_of_children, mctree.is_fully_expanded));
            
        }
        mctree.print_score(&board);
        
        let best_move_index = mctree.best_child().unwrap();
        let best_move = board.all_legal_moves()[best_move_index].clone();
        let best_node = mctree.children[best_move_index].take().unwrap();
        println!("{:?}\nAI played {:?} after {}/{} searches, with {:.2}% winning chance",
                 board, best_move,
                 best_node.searches,
                 searches,
                 100.0 * *best_node.score());
        board.do_move(best_move);
    }
    println!("{:?}\n{:?} won!", board, board.game_result());
}

#[derive(Clone, Debug, PartialEq)]
pub struct MonteCarloTree {
    // All the node's child nodes. 
    pub children: Vec<Option<MonteCarloTree>>, 
    pub score: Score,
    pub value: f64,
    pub searches: u64,
    is_fully_expanded: bool,
    maximizing: bool,
}

impl MonteCarloTree {
    pub fn new_root<B: EvalBoard + fmt::Debug>(board : &mut B) -> Self {
        
        let mut root = MonteCarloTree { children: vec![None; board.all_legal_moves().len()],
                                        score: Score::new(), value: 0.0, searches: 0,
                                        is_fully_expanded: false,
                                        maximizing: 
                                        if board.to_move() == Color::Black {
                                            true 
                                        } 
                                        else { 
                                            false 
                                        }
        };
        let mut rng = rand::weak_rng();
        while !root.is_fully_expanded {
            root.expand(board, &mut rng);
            if root.searches > 10000 {
                panic!("Failed to fully expand root node after 10000 searches")
            }
        }
        root
    }

    pub fn new_child(&self, num_of_children: usize) -> Self {
        MonteCarloTree { children: vec![None; num_of_children], score: Score::new(),
               value: 0.0, searches: 0, is_fully_expanded: false, 
               maximizing: !self.maximizing }
    }

    pub fn score(&self) -> NotNaN<f64> {
        NotNaN::new(self.value / self.searches as f64).unwrap_or(NotNaN::new(0.5).unwrap())
    }

    /// Increments the search counter for the node, and adds 1.0, 0.5 or 0.0 to the value,
    /// depending on the given result
    fn add_value(&mut self, result: GameResult) {
        // TODO: Maybe do not count a draw as a search
        self.searches += 1;
        match result {
            BlackWin => {
                self.value += 1.0;
                self.score.black_wins += 1;
            },
            WhiteWin => {
                self.score.white_wins += 1;
            },
            Draw => {
                self.value += 0.5;
                self.score.draws += 1;
            }
        }
    }

    /// Returns the index of the best child node
    pub fn best_child(&self) -> Option<usize> {
        let iter = self.children.iter()
            .filter(|&opt| opt.is_some())
            .map(Option::as_ref).map(Option::unwrap)
            .enumerate();
        if self.maximizing {
            iter.max_by_key(|&(_, child)| child.score()).map(|(n, _)| n)
        }
        else {
            iter.min_by_key(|&(_, child)| child.score()).map(|(n, _)| n)
        }
    }

    pub fn print_score<B: EvalBoard + fmt::Debug>(&self, board: &B) {
        for (node, go_move) in self.children.iter()
            .map(Option::as_ref)
            .filter(Option::is_some)
            .map(Option::unwrap)
            .zip(board.all_legal_moves())
            
            .sorted_by(|&(node1, _), &(node2, _)| {
                let cmp = node1.score().cmp(&node2.score());
                if self.maximizing { cmp.reverse() }
                else { cmp }
            })
        {
            let mut board_after_move = board.clone();
            board_after_move.do_move(go_move.clone());
            if let Some(best_reply_index) = node.best_child() {
                    let best_reply = board_after_move.all_legal_moves()[best_reply_index].clone();
                    if let Some(ref best_reply_node) = node.children[best_reply_index] {
                        
                        println!("Move {:?} scores {:.2}% ({}/{}/{}), {} searches, best reply {:?} with {:.2}% ({}/{}/{}) after {} searches", 
                                 go_move, 100.0 * *node.score(),
                                 node.score.white_wins, node.score.draws, node.score.black_wins,
                                 node.searches, 
                                 best_reply, 100.0 * *best_reply_node.score(),
                                 best_reply_node.score.white_wins, best_reply_node.score.draws,
                                 best_reply_node.score.black_wins, best_reply_node.searches);
                        if node.searches > 5000 {
                            println!("Children//:");
                            node.print_score(&board_after_move);
                            println!("//children");
                        }                 
                        continue;
                    }
            }
            if node.searches >= 10 {
                println!("Move {:?} scores {:.2}%, {} searches, best reply not found", 
                     go_move, 100.0 * *node.score(), node.searches );
            }
        }
    }

    /// Returns a measure of how attractive this node is to explore further
    /// Nodes with good scores but less searches are more attractive
    /// Higher is always better, even when minimzing
    pub fn move_selection_value(&self, total_searches: u64) -> NotNaN<f64> {
        if self.searches == 0 {
            if self.maximizing { NotNaN::new(f64::MAX).unwrap() }
            else { NotNaN::new(f64::MIN).unwrap() }
        }
        else {
            let adjusted_score = if self.maximizing { 1.0 - *self.score() } else { *self.score() };
            NotNaN::new(adjusted_score + 
                        (2.0f64).sqrt() * (total_searches as f64).ln() / self.searches as f64)
                .unwrap()
        }
    }

    // Select the node. If the selected child is not fully expanded, expand it
    pub fn select<B, R> (&mut self, board: &mut B, total_searches: u64, rng: &mut R) -> GameResult
        where B: EvalBoard + fmt::Debug, R: rand::Rng
    {
        assert!(self.is_fully_expanded, "Tried to select node that wasn't fully expanded");
        match board.game_result() {
            Some(result) => {
                self.add_value(result);
                return result
            }
            None => (),
        }
        
        use std::ops::Add;
        let searches_of_children = self.children.iter()
            .map(Option::as_ref).map(Option::unwrap)
            .map(|n| n.searches)
            .fold(0, u64::add);
        debug_assert!((self.searches as i64 - searches_of_children as i64).abs() <= 1,
                      format!("{} searches in total, but sum of searches of children is {}\n{:?}",
                              self.searches, searches_of_children, self.children));
        
        let moves = board.all_legal_moves();  
        // Find the index of the child node with highest selection value
        let child_index : usize = self.children.iter_mut()
            .map(Option::as_mut).map(Option::unwrap)
            .enumerate()
            .map(|(n, child)| (n, child.move_selection_value(total_searches), child))
            .max_by_key(|&(_, move_value, _)| move_value)
            .map(|(n, _, _)| n).unwrap();
        
        let undo_move = board.do_move(moves[child_index].clone());
        let value;
        {
            let child : &mut MonteCarloTree = self.children[child_index].as_mut().unwrap();
            
            if child.is_fully_expanded {
                value = child.select(board, total_searches, rng);
            }
            else {
                value = child.expand(board, rng);
            }
        }
        board.undo_move(undo_move);
        self.add_value(value);
        value
    }

    // Expand this node, creating a new child
    pub fn expand <B, R> (&mut self, board: &mut B, rng: &mut R) -> GameResult
        where B: EvalBoard + fmt::Debug, R: rand::Rng
    {
        match board.game_result() {
            None => {
                debug_assert!(self.children.contains(&None));
                let moves = board.all_legal_moves();
                assert!(moves.len() == self.children.len(),
                        format!("{} moves available, but tree has {} children\n{:?}",
                                moves.len(), self.children.len(), board));
                
                let mut random_index = rand::random::<usize>() % moves.len();
                loop {
                    if self.children[random_index] == None {
                        let undo_move = board.do_move(moves[random_index].clone());

                        let value;
                        /*
                        if let Some(winner) = board.game_result() {
                            if winner == Color::Black {
                                value = 1.0;
                            }
                            else {
                                value = 0.0;
                            }
                            board.undo_move(undo_move);
                            self.is_fully_expanded = true;
                            debug_assert!(board == &mut old_board);
                        }
                        else
                         */ 
                        let child_moves = board.all_legal_moves();
                        self.children[random_index] = Some(
                            self.new_child(child_moves.len()));
                        if !self.children.contains(&None) {
                            self.is_fully_expanded = true;
                        }
                        {
                            let new_child = &mut self.children[random_index].as_mut().unwrap();
                            //value = new_child.simulate(board.clone(), rng);
                            value = Self::simulate(board.clone(), rng);
                            
                            new_child.add_value(value);
                        }
                        board.undo_move(undo_move);
                        self.add_value(value);
                        return value;
                    }
                    else {
                        random_index = rand::random::<usize>() % moves.len();
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

    fn simulate <B, R> (mut board: B, rng: &mut R) -> GameResult
        where B: EvalBoard + fmt::Debug, R: rand::Rng
    {
        match board.game_result() { 
            Some(result) => return result,
            None => (),
        }
        board.do_random_move(rng);
        Self::simulate(board, rng)
    }
    
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
}

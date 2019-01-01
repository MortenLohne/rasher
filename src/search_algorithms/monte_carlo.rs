use uci_engine::UciEngine;
use uci;
use NodeCount;
use std::time;
use std::sync::Arc;
use std::sync::Mutex;
use uci_engine::UciOption;
use uci::TimeRestriction;
use uci::EngineComm;
use search_algorithms::board::Board;
use uci::UciInfo;
use search_algorithms::board::EvalBoard;
use search_algorithms::board::Color;
use search_algorithms::board::Color::*;
use search_algorithms::board::GameResult::*;
use search_algorithms::board::GameResult;
use std::ops::Add;
use time::Duration;

extern crate ordered_float;
use ordered_float::OrderedFloat;
use rand::Rng;
use std::ops::Not;
use search_algorithms::alpha_beta;
use std::fmt::Debug;
use search_algorithms::alpha_beta::AlphaBeta;
use search_algorithms::board::ExtendedBoard;
use pgn::PgnBoard;
use std::hash::Hash;
use uci_engine::UciOptionType;

const TIME_MS: Duration = Duration::from_millis(60000);
const INC_MS: Duration = Duration::from_millis(600);

pub struct MonteCarlo<B: Board> {
    root: MonteCarloTree<B>,
    time_limit: uci::TimeRestriction,
    start_time: time::Instant,
    engine_comm : Arc<Mutex<EngineComm>>,
}

impl<B> UciEngine<B> for MonteCarlo<B>
where B: ExtendedBoard + PgnBoard + Debug + Hash + Eq + 'static {
    fn init() -> Self {
        MonteCarlo {
            root: MonteCarloTree::new_root(&mut B::start_board()),
            time_limit: TimeRestriction::Infinite,
            engine_comm: Arc::new(Mutex::new(EngineComm::new())),
            start_time: time::Instant::now(),
        }
    }

    fn uci_options(&mut self) -> Vec<UciOption> {
        vec![]
    }

    fn set_uci_option(&mut self, _: UciOption) {}

    fn search(mut self, mut board: B, time_limit: TimeRestriction, engine_comm: Arc<Mutex<EngineComm>>,
              move_list: Option<Vec<<B as Board>::Move>>) -> Box<Iterator<Item=UciInfo<B>>> {

        self.root = MonteCarloTree::new_root(&mut board);
        self.time_limit = time_limit;
        self.engine_comm = engine_comm;
        self.start_time = time::Instant::now();


        Box::new(DepthIterator { monte_carlo: self, root_board: board })
    }
}

struct DepthIterator<B: Board> {
    monte_carlo: MonteCarlo<B>,
    root_board: B,
}

impl<B> Iterator for DepthIterator<B>
where B: ExtendedBoard + PgnBoard + Debug + Hash + Eq + 'static {
    type Item = UciInfo<B>;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        for _ in 0..100 {
            self.monte_carlo.root.select(self.root_board.clone());
        }

        let root = &self.monte_carlo.root;
        println!("Root: {}, static winrate: {}, true winrate: {}, score: {:?}, exploration: {}",
                 root.static_eval, cp_to_win_pct(root.static_eval),
                 root.score.win_rate(),
                 root.score, root.expl_value(root.searches()));

        let (mv, child) = root.children.as_ref().unwrap().iter()
            .min_by_key(|&(_, ref child)| OrderedFloat(child.score.win_rate()))
            .unwrap();

        for (mv, child) in root.children.as_ref().unwrap() {
            println!("\tChild {:?}: {}, static winrate: {}, true winrate: {}, score: {:?}, exploration: {}",
                     mv,
                     child.static_eval, cp_to_win_pct(child.static_eval),
                     child.score.win_rate(),
                     child.score, child.expl_value(root.searches()));
        }
        println!();

        Some(UciInfo {
            color: self.root_board.side_to_move(),
            depth: 0,
            seldepth: 0,
            time: 0,
            nodes: root.searches(),
            hashfull: 0.0,
            pvs: vec![(alpha_beta::Score::Val(win_pct_to_cp(root.score.win_rate())), vec![mv.clone()])]
        })
    }
}

#[derive(Debug)]
struct MonteCarloTree<B: Board> {
    children: Option<Vec<(B::Move, MonteCarloTree<B>)>>,
    score: Score,
    static_eval: f32,
    side_to_move: Color,
}

impl<B> MonteCarloTree<B>
where B: ExtendedBoard + PgnBoard + Debug + Hash + Eq + 'static {
    fn new_root(board: &mut B) -> MonteCarloTree<B> {
        MonteCarloTree {
            children: None,
            score: Score::new(),
            static_eval: board.static_eval(),
            side_to_move: board.side_to_move(),
        }
    }

    fn searches(&self) -> u64 {
        self.score.wins + self.score.losses + self.score.draws
    }

    fn expl_value(self: &MonteCarloTree<B>, parent_searches: u64) -> f64 {
        let exploitation = (self.score.draws as f64 * 0.5 + self.score.losses as f64) / (self.searches() as f64 + 1.0);
        let exploration = 1.42 * f64::sqrt(
            f64::ln(parent_searches as f64 + 1.0) / (self.searches() as f64 + 1.0));
        exploitation + exploration
    }

    fn select(&mut self, mut board: B) -> Score {
        let searches = self.searches();
        if let Some(result) = board.game_result() {
            let mut score = Score::new();
            match (result, board.side_to_move()) {
                (WhiteWin, White) | (BlackWin, Black) => score.wins += 1,
                (WhiteWin, Black) | (BlackWin, White) => score.losses += 1,
                (Draw, _) => score.draws += 1,
            }
            return score;
        }
        if let Some(children) = self.children.as_mut() {

            let child_index = (0..children.len())
                .max_by_key(|&i| OrderedFloat(children[i].1.expl_value(searches)))
                .unwrap().clone();

            let (ref mut mv, ref mut child) = children.get_mut(child_index).unwrap();
            board.do_move(mv.clone());

            let result = !child.select(board);

            self.score = self.score + result;
            return result;
        }

        return self.expand(board);
    }

    fn expand(&mut self, mut board: B) -> Score {
        let mut moves = vec![];
        board.generate_moves(&mut moves);

        let children = moves.into_iter()
            .map(|mv| {
                let reverse_move = board.do_move(mv.clone());
                let child = MonteCarloTree {
                    children: None,
                    score: Score::new(),
                    static_eval: board.static_eval() * board.side_to_move().multiplier() as f32,
                    side_to_move: board.side_to_move(),
                };
                board.reverse_move(reverse_move);
                (mv, child)
            })
            .collect::<Vec<_>>();
        {
            let best_child = children.iter()
                .max_by_key(|child| OrderedFloat(child.1.static_eval))
                .unwrap();
            board.do_move(best_child.0.clone());
        }

        self.children = Some(children);
        let rng = rand::weak_rng();
        let result = !simulate(board, rng);
        self.score = self.score + result;

        result
    }
}

fn simulate<B: EvalBoard, R: Rng>(mut board: B, mut rng: R) -> Score {
    if let Some(result) = board.game_result() {
        let mut score = Score::new();
        match (result, board.side_to_move()) {
            (WhiteWin, White) | (BlackWin, Black) => score.wins += 1,
            (WhiteWin, Black) | (BlackWin, White) => score.losses += 1,
            (Draw, _) => score.draws += 1,
        }
        score
    }
    else {
        let mut moves = vec![];
        board.generate_moves(&mut moves);
        let best_move = moves.into_iter()
            .map(|mv| {
                let reverse_move = board.do_move(mv.clone());
                let eval = board.static_eval() * board.side_to_move().multiplier() as f32 + rng.next_f32();
                board.reverse_move(reverse_move);
                (eval, mv)
            })
            .max_by_key(|(eval, _)| OrderedFloat(*eval))
            .unwrap().1;
        board.do_move(best_move);
        !simulate(board, rng)
    }
}

fn win_pct_to_cp(pct: f32) -> f32 {
    290.680623072 * f32::tan(3.096181612 * (pct - 0.5))
}

fn cp_to_win_pct(cp: f32) -> f32 {
    (f32::atan(cp / 290.680623072) / 3.096181612 + 0.5)
}

#[derive(PartialEq, Clone, Debug, Copy)]
pub struct Score {
    pub wins: u64,
    pub losses: u64,
    pub draws : u64,
}

impl Score {
    pub fn new() -> Self {
        Score { wins: 0, losses: 0, draws: 0 }
    }

    /// Creates a score object containing a single result
    pub fn from_game_result(result: &GameResult) -> Self {
        let mut score = Score::new();
        match *result {
            WhiteWin => score.wins += 1,
            BlackWin => score.losses += 1,
            Draw => score.draws += 1,
        }
        score
    }

    pub fn win_rate(&self) -> f32 {
        (self.wins as f32 + self.draws as f32 * 0.5) / (self.wins + self.draws + self.losses) as f32
    }
}

impl Add for Score {
    type Output = Score;

    fn add(self, rhs: Score) -> <Self as Add<Score>>::Output {
        Score {
            wins: self.wins + rhs.wins,
            losses: self.losses + rhs.losses,
            draws: self.draws + rhs.draws,
        }
    }
}

impl Not for Score {
    type Output = Self;

    fn not(self) -> <Self as Not>::Output {
        Score {
            wins: self.losses,
            losses: self.wins,
            draws: self.draws,
        }
    }
}
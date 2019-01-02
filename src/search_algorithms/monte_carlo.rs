use uci_engine::UciEngine;
use uci;
use std::sync::Arc;
use std::sync::Mutex;
use uci_engine::UciOption;
use uci::TimeRestriction;
use uci::EngineComm;
use search_algorithms::board::Board;
use uci::UciInfo;
use search_algorithms::board::Color;
use search_algorithms::board::Color::*;
use search_algorithms::board::GameResult::*;
use search_algorithms::board::GameResult;
use std::ops::Add;
use time::Duration;

extern crate ordered_float;
use ordered_float::OrderedFloat;
use std::ops::Not;
use search_algorithms::alpha_beta;
use std::fmt::Debug;
use search_algorithms::alpha_beta::AlphaBeta;
use search_algorithms::board::ExtendedBoard;
use pgn::PgnBoard;
use std::hash::Hash;
use uci_engine::UciOptionType;
use uci::TimeInfo;
use std::time::Instant;

const GAME_TIME: Duration = Duration::from_millis(60000);
const INC_MS: Duration = Duration::from_millis(600);

const EVAL_DEPENDENCE : f32 = 20.0;

pub struct MonteCarlo<B: Board> {
    root: MonteCarloTree<B>,
    time_limit: uci::TimeRestriction,
    start_time: Instant,
    engine_comm : Arc<Mutex<EngineComm>>,
}

impl<B> UciEngine<B> for MonteCarlo<B>
where B: ExtendedBoard + PgnBoard + Debug + Hash + Eq + 'static {
    fn init() -> Self {
        MonteCarlo {
            root: MonteCarloTree::new_root(&mut B::start_board()),
            time_limit: TimeRestriction::Infinite,
            engine_comm: Arc::new(Mutex::new(EngineComm::new())),
            start_time: Instant::now(),
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
        self.start_time = Instant::now();


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
        for _ in 0..10 {
            let mut game = vec![];
            let result = self.monte_carlo.root.select(self.root_board.clone(), &mut game);

            let mut board = self.root_board.clone();
            for (i, mv) in game.into_iter().enumerate() {
                if i % 2 == 0 {
                    print!("{}. {} ", i / 2 + 1, board.move_to_san(&mv));
                }
                else {
                    print!("{} ", board.move_to_san(&mv));
                }
                board.do_move(mv);
            }
            println!(" {:?}", result);
        }

        let root = &self.monte_carlo.root;
        println!("Root: {}, static winrate: {}, true winrate: {}, score: {:?}, exploration: {}",
                 root.static_eval, eval_to_win_pct(root.static_eval),
                 root.score.win_rate(),
                 root.score, root.expl_value(root.searches()));

        let (mv, _) = root.children.as_ref().unwrap().iter()
            .min_by_key(|&(_, ref child)| OrderedFloat(child.score.win_rate()))
            .unwrap();

        for (mv, child) in root.children.as_ref().unwrap() {
            println!("\tChild {:?}: {}, static winrate: {}, true winrate: {}, score: {:?}, exploration: {}",
                     mv,
                     child.static_eval, eval_to_win_pct(child.static_eval),
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
            pvs: vec![(alpha_beta::Score::Val(win_pct_to_eval(root.score.win_rate())), vec![mv.clone()])]
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

    fn expl_value(self: &MonteCarloTree<B>, parent_searches: u64) -> f32 {

        let extra_losses = EVAL_DEPENDENCE - eval_to_win_pct(self.static_eval) * EVAL_DEPENDENCE;

        let exploitation = (self.score.draws as f32 * 0.5 + self.score.losses as f32 + extra_losses)
            / (self.searches() as f32 + EVAL_DEPENDENCE);

        let exploration = 1.42 * f32::sqrt(
            f32::ln(parent_searches as f32 + 1.0) / (self.searches() as f32 + EVAL_DEPENDENCE));
        exploitation + exploration
    }

    fn select(&mut self, mut board: B, game: &mut Vec<B::Move>) -> Score {
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
            game.push(mv.clone());

            let result = !child.select(board, game);

            self.score = self.score + result;
            return result;
        }

        return self.expand(board, game);
    }

    fn expand(&mut self, mut board: B, game: &mut Vec<B::Move>) -> Score {
        let mut moves = vec![];
        board.generate_moves(&mut moves);

        let mut engine: AlphaBeta<B> = AlphaBeta::init();

        engine.set_uci_option(UciOption {
            name: "multipv".to_string(),
            option_type: UciOptionType::Spin(moves.len() as i64, 1, 1)
        });

        engine.set_uci_option(UciOption {
            name: "hash".to_string(),
            option_type: UciOptionType::Spin(32, 1, 1)
        });

        let results = engine.best_moves_multipv(
            board.clone(),
            TimeRestriction::MoveTime(INC_MS * f32::sqrt(moves.len() as f32).ceil() as u32),
        None).unwrap();

        let children = results.into_iter()
            .map(|(score, mv)| {
                let reverse_move = board.do_move(mv.clone());
                let child = MonteCarloTree {
                    children: None,
                    score: Score::new(),
                    static_eval: board.side_to_move().multiplier() as f32 * score.to_value(White), // Always get the score from white's perspective, then correct it
                    side_to_move: board.side_to_move(),
                };
                board.reverse_move(reverse_move);
                (mv, child)
            })
            .collect::<Vec<_>>();
        {
            let (mv, _) = children.get(0).unwrap();
            board.do_move(mv.clone());
            game.push(mv.clone());
        }

        self.children = Some(children);
        let result = !simulate(board, game);
        self.score = self.score + result;

        result
    }
}

fn simulate<B>(mut board: B, game: &mut Vec<B::Move>) -> Score
    where B: ExtendedBoard + PgnBoard + Debug + Hash + Eq + 'static {

    let mut white_time = GAME_TIME;
    let mut black_time = GAME_TIME;

    loop {
        if let Some(result) = board.game_result() {
            let mut score = Score::new();
            match (result, board.side_to_move()) {
                (WhiteWin, White) | (BlackWin, Black) => score.wins += 1,
                (WhiteWin, Black) | (BlackWin, White) => score.losses += 1,
                (Draw, _) => score.draws += 1,
            }
            return score;
        }

        let turn_start = Instant::now();

        let mut engine: AlphaBeta<B> = AlphaBeta::init();

        engine.set_uci_option(UciOption {
            name: "hash".to_string(),
            option_type: UciOptionType::Spin(16, 1, 1)
        });

        let time_info = TimeInfo {
            white_time,
            black_time,
            white_inc: INC_MS,
            black_inc: INC_MS,
            moves_to_go: None
        };

        let (_, mv) = engine.best_move(
            board.clone(),
            TimeRestriction::GameTime(time_info),
            None)
            .unwrap(); // TODO: handle errors

        let time_elapsed = Instant::now() - turn_start;

        match board.side_to_move() {
            White if white_time < time_elapsed =>
                return Score::from_game_result(&BlackWin),
            Black if black_time < time_elapsed =>
                return Score::from_game_result(&WhiteWin),
            White => {
                white_time -= time_elapsed;
                white_time += INC_MS;
            },
            Black => {
                black_time -= time_elapsed;
                black_time += INC_MS;
            },
        }

        game.push(mv.clone());
        board.do_move(mv);
    }
}

pub fn win_pct_to_eval(pct: f32) -> f32 {
    2.90680623072 * f32::tan(3.096181612 * (pct - 0.5))
}

pub fn eval_to_win_pct(cp: f32) -> f32 {
    (f32::atan(cp / 2.90680623072) / 3.096181612 + 0.5)
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
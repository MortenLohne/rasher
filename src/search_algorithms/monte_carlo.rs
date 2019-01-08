use uci_engine::UciEngine;
use uci;
use std::sync::Arc;
use std::sync::Mutex;
use uci_engine::UciOption;
use uci::TimeRestriction;
use uci::EngineComm;
use search_algorithms::board::Board;
use uci::UciInfo;
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
use std::sync::RwLock;
use rayon::iter::IntoParallelIterator;
use rayon::iter::ParallelIterator;
use std::io::BufWriter;
use std::fs::OpenOptions;
use std::io::Write;
use serde::Serialize;
use serde::de::DeserializeOwned;

const GAME_TIME: Duration = Duration::from_millis(30000);
const INC_MS: Duration = Duration::from_millis(300);

const EVAL_DEPENDENCE : f32 = 16.0;

pub struct MonteCarlo<B: Board>
    where B::Move: Serialize, B::Move: DeserializeOwned {
    root: MonteCarloTree<B>,
    time_limit: uci::TimeRestriction,
    start_time: Instant,
    engine_comm : Arc<Mutex<EngineComm>>,
}

impl<B> UciEngine<B> for MonteCarlo<B>
where B: ExtendedBoard + PgnBoard + Debug + Hash + Eq + 'static + Sync + Serialize,
      B::Move: Send + Sync + Serialize, B::Move: DeserializeOwned {
    fn init() -> Self {
        MonteCarlo {
            root: MonteCarloTree::new_root(&mut B::start_board()),
            time_limit: TimeRestriction::Infinite,
            engine_comm: Arc::new(Mutex::new(EngineComm::new())),
            start_time: Instant::now(),
        }
    }

    fn uci_options(&mut self) -> Vec<UciOption> {
        vec![UciOption { name: "opening_tree".to_string(),
            option_type: UciOptionType::String("".to_string()) } ]
    }

    fn set_uci_option(&mut self, _: UciOption) {}

    fn search(mut self, mut board: B, time_limit: TimeRestriction, engine_comm: Arc<Mutex<EngineComm>>,
              _: Option<Vec<<B as Board>::Move>>) -> Box<Iterator<Item=UciInfo<B>>> {

        self.root = MonteCarloTree::new_root(&mut board);
        self.time_limit = time_limit;
        self.engine_comm = engine_comm;
        self.start_time = Instant::now();

        Box::new(DepthIterator { monte_carlo: self, root_board: board })
    }
}

struct DepthIterator<B: Board>
    where B::Move: Serialize, B::Move: DeserializeOwned{
    monte_carlo: MonteCarlo<B>,
    root_board: B,
}

impl<B> Iterator for DepthIterator<B>
where B: ExtendedBoard + PgnBoard + Debug + Hash + Eq + 'static + Sync + Serialize,
      B::Move: Send + Sync + Serialize, B::Move: DeserializeOwned{
    type Item = UciInfo<B>;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {

        {
            let engine_comm = self.monte_carlo.engine_comm.lock().unwrap();
            if engine_comm.engine_should_stop {
                return None // "Engine was told to stop"
            }
        }

        let mut open_options = OpenOptions::new();
        open_options.append(true).create(true);
        let pgn_file = open_options.open("sjadam_book.pgn").unwrap();
        let pgn_writer = Mutex::new(BufWriter::new(pgn_file));

        (0..10).into_par_iter().for_each(|i: i32| {
            let mut game = vec![];
            let score = self.monte_carlo.root.select(self.root_board.clone(), &mut game);

            let mut board = self.root_board.clone();

            let result = match board.side_to_move() {
                _ if score.draws == 1 => Some(Draw),
                White if score.wins == 1 => Some(WhiteWin),
                White if score.losses == 1 => Some(BlackWin),
                Black if score.wins == 1 => Some(BlackWin),
                Black if score.losses == 1 => Some(WhiteWin),
                _ => None
            };
            {
                let mut writer = pgn_writer.lock().unwrap();
                board.clone().game_to_pgn(&game, "", "", "????.??.??", &i.to_string(),
                                  "rasher", "rasher", result, &[],
                                  &mut *writer).unwrap();
                (*writer).flush().unwrap();
            }


            for (i, mv) in game.into_iter().enumerate() {
                if i == 0 && board.side_to_move() == Black {
                    print!("{}... {} ", 1, board.move_to_san(&mv));
                }
                else if board.side_to_move() == White {
                    print!("{}. {} ", (i + 1) / 2 + 1, board.move_to_san(&mv));
                }
                else {
                    print!("{} ", board.move_to_san(&mv));
                }
                board.do_move(mv);
            }
            println!(" {:?}", score);
        });

        let serialized = serde_json::to_string(&self.monte_carlo.root).unwrap();
        println!("{}", serialized);

        let root = &self.monte_carlo.root;
        let root_score = root.score.lock().unwrap();
        let root_children = root.children.read().unwrap();
        println!("Root: {}, static winrate: {}, true winrate: {}, score: {:?}",
                 root.static_eval, eval_to_win_pct(root.static_eval),
                 root_score.win_rate(),
                 *root_score);

        let (mv, _) = root_children.as_ref().unwrap().iter()
            .min_by_key(|&(_, child)| OrderedFloat(child.score.lock().unwrap().win_rate()))
            .unwrap();

        for (mv, child) in root_children.as_ref().unwrap() {
            let child_win_rate = { child.score.lock().unwrap().win_rate() };
            println!("\tChild {:?}: {}, static winrate: {}, true winrate: {}, score: {:?}, exploration: {}",
                     mv,
                     child.static_eval, eval_to_win_pct(child.static_eval),
                     child_win_rate,
                     child.score, child.expl_value(root_score.searches()));
        }
        println!();

        Some(UciInfo {
            color: self.root_board.side_to_move(),
            depth: 0,
            seldepth: 0,
            time: 0,
            nodes: root_score.searches(),
            hashfull: 0.0,
            pvs: vec![(alpha_beta::Score::Val(win_pct_to_eval(root_score.win_rate())), vec![mv.clone()])]
        })
    }
}

type MonteCarloChild<B> = (<B as Board>::Move, MonteCarloTree<B>);

#[derive(Debug, Serialize, Deserialize)]
struct MonteCarloTree<B>
where B: Board, B::Move: Serialize, B::Move: DeserializeOwned {
    children: RwLock<Option<Vec<(MonteCarloChild<B>)>>>,
    score: Mutex<Score>,
    static_eval: f32,
}

impl<B> MonteCarloTree<B>
where B: ExtendedBoard + PgnBoard + Debug + Hash + Eq + 'static,
      B::Move: Serialize, B::Move: DeserializeOwned {
    fn new_root(board: &mut B) -> MonteCarloTree<B> {
        MonteCarloTree {
            children: RwLock::new(None),
            score: Mutex::new(Score::new()),
            static_eval: board.static_eval(),
        }
    }

    fn expl_value(self: &MonteCarloTree<B>, parent_searches: u64) -> f32 {

        let score = self.score.lock().unwrap();

        let extra_losses = EVAL_DEPENDENCE - eval_to_win_pct(self.static_eval) * EVAL_DEPENDENCE;

        let exploitation = (score.draws as f32 * 0.5 + score.losses as f32 + extra_losses)
            / (score.searches() as f32 + EVAL_DEPENDENCE);

        let exploration = 1.42 * f32::sqrt(
            f32::ln(parent_searches as f32 + 1.0) / (score.searches() as f32 + EVAL_DEPENDENCE));
        exploitation + exploration
    }

    fn select(&self, mut board: B, game: &mut Vec<B::Move>) -> Score {

        if let Some(result) = board.game_result() {
            let mut score = Score::new();
            match (result, board.side_to_move()) {
                (WhiteWin, White) | (BlackWin, Black) => score.wins += 1,
                (WhiteWin, Black) | (BlackWin, White) => score.losses += 1,
                (Draw, _) => score.draws += 1,
            }
            return score;
        }

        { // Immediately give the position an extra win, so that it is less likely to be simultaneously selected by other threads
            self.score.lock().unwrap().wins += 1;
        }

        let searches = {
            self.score.lock().unwrap().searches().clone()
        };

        if let Some(children) = self.children.read().unwrap().as_ref() {

            let child_index = (0..children.len())
                .max_by_key(|&i| OrderedFloat(children[i].1.expl_value(searches)))
                .unwrap().clone();

            let (ref mv, ref child) = children.get(child_index).unwrap();
            board.do_move(mv.clone());
            game.push(mv.clone());

            let result = !child.select(board, game);

            let mut score = self.score.lock().unwrap();
            (*score).wins -= 1; // Remove the extra win added earlier
            *score = *score + result;
            return result;
        }

        let result =  self.expand(board, game);

        // Remove the extra win added earlier
        self.score.lock().unwrap().wins -= 1;

        result
    }

    fn expand(&self, mut board: B, game: &mut Vec<B::Move>) -> Score {
        let mut moves = vec![];
        board.generate_moves(&mut moves);
        {
            if let Ok(mut self_children) = self.children.try_write() {

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
                        let child = MonteCarloTree {
                            children: RwLock::new(None),
                            score: Mutex::new(Score::new()),
                            static_eval: match score {
                                alpha_beta::Score::Val(val) => -val,
                                alpha_beta::Score::Draw(_) => 0.0,
                                alpha_beta::Score::Win(_) => -100.0,
                                alpha_beta::Score::Loss(_) => 100.0,
                            }
                        };
                        (mv, child)
                    })
                    .collect::<Vec<_>>();

                *self_children = Some(children);

                let (ref mv, ref child) = self_children.as_ref().unwrap()[0];

                board.do_move(mv.clone());
                game.push(mv.clone());

                // Immediately give the position an extra win, so that it is less likely to be simultaneously selected by other threads
                (*child.score.lock().unwrap()).wins += 1;
            }
            else {
                info!("Abort expansion of {}", board.to_fen());
                return self.select(board, game);
            }
        }

        let result = !simulate(board, game);
        {
            self.score.lock().map(|mut score| *score = *score + result).unwrap();
        }
        let children = self.children.read();
        let child = &children.as_ref().unwrap().as_ref().unwrap().get(0).unwrap().1;
        child.score.lock().map(|mut score| {
            (*score).wins -= 1; // Remove the extra win added earlier
            *score = *score + !result
        }).unwrap();

        result
    }
}

fn simulate<B>(mut board: B, game: &mut Vec<B::Move>) -> Score
    where B: ExtendedBoard + PgnBoard + Debug + Hash + Eq + 'static {

    let start_color = board.side_to_move();

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
            println!("{:?} ({} to move) on the board", result, board.side_to_move());
            return if board.side_to_move() == start_color { score } else { !score };
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
            White if white_time < time_elapsed => {
                println!("White wins on time");
                return Score::from_game_result(&BlackWin)
            },
            Black if black_time < time_elapsed => {
                println!("Black wins on time");
                return Score::from_game_result(&WhiteWin)
            },
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

#[derive(PartialEq, Clone, Debug, Copy, Serialize, Deserialize)]
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
        (self.wins as f32 + self.draws as f32 * 0.5) / (self.searches()) as f32
    }

    pub fn searches(&self) -> u64 {
        self.wins + self.draws + self.losses
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
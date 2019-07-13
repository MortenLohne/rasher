use uci;
use board_game_traits::board::Board;
use std::sync::mpsc;
use std::sync::Arc;
use std::sync::Mutex;
use uci::EngineComm;
use search_algorithms::alpha_beta::Score;
use std::error;
use uci::UciInfo;
use std::thread;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct UciOption {
    pub name: String,
    pub option_type: UciOptionType,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum UciOptionType {
    Check(bool),
    Spin(i64, i64, i64), // Contains current value, minimum value, maximum value
    Combo(String, Vec<String>), // Contains current value, predefined values
    Button,
    String(String), // Contains the current value
}

pub trait UciEngine<B: Board>
    where Self: Sized {
    fn init() -> Self;

    fn uci_options(&mut self) -> Vec<UciOption>;

    fn set_uci_option(&mut self, uci_option: UciOption);

    fn search(self, board: B, time_limit: uci::TimeRestriction,
              engine_comm: Arc<Mutex<EngineComm>>, move_list: Option<Vec<B::Move>>)
    -> Box<Iterator<Item=UciInfo<B>>>;

    fn best_move(self, board: B, time_limit: uci::TimeRestriction,
                 move_list: Option<Vec<B::Move>>)
        -> Result<(Score, B::Move), Box<dyn error::Error>> {

        self.search(board, time_limit,
                    Arc::new(Mutex::new(EngineComm::new())), move_list)
            .last()
            .and_then(|uci_info|
                uci_info.pvs.get(0)
                    .and_then(|(score, pv)|
                        pv.get(0).map(|mv| (*score, mv.clone()))))
            .ok_or_else(|| "Engine returned 0 moves".into())
    }

    fn best_moves_multipv(self, board: B, time_limit: uci::TimeRestriction,
                          move_list: Option<Vec<B::Move>>)
        -> Result<Vec<(Score, B::Move)>, Box<dyn error::Error>> {

        self.search(board, time_limit,
                    Arc::new(Mutex::new(EngineComm::new())), move_list)
            .last()
            .map(|uci_info| uci_info.pvs.iter()
                .filter_map(|(score, moves)| moves.get(0).map(|mv| (*score, mv.clone())))
                .collect())
            .ok_or_else(|| "Engine returned 0 moves".into())
    }

    fn search_async(self, board: B, time_limit: uci::TimeRestriction, engine_comm: Arc<Mutex<EngineComm>>,
                    move_list: Option<Vec<B::Move>>)
                    -> (thread::JoinHandle<()>, mpsc::Receiver<uci::UciInfo<B>>)
        where B: Board + Send + 'static + Eq, <B as Board>::Move: Sync + Send, Self: Sized + Send + 'static {
        let (sender, receiver) = mpsc::channel();
        let thread = thread::spawn(move || {
            for mv in self.search(board, time_limit, engine_comm, move_list) {
                sender.send(mv).unwrap();
            }
        });
        (thread, receiver)
    }
}
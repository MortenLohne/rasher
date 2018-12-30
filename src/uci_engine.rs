use uci;
use search_algorithms::board::Board;
use std::sync::mpsc;
use std::sync::Arc;
use std::sync::Mutex;
use uci::EngineComm;
use search_algorithms::alpha_beta::Score;
use std::error;

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

pub trait UciEngine<B: Board> {
    fn init() -> Self;

    fn uci_options(&mut self) -> Vec<UciOption>;

    fn set_uci_option(&mut self, uci_option: UciOption);

    fn search(&mut self, board: B, time_limit: uci::TimeRestriction,
              engine_comm: Arc<Mutex<EngineComm>>,
              move_list: &Option<Vec<B::Move>>, channel: mpsc::Sender<uci::UciInfo<B>>);

    fn best_move(&mut self, board: B, time_limit: uci::TimeRestriction,
                 move_list: &Option<Vec<B::Move>>)
        -> Result<(Score, B::Move), Box<dyn error::Error>> {

        let (tx, rx) = mpsc::channel();

        self.search(board,
                    time_limit, Arc::new(Mutex::new(EngineComm::new())),
                    move_list, tx);

        let mut last_info = None;
        loop { // The channel will return error when closed
            match rx.recv() {
                Ok(uci_info) => last_info = Some(uci_info),
                Err(_) => {
                    if let Some(uci_info) = last_info {
                        if uci_info.pvs.is_empty() || uci_info.pvs[0].1.is_empty() {
                            return Err("Engine returned 0 moves".into());
                        }
                        let (score, ref moves) = uci_info.pvs[0];
                        return Ok((score, moves[0].clone()))
                    }
                    else {
                        return Err("Engine returned no output".into());
                    }
                },
            }
        }
    }
}


use uci;
use search_algorithms::board::Board;
use std::sync::mpsc;
use std::sync::Arc;
use std::sync::Mutex;
use uci::EngineComm;

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
              move_list: &Option<Vec<B::Move>>, channel: mpsc::Sender<uci::UciInfo>);
}


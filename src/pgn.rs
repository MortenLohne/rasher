use search_algorithms::board;

pub trait UciBoard: Sized + board::Board {
    fn from_fen(fen: &str) -> Result<Self, String>;
    fn to_fen(&self) -> String;

    fn from_alg(&self, input: &str) -> Result<Self::Move, String>; // Rename move_from_lan
    fn to_alg(&self, mv: &Self::Move) -> String; // lan_move_string
}
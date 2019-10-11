use board::chess::mv::{ChessMove, ChessReverseMove};
use board::chess::board::*;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CrazyhouseReverseMove {
    NormalMove(ChessReverseMove),
    CrazyMove(PieceType, Square, u8),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum CrazyhouseMove {
    NormalMove(ChessMove),
    CrazyMove(PieceType, Square, u8),
}

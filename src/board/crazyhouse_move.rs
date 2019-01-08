use board::std_move::{ChessMove, ChessReverseMove};
use board::std_board::*;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CrazyhouseReverseMove {
    NormalMove(ChessReverseMove),
    CrazyMove(PieceType, Square, u8),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum CrazyhouseMove {
    NormalMove(ChessMove),
    CrazyMove(PieceType, Square, u8),
}

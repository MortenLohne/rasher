use board::std_move::{ChessMove, ChessUndoMove};
use board::std_board::*;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CrazyhouseUndoMove {
    NormalMove(ChessUndoMove),
    CrazyMove(PieceType, Square, u8),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CrazyhouseMove {
    NormalMove(ChessMove),
    CrazyMove(PieceType, Square, u8),
}

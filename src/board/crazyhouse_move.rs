use board::std_move::ChessMove;
use board::std_board::*;

enum CrazyHouseMove {
    NormalMove(ChessMove),
    CrazyMove(PieceType, Square),
}

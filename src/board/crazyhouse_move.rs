use board::std_move::Move;
use board::std_board::*;

enum CrazyHouseMove {
    NormalMove(Move),
    CrazyMove(PieceType, Square),
}

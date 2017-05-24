use board::std_move::{ChessMove, ChessUndoMove};
use board::std_board::*;
use search_algorithms::game_move::Move;

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

impl Move for CrazyhouseMove { 
    fn from_alg(input : &str) -> Result<Self, String> {
        use board::std_board::PieceType::*;
        if input.contains('@') {
            let piece_type = match input.chars().next().unwrap() {
                '@' => Pawn,
                'N' => Knight,
                'B' => Bishop,
                'R' => Rook,
                'Q' => Queen,
                'K' => King,
                _ => return Err(format!("Couldn't parse move {}.", input)),
            };
            let square_str : String = input.chars().skip_while(|&c| c != '@').collect();
            let square = Square::from_alg(&square_str).ok_or(
                format!("Failed to parse destination square for move {}", input));
            square.map(|sq| CrazyhouseMove::CrazyMove(piece_type, sq, 0))
        }
        else {
            ChessMove::from_alg(input).map(|mv| CrazyhouseMove::NormalMove(mv))
        }
    }
    
    fn to_alg(&self) -> String {
        use board::std_board::PieceType::*;
        match self {
            &CrazyhouseMove::NormalMove(mv) => mv.to_alg(),
            &CrazyhouseMove::CrazyMove(piece, square, _) => match piece {
                Knight => "N",
                Bishop => "B",
                Rook => "R",
                Queen => "Q",
                King => "K",
                Pawn => "",
                Empty => panic!("Encountered move with empty piece")
            }.to_string() 
                + "@" + &square.to_string()
        }
    }

}

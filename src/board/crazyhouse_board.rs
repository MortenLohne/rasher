use board::std_board::PieceType;
use board::std_board::ChessBoard;
use board::std_board;
use board::std_board::Piece;
use board::board::Board;
use board::board::Color;
use board::board::Color::*;

use uci;

use board::crazyhouse_move::CrazyhouseMove;

#[derive(Clone, Eq, PartialEq)]
pub struct CrazyhouseBoard {
    base_board : ChessBoard,
    white_available_pieces : Vec<PieceType>,
    black_available_pieces : Vec<PieceType>,
}

impl Board for CrazyhouseBoard {

    type Move = CrazyhouseMove;
    type UndoMove = CrazyhouseMove;    

    fn to_move(&self) -> Color {
        self.base_board.to_move()
    }

    fn is_mate_or_stalemate(&self) -> ::Score {
        self.base_board.is_mate_or_stalemate()
    }

    fn start_board() -> &'static Self {
        &START_BOARD
    }

    fn score_board(&self) -> ::Score {
        // TODO: Make this take into account available pieces
        self.base_board.score_board()
    }

    fn all_legal_moves(&self) -> Vec<Self::Move> {
        use board::std_board::PieceType::*;
        let mut moves : Vec<CrazyhouseMove> = self.base_board.all_legal_moves().iter()
            .map(|&mv| CrazyhouseMove::NormalMove(mv)).collect();
        let board_iter = std_board::BoardIter::new();
        for square in board_iter {
            let to_move = self.to_move();
            if to_move == White {
                for piece_type in self.white_available_pieces.iter() {
                    moves.push(CrazyhouseMove::CrazyMove(*piece_type, square));
                }
            }
            else {
                for piece_type in self.black_available_pieces.iter() {
                    moves.push(CrazyhouseMove::CrazyMove(*piece_type, square));
                }
            }
        }
        moves
    }
    
    fn do_move(&mut self, mv : Self::Move) -> Self::UndoMove {
        use board::crazyhouse_move::CrazyhouseMove::*;
        match mv {
            NormalMove(normal_move) => {
                if normal_move.capture != PieceType::Empty {
                    if self.to_move() == Black {
                        self.black_available_pieces.push(normal_move.capture);
                    }
                    else {
                        self.white_available_pieces.push(normal_move.capture);
                    }
                }
                NormalMove(self.base_board.do_move(normal_move))
                
            },
            CrazyMove(piecetype, square) => {
                let (file, rank) = square.file_rank();
                if self.to_move() == White {
                    match self.white_available_pieces.iter()
                        .rposition(|&p| p == piecetype) {
                            Some(index) =>
                                self.base_board.board[rank as usize][file as usize] =
                                Piece (self.white_available_pieces.remove(index), White),
                            None => panic!("White tried to make illegal move {:?}, but available pieces were only {:?}", mv, self.white_available_pieces),
                        }
                }
                else {
                    let index = self.black_available_pieces.iter()
                        .rposition(|&p| p == piecetype).unwrap();
                    self.base_board.board[rank as usize][file as usize] =
                        Piece (self.black_available_pieces.remove(index), Black);
                }
                self.base_board.to_move = !self.base_board.to_move;
                mv
            },
        }
    }

    fn undo_move(&mut self, mv : Self::UndoMove) {
        use board::crazyhouse_move::CrazyhouseMove::*;
        match mv {
            NormalMove(normal_move) => self.base_board.undo_move(normal_move),
            CrazyMove(piecetype, square) => {
                let (file, rank) = square.file_rank();
                if self.to_move() == White {
                    self.white_available_pieces.push(piecetype);
                    self.base_board.board[rank as usize][file as usize] = Piece(PieceType::Empty, White);
                }
                else {
                    self.black_available_pieces.push(piecetype);
                    self.base_board.board[rank as usize][file as usize] = Piece(PieceType::Empty, White);
                }
                self.base_board.to_move = !self.base_board.to_move;
            }
        }
    }
}

lazy_static! {
    static ref START_BOARD : CrazyhouseBoard =
        CrazyhouseBoard {base_board: ChessBoard::start_board().clone(),
                         white_available_pieces: vec![],
                         black_available_pieces: vec![] };
}
    

impl uci::UciBoard for CrazyhouseBoard {
    fn from_fen(fen : &str) -> Result<Self,String> {
        
        let fen_split : Vec<&str> = fen.split(" ").collect();
        if fen_split.len() < 5 || fen_split.len() > 7 {
            return Err(format!("Invalid FEN string \"{}\": Had {} fields instead of [5, 6, 7]",
                           fen, fen_split.len()));
        }
        let mut std_fen : Vec<&str> = fen_split.clone();
        std_fen.remove(1);
        let std_fen_string : String = std_fen.iter()
            .fold("".to_string(), |mut acc, &s| { acc.push_str(s); acc.push(' '); acc });
        let std_board = try!(ChessBoard::from_fen(&std_fen_string));

        let mut board = CrazyhouseBoard { base_board: std_board, white_available_pieces: vec![],
                                          black_available_pieces: vec![] };
        
        if fen_split[1] == "-" {
            return Ok(board)
        }
        
        for ch in fen_split[1].chars() {
            match std_board::CHAR_PIECE_MAP.get(&ch) {
                Some(&Piece(piece, White)) => board.white_available_pieces.push(piece),
                Some(&Piece(piece, Black)) => board.black_available_pieces.push(piece),
                None => return Err(format!("Found unknown character {} in available pieces list {} in fen string {}", ch, fen_split[1], fen)),
            }
        }
        Ok(board)
    }
    fn to_fen(&self) -> String {
        "".to_string() // TODO: write
    }
}

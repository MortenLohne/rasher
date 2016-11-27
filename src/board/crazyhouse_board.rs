use board::std_board::PieceType;
use board::std_board::ChessBoard;
use board::std_board;
use board::std_board::Piece;
use board::board::Board;
use board::board::Color;
use board::board::Color::*;

use uci;

use board::crazyhouse_move::CrazyhouseMove;

#[derive(Clone, Eq)]
pub struct CrazyhouseBoard {
    base_board : ChessBoard,
    white_available_pieces : Vec<PieceType>,
    black_available_pieces : Vec<PieceType>,
    crazyhouse_moves : Vec<(u16, CrazyhouseMove)>,
}

impl PartialEq for CrazyhouseBoard {
    fn eq(&self, other: &CrazyhouseBoard) -> bool {
        if self.base_board != other.base_board {
            false
        }
        // Check that available moves are the same, even if they do not appear in the same order
        else {
            let equals_vec = |vec1 : &Vec<_>, vec2 : &Vec<_>| {
                let mut sorted1 = vec1.clone();
                let mut sorted2 = vec2.clone();
                sorted1.sort();
                sorted2.sort();
                sorted1 == sorted2
            };
            equals_vec(&self.white_available_pieces, &other.white_available_pieces) &&
                equals_vec(&self.black_available_pieces, &other.black_available_pieces)            
        }
    }
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
        let mut moves : Vec<CrazyhouseMove> = self.base_board.all_legal_moves().iter()
            .map(|&mv| CrazyhouseMove::NormalMove(mv)).collect();
        let board_iter = std_board::BoardIter::new();
        for square in board_iter.filter(|&sq| self.base_board.piece_at(sq).0 == PieceType::Empty) {
            let to_move = self.to_move();
            if to_move == White {
                for piece_type in self.white_available_pieces.iter() {
                    // Make sure you don't put pawns on the back ranks
                    let (file, rank) = square.file_rank(); 
                    if piece_type != &PieceType::Pawn || (rank > 0 && rank < 7) {
                        moves.push(CrazyhouseMove::CrazyMove(*piece_type, square,
                                                             self.base_board.castling_en_passant));
                    }
                }
            }
            else {
                for piece_type in self.black_available_pieces.iter() {
                    let (file, rank) = square.file_rank();
                    if piece_type != &PieceType::Pawn || (rank > 0 && rank < 7) {
                        moves.push(CrazyhouseMove::CrazyMove(*piece_type, square,
                                                             self.base_board.castling_en_passant));
                    }
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
            CrazyMove(piecetype, square, _) => {
                let (file, rank) = square.file_rank();
                if self.to_move() == White {
                    match self.white_available_pieces.iter()
                        .rposition(|&p| p == piecetype) {
                            Some(index) =>
                                self.base_board.board[rank as usize][file as usize] =
                                Piece (self.white_available_pieces.remove(index), White),
                            None => panic!("{:?}\nWhite tried to make illegal move {:?}, but available pieces were only {:?}", self, mv, self.white_available_pieces),
                        }
                }
                else {
                    match self.black_available_pieces.iter()
                        .rposition(|&p| p == piecetype) {
                            Some (index) =>
                                self.base_board.board[rank as usize][file as usize] =
                                Piece (self.black_available_pieces.remove(index), Black),
                            None => panic!("{:?}\nBlack tried to make illegal move {:?}, but available pieces were only {:?}", self, mv, self.black_available_pieces),
                            
                        }
                }
                // Remove any en passant square. If it was available to this player,
                self.base_board.set_en_passant_square(None);
                self.base_board.to_move = !self.base_board.to_move;
                self.base_board.move_num += 1;
                self.crazyhouse_moves.push((self.base_board.move_num, mv));
                mv
            },
        }
    }

    fn undo_move(&mut self, mv : Self::UndoMove) {
        use board::crazyhouse_move::CrazyhouseMove::*;
        match mv {
            // If the normal move was a capture, remove 
            NormalMove(normal_move) => {
                self.base_board.undo_move(normal_move);
                match (self.to_move(), normal_move.capture) {
                    (_, PieceType::Empty) => PieceType::Empty, // This is not used for anything
                    (White, piecetype) =>
                        match self.white_available_pieces.iter().rposition(|&p| p == piecetype) {
                            Some(index) =>
                                self.white_available_pieces.remove(index),
                            None => panic!("Tried to make illegal move {:?}", mv),
                        },
                    (Black, piecetype) => 
                        match self.black_available_pieces.iter().rposition(|&p| p == piecetype) {
                            Some(index) =>
                                self.black_available_pieces.remove(index),
                            None => panic!("Tried to make illegal move {:?}", mv),
                        },
                };
            },
            CrazyMove(piecetype, square, castling_en_passant) => {
                let (file, rank) = square.file_rank();
                if self.to_move() == Black {
                    self.white_available_pieces.push(piecetype);
                    self.base_board.board[rank as usize][file as usize] = Piece(PieceType::Empty, White);
                }
                else {
                    self.black_available_pieces.push(piecetype);
                    self.base_board.board[rank as usize][file as usize] = Piece(PieceType::Empty, White);
                }
                self.base_board.to_move = !self.base_board.to_move;
                match self.crazyhouse_moves.pop() {
                    None => panic!("Tried to undo crazyhouse move on a board with no crazyhouse moves played"),
                    Some((num, last_move)) => {
                        debug_assert_eq!(last_move, mv);
                        debug_assert_eq!(self.base_board.move_num, num,
                                         "popped crazyhouse move {}, but board sees move {}\n{:?}.",
                                         num, self.base_board.move_num, self);
                    },
                }
                self.base_board.castling_en_passant = castling_en_passant;
                self.base_board.move_num -= 1;
            }
        }
    }
}

lazy_static! {
    static ref START_BOARD : CrazyhouseBoard =
        CrazyhouseBoard {base_board: ChessBoard::start_board().clone(),
                         white_available_pieces: vec![],
                         black_available_pieces: vec![],
                         crazyhouse_moves: vec![]};
}

use std::fmt;
impl fmt::Debug for CrazyhouseBoard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}\nCrazyhouse moves: {:?}\nWhite pieces: {:?}\nBlack pieces: {:?}",
               self.base_board, self.crazyhouse_moves,
               self.white_available_pieces, self.black_available_pieces)
    }
}

impl uci::UciBoard for CrazyhouseBoard {
    fn from_fen(fen : &str) -> Result<Self,String> {
        
        let fen_split : Vec<&str> = fen.split_whitespace().collect();
        if fen_split.len() < 5 || fen_split.len() > 7 {
            return Err(format!("Invalid FEN string \"{}\": Had {} fields instead of [5, 6, 7]",
                           fen, fen_split.len()));
        }
        let ranks : Vec<&str> = fen_split.split("/").collect();
        let captured_pieces = ranks.pop();
        let mut std_fen : Vec<&str> = fen_split.clone();
        std_fen.remove(1);
        let mut std_fen_string : String = std_fen.iter()
            .fold("".to_string(), |mut acc, &s| { acc.push_str(s); acc.push(' '); acc });
        std_fen_string.pop();
        
        let std_board = try!(ChessBoard::from_fen(&std_fen_string));

        let mut board = Self::start_board().clone();
        board.base_board = std_board;
        
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

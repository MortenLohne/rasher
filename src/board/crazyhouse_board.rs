use board::std_board::PieceType;
use board::std_board::ChessBoard;
use board::std_board;
use board::std_board::Square;
use board::std_board::Piece;
use search_algorithms::board::EvalBoard;
use search_algorithms::board::Color;
use search_algorithms::board::Color::*;
use board::std_move_gen::move_gen;
use search_algorithms::board;
use pgn;

use itertools::Itertools;

use std::hash::{Hash, Hasher};
use board::crazyhouse_move::{CrazyhouseMove, CrazyhouseReverseMove};

#[derive(Clone, Eq)]
pub struct CrazyhouseBoard {
    base_board : ChessBoard,
    white_available_pieces : Vec<PieceType>,
    black_available_pieces : Vec<PieceType>,
    crazyhouse_moves : Vec<(u16, CrazyhouseMove)>,
}

impl Hash for CrazyhouseBoard {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.base_board.hash(state);
        self.white_available_pieces.hash(state);
        self.black_available_pieces.hash(state);
    }
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
    type ReverseMove = CrazyhouseReverseMove;

    fn side_to_move(&self) -> Color {
        self.base_board.side_to_move()
    }

    fn start_board() -> Self {
        CrazyhouseBoard {base_board: ChessBoard::start_board().clone(),
            white_available_pieces: vec![],
            black_available_pieces: vec![],
            crazyhouse_moves: vec![]}
    }
    fn game_result(&self) -> Option<board::GameResult> {
        self.base_board.game_result()
    }

    fn generate_moves(&self, moves: &mut Vec<Self::Move>) {
        let mut normal_moves = vec![];
        self.base_board.generate_moves(&mut normal_moves);
        moves.extend(normal_moves.iter()
            .map(|&mv| CrazyhouseMove::NormalMove(mv)));
        let board_iter = std_board::BoardIter::new();
        let king_pos = move_gen::king_pos(&self.base_board);
        let is_in_check = move_gen::is_attacked(&self.base_board, king_pos);

        let leaves_king_in_check = |mv| {
            let mut cloned = self.clone();
            cloned.do_move(mv);
            cloned.base_board.to_move = !cloned.side_to_move();

            debug_assert_eq!(king_pos, move_gen::king_pos(&self.base_board));
            move_gen::is_attacked(&cloned.base_board, king_pos)
        };

        for square in board_iter.filter(|&sq| self.base_board[sq].is_empty()) {
            let to_move = self.side_to_move();
            if to_move == White {
                for piece_type in &self.white_available_pieces {
                    // Make sure you don't put pawns on the back ranks
                    let (_, rank) = square.file_rank();
                    if piece_type != &PieceType::Pawn || (rank > 0 && rank < 7) {
                        let mv = CrazyhouseMove::CrazyMove(
                            *piece_type, square, self.base_board.castling_en_passant);
                        if !is_in_check || !leaves_king_in_check(mv) {
                            moves.push(mv);
                        }
                    }
                }
            }
                else {
                    for piece_type in &self.black_available_pieces {
                        let (_, rank) = square.file_rank();
                        if piece_type != &PieceType::Pawn || (rank > 0 && rank < 7) {
                            let mv = CrazyhouseMove::CrazyMove(
                                *piece_type, square, self.base_board.castling_en_passant);
                            if !is_in_check || !leaves_king_in_check(mv) {
                                moves.push(mv);

                            }
                        }
                    }
                }
        }
    }

    fn do_move(&mut self, mv : Self::Move) -> Self::ReverseMove {
        match mv {
            CrazyhouseMove::NormalMove(normal_move) => {
                let capture = self.base_board[normal_move.to].piece_type();
                if capture != PieceType::Empty {
                    if self.side_to_move() == Black {
                        self.black_available_pieces.push(capture);
                    }
                        else {
                            self.white_available_pieces.push(capture);
                        }
                }
                CrazyhouseReverseMove::NormalMove(self.base_board.do_move(normal_move))

            },
            CrazyhouseMove::CrazyMove(piecetype, square, n) => {
                let (file, rank) = square.file_rank();
                if self.side_to_move() == White {
                    match self.white_available_pieces.iter()
                        .rposition(|&p| p == piecetype) {
                        Some(index) =>
                            self.base_board.board[rank as usize][file as usize] =
                                Piece::new(self.white_available_pieces.remove(index), White),
                        None => panic!("{:?}\nWhite tried to make illegal move {:?}, but available pieces were only {:?}", self, mv, self.white_available_pieces),
                    }
                }
                    else {
                        match self.black_available_pieces.iter()
                            .rposition(|&p| p == piecetype) {
                            Some (index) =>
                                self.base_board.board[rank as usize][file as usize] =
                                    Piece::new(self.black_available_pieces.remove(index), Black),
                            None => panic!("{:?}\nBlack tried to make illegal move {:?}, but available pieces were only {:?}", self, mv, self.black_available_pieces),

                        }
                    }
                // Remove any en passant square. If it was available to this player,
                self.base_board.set_en_passant_square(None);
                self.base_board.to_move = !self.base_board.to_move;
                self.base_board.move_num += 1;
                self.crazyhouse_moves.push((self.base_board.move_num, mv));
                CrazyhouseReverseMove::CrazyMove(piecetype, square, n)
            },
        }
    }

    fn reverse_move(&mut self, mv : Self::ReverseMove) {
        match mv {
            // If the normal move was a capture, remove
            CrazyhouseReverseMove::NormalMove(normal_move) => {
                self.base_board.reverse_move(normal_move);
                match (self.side_to_move(), normal_move.capture) {
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
            CrazyhouseReverseMove::CrazyMove(piecetype, square, castling_en_passant) => {
                let (file, rank) = square.file_rank();
                if self.side_to_move() == Black {
                    self.white_available_pieces.push(piecetype);
                    self.base_board.board[rank as usize][file as usize] = Piece::empty();
                }
                    else {
                        self.black_available_pieces.push(piecetype);
                        self.base_board.board[rank as usize][file as usize] = Piece::empty();
                    }
                self.base_board.to_move = !self.base_board.to_move;
                match self.crazyhouse_moves.pop() {
                    None => panic!("Tried to undo crazyhouse move on a board with no crazyhouse moves played"),
                    Some((num, _)) => {
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

impl EvalBoard for CrazyhouseBoard {
    fn static_eval(&self) -> f32 {
        // TODO: Make this take into account available pieces
        let score = self.base_board.static_eval();
        0.0 + score -
            self.black_available_pieces.iter().cloned().map(PieceType::value).sum::<f32>() +
            self.white_available_pieces.iter().cloned().map(PieceType::value).sum::<f32>()
    }
}

impl ExtendedBoard for CrazyhouseBoard {
    type HashBoard = Self;

    fn hash_board(&self) -> Self {
        self.clone()
    }

    fn active_moves(&self, _: &mut Vec<<Self as Board>::Move>) {}
}

use std::fmt;
use search_algorithms::board::Board;
use search_algorithms::board::ExtendedBoard;
use pgn::UciBoard;

impl fmt::Debug for CrazyhouseBoard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}\nCrazyhouse moves: {:?}\nWhite pieces: {:?}\nBlack pieces: {:?}",
               self.base_board, self.crazyhouse_moves,
               self.white_available_pieces, self.black_available_pieces)
    }
}

impl UciBoard for CrazyhouseBoard {
    fn from_fen(fen : &str) -> Result<Self, pgn::Error> {
        
        let fen_split : Vec<&str> = fen.split_whitespace().collect();
        if fen_split.len() < 5 || fen_split.len() > 7 {
            return Err(pgn::Error::new(
                pgn::ErrorKind::ParseError,
                format!("Invalid FEN string \"{}\": Had {} fields instead of [5, 6, 7]",
                           fen, fen_split.len())));
        }
        let mut ranks : Vec<&str> = fen_split[0].split('/').collect();
        if ranks.len() != 9 {
            return Err(pgn::Error::new(
                pgn::ErrorKind::ParseError,
                format!("Invalid FEN string \"{}\": Found {} ranks, expected 9",
                               fen, ranks.len())));
        }
        let captured_pieces = ranks.pop().unwrap();

        let mut std_fen_string : String = ranks.join("/");
        std_fen_string.push(' ');
        std_fen_string.push_str(&fen_split[1..].iter().join(" "));
        
        let std_board = try!(ChessBoard::from_fen(&std_fen_string));

        let mut board = Self::start_board().clone();
        board.base_board = std_board;
        
        for ch in captured_pieces.chars() {
            if let Some(piece) = Piece::from_letter(ch) {
                let piece_type = piece.piece_type();
                match piece.color() {
                    Some(White) => board.white_available_pieces.push(piece_type),
                    Some(Black) => board.black_available_pieces.push(piece_type),
                    _ => panic!(),
                }
            }
            else {
                return Err(pgn::Error::new(
                    pgn::ErrorKind::ParseError,
                    format!("Illegal character {} in available pieces list {} in fen string {}",
                            ch, fen_split[1], fen)));
            }
        }
        Ok(board)
    }
        
    fn to_fen(&self) -> String {
        "".to_string() // TODO: write
    }

    fn move_from_lan(&self, input : &str) -> Result<Self::Move, pgn::Error> {
        use board::std_board::PieceType::*;
        if input.contains('@') {
            let piece_type = match input.chars().next().unwrap() {
                '@' => Pawn,
                'N' => Knight,
                'B' => Bishop,
                'R' => Rook,
                'Q' => Queen,
                'K' => King,
                _ => return Err(pgn::Error::new(
                    pgn::ErrorKind::ParseError,
                    format!("Couldn't parse move {}.", input))),
            };
            let square_str : String = input.chars()
                .skip_while(|&c| c != '@')
                .skip(1)
                .collect();
            let square = Square::from_alg(&square_str).map_err(|err|
                pgn::Error::new_caused_by(
                    pgn::ErrorKind::ParseError,
                    format!("Invalid destination square for move {}", input), err));
            square.map(|sq|
                CrazyhouseMove::CrazyMove(piece_type, sq, self.base_board.castling_en_passant))
        }
        else {
            self.base_board.move_from_lan(input).map(CrazyhouseMove::NormalMove)
        }
    }
    
    fn move_to_lan(&self, mv: &Self::Move) -> String {
        use board::std_board::PieceType::*;
        match *mv {
            CrazyhouseMove::NormalMove(mv) => self.base_board.move_to_lan(&mv),
            CrazyhouseMove::CrazyMove(piece, square, _) => match piece {
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

    fn move_to_san(&self, mv: &<Self as Board>::Move) -> String {
        match *mv {
            CrazyhouseMove::NormalMove(mv) => self.base_board.move_to_san(&mv),
            CrazyhouseMove::CrazyMove(_, _, _) => self.move_to_lan(mv),
        }
    }

    fn move_from_san(&self, input: &str) -> Result<<Self as Board>::Move, pgn::Error> {
        if input.contains('@') {
            self.move_from_lan(input)
        }
        else {
            self.base_board.move_from_san(input).map(CrazyhouseMove::NormalMove)
        }
    }
}

use board::std_board;
use board::std_board::{ChessBoard, Piece, PieceType, Square};
use board::sjadam_move::{SjadamMove, SjadamUndoMove};
use board::sjadam_move_gen;
use board::std_move_gen::move_gen;

use search_algorithms::board::EvalBoard;
use search_algorithms::board::Color;
use search_algorithms::board::Color::*;
use search_algorithms::board::GameResult;

use rand;
use uci::UciBoard;

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct SjadamBoard {
    pub base_board: std_board::ChessBoard,
}

impl EvalBoard for SjadamBoard {
    type Move = SjadamMove;
    type UndoMove = SjadamUndoMove;

    fn to_move(&self) -> Color {
        self.base_board.to_move()
    }

    fn do_move(&mut self, mv: Self::Move) -> Self::UndoMove {
        let start_color = self.to_move();
        debug_assert!(mv.from != mv.sjadam_square || mv.sjadam_square != mv.to);

        let undo_move = SjadamUndoMove {
            from: mv.from, sjadam_square: mv.sjadam_square,
            capture: self.base_board[mv.to].piece_type(),
            to: mv.to, piece_moved: self.base_board[mv.from].piece_type(),
            old_castling_en_passant: self.base_board.castling_en_passant,
            old_half_move_clock: self.base_board.half_move_clock };

        // Do sjadam move. Has no effect if there is no sjadam jump
        self.base_board[mv.sjadam_square] = self.base_board[mv.from];
        if mv.from != mv.sjadam_square {
            self.base_board[mv.from] = Piece::empty();
        }

        // Remove castling priviledges on king moves
        if undo_move.piece_moved == PieceType::King {
            let color = self.to_move();
            self.base_board.disable_castling(color);
        }
        
        // Remove castling priviledges if anything moves from or to a corner

        let a1 = Square::from_alg("a1").unwrap();
        let h1 = Square::from_alg("h1").unwrap();
        let a8 = Square::from_alg("a8").unwrap();
        let h8 = Square::from_alg("h8").unwrap();
        match mv.from {
            square if square == a1 =>
                self.base_board.disable_castling_queenside(White),
            square if square == h1 =>
                self.base_board.disable_castling_kingside(White),
            square if square == a8 =>
                self.base_board.disable_castling_queenside(Black),
            square if square == h8 =>
                self.base_board.disable_castling_kingside(Black),
            _ => (),
        }

        match mv.to {
            square if square == a1 =>
                self.base_board.disable_castling_queenside(White),
            square if square == h1 =>
                self.base_board.disable_castling_kingside(White),
            square if square == a8 =>
                self.base_board.disable_castling_queenside(Black),
            square if square == h8 =>
                self.base_board.disable_castling_kingside(Black),
            _ => (),
        }
        
        let chess_move = mv.chess_move(self);

        match chess_move {
            None => {
                if undo_move.piece_moved != PieceType::Pawn {
                    self.base_board.half_move_clock += 1; // Pure sjadam moves are never captures
                }
                else {
                    self.base_board.half_move_clock = 0;
                }

                // Sjadam moves never create en passant 
                self.base_board.set_en_passant_square(None);
                self.base_board.to_move = !self.base_board.to_move;
                
            }, // TODO: Figure out if anything else needs to be done here
            Some(chess_move) => {
                self.base_board.do_move(chess_move);
            },
        }
        if (start_color == White && mv.to.rank() == 0)
            || (start_color == Black && mv.to.rank() == 7) {
                debug_assert!(!self.base_board[mv.to].is_empty());
                if self.base_board[mv.to].piece_type() != PieceType::King {
                    self.base_board[mv.to] = Piece::new(PieceType::Queen, start_color);
                }
            }
        debug_assert_ne!(start_color, self.to_move());
        debug_assert!(self.base_board.castling_en_passant & 15 <= undo_move.old_castling_en_passant & 15);
        undo_move
    }

    fn undo_move(&mut self, mv: Self::UndoMove) {
        let start_color = self.to_move();
        match mv.chess_move(self) {
            None => {
                self.base_board.to_move = !self.base_board.to_move;
            },
            Some(chess_move) => self.base_board.undo_move(chess_move),
        }
        self.base_board.half_move_clock = mv.old_half_move_clock;
        self.base_board.castling_en_passant = mv.old_castling_en_passant;

        let color = self.base_board[mv.sjadam_square].color().unwrap();
        self.base_board[mv.from] = Piece::new(mv.piece_moved, color); 
        // Undo sjadam move
        if mv.from != mv.sjadam_square {
            self.base_board[mv.sjadam_square] = Piece::empty();
        }
        debug_assert_ne!(start_color, self.to_move());
        
    }

    fn start_board() -> Self {
        SjadamBoard {
            base_board: std_board::ChessBoard::start_board(),
        }
    }

    fn all_legal_moves(&self) -> Vec<Self::Move> {
        let mut cloned_board = self.clone();
        sjadam_move_gen::all_legal_moves(&mut cloned_board)
    }

    fn game_result(&self) -> Option<GameResult> {
        // In sjadam, king may be actually captured.
        // Check if the king is gone
        match self.king_on_board() {
            (true, false) => Some(GameResult::WhiteWin),
            (false, true) => Some(GameResult::BlackWin),
            (false, false) => panic!("Neither side has a king on the board:\n{:?}", self),
            (true, true) => {
                // First check if it is attacked by regular moves
                if !sjadam_move_gen::any_legal_moves(&mut self.clone()) {
                    if move_gen::is_attacked(&self.base_board, move_gen::king_pos(&self.base_board)) {
                        match self.to_move() {
                            Black => Some(GameResult::WhiteWin),
                            White => Some(GameResult::BlackWin),
                        }
                    }
                    else {
                        Some(GameResult::Draw)
                    }
                }
                // The king might still be directly captured by a sjadam move next move,
                // but that is fine
                else {
                    if self.base_board.half_move_clock > 50 {
                        Some(GameResult::Draw)
                    }
                    else { None }
                }
            },
        }
    }
    fn do_random_move<R: rand::Rng>(&mut self, rng: &mut R) {
        let moves = self.all_legal_moves();
        assert!(!moves.is_empty(),
                "Tried to do a random move, but there were no legal moves on {:?}",
                self);
        self.do_move(moves[rng.gen_range(0, moves.len())].clone());
    }
    fn eval_board(&self) -> f32 {
        self.base_board.eval_board()
    }

    fn branch_factor() -> u64 {
        40
    }
}

impl UciBoard for SjadamBoard {
    fn from_fen(input: &str) -> Result<Self, String> {
        Ok(SjadamBoard { base_board: try!(ChessBoard::from_fen(input)) })
    }

    fn to_fen(&self) -> String {
        self.base_board.to_fen()
    }
}

impl SjadamBoard {
    fn king_on_board(&self) -> (bool, bool) {
        let mut white_king = false;
        let mut black_king = false;
        let board = &self.base_board;
        for square in std_board::BoardIter::new() {
            if board[square].piece_type() == PieceType::King {
                match board[square].color().unwrap() {
                    White => white_king = true,
                    Black => black_king = true,
                }
            }
        }
        (white_king, black_king)
    }
}

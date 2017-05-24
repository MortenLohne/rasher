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

use std::cell::RefCell;

use uci::UciBoard;

#[derive(Debug)]
pub struct SjadamBoard {
    pub base_board: std_board::ChessBoard,
    legal_moves_cache: RefCell<Option<Vec<SjadamMove>>>,
}

impl PartialEq for SjadamBoard {
    fn eq(&self, other: &SjadamBoard) -> bool {
        self.base_board == other.base_board
    }
}

impl Clone for SjadamBoard {
    fn clone(&self) -> Self {
        // When cloning the board, the cache is not brought along
        SjadamBoard { base_board: self.base_board.clone(), legal_moves_cache: RefCell::new(None) }
    }
}

impl EvalBoard for SjadamBoard {
    type Move = SjadamMove;
    type UndoMove = SjadamUndoMove;

    fn to_move(&self) -> Color {
        self.base_board.to_move()
    }

    fn do_move(&mut self, mv: Self::Move) -> Self::UndoMove {
        let start_color = self.to_move();
        *self.legal_moves_cache.borrow_mut() = None;
        debug_assert!(mv.from != mv.sjadam_square || mv.sjadam_square != mv.to);

        let undo_move = SjadamUndoMove {
            from: mv.from, sjadam_square: mv.sjadam_square,
            capture: self.base_board[mv.to].0,
            prom: mv.prom /*(self.base_board[mv.from].0 != PieceType::King)
                && (self.to_move() == Black && mv.to.0 >= 56
                    || self.to_move() == White && mv.to.0 < 8)*/,
            to: mv.to, piece_moved: self.base_board[mv.from].0,
            old_castling_en_passant: self.base_board.castling_en_passant,
            old_half_move_clock: self.base_board.half_move_clock };

        // Do sjadam move. Has no effect if there is no sjadam jump
        self.base_board[mv.sjadam_square] = self.base_board[mv.from];
        if mv.from != mv.sjadam_square {
            self.base_board[mv.from] = Piece::empty();
        }
        
        let chess_move = mv.chess_move(self);
        match chess_move {
            None => {
                if undo_move.piece_moved == PieceType::King {
                    let color = self.to_move();
                    self.base_board.disable_castling(color)
                }
                // TODO: Remove castling privileges on rook moves
                if undo_move.piece_moved != PieceType::Pawn {
                    self.base_board.half_move_clock += 1; // Pure sjadam moves are never captures
                }

                // Sjadam moves never create en passant squares
                self.base_board.set_en_passant_square(None); 
                self.base_board.to_move = !self.base_board.to_move;
                
            }, // TODO: Figure out if anything else needs to be done here
            Some(chess_move) => {
                self.base_board.do_move(chess_move);
            },
        }
        
        for square in 56..63 {
            let Piece(piece, color) = self.base_board[Square(square)];
            if color == Black && piece != PieceType::King && piece != PieceType::Empty{
                self.base_board[Square(square)] = Piece(PieceType::Queen, Black)
            }
        }
        for square in 0..7 {
            let Piece(piece, color) = self.base_board[Square(square)];
            if color == White && piece != PieceType::King && piece != PieceType::Empty {
                self.base_board[Square(square)] = Piece(PieceType::Queen, White)
            }
        }
        debug_assert!(start_color != self.to_move());
        undo_move
    }

    fn undo_move(&mut self, mv: Self::UndoMove) {
        let start_color = self.to_move();
        *self.legal_moves_cache.borrow_mut() = None;
        match mv.chess_move(self) {
            None => {
                self.base_board.to_move = !self.base_board.to_move;
            },
            Some(chess_move) => self.base_board.undo_move(chess_move),
        }
        self.base_board.half_move_clock = mv.old_half_move_clock;
        self.base_board.castling_en_passant = mv.old_castling_en_passant;

        let color = self.base_board[mv.sjadam_square].1;
        self.base_board[mv.from] = Piece(mv.piece_moved, color); 
        // Undo sjadam move
        if mv.from != mv.sjadam_square {
            self.base_board[mv.sjadam_square] = Piece::empty();
        }
        debug_assert!(start_color != self.to_move());
        
    }

    fn start_board() -> Self {
        SjadamBoard {
            base_board: std_board::ChessBoard::start_board(),
            legal_moves_cache: RefCell::new(None),
        }
    }

    fn all_legal_moves(&self) -> Vec<Self::Move> {
        
        /*let moves;
        let mut moves_cache = self.legal_moves_cache.borrow_mut();
        if (*moves_cache).is_some(){
            moves = (*moves_cache).as_ref().unwrap().clone();
        }
        else {*/
        let mut cloned_board = self.clone();
        let moves = sjadam_move_gen::all_legal_moves(&mut cloned_board);
        //*moves_cache = Some(moves.clone());
        
        moves
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
        assert!(moves.len() > 0,
                "Tried to do a random move, but there were no legal moves on {:?}",
                self);
        self.do_move(moves[rng.gen_range(0, moves.len())].clone());
    }
    fn eval_board(&self) -> f32 {
        self.base_board.eval_board()
    }
}

impl UciBoard for SjadamBoard {
    fn from_fen(input: &str) -> Result<Self, String> {
        Ok(SjadamBoard { base_board: try!(ChessBoard::from_fen(input)),
                         legal_moves_cache: RefCell::new(None), })
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
            match board[square] {
                Piece(PieceType::King, White) => white_king = true,
                Piece(PieceType::King, Black) => black_king = true,
                _ => (),
            }
        }
        (white_king, black_king)
    }
}

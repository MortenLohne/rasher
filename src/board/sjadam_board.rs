use board::std_board;
use board::std_board::PieceType::*;
use board::std_board::{ChessBoard, Piece, PieceType, Square};
use board::sjadam_move::{SjadamMove, SjadamUndoMove};
use board::sjadam_move_gen;

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
        debug_assert_ne!(mv.from(), mv.to());
        debug_assert!(!self.base_board[mv.from()].is_empty(),
                      "Tried to do move {} from empty square at \n{:?}", mv, self);

        let en_passant = mv.en_passant(&self);
        let piece_moved = self.base_board[mv.from()].piece_type();
        let undo_move = SjadamUndoMove {
            from: mv.from(), to: mv.to(),
            castling: mv.castling(), en_passant: en_passant,
            capture: self.base_board[mv.to()].piece_type(),
            piece_moved: self.base_board[mv.from()].piece_type(),
            old_castling_en_passant: self.base_board.castling_en_passant,
            old_half_move_clock: self.base_board.half_move_clock
        };
        debug_assert!(undo_move.piece_moved != PieceType::Empty,
                      "Empty piece_moved when doing {} on \n{:?}", mv, self);

        self.base_board.set_en_passant_square(None);
        
        // Remove castling priviledges on king moves
        if undo_move.piece_moved == PieceType::King {
            let color = self.to_move();
            self.base_board.disable_castling(color);
        }
        
        // Remove castling priviledges if anything moves from or to a corner
        match mv.from() {
            square if square == Square::A1 =>
                self.base_board.disable_castling_queenside(White),
            square if square == Square::H1 =>
                self.base_board.disable_castling_kingside(White),
            square if square == Square::A8 =>
                self.base_board.disable_castling_queenside(Black),
            square if square == Square::H8 =>
                self.base_board.disable_castling_kingside(Black),
            _ => (),
        }

        match mv.to() {
            square if square == Square::A1 =>
                self.base_board.disable_castling_queenside(White),
            square if square == Square::H1 =>
                self.base_board.disable_castling_kingside(White),
            square if square == Square::A8 =>
                self.base_board.disable_castling_queenside(Black),
            square if square == Square::H8 =>
                self.base_board.disable_castling_kingside(Black),
            _ => (),
        }

        // Set en passant square
        let mid_sq = Square::from_ints(mv.to().file(), (mv.from().rank() + mv.to().rank()) / 2);
        if piece_moved == PieceType::Pawn && mv.from().file() == mv.to().file()
            && i8::abs(mv.from().rank() as i8 - mv.to().rank() as i8) == 2
            && self.base_board[mid_sq].is_empty() {
                //println!("Mid square: {}", mid_sq);
                if start_color == White && mv.from().rank() == 6 {
                    self.base_board.set_en_passant_square(Some(mid_sq));
                }
                else if start_color == Black && mv.from().rank() == 1 {
                    self.base_board.set_en_passant_square(Some(mid_sq));
                }
        }
        
        self.base_board[mv.to()] = self.base_board[mv.from()];
        self.base_board[mv.from()] = Piece::empty();
        if mv.castling() {
            // Move the rook too
            let (rook_from, rook_to) = if mv.to().file() == 6 { (7, 5) } else { (0, 3) };
            self.base_board[Square::from_ints(rook_to, mv.from().rank())] = self.base_board[Square::from_ints(rook_from, mv.from().rank())];
            self.base_board[Square::from_ints(rook_from, mv.from().rank())] = Piece::empty();
        }
        else if en_passant {
            // Remove the captured pawn
            let ep_square_rank = if start_color == Black { mv.to().rank() - 1 } else { mv.to().rank() + 1 };
            self.base_board[Square::from_ints(mv.to().file(), ep_square_rank)] = Piece::empty();
        }
        else if (start_color == White && mv.to().rank() == 0)
            || (start_color == Black && mv.to().rank() == 7) {
                debug_assert!(!self.base_board[mv.to()].is_empty());
                if self.base_board[mv.to()].piece_type() != PieceType::King {
                    self.base_board[mv.to()] = Piece::new(PieceType::Queen, start_color);
                }
}
        self.base_board.to_move = !self.to_move();
        debug_assert_ne!(start_color, self.to_move());
        debug_assert!(self.base_board.castling_en_passant & 15 <= undo_move.old_castling_en_passant & 15);
        undo_move
    }

    fn undo_move(&mut self, mv: Self::UndoMove) {
        let start_color = self.to_move();
        self.base_board[mv.from()] = Piece::new(mv.piece_moved, !start_color);
        self.base_board[mv.to()] = Piece::new(mv.capture, start_color);
        if mv.castling() {
            // Move the rook too
            let (rook_from, rook_to) = if mv.to().file() == 6 { (7, 5) } else { (0, 3) };
            self.base_board[Square::from_ints(rook_from, mv.from().rank())] = self.base_board[Square::from_ints(rook_to, mv.from().rank())];
            self.base_board[Square::from_ints(rook_to, mv.from().rank())] = Piece::empty();
        }
        else if mv.en_passant() {
            // Replace the captured pawn
            let ep_square_rank = if start_color == Black { mv.to().rank() + 1 } else { mv.to().rank() - 1 };
            self.base_board[Square::from_ints(mv.to().file(), ep_square_rank)] = Piece::new(PieceType::Pawn, start_color);
        }
        self.base_board.half_move_clock = mv.old_half_move_clock;
        self.base_board.castling_en_passant = mv.old_castling_en_passant;

        self.base_board.to_move = !self.to_move();
        debug_assert_ne!(start_color, self.to_move());
        debug_assert!(!self.base_board[mv.from()].is_empty());
        
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
            (true, true) if self.base_board.half_move_clock > 50 => Some(GameResult::Draw),
            (true, true) => None,
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

    fn from_alg(&self, input: &str) -> Result<Self::Move, String> {
        if input.len() < 4 {
            return Err(format!("Move \"{}\" was too short to parse", input))
        }
        #[cfg(feature = "legacy_sjadam_move_format")]
        {
            if input.len() == 5 && (input.as_bytes()[4] as char == '-' || input.as_bytes()[0] as char == '-') || input.len() == 6 {
                let mut chars = input.chars().peekable();
                if *chars.peek().unwrap() == '-' {
                    let from = Square::from_alg(&input[1..3]).ok_or("Illegal square")?;
                    let to = Square::from_alg(&input[3..5]).ok_or("Illegal square")?;
                    // TODO: Check for castling
                    return Ok(SjadamMove::new(from, to, false))
                }
                let alg : String = chars.by_ref().collect();
                let from = Square::from_alg(&alg[0..2]).ok_or("Illegal square")?;
                let sjadam = Square::from_alg(&alg[2..4]).ok_or("Illegal square")?;
                if alg.chars().last().unwrap() == '-' {
                    return Ok(SjadamMove::new(from, sjadam, false));
                }
                else {
                    let to = Square::from_alg(&alg[4..]).ok_or("Illegal square")?;
                    return Ok(SjadamMove::new(from, to, false));
                }
            }
        }
        if input.len() > 5 {
            return Err(format!("Move \"{}\" was too long to parse", input))
        }
        let from = Square::from_alg(&input[0..2]).ok_or("Illegal square")?;
        let to = Square::from_alg(&input[2..4]).ok_or("Illegal square")?;
        match input.len() {
            4 => Ok(SjadamMove::new(from, to, false)),
            5 if input.as_bytes()[4] == 'c' as u8 => Ok(SjadamMove::new(from, to, true)),
            _ => Err(format!("Couldn't parse move {}", input))
        }
    }
    fn to_alg(&self, mv: &Self::Move) -> String {
        debug_assert_eq!(self.base_board[mv.from()].color(), Some(self.to_move()));
        debug_assert_ne!(self.base_board[mv.to()].color(), Some(self.to_move()));
        #[cfg(feature = "legacy_sjadam_move_format")]
        {
            // println!("Converting move {:?} on\n{:?}", mv, self);
            let dia_neighbours = |square: i8| [square - 9, square - 7, square + 7, square + 9]
                .iter().cloned()
                .filter(|&sq| sq >= 0 && sq < 64)
                .map(|sq| Square(sq as u8))
                .filter(|&sq| self.base_board[sq].is_empty() || sq == mv.from())
                .collect::<Vec<Square>>();
            let orth_neighbours = |square: i8| [square - 8, square - 1, square + 1, square + 8]
                .iter().cloned()
                .filter(|&sq| sq >= 0 && sq < 64)
                .map(|sq| Square(sq as u8))
                .filter(|&sq| self.base_board[sq].is_empty() || sq == mv.from())
                .collect::<Vec<Square>>();
            let knight_neighbours = |square: i8| [square - 17, square - 15, square - 9, square - 6,
                                                  square + 6, square + 9, square + 15, square + 17]
                .iter().cloned()
                .filter(|&sq| sq >= 0 && sq < 64)
                .map(|sq| Square(sq as u8))
                .filter(|&sq| self.base_board[sq].is_empty() || sq == mv.from())
                .collect::<Vec<Square>>();

            let pawn_neighbours = |square: i8| [square + 7, square + 9]
                .iter().cloned()
                .map(|sq| if self.to_move() == Black { sq - 16 } else { sq } )
                .filter(|&sq| sq >= 0 && sq < 64)
                .map(|sq| Square(sq as u8))
                .inspect(|&sq| println!("Inspecting pawn square {}", sq))
                .filter(|&sq| self.base_board[sq].is_empty() || sq == mv.from())
                .inspect(|&sq| println!("Approved pawn square {}", sq))
                .collect::<Vec<Square>>();

            // If destination square is empty, do it as a pure sjadam move
            let sjadam_square = if self.base_board[mv.to()].is_empty() {
                if mv.castling() {
                    mv.from()
                }
                else {
                    mv.to() // TODO: Doesn't work with castling. Does not create en passant squares
                }
            }
            else {
                match self.base_board[mv.from()].piece_type() {
                    Bishop => *dia_neighbours(mv.to().0 as i8).get(0).expect(&mv.to_string()),
                    Rook => *orth_neighbours(mv.to().0 as i8).get(0).expect(&mv.to_string()),
                    Queen | King => { let mut neighbs = orth_neighbours(mv.to().0 as i8);
                                      neighbs.append(&mut dia_neighbours(mv.to().0 as i8));
                                      *neighbs.get(0).expect(&mv.to_string())
                    },
                    Knight => *knight_neighbours(mv.to().0 as i8).get(0).expect(&mv.to_string()),
                    Pawn => *pawn_neighbours(mv.to().0 as i8).get(0).expect(&mv.to_string()),
                    
                    Empty => unreachable!(),
                }
            };

            
            
            let mut f = String::new();
            use fmt::Write;
            write!(f, "{}{}{}",
                   if mv.from() == sjadam_square {
                       "-".to_string()
                   }
                   else {
                       mv.from().to_string()
                   },
                   sjadam_square,
                   if sjadam_square == mv.to() {
                       "-".to_string()
                   }
                   else {
                       mv.to().to_string()
                   }).unwrap();
            return f;
        }
        mv.to_string()
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

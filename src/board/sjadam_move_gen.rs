use board::sjadam_move::SjadamMove;
use board::std_board::BoardIter;
use board::std_board::ChessBoard;
use board::std_board::PieceType;
use board::std_board::Piece;
use board::std_board::Square;

use search_algorithms::board::EvalBoard;
use board::sjadam_board::SjadamBoard;

use board::std_move_gen::move_gen;

use std::fmt;

#[derive(PartialEq, Eq, Clone)]
struct BitBoard {
    board: u64,
}

impl BitBoard {
    fn empty() -> Self {
        BitBoard { board: 0 }
    }
    /// Returns a bitboard representation of the board, with all bits set
    /// where f(piece) is true for the piece on the square
    fn from_board<F: Fn(Piece) -> bool> (board: &ChessBoard, f: F) -> Self {
        let mut bit_board = BitBoard::empty();
        for square in BoardIter::new() {
            if f(board[square]) {
                bit_board.set(square);
            }
        }
        bit_board
    }
    fn all_from_board(board: &ChessBoard) -> Self {
        Self::from_board(board, |Piece(piece, _)| piece != PieceType::Empty)
    }
    fn get(&self, idx: Square) -> bool {
        let Square(i) = idx;
        debug_assert!(i < 64, format!("Tried to index pos {} on board{:?}!", idx, self));
        self.board & (1<<i) != 0
    }
    fn set(&mut self, idx: Square) {
        let Square(i) = idx;
        debug_assert!(i < 64, format!("Tried to index pos {} on board{:?}!", idx, self));
        self.board |= 1<<i;
    }

    fn clear(&mut self, idx: Square) {
        let Square(i) = idx;
        debug_assert!(i < 64, format!("Tried to index pos {} on board{:?}!", idx, self));
        self.board &= !(1<<i);
    }
}

impl fmt::Debug for BitBoard {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for n in 0..8 {
            write!(f, "{:08b}\n", (self.board >> (n * 8)) as u8).unwrap();
        }
        Ok(())
    }
}

pub fn all_legal_moves(board: &mut SjadamBoard) -> Vec<SjadamMove> {
    //self.base_board.all_legal_moves().iter().map(SjadamMove::from_chess_move).collect()
    let mut moves = vec![];
    for square in BoardIter::new() {
        let Piece(piece, color) = board.base_board[square];
        if piece != PieceType::Empty && color == board.to_move() {
            moves.append(&mut legal_moves_for_square(board, square));
        }
    }
    moves
}

pub fn any_legal_moves(board: &mut SjadamBoard) -> bool {
    for square in BoardIter::new() {
        let Piece(piece, color) = board.base_board[square];
        if piece != PieceType::Empty && color == board.to_move() {
            if legal_moves_for_square(board, square).len() > 0 {
                return true;
            }
        }
    }
    false
}

fn legal_moves_for_square(board: &mut SjadamBoard, square: Square) -> Vec<SjadamMove> {
    let mut sjadam_squares = BitBoard::empty();
    sjadam_squares.set(square);
    let friendly_pieces =
        BitBoard::from_board(&board.base_board,
                             |Piece(piece_type, color)| color == board.to_move()
                             && piece_type != PieceType::Empty);
    let opponent_pieces =
        BitBoard::from_board(&board.base_board,
                             |Piece(piece_type, color)| color != board.to_move()
                             && piece_type != PieceType::Empty);
    let all_pieces = BitBoard::all_from_board(&board.base_board);
    
    sjadam_friendly_moves(&mut sjadam_squares, &friendly_pieces,
                          &all_pieces, square);

    //println!("Friendly squares:\n{:?}All_pieces:\n{:?}", friendly_pieces, all_pieces);
    //println!("Sjadam squares from {}:\n{:?}", square, sjadam_squares);

    sjadam_opponent_moves(&mut sjadam_squares, &opponent_pieces, &all_pieces);

    let mut chess_moves = vec![]; // Moves with both
    let mut pure_sjadam_moves = vec![]; // Moves with only a sjadam part

    let king_pos = move_gen::king_pos(&board.base_board);
    let piece_moved = board.base_board[square];
    
    for chess_move_square in BoardIter::new()
        .filter(|&i| sjadam_squares.get(i))
        .filter(|&i| piece_moved.0 != PieceType::Pawn || (i.file_rank().1 != 0 && i.file_rank().1 != 7)) // TODO: Pawns cannot promote in sjadam now
        
    {
        // TODO: Currently only checks if player is in regular check, not in sjadam.
        // TODO: Currently pawns are not allowed to queen in a sjadam move

        let sjadam_move = SjadamMove::from_sjadam_move (square, chess_move_square, board);
        let old_board = board.clone();
        if chess_move_square != square {
            // "Do" the pure sjadam part of the move, but keep the same player to move
            board.do_move(sjadam_move.clone());
            board.base_board.to_move = !board.base_board.to_move;
        }

        debug_assert!(!board.base_board[chess_move_square].is_empty(),
                      "Square {} is empty on \n{:?}, but is marked in {:?}",
                      chess_move_square, board.base_board, sjadam_squares);

        // TODO: Currently does not allow moves that puts the player in conventional check,
        // even though this is allowed in sjadam
        if piece_moved.0 == PieceType::King {
            let new_king_pos = move_gen::king_pos(&board.base_board);
            let is_in_check = move_gen::is_attacked(&board.base_board, new_king_pos);
            move_gen::legal_moves_for_piece(&mut board.base_board, chess_move_square,
                                            &mut chess_moves, is_in_check, new_king_pos);
            if chess_move_square != square && !is_in_check {
                pure_sjadam_moves.push(sjadam_move.clone());
            }
        }
        else {
            let is_in_check = move_gen::is_attacked(&board.base_board, king_pos);
            move_gen::legal_moves_for_piece(&mut board.base_board, chess_move_square,
                                            &mut chess_moves, is_in_check, king_pos);
            if chess_move_square != square && !is_in_check {
                pure_sjadam_moves.push(sjadam_move.clone());
            }
        }

        if chess_move_square != square {
            board.undo_move(sjadam_move);
            board.base_board.to_move = !board.base_board.to_move;
            debug_assert_eq!(*board, old_board)
        }
    }
    let mut combined_moves = chess_moves.iter()
        .map(|mv| SjadamMove { checkers: if square != mv.from { Some((square, mv.from)) }
                               else { None },
                               chess_move: Some(*mv),
                               old_castling_en_passant: board.base_board.castling_en_passant,
                               old_half_move_clock: board.base_board.half_move_clock })
        .collect::<Vec<_>>();
    combined_moves.append(&mut pure_sjadam_moves);
    let move_cmp = |mv1: &SjadamMove, mv2: &SjadamMove| {
        let (from1, to1) = mv1.from_to_squares();
        let (from2, to2) = mv2.from_to_squares();
        to1.cmp(&to2)
            .then((board.base_board[from1].0).cmp(&board.base_board[from2].0))
    };
    let move_eq = |mv1: &mut SjadamMove, mv2: &mut SjadamMove| {
        let (from1, to1) = mv1.from_to_squares();
        let (from2, to2) = mv2.from_to_squares();
        to1 == to2 && board.base_board[from1].0 == board.base_board[from2].0
    };
    // Two moves are considered equal if they place the same piece on the same square,
    // and do not castle
    combined_moves.sort_by(move_cmp);
    //println!("Found {} moves for {} at {} before removing duplicates",
             //combined_moves.len(), piece_moved.0, square);
    combined_moves.dedup_by(move_eq);
    //println!("Found {} moves for {} at {} after removing duplicates",
             //combined_moves.len(), piece_moved.0, square);
    // TODO: Here castling and non-castling moves will be considered equal,
    // and may be removed. Fix that
    combined_moves
}

/// Recursively sets all available sjadam-move squares 
fn sjadam_friendly_moves(sjadam_squares: &mut BitBoard, friendly_pieces: &BitBoard,
                         all_pieces: &BitBoard, square: Square) {
    let Square(i) = square;
    for x in [-1, 0, 1].iter() {
        for y in [-1, 0, 1].iter() {
            let (file, rank) = (square.file_rank().0 as i8, square.file_rank().1 as i8);
            if (file <= 1 && *x == -1) || (file >= 6 && *x == 1)
                || (rank <= 1 && *y == -1) || (rank >= 6 && *y == 1) {
                    continue
                }
            let dest_square = Square(i.wrapping_add((16 * y) as u8).wrapping_add((2 * x) as u8));
            let jumping_square = Square(i.wrapping_add((8 * y) as u8).wrapping_add(*x as u8));
            if friendly_pieces.get(jumping_square) && !all_pieces.get(dest_square) &&
                !sjadam_squares.get(dest_square) {
                    sjadam_squares.set(dest_square);
                    sjadam_friendly_moves(sjadam_squares, friendly_pieces,
                                          all_pieces, dest_square);
                }
        }
    }
}

/// Sets all available opponent sjadam-move squares 
fn sjadam_opponent_moves(sjadam_squares: &mut BitBoard, opponent_pieces: &BitBoard,
                         all_pieces: &BitBoard) {
    let old_sjadam_squares = sjadam_squares.clone();
    for square in BoardIter::new()
        .filter(|i|old_sjadam_squares.get(*i))
    {
        let Square(i) = square;
        for x in [-1, 0, 1].iter() {
            for y in [-1, 0, 1].iter() {
                let (file, rank) = (square.file_rank().0 as i8, square.file_rank().1 as i8);
                if (file <= 1 && *x == -1) || (file >= 6 && *x == 1)
                    || (rank <= 1 && *y == -1) || (rank >= 6 && *y == 1) {
                        continue
                    }
                let dest_square = Square(i.wrapping_add((16 * y) as u8).wrapping_add((2 * x) as u8));
                let jumping_square = Square(i.wrapping_add((8 * y) as u8).wrapping_add(*x as u8));
                if opponent_pieces.get(jumping_square) && !all_pieces.get(dest_square) {
                    sjadam_squares.set(dest_square);
                }
            }
        }
    }
}

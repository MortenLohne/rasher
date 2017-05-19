use board::sjadam_move::SjadamMove;
use board::std_board::BoardIter;
use board::std_board::ChessBoard;
use board::std_board::PieceType;
use board::std_board::Piece;
use board::std_board::Square;
use board::std_move::ChessMove;
use board::std_board::PieceType::*;

use search_algorithms::board::Color::*;
use search_algorithms::board::EvalBoard;
use board::sjadam_board::SjadamBoard;

// use board::std_move_gen::move_gen;

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
    // Sets the square to true
    fn set(&mut self, idx: Square) {
        let Square(i) = idx;
        debug_assert!(i < 64, format!("Tried to index pos {} on board{:?}!", idx, self));
        self.board |= 1<<i;
    }
    #[allow(dead_code)]
    // Sets the square to false
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

    let old_king_pos = king_pos(&board.base_board);
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
        // even though this is "allowed" in sjadam
        if piece_moved.0 == PieceType::King {
            let new_king_pos = king_pos(&board.base_board);
            legal_moves_for_piece(&mut board.base_board, chess_move_square,
                                  &mut chess_moves, false, new_king_pos);
            if chess_move_square != square {
                pure_sjadam_moves.push(sjadam_move.clone());
            }
        }
        else {
            legal_moves_for_piece(&mut board.base_board, chess_move_square,
                                            &mut chess_moves, false, old_king_pos);
            if chess_move_square != square {
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



#[inline(never)]
pub fn all_legal_base_moves (board : &ChessBoard) -> Vec<ChessMove> {
    let mut mut_board = board.clone();
    if board.half_move_clock > 100 { // Draw by 50-move rule
        return vec![]
    }
    let mut moves = Vec::new();
    let king_pos = king_pos(board);
    for i in 0..64 {
        match board.piece_at(Square(i)) {
            Piece(Empty, White) => continue,
            Piece(_, color) if color == board.to_move => {
                legal_moves_for_piece(&mut mut_board, Square(i), &mut moves, false, king_pos);
            }
            _ => continue,
        }
    }
    moves
}

/// Adds all the legal moves for the piece in this position, to the input vector
/// Adds all moves, also those that put the player in check
#[inline(never)]
pub fn legal_moves_for_piece(board : &mut ChessBoard, square : Square, moves : &mut Vec<ChessMove>,
                         is_in_check : bool, king_pos : Square) { 
    let Piece(piece, color) = board.piece_at(square);
    
    debug_assert!(color == board.to_move && piece != Empty,
                  "Tried to find move for {} at {} at:\n{:?}",
                  board[square], square, board);
    
    match piece {
        King => legal_moves_for_king(board, square, moves),
        
        Queen =>  {
            add_moves_diagonally (board, square, moves, king_pos, is_in_check);
            add_straight_moves(board, square, moves, king_pos, is_in_check);
        },
        Rook => {
            add_straight_moves(board, square, moves, king_pos, is_in_check);
        },
        Bishop => {
            add_moves_diagonally(board, square, moves, king_pos, is_in_check);
        },
        Knight => legal_moves_for_knight(board, square, moves, is_in_check, king_pos),
        
        Pawn => legal_moves_for_pawn(board, square, moves, is_in_check, king_pos),
        
        Empty => (),
    }
}

#[inline(never)]
fn legal_moves_for_king(board : &mut ChessBoard, square : Square, moves : &mut Vec<ChessMove>) {
    
    let file = (square.0 & 0b0000_0111) as i8;
    let rank = (square.0 >> 3) as i8;
    // If the king and the two castling squares are not in check, castling is allowed
    // There must be no pieces between the castling pieces

    // Kingside castling
    if board.can_castle_kingside(board.to_move) {
        
        let mut can_castle_here = true;
        if is_attacked(board, square) {
            can_castle_here = false;
        }
        
        // Check that the two squares are empty and not in check
        for n in [1, 2].iter() {
            debug_assert!(file == 4, format!("Error: King tried to castle from {} on:{}.",
                                             square, board));
            let square_checked = Square(square.0 + n);
            if board.piece_at(square_checked) != Piece(Empty, White)
                || is_attacked(board, square_checked) {
                    can_castle_here = false;
                }
            
        }
        if can_castle_here {
            moves.push(ChessMove::new(&board, square, Square(square.0 + 2)));
        }
        
        
    }
    // Queenside castling
    if board.can_castle_queenside(board.to_move) {
        let mut can_castle_here = true;
        if is_attacked(board, square) {
            can_castle_here = false;
        }
        // Check that the two squares are empty and not in check
        for n in [1, 2].iter() {
            debug_assert!(file == 4, format!("Error: File is {}.", file));
            let square_checked = Square(square.0 - n);
            if board.piece_at(square_checked) != Piece(Empty, White) ||
                is_attacked(board, square_checked)
                 {
                    can_castle_here = false;
                }
        }
        // Check that the knight-square is empty
        if board.piece_at(Square(square.0 - 3)) != Piece(Empty, White) {
            can_castle_here = false;
        }
        if can_castle_here {
            moves.push(ChessMove::new(&board, square, Square(square.0 - 2)));
        }
        
    }
    
    for i in -1..2 {
        for j in -1..2 {
            
            if file + i < 0 || file + i >= 8 ||
                rank + j < 0 || rank + j >= 8 ||
                (j == 0 && i == 0) {
                    continue;
                }
            let new_pos = ((rank + j) * 8 + file + i) as u8;

            
            // Check that the square is not occupied by a friendly piece
            let Piece(piece_to, color_to) = board.piece_at(Square(new_pos));
            let c_move = ChessMove::new(board, square, Square(new_pos));

            if piece_to == Empty {
                let old_piece = board[square];
                board[square] = Piece::empty();
                moves.push(c_move);
                board[square] = old_piece;
            }
            else if color_to != board.to_move {
                add_if_legal_simple(board, c_move, moves);
            }
        }
    }
}

#[inline(never)]
fn legal_moves_for_knight(board : &ChessBoard, square : Square, moves : &mut Vec<ChessMove>,
                          is_in_check : bool, king_pos : Square) {
    let file = (square.0 & 0b0000_0111) as i8;
    let rank = (square.0 >> 3) as i8;
    
    for &(i, j) in [(-2, -1), (-2, 1), (-1, -2), (-1, 2),
                    (1, -2), (1, 2), (2, -1), (2, 1)].iter() {
        if file + i < 0 || file + i >= 8 || rank + j < 0 || rank + j >= 8 {
            continue;
        }
        let new_pos = ((rank + j) * 8 + file + i) as u8;
        
        let Piece(piece_to, color_to) = board.piece_at(Square(new_pos));
        let c_move = ChessMove::new (&board, square, Square(new_pos));

        if piece_to == Empty || color_to != board.to_move {
            //println!("Knight can move to {}, onto a {} {}, on: {}",
            //             Square(new_pos), color_to, piece_to, board);
            add_if_legal(board, c_move, moves,
                         king_pos, is_in_check);
        }
    }
}

#[inline(never)]
fn legal_moves_for_pawn(board : &ChessBoard, square : Square, moves : &mut Vec<ChessMove>,
                          is_in_check : bool, king_pos : Square) {
    // Helper variables
    let file = (square.0 & 0b0000_0111) as i8;
    let rank = (square.0 >> 3) as i8;
    let pos = square.0 as i8;
    let (start_rank, prom_rank, direction) =
        if board.to_move == White { (6, 1, -1) } else { (1, 6, 1) };
    debug_assert!(rank > 0 && rank < 7);;
    // Checks if there are any pieces available for capture to the left,
    // including en passant capture
    if file > 0 {
        let take_square = Square((pos + direction * 8) as u8 - 1);
        let Piece(piece, color) = board.piece_at(take_square);
        if piece != Empty && color != board.to_move {
            if rank == prom_rank {
                for piece_type in [Queen, Rook, Bishop, Knight].iter() {
                    let c_move = ChessMove::new_prom(&board, square, take_square, *piece_type);
                    add_if_legal(board, c_move,
                                 moves, king_pos, is_in_check);
                }
            }
            else {
                let c_move = ChessMove::new(board, square, take_square);
                add_if_legal(board, c_move, moves,
                             king_pos, is_in_check);
            }
        }
        if board.en_passant_square().is_some()
            && take_square == board.en_passant_square().unwrap()
        {
            let c_move = ChessMove::new(board, square, take_square);
            add_if_legal_simple(board, c_move, moves);
        }
    }
    // Ditto, but to the right
    if file < 7 {
        let take_square = Square((pos + direction * 8) as u8 + 1);
        let Piece(piece, color) = board.piece_at(take_square);
        if piece != Empty && color != board.to_move {
            if rank == prom_rank {
                for piece_type in [Queen, Rook, Bishop, Knight].iter() {
                    let c_move = ChessMove::new_prom(board, square, take_square, *piece_type);
                    add_if_legal(board, c_move,
                                 moves, king_pos, is_in_check);
                }
            }
            else {
                let c_move = ChessMove::new(board, square, take_square);
                add_if_legal(board, c_move, moves,
                             king_pos, is_in_check);
            }
        }
        if board.en_passant_square().is_some()
            && take_square == board.en_passant_square().unwrap()
        {
            let c_move = ChessMove::new(board, square, take_square);
            add_if_legal_simple(board, c_move, moves);
        }
    }

    //Checks if the pawn can walk forward one or two squares, promote
    let square_in_front = Square::from_ints(file as u8, (rank + direction) as u8);
    
    if board.piece_at(square_in_front).is_empty() {
        if rank == prom_rank {
            for piece_type in [Queen, Rook, Bishop, Knight].iter() {
                let c_move = ChessMove::new_prom(board, square, square_in_front, *piece_type);
                add_if_legal(board, c_move,
                             moves, king_pos, is_in_check);
            }
        }
        else {
            let c_move = ChessMove::new(board, square, square_in_front);
            add_if_legal(board, c_move, moves, king_pos,
                         is_in_check);
        }
        if rank == start_rank {
            let square_2_in_front = Square::from_ints(file as u8,
                                                      (rank + direction * 2) as u8);
            let c_move = ChessMove::new(board, square, square_2_in_front);
            if board.piece_at(square_2_in_front).is_empty() {
                add_if_legal(board, c_move, moves,
                             king_pos, is_in_check);
            }
        }
    }
}

/// Adds a move to the list. In sjadam, every move is considered legal.
fn add_if_legal(board : &ChessBoard, c_move : ChessMove, moves : &mut Vec<ChessMove>,
                king_pos_c : Square, is_in_check : bool) {
    moves.push(c_move);
}

/// Adds a move to the list. In sjadam, every move is considered legal.
fn add_if_legal_simple (board : &ChessBoard, c_move : ChessMove, moves : &mut Vec<ChessMove>) {
    moves.push(c_move);
}

/// Returns whether a square is under attack by the side not to move
pub fn is_attacked(board : &ChessBoard, square : Square) -> bool {
    if board.piece_at(square).0 != Empty {
        debug_assert_eq!(board.to_move(), board.piece_at(square).1,
                         "{:?}\n Tried to check if {} {} at {} was attacked.",
                         board,  board.piece_at(square).1, board.piece_at(square).0, square);
    }
    // Direction enemy pawns are coming from
    let pawn_direction = if board.to_move == White { -1 } else { 1 };
    let pos = square.0 as i8;
    let file = (square.0 & 0b0000_0111) as i8;
    let rank = (square.0 >> 3) as i8;

    if file > 0 {
        if (board.to_move == White && rank > 1) || (board.to_move == Black && rank < 6) {
            let pawn_square = Square((pos + pawn_direction * 8) as u8 - 1);
            let Piece(piece, color) = board.piece_at(pawn_square);
            if piece == Pawn && color != board.to_move { return true; }
        }
    }
    if file < 7 {
        if (board.to_move == White && rank > 1) || (board.to_move == Black && rank < 6) {
            let pawn_square = Square((pos + pawn_direction * 8) as u8 + 1);
            
            let Piece(piece, color) = board.piece_at(pawn_square);
            if piece == Pawn && color != board.to_move { return true; }
        }
    }
    // println!("Checked pawns");

    for &(i, j) in [(0, 1), (1, 0), (0, -1), (-1, 0)].iter() {
        if check_threats_in_direction (i, j, board, square, &vec![Queen, Rook]) {
            return true;
        }
    }
    for &(i, j) in [(1, 1), (1, -1), (-1, 1), (-1, -1)].iter() {
        if check_threats_in_direction (i, j, board, square, &vec![Queen, Bishop]) {
            return true;
        }
    }
    
    for &(i, j) in [(-2, -1), (-2, 1), (-1, -2), (-1, 2),
                    (1, -2), (1, 2), (2, -1), (2, 1)].iter() {
        if file + i < 0 || file + i >= 8 || rank + j < 0 || rank + j >= 8 {
            continue;
        }
        let new_pos = ((rank + j) * 8 + file + i) as u8;
        
        let Piece(piece_to, color_to) = board.piece_at(Square(new_pos));
        if piece_to == Knight && color_to != board.to_move {
            return true;
        }
    }
    for i in -1..2 {
        for j in -1..2 {
            if file + i < 0 || file + i >= 8 || rank + j < 0 || rank + j >= 8 ||
                (j == 0 && i == 0) {
                    continue;
                }
            let new_pos = ((rank + j) * 8 + file + i) as u8;

            // Check that there is no enemy king around
            let Piece(piece_to, color_to) = board.piece_at(Square(new_pos));
            if piece_to == King && color_to != board.to_move {
                return true;
            }
        }
    }
    
    false
}

fn check_threats_in_direction (i : i8, j : i8, board : &ChessBoard, square : Square,
                               threats : &Vec<PieceType>) -> bool {
    let mut file = (square.0 & 0b0000_0111) as i8;
    let mut rank = (square.0 >> 3) as i8;
    loop {
        file += i;
        rank += j;
        if file < 0 || rank < 0 || file >= 8 || rank >= 8 {
            return false;
        }
        match board.piece_at(Square::from_ints(file as u8, rank as u8)) {
            Piece(Empty, _) => continue,
            Piece(piece, color) => {
                if color == board.to_move { return false; }
                for threat in threats {
                    if piece == *threat {
                        return true;
                    }
                }
                return false;
            },
        }
    }
}

fn add_moves_diagonally (board : &ChessBoard, square : Square, moves : &mut Vec<ChessMove>,
                         king_pos : Square, is_in_check : bool) {
    for &(i, j) in [(1, 1), (1, -1), (-1, 1), (-1, -1)].iter() {
        add_moves_in_direction(i, j, board, square, moves, king_pos, is_in_check);
    }
}

fn add_straight_moves(board : &ChessBoard, square : Square, moves : &mut Vec<ChessMove>,
                      king_pos : Square, is_in_check : bool) {
    for &(i, j) in [(0, 1), (1, 0), (0, -1), (-1, 0)].iter() {
        add_moves_in_direction(i, j, board, square, moves, king_pos, is_in_check);
    }
}
    
fn add_moves_in_direction (i : i8, j : i8, board : &ChessBoard, square : Square,
                           moves : &mut Vec<ChessMove>, king_pos : Square, is_in_check : bool) {
    
    let mut file = (square.0 & 0b0000_0111) as i8;
    let mut rank = (square.0 >> 3) as i8;
    loop {
        file += i;
        rank += j;

        if file < 0 || rank < 0 || file >= 8 || rank >= 8 {
            break;
        }
        let target_square = Square::from_ints(file as u8, rank as u8);
        let piece_to = board.piece_at(target_square);

        let c_move = ChessMove::new(board, square, target_square);
        match piece_to {
            Piece(Empty, _) => {
                add_if_legal(board, c_move, moves, king_pos, is_in_check);
                continue; },
            
            Piece(_, color) => {
                if color != board.to_move {
                    add_if_legal(board, c_move, moves, king_pos, is_in_check);
                }
                break; }, // Break after finding a piece, friend or foe
        }
    }
}

pub fn king_pos (board : &ChessBoard) -> Square {
    board.king_pos(board.to_move())
}

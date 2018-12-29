use board::std_board::ChessBoard;
use board::std_board::Piece;
use board::std_board::Square;
use board::std_move::ChessMove;
use board::std_board::PieceType;
use board::std_board::PieceType::*;
use search_algorithms::board::Color::*;

use std::cmp::Ordering;
use search_algorithms::board::Board;
use search_algorithms::board::Color;

#[inline(never)]
pub fn all_legal_moves (board : &ChessBoard) -> (Vec<ChessMove>, Vec<ChessMove>) {
    let mut mut_board = board.clone();
    if board.half_move_clock > 100 { // Draw by 50-move rule
        return (vec![], vec![])
    }
    let mut winning_moves = Vec::new();
    let mut active_moves = Vec::new();
    let mut quiet_moves = Vec::new();
    let king_pos = king_pos(board);
    let is_in_check = is_attacked(board, king_pos);
    for i in 0..64 {
        if board[Square(i)].color() == Some(board.to_move) {
            legal_moves_for_piece(&mut mut_board, Square(i), &mut winning_moves,
                                  &mut active_moves, &mut quiet_moves, is_in_check, king_pos);
        }
    }
    winning_moves.append(&mut active_moves);
    (winning_moves, quiet_moves)
}

/// Adds all the legal moves for the piece in this position, to the input vector
/// Takes in the king position for the moving player, and whether they are currently in check,
/// to speed up the move generation
#[inline(never)]
pub fn legal_moves_for_piece(board: &mut ChessBoard, square: Square,
                             winning_moves: &mut Vec<ChessMove>,
                             active_moves: &mut Vec<ChessMove>,
                             quiet_moves: &mut Vec<ChessMove>,
                             is_in_check : bool, king_pos : Square) { 
    let piece = board[square].piece_type();
    match piece {
        King => legal_moves_for_king(board, square, quiet_moves),
        
        Queen =>  {
            add_moves_diagonally (board, square, winning_moves, active_moves, quiet_moves,
                                  king_pos, is_in_check);
            add_straight_moves(board, square, winning_moves, active_moves, quiet_moves,
                               king_pos, is_in_check);
        },
        Rook => {
            add_straight_moves(board, square, winning_moves, active_moves, quiet_moves,
                               king_pos, is_in_check);
        },
        Bishop => {
            add_moves_diagonally(board, square, winning_moves, active_moves, quiet_moves,
                                 king_pos, is_in_check);
        },
        Knight => legal_moves_for_knight(board, square, winning_moves, active_moves,
                                         quiet_moves, is_in_check, king_pos),
        
        Pawn => legal_moves_for_pawn(board, square, winning_moves, active_moves, quiet_moves,
                                     is_in_check, king_pos),
        
        Empty => (),
    }
}

#[inline(never)]
fn legal_moves_for_king(board : &mut ChessBoard, square : Square, moves : &mut Vec<ChessMove>) {
    
    let file = square.file() as i8;
    let rank = square.rank() as i8;
    // If the king and the two castling squares are not in check, castling is allowed
    // There must be no pieces between the castling pieces

    // Kingside castling
    if board.can_castle_kingside(board.to_move) {
        
        let mut can_castle_here = !is_attacked(board, square);
        
        // Check that the two squares are empty and not in check
        for n in &[1, 2] {
            debug_assert!(file == 4, format!("Error: King tried to castle from {} on:{}.",
                                             square, board));
            let square_checked = Square(square.0 + n);
            if !board.piece_at(square_checked).is_empty()
                || is_attacked(board, square_checked) {
                    can_castle_here = false;
                }
        }
        if can_castle_here {
            moves.push(ChessMove::new(square, Square(square.0 + 2)));
        }
    }
    
    // Queenside castling
    if board.can_castle_queenside(board.to_move) {
        let mut can_castle_here = !is_attacked(board, square);

        // Check that the two squares are empty and not in check
        for n in &[1, 2] {
            debug_assert!(file == 4, format!("Error: File is {}.", file));
            let square_checked = Square(square.0 - n);
            if !board.piece_at(square_checked).is_empty() ||
                is_attacked(board, square_checked)
                 {
                    can_castle_here = false;
                }
        }
        // Check that the knight-square is empty
        if !board.piece_at(Square(square.0 - 3)).is_empty() {
            can_castle_here = false;
        }
        if can_castle_here {
            moves.push(ChessMove::new(square, Square(square.0 - 2)));
        }
    }
    
    for i in -1..2 {
        for j in -1..2 {
            
            if file + i < 0 || file + i >= 8 ||
                rank + j < 0 || rank + j >= 8 ||
                (j == 0 && i == 0) {
                    continue;
                }
            let new_pos = Square(((rank + j) * 8 + file + i) as u8);

            // Check that the square is not occupied by a friendly piece
            let mv = ChessMove::new(square, new_pos);
            if board[new_pos].is_empty() {
                let old_piece = board[square];
                board[square] = Piece::empty();
                if !is_attacked(board, new_pos) {
                    moves.push(mv);
                }
                board[square] = old_piece;
            }
            else if board[new_pos].color().unwrap() != board.to_move {
                add_if_legal_simple(board, mv, moves);
            }
        }
    }
}

#[inline(never)]
fn legal_moves_for_knight(board: &ChessBoard, square: Square, winning_moves: &mut Vec<ChessMove>,
                          active_moves: &mut Vec<ChessMove>, quiet_moves: &mut Vec<ChessMove>,
                          is_in_check: bool, king_pos: Square) {
    let file = square.file() as i8;
    let rank = square.rank() as i8;
    
    for &(i, j) in &[(-2, -1), (-2, 1), (-1, -2), (-1, 2),
                    (1, -2), (1, 2), (2, -1), (2, 1)] {
        if file + i < 0 || file + i >= 8 || rank + j < 0 || rank + j >= 8 {
            continue;
        }
        let new_pos = Square(((rank + j) * 8 + file + i) as u8);
        
        let piece_to = board[new_pos];
        let mv = ChessMove::new(square, new_pos);
        
        if board[new_pos].color() != Some(board.to_move) {
            match piece_to.value().abs().partial_cmp(&board[square].value().abs()).unwrap() {
                Ordering::Less =>
                    add_if_legal(board, mv, quiet_moves, king_pos, is_in_check),
                Ordering::Equal =>
                    add_if_legal(board, mv, active_moves, king_pos, is_in_check),
                Ordering::Greater =>
                    add_if_legal(board, mv, winning_moves, king_pos, is_in_check),
            }
        }
    }
}

#[inline(never)]
fn legal_moves_for_pawn(board: &ChessBoard, square: Square, winning_moves: &mut Vec<ChessMove>,
                        _active_moves: &mut Vec<ChessMove>, quiet_moves: &mut Vec<ChessMove>,
                        is_in_check: bool, king_pos: Square) {
    // Helper variables
    let file = square.file() as i8;
    let rank = square.rank() as i8;
    let pos = square.0 as i8;
    
    let (start_rank, prom_rank, direction) =
        if board.to_move == White { (6, 1, -1) } else { (1, 6, 1) };
    debug_assert!(rank > 0 && rank < 7);
    
    // Checks if there are any pieces available for capture to the left,
    // including en passant capture
    if file > 0 {
        let take_square = Square((pos + direction * 8) as u8 - 1);
        if !board[take_square].is_empty() && board[take_square].color().unwrap() != board.to_move {
            if rank == prom_rank {
                for piece_type in &[Queen, Rook, Bishop, Knight] {
                    let mv = ChessMove::new_prom(square, take_square, *piece_type);
                    add_if_legal(board, mv,
                                 winning_moves, king_pos, is_in_check);
                }
            }
            else {
                let mv = ChessMove::new(square, take_square);
                if board[take_square].piece_type() == Pawn {
                    add_if_legal(board, mv, quiet_moves,
                                 king_pos, is_in_check);
                }
                else {
                    add_if_legal(board, mv, winning_moves,
                                 king_pos, is_in_check);
                }
            }
        }
        if board.en_passant_square().is_some()
            && take_square == board.en_passant_square().unwrap()
        {
            let mv = ChessMove::new(square, take_square);
            add_if_legal_simple(board, mv, quiet_moves);
        }
    }
    // Ditto, but to the right
    if file < 7 {
        let take_square = Square((pos + direction * 8) as u8 + 1);
        let piece = board[take_square].piece_type();
        if piece != Empty && board[take_square].color().unwrap() != board.to_move {
            if rank == prom_rank {
                for piece_type in &[Queen, Rook, Bishop, Knight] {
                    let mv = ChessMove::new_prom(square, take_square, *piece_type);
                    add_if_legal(board, mv, winning_moves, king_pos, is_in_check);
                }
            }
            else {
                let mv = ChessMove::new(square, take_square);
                if board[take_square].piece_type() == Pawn {
                    add_if_legal(board, mv, quiet_moves, king_pos, is_in_check);
                }
                else {
                    add_if_legal(board, mv, winning_moves, king_pos, is_in_check);
                }
            }
        }
        if board.en_passant_square().is_some()
            && take_square == board.en_passant_square().unwrap()
        {
            let mv = ChessMove::new(square, take_square);
            add_if_legal_simple(board, mv, quiet_moves);
        }
    }

    //Checks if the pawn can walk forward one or two squares, promote
    let square_in_front = Square::from_ints(file as u8, (rank + direction) as u8);
    
    if board.piece_at(square_in_front).is_empty() {
        if rank == prom_rank {
            for piece_type in &[Queen, Rook, Bishop, Knight] {
                let mv = ChessMove::new_prom(square, square_in_front, *piece_type);
                add_if_legal(board, mv,
                             winning_moves, king_pos, is_in_check);
            }
        }
        else {
            let mv = ChessMove::new(square, square_in_front);
            add_if_legal(board, mv, quiet_moves, king_pos,
                         is_in_check);
        }
        if rank == start_rank {
            let square_2_in_front = Square::from_ints(file as u8,
                                                      (rank + direction * 2) as u8);
            let mv = ChessMove::new(square, square_2_in_front);
            if board.piece_at(square_2_in_front).is_empty() {
                add_if_legal(board, mv, quiet_moves,
                             king_pos, is_in_check);
            }
        }
    }
}

/// Checks that the move does not put the player in check
/// Does not work in some special cases, such as en passant. `add_if_legal_simple` should be used then
fn move_is_not_check(board: &ChessBoard, mv: ChessMove,
                 king_pos: Square, is_in_check: bool) -> bool {
    !(is_in_check || is_pinned_to_piece(board, mv.from, king_pos)) ||
        move_is_not_check_simple(board, mv)

}

/// Checks that the move does not put the player in check
/// This check is more expensive, but works for _all_ positions
fn move_is_not_check_simple(board: &ChessBoard, mv: ChessMove) -> bool {
    let mut cloned = board.clone();
    cloned.do_move(mv);

    cloned.to_move = !cloned.to_move;

    !is_attacked(&cloned, king_pos(&cloned))
}

/// Checks that the move does not put the player in check
/// , and add the move to the vector
/// Does not work in some special cases, such as en passant. `add_if_legal_simple` should be used then
#[inline(never)]
fn add_if_legal(board : &ChessBoard, mv : ChessMove, moves : &mut Vec<ChessMove>,
                king_pos : Square, is_in_check : bool) {

    if move_is_not_check(board, mv, king_pos, is_in_check) {
        moves.push(mv);
    }
}

/// Checks that the move does not put the player in check
/// , and add the move to the vector
/// This check is more expensive, but works for _all_ positions
#[inline(never)]
fn add_if_legal_simple (board : &ChessBoard, mv : ChessMove, moves : &mut Vec<ChessMove>) {

    if move_is_not_check_simple(board, mv) {
        moves.push(mv);
    }
}

#[inline(never)]
pub fn is_pinned_to_piece(board : &ChessBoard, pinee_pos : Square, pinner_pos : Square) -> bool {
    debug_assert!(pinee_pos != pinner_pos);
    let (pinner_file, pinner_rank) = pinner_pos.file_rank(); // The piece it is pinned to
    let (pinee_file, pinee_rank) = pinee_pos.file_rank(); // The piece that is pinned
    let (file_diff, rank_diff) = ((pinee_file as i8 - pinner_file as i8),
                                  (pinee_rank as i8 - pinner_rank as i8));
    // If on the same diagonal
    if file_diff.abs() == rank_diff.abs() {
        
        let (dir_file, dir_rank) = (file_diff.signum(), rank_diff.signum());
        let (mut cur_file, mut cur_rank) = (pinner_file as i8, pinner_rank as i8);
        
        // Check for enemy pieces, or pieces blocking, on the diagonal
        loop {
            cur_file += dir_file;
            cur_rank += dir_rank;
            if cur_file < 0 || cur_file >= 8 || cur_rank < 0 || cur_rank >= 8 {
                return false;
            }
            if cur_file == pinee_file as i8 {
                continue;
            }
            let piece = board[Square::from_ints(cur_file as u8, cur_rank as u8)];
            match piece.piece_type() {
                Empty => continue,
                // If you first find an opposite color queen/bishop, you're pinned
                Bishop | Queen if piece.color().unwrap() != board.to_move => return true,
                // If you first find any other piece blocking the check, you're not pinned
                _ => return false,
            }
        }
    }
    // If on the same file
    else if file_diff == 0 {
        debug_assert!(rank_diff != 0);
        let dir_rank = rank_diff.signum();
        let mut cur_rank = pinner_rank as i8;
        
        // Check for enemy pieces, or pieces blocking, on the file
        loop {
            cur_rank += dir_rank;
            if cur_rank < 0 || cur_rank >= 8 {
                return false;
            }
            if cur_rank == pinee_rank as i8 {
                continue;
            }
            let piece = board[Square::from_ints(pinner_file as u8, cur_rank as u8)];
            match piece.piece_type() {
                Empty => continue,
                Rook | Queen if piece.color().unwrap() != board.to_move => return true,
                _ => return false,
            }
        }
    }
    else if rank_diff == 0 {
        debug_assert!(file_diff != 0);
        let dir_file = file_diff.signum();
        let mut cur_file = pinner_file as i8;
        
        // Check for enemy pieces, or pieces blocking, on the file
        loop {
            cur_file += dir_file;
            if cur_file < 0 || cur_file >= 8 {
                return false;
            }
            if cur_file == pinee_file as i8 {
                continue;
            }
            let piece = board[Square::from_ints(cur_file as u8, pinner_rank as u8)];
            if piece.is_empty() { continue }
            if (piece.piece_type() == Rook || piece.piece_type() == Queen)
                && piece.color().unwrap() != board.to_move {
                    return true;
                }
            return false;
        }
    }
    else {
        false
    }
}

/// Returns whether a square is under attack
pub fn is_attacked_by_color(board: &ChessBoard, square: Square, color: Color) -> bool {
    // Direction enemy pawns are coming from
    let pawn_direction : i8 = if color == White { -1 } else { 1 };
    let file = square.file() as i8;
    let rank = square.rank() as i8;

    if file > 0 && ((color == White && rank > 1) || (color == Black && rank < 6))
    {
        let pawn_square = Square::from_ints(file as u8 - 1, (rank as i8 + pawn_direction) as u8);

        if board[pawn_square].piece_type() == Pawn
            && board[pawn_square].color().unwrap() != color
        {
            return true;
        }
    }

    if file < 7 && ((color == White && rank > 1) || (color == Black && rank < 6))
    {

        let pawn_square = Square::from_ints(file as u8 + 1, (rank as i8 + pawn_direction) as u8);

        if board[pawn_square].piece_type() == Pawn
            && board[pawn_square].color().unwrap() != color
        {
            return true;
        }
    }


    for &(i, j) in &[(0, 1), (1, 0), (0, -1), (-1, 0)] {
        if check_threats_in_direction (i, j, board, square, &[Queen, Rook], color) {
            return true;
        }
    }
    for &(i, j) in &[(1, 1), (1, -1), (-1, 1), (-1, -1)] {
        if check_threats_in_direction (i, j, board, square, &[Queen, Bishop], color) {
            return true;
        }
    }

    for &(i, j) in &[(-2, -1), (-2, 1), (-1, -2), (-1, 2),
        (1, -2), (1, 2), (2, -1), (2, 1)] {
        if file + i < 0 || file + i >= 8 || rank + j < 0 || rank + j >= 8 {
            continue;
        }
        let new_pos = Square(((rank + j) * 8 + file + i) as u8);

        if board[new_pos].piece_type() == Knight
            && board[new_pos].color().unwrap() != color {
            return true;
        }
    }
    for i in -1..2 {
        for j in -1..2 {
            if file + i < 0 || file + i >= 8 || rank + j < 0 || rank + j >= 8 ||
                (j == 0 && i == 0) {
                continue;
            }
            let new_pos = Square(((rank + j) * 8 + file + i) as u8);

            // Check that there is no enemy king around
            if board[new_pos].piece_type() == King
                && board[new_pos].color().unwrap() != color
            {
                return true;
            }
        }
    }
    false
}

/// Returns whether a square is under attack by the side not to move
#[inline(never)]
pub fn is_attacked(board : &ChessBoard, square : Square) -> bool {
    is_attacked_by_color(board, square, board.side_to_move())
}

/// Checks if there are any threats in the direction to the given color.
fn check_threats_in_direction (i : i8, j : i8, board : &ChessBoard, square : Square,
                               threats : &[PieceType], color: Color) -> bool {
    let mut file = (square.0 & 0b0000_0111) as i8;
    let mut rank = (square.0 >> 3) as i8;
    loop {
        file += i;
        rank += j;
        if file < 0 || rank < 0 || file >= 8 || rank >= 8 {
            return false;
        }
        let piece = board[Square::from_ints(file as u8, rank as u8)];
        match piece.piece_type() {
            Empty => continue,
            piece_type => {
                if piece.color() == Some(color) { return false; }
                
                for threat in threats {
                    if piece_type == *threat {
                        return true;
                    }
                }
                return false;
            },
        }
    }
}

fn add_moves_diagonally (board: &ChessBoard, square: Square, winning_moves: &mut Vec<ChessMove>,
                         active_moves: &mut Vec<ChessMove>, quiet_moves: &mut Vec<ChessMove>,
                         king_pos: Square, is_in_check: bool) {
    for &(i, j) in &[(1, 1), (1, -1), (-1, 1), (-1, -1)] {
        add_moves_in_direction(i, j, board, square, winning_moves, active_moves, quiet_moves,
                               king_pos, is_in_check);
    }
}

fn add_straight_moves(board: &ChessBoard, square: Square, winning_moves: &mut Vec<ChessMove>,
                             active_moves: &mut Vec<ChessMove>, quiet_moves: &mut Vec<ChessMove>,
                      king_pos : Square, is_in_check : bool) {
    for &(i, j) in &[(0, 1), (1, 0), (0, -1), (-1, 0)] {
        add_moves_in_direction(i, j, board, square, winning_moves, active_moves, quiet_moves,
                               king_pos, is_in_check);
    }
}
    
fn add_moves_in_direction (i: i8, j: i8, board: &ChessBoard, square: Square,
                           winning_moves: &mut Vec<ChessMove>, active_moves: &mut Vec<ChessMove>,
                           quiet_moves: &mut Vec<ChessMove>, king_pos: Square, is_in_check: bool) {
    
    // let mut pos = square.0 as i8;
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

        let mv = ChessMove::new(square, target_square);
        if piece_to.is_empty() {
            add_if_legal(board, mv, quiet_moves, king_pos, is_in_check);
        }
        else {
            if piece_to.color().unwrap() != board.to_move {
                match piece_to.value().abs().partial_cmp(&board[square].value().abs()).unwrap() {
                    Ordering::Less =>
                        add_if_legal(board, mv, quiet_moves, king_pos, is_in_check),
                    Ordering::Equal =>
                        add_if_legal(board, mv, active_moves, king_pos, is_in_check),
                    Ordering::Greater =>
                        add_if_legal(board, mv, winning_moves, king_pos, is_in_check),
                }
            }
            break; // Break after finding a piece, friend or foe
        }
    }
}

pub fn king_pos (board : &ChessBoard) -> Square {
    board.king_pos(board.side_to_move())
}

use board;
use board::Board;
use board::Piece;
use board::Square;
use board::Move;
use board::PieceType;
use board::PieceType::*;
use board::Color::*;

pub fn all_legal_moves (board : &board::Board) -> Vec<Move> {
    let mut moves = Vec::new();
    let king_pos = king_pos(board);
    let is_in_check = is_attacked(board, king_pos);
    for i in 0..64 {
        match board.piece_at(board::Square(i)) {
            Piece(Empty, White) => continue,
            Piece(_, color) if color == board.to_move => {
                legal_moves_for_piece(board, Square(i), &mut moves, is_in_check, king_pos);
            }
            _ => continue,
        }
    }
    moves
}

#[inline]
fn legal_moves_for_piece(board : &Board, square : Square, moves : &mut Vec<board::Move>,
                         is_in_check : bool, king_pos : Square) { 
    let Piece(piece, color) = board.piece_at(square);
    debug_assert!(color == board.to_move && piece != Empty);
    
    let pos = square.0 as i8;
    let file = (square.0 & 0b0000_0111) as i8;
    let rank = (square.0 >> 3) as i8;
    
    match piece {
        King => {
            // If the king and the two castling squares are not in check, castling is allowed
            // There must be no pieces between the castling pieces

            // Kingside castling
            if (board.to_move == White && board.castling[0]) ||
                (board.to_move == Black && board.castling[2]) {
                    
                    let mut can_castle_here = true;
                    if is_attacked(board, square) {
                        can_castle_here = false;
                    }
                    
                    // Check that the two squares are empty and not in check
                    for n in [1, 2].iter() {
                        debug_assert!(file == 4, format!("Error: File is {}.", file));
                        let square_checked = Square(square.0 + n);
                        if is_attacked(board, square_checked) ||
                            board.piece_at(square_checked) != Piece(Empty, White) {
                                can_castle_here = false;
                            }
                    }
                    if can_castle_here {
                        moves.push(Move::new(square, Square(square.0 + 2)));
                    }
                    
                    
                }
            // Queenside castling
            if (board.to_move == White && board.castling[1]) ||
                (board.to_move == Black && board.castling[3]) {
                    let mut can_castle_here = true;
                    if is_attacked(board, square) {
                        can_castle_here = false;
                    }
                    // Check that the two squares are empty and not in check
                    for n in [1, 2].iter() {
                        debug_assert!(file == 4, format!("Error: File is {}.", file));
                        let square_checked = Square(square.0 - n);
                        if is_attacked(board, square_checked) ||
                            board.piece_at(square_checked) != Piece(Empty, White) {
                                can_castle_here = false;
                            }
                    }
                    // Check that the knight-square is empty
                    if board.piece_at(Square(square.0 - 3)) != Piece(Empty, White) {
                        can_castle_here = false;
                    }
                    if can_castle_here {
                        moves.push(Move::new(square, Square(square.0 - 2)));
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
                    if piece_to == Empty || color_to != board.to_move {
                        add_if_legal_simple(board, Move::new(square, Square(new_pos)), moves);
                    }
                }
            }
        },
        Queen =>  {
            add_moves_diagonally (&board, square, moves, king_pos, is_in_check);
            add_straight_moves(&board, square, moves, king_pos, is_in_check);
        },
        Rook => {
            add_straight_moves(&board, square, moves, king_pos, is_in_check);
        },
        Bishop => {
            add_moves_diagonally(&board, square, moves, king_pos, is_in_check);
        },
        Knight => {
            for &(i, j) in [(-2, -1), (-2, 1), (-1, -2), (-1, 2),
                           (1, -2), (1, 2), (2, -1), (2, 1)].iter() {
                if file + i < 0 || file + i >= 8 || rank + j < 0 || rank + j >= 8 {
                    continue;
                }
                let new_pos = ((rank + j) * 8 + file + i) as u8;
                
                let Piece(piece_to, color_to) = board.piece_at(Square(new_pos));
                if piece_to == Empty || color_to != board.to_move {
                    add_if_legal(board, Move::new (square, Square(new_pos)), moves,
                                 king_pos, is_in_check);
                }
            }
        },
        Pawn => {
            // Helper variables
            let (start_rank, prom_rank, direction) =
                if board.to_move == White { (6, 1, -1) } else { (1, 6, 1) };

            // Checks if there are any pieces available for capture to the left,
            // including en passant capture
            if file > 0 {
                let take_square = Square((pos + direction * 8) as u8 - 1);
                let Piece(piece, color) = board.piece_at(take_square);
                if piece != Empty && color != board.to_move {
                    if rank == prom_rank {
                        for piece_type in [Queen, Rook, Bishop, Knight].iter() {
                            add_if_legal(board,
                                         Move::new_prom(square, take_square,
                                                        Piece(*piece_type, board.to_move)),
                                         moves, king_pos, is_in_check);
                        }
                    }
                    else {
                        add_if_legal(board, Move::new(square, take_square), moves,
                                     king_pos, is_in_check);
                    }
                }
                if board.en_passant.is_some() && take_square == board.en_passant.unwrap() {
                    add_if_legal_simple(board, Move::new(square, take_square), moves);
                }
            }
            // Ditto, but to the right
            if file < 7 {
                let take_square = Square((pos + direction * 8) as u8 + 1);
                let Piece(piece, color) = board.piece_at(take_square);
                if piece != Empty && color != board.to_move {
                    if rank == prom_rank {
                        for piece_type in [Queen, Rook, Bishop, Knight].iter() {
                            add_if_legal(board,
                                         Move::new_prom(square, take_square,
                                                        Piece(*piece_type, board.to_move)),
                                         moves, king_pos, is_in_check);
                        }
                    }
                    else {
                        add_if_legal(board, Move::new(square, take_square), moves,
                                     king_pos, is_in_check);
                    }
                }
                if board.en_passant.is_some() && take_square == board.en_passant.unwrap() {
                    add_if_legal_simple(board, Move::new(square, take_square), moves);
                }
            }

            //Checks if the pawn can walk forward one or two squares, promote
            let square_in_front = Square::from_ints(file as u8, (rank + direction) as u8);
            
            if board.piece_at(square_in_front).is_empty() {
                if rank == prom_rank {
                    for piece_type in [Queen, Rook, Bishop, Knight].iter() {
                        add_if_legal(board,
                                     Move::new_prom(square, square_in_front,
                                                    Piece(*piece_type, board.to_move)),
                                     moves, king_pos, is_in_check);
                    }
                }
                else {
                    add_if_legal(board, Move::new(square, square_in_front), moves, king_pos,
                                 is_in_check);
                }
                if rank == start_rank {
                    let square_2_in_front = Square::from_ints(file as u8,
                                                              (rank + direction * 2) as u8);
                    if board.piece_at(square_2_in_front).is_empty() {
                        add_if_legal(board, Move::new(square, square_2_in_front), moves,
                                     king_pos, is_in_check);
                    }
                }
            }
        },
        Empty => (),
    }
}
/// Checks that the move does not put the player in check
/// , and add the move to the vector
fn add_if_legal(board : &Board, c_move : Move, moves : &mut Vec<Move>, king_pos_c : Square,
                is_in_check : bool) {
    if is_in_check || is_pinned_to_piece(board, c_move.from, king_pos_c) {
        add_if_legal_simple(board, c_move, moves);
    }
    else {
        debug_assert!(!is_attacked(board, king_pos_c));

        moves.push(c_move);
    }
}

fn add_if_legal_simple (board : &Board, c_move : Move, moves : &mut Vec<Move>) {
    let mut test_board = board.do_move(c_move);

    if test_board.to_move == White {
        test_board.to_move = Black
    }
    else { test_board.to_move = White }
    if !is_attacked(&test_board, king_pos(&test_board)) {
        moves.push(c_move);
    }
}

pub fn is_pinned_to_piece(board : &Board, pinee_pos : Square, pinner_pos : Square) -> bool {
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
            match board.piece_at(Square::from_ints(cur_file as u8, cur_rank as u8)) {
                Piece(Empty, White) => continue,
                // If you first find an opposite color queen/bishop, you're pinned
                Piece(Bishop, color) | Piece(Queen, color) if color != board.to_move =>
                    return true,
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
            match board.piece_at(Square::from_ints(pinner_file as u8, cur_rank as u8)) {
                Piece(Empty, White) => continue,
                Piece(Rook, color) | Piece(Queen, color) if color != board.to_move =>
                    return true,
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
            match board.piece_at(Square::from_ints(cur_file as u8, pinner_rank as u8)) {
                Piece(Empty, White) => continue,
                Piece(Rook, color) | Piece(Queen, color) if color != board.to_move =>
                    return true,
                _ => return false,
            }
        }
    }
    else {
        false
    }
}

pub fn is_attacked(board : &Board, square : Square) -> bool {
    // Direction enemy pawns are coming from
    let pawn_direction = if board.to_move == White { -1 } else { 1 };
    let pos = square.0 as i8;
    let file = (square.0 & 0b0000_0111) as i8;
    let rank = (square.0 >> 3) as i8;

    if file > 0 {
        if (board.to_move == White && rank > 1) || (board.to_move == Black && rank < 6) {
            let pawn_square = Square((pos + pawn_direction * 8) as u8 - 1);
            let Piece(piece, color) = board.piece_at(pawn_square);
            //println!("Pawn square: {} has a {} {}", pawn_square, piece, color);
            if piece == Pawn && color != board.to_move { return true; }
        }
    }
    if file < 7 {
        if (board.to_move == White && rank > 1) || (board.to_move == Black && rank < 6) {
            let pawn_square = Square((pos + pawn_direction * 8) as u8 + 1);
            
            let Piece(piece, color) = board.piece_at(pawn_square);
            //println!("Pawn square: {} has a {} {}", pawn_square, piece, color);
            if piece == Pawn && color != board.to_move { return true; }
        }
    }
    // println!("Checked pawns");

    for &(i, j) in [(0, 1), (1, 0), (0, -1), (-1, 0)].iter() {
        if check_threats_in_direction (i, j, board, square, &vec![Queen, Rook]) {
            //println!("Is under attack diagonally!");
            return true;
        }
    }
    for &(i, j) in [(1, 1), (1, -1), (-1, 1), (-1, -1)].iter() {
        if check_threats_in_direction (i, j, board, square, &vec![Queen, Bishop]) {
            //println!("Is under attack veritcally or horizontally!");
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

fn check_threats_in_direction (i : i8, j : i8, board : &Board, square : Square,
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
               // println!("Piece {} may be threatening.", piece);
                for threat in threats {
                    if piece == *threat {
                        //println!("Piece {} was threatening!", piece);
                        return true;
                    }
                }
                //println!("Piece {} was not threatening.", piece);
                return false;
            },
        }
    }
}

fn add_moves_diagonally (board : &Board, square : Square, moves : &mut Vec<Move>,
                         king_pos : Square, is_in_check : bool) {
    for &(i, j) in [(1, 1), (1, -1), (-1, 1), (-1, -1)].iter() {
        add_moves_in_direction(i, j, &board, square, moves, king_pos, is_in_check);
    }
}

fn add_straight_moves(board : &Board, square : Square, moves : &mut Vec<Move>,
                      king_pos : Square, is_in_check : bool) {
    for &(i, j) in [(0, 1), (1, 0), (0, -1), (-1, 0)].iter() {
        add_moves_in_direction(i, j, &board, square, moves, king_pos, is_in_check);
    }
}
    
fn add_moves_in_direction (i : i8, j : i8, board : &Board, square : Square,
                           moves : &mut Vec<Move>, king_pos : Square, is_in_check : bool) {
    
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
        let c_move = Move::new(square, target_square);
        match board.piece_at(target_square) {
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

fn king_pos (board : &Board) -> Square {
    board.king_pos()
}

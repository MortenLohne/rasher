use board::sjadam_move::SjadamMove;
use board::std_board::Square;
use board::std_board::Piece;
use board::std_board::PieceType;
use board::std_board::PieceType::*;

use search_algorithms::board::Color::*;
use search_algorithms::board::EvalBoard;

use board::sjadam_board::SjadamBoard;
use board::sjadam_board::BitBoard;

lazy_static! {
    static ref ROOK_TABLE : [[u8; 256]; 32] = {
        let mut table = [[0; 256]; 32];
        for sjadam_squares in (0..256)
            .filter(|i| i & 0b1010_1010 == 0 || i & 0b0101_0101 == 0)
            .map(BitBoard::from_u64)
        {
            for all_pieces in (0..256).map(BitBoard::from_u64) {
                let mut target_squares = sjadam_squares.clone();
                for file in 0..8 {
                    if sjadam_squares.get(Square(file)) {
                        let mut cur_file = file.overflowing_sub(1).0;
                        
                        while cur_file < 8 {
                            target_squares.set(Square(cur_file));
                            if all_pieces.get(Square(cur_file)) {
                                break;
                            }
                            cur_file = cur_file.overflowing_sub(1).0;
                        }
                        
                        cur_file = file + 1;
                        
                        while cur_file < 8 {
                            target_squares.set(Square(cur_file));
                            if all_pieces.get(Square(cur_file)) {
                                break;
                            }
                            cur_file += 1;
                        }
                    }
                }
                debug_assert_eq!(table[sjadam_lookup_index(sjadam_squares.rank(0))]
                                 [all_pieces.rank(0) as usize], 0);
                
                table[sjadam_lookup_index(sjadam_squares.rank(0))]
                    [all_pieces.rank(0) as usize]
                    = target_squares.rank(0);
            }
        }
        table
    };
    static ref KNIGHT_TABLE : [u64; 256] = {
        let mut table = [0; 256];
        let c = 1 << 18;
        let attacks = c << 17 | c << 15 | c << 10 | c << 6 | c | c >> 6 | c >> 10 | c >> 15 | c >> 17;
        let left_side = 0b11111100_11111100_11111100_11111100_11111100_11111100_11111100_11111100;
        let right_side = 0b00111111_00111111_00111111_00111111_00111111_00111111_00111111_00111111;

        for pieces in 0..256 {
            let mut targets : u64 = 0;
            for file in 0..2 {
                if (pieces >> file) % 2 != 0 {
                    targets |= right_side & (attacks >> (2 - file))
                }
            }
            
            for file in 2..6 {
                if (pieces >> file) % 2 != 0 {
                    targets |= attacks << (file - 2)
                }
            }
            
            for file in 6..8 {
                if (pieces >> file) % 2 != 0 {
                    targets |= left_side & (attacks << (file - 2))
                }
            }
            
            table[pieces] = targets;
        }
        table
    };
    static ref KING_TABLE : [u64; 256] = {
        let mut table = [0; 256];
        let attacks = 0b11100000_11100000_111;
        let left_side = 0b11111100_11111100_11111100_11111100_11111100_11111100_11111100_11111100;
        let right_side = 0b00111111_00111111_00111111_00111111_00111111_00111111_00111111_00111111;

        for pieces in 0..256 {
            let mut targets : u64 = 0;
            if pieces % 2 != 0 {
                targets |= right_side & (attacks >> 1);
            }
            
            for file in 1..7 {
                if (pieces >> file) % 2 != 0 {
                    targets |= attacks << (file - 1)
                }
            }
            
            if (pieces >> 7) % 2 != 0 {
                targets |= left_side & (attacks << 6);
            }
            
            table[pieces] = targets;
        }
        table
    };
}

pub fn all_legal_moves(board: &SjadamBoard) -> (Vec<SjadamMove>, Vec<SjadamMove>) {
    let mut moves = Vec::with_capacity(250);
    let mut active_moves = Vec::with_capacity(50);
    let mut winning_moves = Vec::with_capacity(300);

    let color = board.to_move();
    let all_pieces = board.all_pieces();

    for (piece, mut bitboard) in [Knight, Bishop, Rook, Queen, Pawn, King].iter()
        .map(|&piece_type| (Piece::new(piece_type, color),
                            board.get_piece(Piece::new(piece_type, color))))
    {
        while let Some(square) = bitboard.first_piece() {
            legal_moves_for_square(&board, square, piece.piece_type(),
                                   &mut moves, &mut active_moves, &mut winning_moves);
            bitboard.clear(square);
        }
    }

    if color == White {
        if board.can_castle_kingside(White)
            && all_pieces.rank(7) & 0b01100000 == 0 {
                moves.push(SjadamMove::new(Square::E1, Square::G1, true));
            }
        if board.can_castle_queenside(White)
            && all_pieces.rank(7) & 0b1110 == 0 {
                moves.push(SjadamMove::new(Square::E1, Square::C1, true));
            }
    }
    else {
        if board.can_castle_kingside(Black)
            && all_pieces.rank(0) & 0b0110_0000 == 0 {
                moves.push(SjadamMove::new(Square::E8, Square::G8, true));
            }
        if board.can_castle_queenside(Black)
            && all_pieces.rank(0) & 0b1110 == 0 {
                moves.push(SjadamMove::new(Square::E8, Square::C8, true));
            }
    }
    winning_moves.append(&mut active_moves);
    (winning_moves, moves)
}
    
fn legal_moves_for_square(board: &SjadamBoard, square: Square, piece_type: PieceType,
                          quiet_moves: &mut Vec<SjadamMove>,
                          active_moves: &mut Vec<SjadamMove>,
                          winning_moves: &mut Vec<SjadamMove>) {
    
    let mut sjadam_squares = BitBoard::empty();
    sjadam_squares.set(square);
    let color = board.to_move();
    
    let (friendly_pieces, opponent_pieces) = match color {
        White => (board.white_pieces(), board.black_pieces()),
        Black => (board.black_pieces(), board.white_pieces()),
    };
        
    let all_pieces = board.all_pieces();
    
    sjadam_friendly_moves(&mut sjadam_squares, &friendly_pieces,
                          &all_pieces, square);

    sjadam_opponent_moves(&mut sjadam_squares, &opponent_pieces, &all_pieces);

    let moves : BitBoard = match piece_type {
        Rook => rook_moves(sjadam_squares, friendly_pieces, all_pieces),
        Bishop => bishop_moves(sjadam_squares, friendly_pieces, all_pieces),
        Queen => {
            let mut queen_moves = bishop_moves(sjadam_squares, friendly_pieces, all_pieces);
            queen_moves.board |= rook_moves(sjadam_squares, friendly_pieces, all_pieces).board;
            queen_moves
        },
        Knight => knight_moves(sjadam_squares, friendly_pieces),
        Pawn => {
            let mut all_pieces_pawns = all_pieces.clone();
            if let Some(ep_square) = board.en_passant_square() {
                all_pieces_pawns.set(ep_square);
            }
            if board.to_move() == White {
                pawn_moves_white(sjadam_squares, friendly_pieces, all_pieces_pawns)
            }
            else {
                pawn_moves_black(sjadam_squares, friendly_pieces, all_pieces_pawns)
            }
        },
        King => king_moves(sjadam_squares, friendly_pieces),
        Empty => BitBoard::empty(),
    };
    
    for i in 0..64 {
        if moves.get(Square(i)) && Square(i) != square {
            let target = board.get_square(Square(i));
            let mv = SjadamMove::new(square, Square(i), false);
            if target.color().is_some() && target.color().unwrap() != board.to_move()
                && target.piece_type() != Pawn
            {
                if target.value().abs() > piece_type.value().abs()
                    || target.piece_type() == King {
                    winning_moves.push(mv);
                }
                else if target.value().abs() == piece_type.value().abs() {
                    active_moves.push(mv);
                }
                else {
                    quiet_moves.push(mv);
                }
            }
            else {
                quiet_moves.push(mv);
            }
        }
    }
}

/// Recursively sets all available sjadam-move squares
#[inline(never)]
fn sjadam_friendly_moves(sjadam_squares: &mut BitBoard, friendly_pieces: &BitBoard,
                         all_pieces: &BitBoard, square: Square) {
    let Square(i) = square;
    for x in &[-1, 0, 1] {
        for y in &[-1, 0, 1] {
            let (file, rank) = (square.file_rank().0 as i8, square.file_rank().1 as i8);
            
            if (file <= 1 && *x == -1) || (file >= 6 && *x == 1)
                || (rank <= 1 && *y == -1) || (rank >= 6 && *y == 1)
            {
                continue
            }
            let dest_square = Square(i.wrapping_add((16 * y) as u8).wrapping_add((2 * x) as u8));
            let jumping_square = Square(i.wrapping_add((8 * y) as u8).wrapping_add(*x as u8));
            
            if friendly_pieces.get(jumping_square) && !all_pieces.get(dest_square) &&
                !sjadam_squares.get(dest_square)
            {
                sjadam_squares.set(dest_square);
                sjadam_friendly_moves(sjadam_squares, friendly_pieces,
                                      all_pieces, dest_square);
            }
        }
    }
}

/// Sets all available opponent sjadam-move squares
#[inline(never)]
fn sjadam_opponent_moves(sjadam_squares: &mut BitBoard, opponent_pieces: &BitBoard,
                         all_pieces: &BitBoard) {
    let type1_mask = 0b00000000_01010101_00000000_01010101_00000000_01010101_00000000_01010101;
    let type2_mask = 0b00000000_10101010_00000000_10101010_00000000_10101010_00000000_10101010;
    let type3_mask = 0b01010101_00000000_01010101_00000000_01010101_00000000_01010101_00000000;
    
    let color_offset = if type1_mask & sjadam_squares.board != 0 { 0 }
    else if type2_mask & sjadam_squares.board != 0 { 1 }
    else if type3_mask & sjadam_squares.board != 0 { 8 }
    else { 9 };
    
    let old_sjadam_squares = sjadam_squares.clone();
    
    for i in [0, 2, 4, 6, 16, 18, 20, 22, 32, 34, 36, 38, 48, 50, 52, 54].iter()
        .map(|&i| i + color_offset)
        .filter(|&i| old_sjadam_squares.get(Square(i)))
    {
        let square = Square(i);
        
        for x in &[-1, 0, 1] {
            for y in &[-1, 0, 1] {
                let (file, rank) = (square.file_rank().0 as i8, square.file_rank().1 as i8);
                
                if (file <= 1 && *x == -1) || (file >= 6 && *x == 1)
                    || (rank <= 1 && *y == -1) || (rank >= 6 && *y == 1)
                {
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
fn king_moves(sjadam_squares: BitBoard, friendly_pieces: BitBoard) -> BitBoard {
    let mut moves = 0;
    for rank in 0..1 {
        let index = ((sjadam_squares.board >> (rank * 8)) & 255) as usize;
        moves |= KING_TABLE[index] >> ((- rank * 8) + 8);
    }
    for rank in 1..8 {
        let index = ((sjadam_squares.board >> (rank * 8)) & 255) as usize;
        moves |= KING_TABLE[index] << ((rank * 8) - 8);
    }
    BitBoard::from_u64(moves & !friendly_pieces.board)
}

#[inline(never)]
fn knight_moves(sjadam_squares: BitBoard, friendly_pieces: BitBoard) -> BitBoard {
    let mut moves = 0;
    for rank in 0..2 {
        let index = ((sjadam_squares.board >> (rank * 8)) & 255) as usize;
        moves |= KNIGHT_TABLE[index] >> ((- rank * 8) + 16);
    }
    for rank in 2..8 {
        let index = ((sjadam_squares.board >> (rank * 8)) & 255) as usize;
        moves |= KNIGHT_TABLE[index] << ((rank * 8) - 16);
    }
    BitBoard::from_u64(moves & !friendly_pieces.board)
}

const RIGHT_MASK : u64 = 0b01111111_01111111_01111111_01111111_01111111_01111111_01111111_01111111;
const LEFT_MASK : u64 = 0b11111110_11111110_11111110_11111110_11111110_11111110_11111110_11111110;

#[inline(never)]
fn pawn_moves_black(sjadam_squares: BitBoard, friendly_pieces: BitBoard,
              all_pieces: BitBoard) -> BitBoard {
    let opponent_pieces = BitBoard::from_u64(all_pieces.board ^ friendly_pieces.board);
    let left_captures = ((sjadam_squares.board & LEFT_MASK) << 7) & opponent_pieces.board; // & LEFT_MASK;
    let right_captures = ((sjadam_squares.board & RIGHT_MASK) << 9) & opponent_pieces.board; // & RIGHT_MASK;
    
    let forward = sjadam_squares.board << 8 & !all_pieces.board;
    let forward_two = (sjadam_squares.board & (255 << 8)) << 16 & !all_pieces.board & !(all_pieces.board << 8);
    
    BitBoard::from_u64(sjadam_squares.board | left_captures | right_captures | forward | forward_two)
}

#[inline(never)]
fn pawn_moves_white(sjadam_squares: BitBoard, friendly_pieces: BitBoard,
              all_pieces: BitBoard) -> BitBoard {
    let opponent_pieces = BitBoard::from_u64(all_pieces.board ^ friendly_pieces.board);
    
    let left_captures = ((sjadam_squares.board & RIGHT_MASK) >> 7) & opponent_pieces.board;
    let right_captures = ((sjadam_squares.board & LEFT_MASK) >> 9) & opponent_pieces.board;
    
    let forward = sjadam_squares.board >> 8 & !all_pieces.board;
    let forward_two = (sjadam_squares.board & (255 << 48)) >> 16 & !all_pieces.board & !(all_pieces.board >> 8);
    
    BitBoard::from_u64(sjadam_squares.board | left_captures | right_captures | forward | forward_two)
}

#[inline(never)]
fn bishop_moves(sjadam_squares: BitBoard, friendly_pieces: BitBoard,
                all_pieces: BitBoard) -> BitBoard {
    let mut bishop_moves = sjadam_squares.clone();
    let mut sjadam_squares_45 = sjadam_squares.rotate_45();
    
    let all_pieces_45 = all_pieces.rotate_45();
    for diagonal in 0..15 {
        let diagonal_bits = sjadam_squares_45.diagonal(diagonal);
        let mut target_rank = ROOK_TABLE
            [sjadam_lookup_index(diagonal_bits)][all_pieces_45.diagonal(diagonal) as usize];

        let len : u64 = if diagonal >= 8 { 15 - diagonal } else { diagonal + 1 } as u64;

        target_rank &= !(!(0 as u64) << len) as u8;

        let n = diagonal as i8 - 7;
        let offset = if n <= 0 { n * (-8)} else { 8 * (8 - n) + n };

        sjadam_squares_45.board |= (target_rank as u64) << offset;
    }
    
    let diagonal_moves = (0..7).fold(sjadam_squares_45, |acc, _| acc.rotate_45());

    let mut sjadam_squares_315 = sjadam_squares.rotate_315();
    let all_pieces_315 = all_pieces.rotate_315();
    
    for antidiagonal in (-7)..8 {
        let diagonal_bits = sjadam_squares_315.antidiagonal(antidiagonal);
        let mut target_rank = ROOK_TABLE
            [sjadam_lookup_index(diagonal_bits)][all_pieces_315.antidiagonal(antidiagonal) as usize];
        let len = 8 - antidiagonal.abs();
        target_rank &= !(!(0 as u64) << len) as u8;
        let n = antidiagonal;
        let offset = if n <= 0 { n * (-8) - n} else { 8 * (8 - n) };

        sjadam_squares_315.board |= (target_rank as u64) << offset;
    }
    sjadam_squares_315 = (0..7).fold(sjadam_squares_315, |acc, _| acc.rotate_315());
    
    bishop_moves.board |= diagonal_moves.board;
    bishop_moves.board |= sjadam_squares_315.board;
    bishop_moves.board &= !friendly_pieces.board;
    bishop_moves
}

#[inline(never)]
fn rook_moves(sjadam_squares: BitBoard, friendly_pieces: BitBoard,
              all_pieces: BitBoard) -> BitBoard {
    let mut sjadam_squares_rotated = sjadam_squares.rotate();
    let all_pieces_rotated = all_pieces.rotate();
    
    for file in 0..8 {
        lookup_rook(file, &mut sjadam_squares_rotated, all_pieces_rotated);
    }
    let mut rook_moves = sjadam_squares_rotated.rotate_270();
    
    rook_moves.board &= !friendly_pieces.board;
    let mut horizontal_moves = sjadam_squares.clone();
    for rank in 0..8 {
        lookup_rook(rank, &mut horizontal_moves, all_pieces);
    }
    horizontal_moves.board &= !friendly_pieces.board;
    rook_moves.board |= horizontal_moves.board;
    rook_moves
}

/// Take the index of a rank, and a bitboard of available sjadam squares,
/// and set all available rook moves on the rank
/// Also sets captures of own pieces
fn lookup_rook(rank: u8, sjadam_squares: &mut BitBoard, all_pieces: BitBoard) {
    let rank_bits = sjadam_squares.rank(rank);

    let target_rank = ROOK_TABLE
        [sjadam_lookup_index(rank_bits)][all_pieces.rank(rank) as usize];
    sjadam_squares.board |= (target_rank as u64) << (8 * rank as u64);
}
    
fn sjadam_lookup_index(rank_bits: u8) -> usize {
    let mut index;
    if rank_bits.leading_zeros() % 2 == 0 {
        index = 16;
        index |= (rank_bits & 2) >> 1;
        index |= (rank_bits & 8) >> 2;
        index |= (rank_bits & 32) >> 3;
        index |= (rank_bits & 128) >> 4;
    }
    else {
        index = 0;
        index |= rank_bits & 1;
        index |= (rank_bits & 4) >> 1;
        index |= (rank_bits & 16) >> 2;
        index |= (rank_bits & 64) >> 3;
    }
    debug_assert!(index < 32);
    index as usize
}

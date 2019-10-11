use self::PieceType::*;
use board::chess::mv;
use board_game_traits::board;
use board_game_traits::board::{EvalBoard, GameResult};
use board::chess::move_gen;
use board::chess::mv::ChessMove;
use board_game_traits::board::Color;
use board_game_traits::board::Color::*;
use pgn_traits::pgn;
use pgn_traits::pgn::PgnBoard;

use std::ops;
use std::fmt;
use std::fmt::Write;
use std::mem;
use std::hash::{Hash, Hasher};
use board_game_traits::board::Board;
use board_game_traits::board::ExtendedBoard;
use board::chess::mv::ChessReverseNullMove;
use std::collections::hash_map::DefaultHasher;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialOrd, Ord, PartialEq, Serialize, Deserialize)]
pub enum PieceType {
    Empty = 0,
    Pawn = 1,
    Knight = 2,
    Bishop = 3,
    Rook = 4,
    Queen = 5,
    King = 6,
}

impl PieceType {
    pub fn value(self) -> f32 {
        match self {
            Pawn => 1.0,
            Knight => 3.0,
            Bishop => 3.0,
            Rook => 5.0,
            Queen => 9.0,
            King => 100.0,
            Empty => 0.0,
        }
    }
    pub fn letter(self) -> char {
        match self {
            Empty => ' ',
            Pawn => 'P',
            Knight => 'N',
            Bishop => 'B',
            Rook => 'R',
            Queen => 'Q',
            King => 'K',
        }
    }
    pub fn from_letter(ch: char) -> Option<Self> {
        match ch {
            ' ' => Some(Empty),
            'P' => Some(Pawn),
            'N' => Some(Knight),
            'B' => Some(Bishop),
            'R' => Some(Rook),
            'Q' => Some(Queen),
            'K' => Some(King),
            _ => None,
        }
    }
    #[allow(dead_code)]
    pub fn from_disc(disc: u32) -> Option<Self> {
        if disc > 6 { None }
        else { Some(unsafe { mem::transmute(disc as u8) }) }
    }
}

impl fmt::Display for PieceType {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let _ = fmt.write_str(
            match *self {
                Pawn => "Pawn",
                Knight => "Knight",
                Bishop => "Bishop",
                Rook => "Rook",
                Queen => "Queen",
                King => "King",
                Empty => "Empty square",
            }
        );
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
#[allow(dead_code)]
pub enum Piece {
    Empty = 0,
    WhitePawn = 2,
    BlackPawn = 3,
    WhiteKnight = 4,
    BlackKnight = 5,
    WhiteBishop = 6,
    BlackBishop = 7,
    WhiteRook = 8,
    BlackRook = 9,
    WhiteQueen = 10,
    BlackQueen = 11,
    WhiteKing = 12,
    BlackKing = 13,
}

impl Default for Piece {
    fn default() -> Self {
        Piece::Empty
    }
}

impl fmt::Display for Piece {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "{}", match self.color().unwrap_or(White) {
            White => self.piece_type().letter(),
            Black => self.piece_type().letter().to_ascii_lowercase(),
        })
    }
}

impl Piece {
    pub fn from_letter(ch: char) -> Option<Self> {
        match (PieceType::from_letter(ch.to_ascii_uppercase()), ch.is_lowercase()) {
            (Some(Empty), _) => Some(Piece::Empty),
            (Some(piece_type), true) => Some(Self::from_type_color(piece_type, Black)),
            (Some(piece_type), false) => Some(Self::from_type_color(piece_type, White)),
            (None, _) => None,
        }
    }
    
    pub fn from_type_color(piece_type: PieceType, color: Color) -> Self {
        if piece_type == Empty {
            Piece::Empty
        }
        else {
            unsafe {
                match color {
                    White => mem::transmute::<u8, Piece>((piece_type as u32 as u8) << 1),
                    Black => mem::transmute::<u8, Piece>(((piece_type as u32 as u8) << 1) + 1),
                }
            }
        }
    }
    pub fn value(self) -> f32 {
        if self.is_empty() { 0.0 }
        else {
            let abs_value = self.piece_type().value();
            match self.color().unwrap() {
                White => abs_value,
                Black => -1.0 * abs_value,
            }
        }
    }
    pub fn piece_type(self) -> PieceType {
        unsafe {
            mem::transmute::<u8, PieceType>((self as u32 >> 1) as u8)
        }
    }
    pub fn color(self) -> Option<Color> {
        match self {
            Piece::Empty => None,
            _ => if self as u32 % 2 == 0 {
                Some(White)
            }
            else {
                Some(Black)
            },
        }
    }
    pub fn is_empty(self) -> bool {
        self == Piece::Empty
    }
    pub fn empty() -> Self {
        Piece::Empty
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct Square(pub u8);

impl fmt::Display for Square {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let (file, rank) = self.file_rank();
        let actual_rank = ((rank as i8 - 8).abs() as u8 + b'0') as char;
        
        let _ = fmt.write_str(&format!("{}{}",
                                       (file + b'a') as char,
                                       actual_rank));
        Ok(())   
    }
}

impl Square {
    pub fn from_alg (alg : &str) -> Result<Self, pgn::Error> {
        if alg.len() != 2 { Err(pgn::Error::new(
            pgn::ErrorKind::ParseError,
            format!("Invalid square {}", alg))) }
        else {
            let (file, rank) = (alg.chars().nth(0).unwrap(), alg.chars().nth(1).unwrap());
            if file < 'a' || file > 'h' || rank < '1' || rank > '8' { Err(pgn::Error::new(
                pgn::ErrorKind::ParseError,
                format!("Invalid square {}", alg))) }
            else {
                let (file_u8, rank_u8) = (file as u8 - b'a',
                                          8 - (rank as u8 - b'0'));

            let square = rank_u8 * 8 + file_u8;
                Ok(Square(square)) }
        }
    }
    pub fn from_ints (file : u8, rank : u8) -> Self {
        debug_assert!(file < 8 && rank < 8);
        Square((rank << 3) | file)
    }
    pub fn file_rank (self) -> (u8, u8) {
        (self.file(), self.rank())
    }
    pub fn file(self) -> u8 {
        self.0 & 0b0000_0111
    }
    pub fn rank(self) -> u8 {
        self.0 >> 3
    }
    
    pub const H1 : Square = Square(63);
    pub const G1 : Square = Square(62);
    pub const E1 : Square = Square(60);
    pub const C1 : Square = Square(58);
    pub const A1 : Square = Square(56);

    pub const H8 : Square = Square(7);
    pub const G8 : Square = Square(6);
    pub const E8 : Square = Square(4);
    pub const C8 : Square = Square(2);
    pub const A8 : Square = Square(0);
}

#[derive(Clone)]
pub struct ChessBoard {
    pub board : [[Piece; 8]; 8],
    pub to_move : Color,
    pub castling_en_passant : u8,
    pub half_move_clock : u8,
    pub move_num : u16,
    pub past_board_hashes: Vec<u64>,
}

impl Hash for ChessBoard {
    fn hash<H: Hasher> (&self, state: &mut H) {
        self.board.hash(state);
        self.to_move.hash(state);
        self.castling_en_passant.hash(state);
    }
}

impl PartialEq for ChessBoard {
    fn eq(&self, other: &ChessBoard) -> bool {
        self.board == other.board && self.to_move == other.to_move
            && self.castling_en_passant == other.castling_en_passant
    }
}

impl Eq for ChessBoard {}

pub struct BoardIter {
    index: u8,
}

impl BoardIter {
    pub fn new() -> Self {
        BoardIter { index: 0 }
    }
}

impl Iterator for BoardIter {
    type Item = Square;
    fn next(&mut self) -> Option<Square> {
        if self.index >= 64 {
            None
        }
        else {
            self.index += 1;
            Some(Square(self.index - 1))
        }
    }
}

impl IntoIterator for ChessBoard {
    type Item = Square;
    type IntoIter = BoardIter;
    fn into_iter(self) -> Self::IntoIter {
        BoardIter::new()
    }
}

// Parses the first token in a FEN string, which describes the positions
// of the pieces
fn parse_fen_board(fen_board : &str) -> Result<[[Piece;8];8], pgn::Error> {
    let mut board = [[Piece::empty(); 8]; 8];
    let ranks : Vec<&str> = fen_board.split('/').collect();
    if ranks.len() != 8 {
        return Err(
            pgn::Error::new(pgn::ErrorKind::ParseError,
                            format!("Invalid FEN \"{}\": Had {} ranks instead of 8",
                                    fen_board, ranks.len())));
    }
    for i in 0..8 {
        let mut cur_rank : Vec<Piece> = Vec::new();
        for c in ranks[i].chars() {
            match Piece::from_letter(c) {
                Some(piece) => cur_rank.push(piece),
                None => match c.to_digit(10) {
                    Some(mut i) => {
                        while i > 0 {
                            cur_rank.push(Piece::empty());
                            i -= 1;
                        }
                    },
                    None => return Err(pgn::Error::new(
                        pgn::ErrorKind::ParseError,
                        format!("Invalid FEN \"{}\":: Illegal character {}", fen_board, c))),
                },
            }
        }
        if cur_rank.len() != 8 {
            return Err(
                pgn::Error::new(pgn::ErrorKind::ParseError,
                                format!("Invalid FEN \"{}\": Specified {} pieces on rank {}.",
                                        fen_board, cur_rank.len(), i)))
        }
        else {
            board[i][..8].clone_from_slice(&cur_rank[..8]);
        }
    }
    Ok(board)
}

fn parse_fen_to_move (to_move : &str) -> Result<Color, pgn::Error> {
    let char_to_move = to_move.chars().collect::<Vec<_>>()[0];
    if char_to_move == 'w' { Ok(White) }
    else if char_to_move == 'b' { Ok(Black) }
    else { Err(pgn::Error::new(
        pgn::ErrorKind::ParseError,
        format!("Invalid FEN: Found {} in side-to-move-field, expected 'w' or 'b'",
                to_move))) }
}

fn parse_fen_castling_rights(castling_str : &str, board: &mut ChessBoard) -> Result<(), pgn::Error> {
    let mut castling_rights = [false; 4];
    for c in castling_str.chars() {
        match c {
            '-' => break,
            'K' => castling_rights[0] = true,
            'Q' => castling_rights[1] = true,
            'k' => castling_rights[2] = true,
            'q' => castling_rights[3] = true,
            _ => return Err(
                pgn::Error::new(pgn::ErrorKind::ParseError,
                                "Invalid FEN string: Error in castling field.".to_string())),
        }
    }
    if !castling_rights[0] { board.disable_castling_kingside(White) }
    if !castling_rights[1] { board.disable_castling_queenside(White) }
    if !castling_rights[2] { board.disable_castling_kingside(Black) }
    if !castling_rights[3] { board.disable_castling_queenside(Black) }
    Ok(())
}

impl PgnBoard for ChessBoard {
    
    fn from_fen(fen : &str) -> Result<Self, pgn::Error> {
        let fen_split : Vec<&str> = fen.split(' ').collect();
        if fen_split.len() < 4 || fen_split.len() > 6 {
            return Err(pgn::Error::new(
                pgn::ErrorKind::ParseError,
                format!("FEN string \"{}\" has {} fields instead of [4, 5, 6]",
                               fen, fen_split.len())));
        }

        let mut board = ChessBoard { board: [[Piece::empty(); 8]; 8], to_move: White,
                                     castling_en_passant: 15, half_move_clock: 0, move_num: 0,
            past_board_hashes: vec![] };
        board.board = parse_fen_board(fen_split[0])?;
        
        if fen_split[1].len() != 1 {
            return Err(pgn::Error::new(
                pgn::ErrorKind::ParseError,
                "Invalid FEN string: Error in side to move-field"));
        }

        // Check side to move
        board.to_move = parse_fen_to_move(fen_split[1])?;

        // Check castling rights field
        parse_fen_castling_rights(fen_split[2], &mut board)?;

        // Check en passant field
        if fen_split[3] != "-" {
            let ep_square = Square::from_alg(fen_split[3])
                .map_err(|err| pgn::Error::new_caused_by(
                    pgn::ErrorKind::ParseError,
                    format!("Invalid en passant square on {}", fen), err)
                )?;
            board.set_en_passant_square(Some(ep_square));
        }

        let (half_clock, move_num) : (u8, u16) =
            if fen_split.len() > 4 {
                (fen_split[4].parse().map_err(|_|
                    pgn::Error::new(
                        pgn::ErrorKind::ParseError,
                        "Invalid half_move number in FEN string")
                )?,
                 fen_split[5].parse().map_err(|_|
                     pgn::Error::new(
                         pgn::ErrorKind::ParseError,
                         "Invalid half_move number in FEN string")
                 )?)
            }
        else {
            (0, 0)
        };

        board.half_move_clock = half_clock;
        board.move_num = move_num;

        if (board.can_castle_kingside(White) || board.can_castle_queenside(White))
            && board[Square::E1] != Piece::from_type_color(King, White) {
            return Err(pgn::Error::new(
                pgn::ErrorKind::IllegalPosition,
                "FEN string has white castling rights, but white's king is not on e1"));
            }

        if (board.can_castle_kingside(Black) || board.can_castle_queenside(Black))
            && board[Square::E8] != Piece::from_type_color(King, Black) {
            return Err(pgn::Error::new(
                pgn::ErrorKind::IllegalPosition,
                "FEN string has black castling rights, but black's king is not on e8"));
        }
    
        Ok(board)
    }
    fn to_fen(&self) -> String {
        let mut string = String::new();
        for rank in 0..8 {
            let mut last_piece_file = -1;
            for file in 0..8 {
                let square = Square::from_ints(file, rank);
                if !self[square].is_empty() {
                    let num_empty = file as i8 - last_piece_file - 1;
                    last_piece_file = file as i8;
                    if num_empty > 0 {
                        write!(string, "{}{}", num_empty, self[square]).unwrap();
                    }
                    else {
                        write!(string, "{}", self[square]).unwrap();
                    }
                }
            }
            if last_piece_file < 7 {
                write!(string, "{}", 7 - last_piece_file).unwrap();
            }
            string.push('/');
        }
        string.pop();

        match self.side_to_move() {
            White => string.push_str(" w"),
            Black => string.push_str(" b"),
        }
        
        string.push(' ');
        if self.can_castle_kingside(White) {
            string.push('K');
        }
        if self.can_castle_queenside(White) {
            string.push('Q');
        }
        if self.can_castle_kingside(Black) {
            string.push('k');
        }
        if self.can_castle_queenside(Black) {
            string.push('q');
        }
        if string.ends_with(' ') {
            string.push('-');
        }
        
        match self.en_passant_square() {
            Some(square) => {
                write!(string, " {}", square).unwrap();
            },
            None => string.push_str(" -"),
        }
        
        write!(string, " {}", self.half_move_clock).unwrap();
        write!(string, " {}", self.move_num).unwrap();
        string
    }

    fn move_to_san(&self, mv: &<Self as Board>::Move) -> String {
        let color = self.side_to_move();
        let piece_type = self.piece_at(mv.from).piece_type();

        let mut output = String::new();

        if piece_type == King && mv.from.file() == 4 && mv.to.file() == 2 {
            output.push_str("0-0-0");
        }
        else if piece_type == King && mv.from.file() == 4 && mv.to.file() == 6 {
            output.push_str("0-0");
        }
        else {
            if piece_type != Pawn {
                output.push(piece_type.letter());
            }

            let mut moves = vec![];
            self.generate_moves(&mut moves);

            let alternative_from_squares = moves.iter()
                .filter(|&cand_mv| cand_mv.from != mv.from)
                .filter(|&cand_mv|
                    cand_mv.to == mv.to && self.piece_at(cand_mv.from) == self.piece_at(mv.from))
                .map(|cand_mv| cand_mv.from)
                .collect::<Vec<_>>();

            if alternative_from_squares.is_empty()
                && (piece_type != Pawn || mv.from.file() == mv.to.file()) {
                ();
            }
                // Disambiguate with departure file
            else if alternative_from_squares.iter().all(|square| square.file() != mv.from.file()) {
                output.push(mv.from.to_string().chars().next().unwrap());
            }
                // Disambiguate with departure rank
            else if alternative_from_squares.iter().all(|square| square.rank() != mv.from.rank()) {
                output.push(mv.from.to_string().chars().last().unwrap());
            }
                // Disambiguate with full square
            else {
                output.push_str(&mv.from.to_string());
            };

            if self.piece_at(mv.to) != Piece::Empty {
                output.push('x');
            }

            output.push_str(&mv.to.to_string());

            match mv.prom {
                Some(Queen) => output.push_str("=Q"),
                Some(Rook) => output.push_str("=R"),
                Some(Knight) => output.push_str("=N"),
                Some(Bishop) => output.push_str("=B"),
                None => (),
                _ => panic!("Illegal promotion move"),
            };
        }

        let mut cloned_board = self.clone();
        cloned_board.do_move(mv.clone());
        if cloned_board.game_result() == Some(board::GameResult::WhiteWin)
            || cloned_board.game_result() == Some(board::GameResult::BlackWin) {
            output.push('#');
        }
        else if move_gen::is_attacked(&cloned_board, cloned_board.king_pos(!color)) {
            output.push('+');
        }

        return output;
    }

    fn move_from_san(&self, input: &str) -> Result<<Self as Board>::Move, pgn::Error> {

        if input == "0-0-0" || input == "0-0-0+" || input == "0-0-0#" {
            match self.side_to_move() {
                White => return self.move_from_lan("e1c1"),
                Black => return self.move_from_lan("e8c8"),
            }
        }

            else if input == "0-0" || input == "0-0+" || input == "0-0#" {
                match self.side_to_move() {
                    White => return self.move_from_lan("e1g1"),
                    Black => return self.move_from_lan("e8g8"),
                }
            }

        if input.chars().count() < 2 {
            return Err(pgn::Error::new(pgn::ErrorKind::ParseError,
                                       format!("Invalid move {}: Too short at length {:?}",
                                               input, input.chars().count())));
        }

        let piece_type = PieceType::from_letter(input.chars().next().unwrap())
            .unwrap_or(PieceType::Pawn);

        let mut reverse_chars = input.chars().rev().peekable();

        if reverse_chars.peek() == Some(&'+') || reverse_chars.peek() == Some(&'#') {
            reverse_chars.next();
        }

        let promotion = reverse_chars.peek().cloned().and_then(PieceType::from_letter);
        if promotion.is_some() {
            reverse_chars.next();
            if reverse_chars.next() != Some('=') {
                return Err(pgn::Error::new(pgn::ErrorKind::ParseError,
                                           format!("Illegal promotion on {}", input)));
            }
        }

        let dest_square =
            Square::from_alg(&[reverse_chars.next(), reverse_chars.next()]
                .iter().rev()
                .filter_map(|&ch| ch)
                .collect::<String>())
                .map_err(|err|
                    pgn::Error::new_caused_by(pgn::ErrorKind::ParseError,
                                              format!("Illegal move {}", input), err))?;

        if reverse_chars.peek() == Some(&'x') {
            reverse_chars.next();
        }

        let mut disambig_string = reverse_chars
            .take_while(|&ch| PieceType::from_letter(ch).is_none())
            .collect::<String>();
        disambig_string = disambig_string.chars().rev().collect();

        let move_filter : Box<Fn(&ChessMove) -> bool> =
            match (disambig_string.chars().count(), disambig_string.chars().next().clone()) {
            (0, None) => Box::new(|_| true),

            (1, Some(rank)) if rank >= '1' && rank <= '8' =>
                Box::new(move |mv: &ChessMove| mv.from.rank() == 7 - (rank as u8 - '1' as u8)),

            (1, Some(file)) if file >= 'a' && file <= 'h' =>
                Box::new(move |mv: &ChessMove| mv.from.file() == (file as u8 - 'a' as u8)),

            (2, _) if Square::from_alg(&disambig_string).is_ok() =>
                Box::new(move |mv: &ChessMove| mv.from == Square::from_alg(&disambig_string).unwrap()),

            _ => return Err(pgn::Error::new(pgn::ErrorKind::ParseError,
                                        format!("Invalid move disambiguation in {}", input))),
        };

        let mut moves = vec![];
        self.generate_moves(&mut moves);
        let filtered_moves = moves.iter()
            .filter(|mv|
                mv.to == dest_square
                    && self.piece_at(mv.from).piece_type() == piece_type
                    && mv.prom == promotion)
            .filter(|mv| move_filter(mv))
            .collect::<Vec<_>>();

        if filtered_moves.len() > 1 {
            Err(pgn::Error::new(pgn::ErrorKind::AmbiguousMove, format!("{} could be any of {:?}", input, filtered_moves)))
        }
        else if filtered_moves.len() == 0 {
            Err(pgn::Error::new(pgn::ErrorKind::IllegalMove,
                                format!("{} ({} to {}) {:?} {:?} is not a legal move in the position",
                                        input, piece_type, dest_square, moves.iter().filter(|mv| move_filter(mv)).collect::<Vec<_>>(),
                                        moves.iter().filter(|mv| mv.to == dest_square && self.piece_at(mv.from).piece_type() == piece_type).collect::<Vec<_>>())))
        }
        else {
            Ok(filtered_moves[0].clone())
        }
    }

    fn move_to_lan(&self, mv: &Self::Move) -> String {
        let (file_from, rank_from) = mv.from.file_rank();
        let (file_to, rank_to) = mv.to.file_rank();
        let mut s : String = "".to_string();
        s.push_str(&format!("{}{}{}{}", (file_from + b'a') as char,
                            (8 - rank_from + b'0') as char,
                            (file_to + b'a') as char, (8 - rank_to + b'0') as char));
        match mv.prom {
            Some(Queen) => s.push('q'),
            Some(Rook) => s.push('r'),
            Some(Knight) => s.push('n'),
            Some(Bishop) => s.push('b'),
            None => (),
            _ => panic!("Illegal promotion move"),
        }
        s
    }

    // Parse a ChessMove from short algebraic notation (e2e4, g2g1Q, etc)
    fn move_from_lan(&self, alg : &str) -> Result<Self::Move, pgn::Error> {
        // Some GUIs send moves as "e2-e4" instead of "e2e4".
        // In that case, remove the dash and try again
        if alg.chars().nth(2) == Some('-') {
            let mut fixed_alg = alg.to_string();
            fixed_alg.remove(2);
            self.move_from_lan(&fixed_alg)
        }
        else if alg.len() == 4 || alg.len() == 5 {
            let from = Square::from_alg(&alg[0..2]).unwrap_or(Square(0));
            let to = Square::from_alg(&alg[2..4]).unwrap_or(Square(0));
            if alg.len() == 4 {
                Ok(ChessMove { from, to, prom: None })
            }
            else {
                Ok(ChessMove { from, to, prom: Some(
                    match alg.chars().nth(4) {
                        Some('Q') => Queen,
                        Some('q') => Queen,
                        Some('R') => Rook,
                        Some('r') => Rook,
                        Some('N') => Knight,
                        Some('n') => Knight,
                        Some('B') => Bishop,
                        Some('b') => Bishop,
                        Some(ch) => return Err(pgn::Error::new(
                            pgn::ErrorKind::ParseError,
                            format!("Bad promotion letter {} in move {}", ch, alg))),
                        None => return Err(pgn::Error::new(
                            pgn::ErrorKind::IllegalMove,
                            format!("No promotion letter in move {}", alg))),
                    })
                })
            }
        }
        
        else {
            Err(pgn::Error::new(
                pgn::ErrorKind::ParseError,
                format!("Move {} had incorrect length: Found {}, expected 4/5", alg, alg.len())))
        }
    }
}

impl Board for ChessBoard {
    type Move = mv::ChessMove;
    type ReverseMove = mv::ChessReverseMove;

    fn side_to_move(&self) -> Color {
        self.to_move
    }

    fn do_move(&mut self, c_move : Self::Move) -> Self::ReverseMove {
        // Helper variables
        let (file_from, rank_from) = c_move.from.file_rank();
        let (file_to, rank_to) = c_move.to.file_rank();
        let color = self.to_move;
        let piece_moved = self.piece_at(c_move.from).piece_type();
        let captured_piece : PieceType = self[c_move.to].piece_type();
        let mut reverse_move = mv::ChessReverseMove::from_move(c_move, self);

        // Increment or reset the half-move clock
        if piece_moved == Pawn || captured_piece != Empty {
            self.half_move_clock = 0;
            reverse_move.old_past_move_hashes = Some(self.past_board_hashes.clone());
            self.past_board_hashes.clear();
        }
        else {
            self.half_move_clock += 1;
            let mut hasher = DefaultHasher::new();
            self.hash(&mut hasher);
            let hash = hasher.finish();
            self.past_board_hashes.push(hash);
        }

        self.move_num += 1;

        // Perform castling
        // It will castle regardless of whether it is legal to do so
        if piece_moved == King &&
            (file_from as i8 - file_to as i8).abs() == 2 {
            debug_assert_eq!(file_from, 4, "Tried to castle from the {}th file", file_from);

            // Simple helper closure that moves a piece, emptying the square it came from
            // Checks nothing, not even if the square has a piece. Use carefully.
            let do_simple_move = |board : &mut Self, f_from, r_from, f_to, r_to| {
                board.board[r_to as usize][f_to as usize]
                    = board.board[r_from as usize][f_from as usize];
                board.board[r_from as usize][f_from as usize] = Piece::empty();
            };
            // Assume castling is legal, and move the king and rook to where they should go
            match (color, file_to) {
                (White, 2) => {
                    do_simple_move(self, 4, 7, 2, 7);
                    do_simple_move(self, 0, 7, 3, 7);
                },
                (White, 6) => {
                    do_simple_move(self, 4, 7, 6, 7);
                    do_simple_move(self, 7, 7, 5, 7);
                },
                (Black, 2) => {
                    do_simple_move(self, 4, 0, 2, 0);
                    do_simple_move(self, 0, 0, 3, 0);
                },
                (Black, 6) => {
                    do_simple_move(self, 4, 0, 6, 0);
                    do_simple_move(self, 7, 0, 5, 0);
                },
                (_, _) => panic!(format!(
                    "Error: Tried to castle to the {}th file. ", file_to)),
            }
        }
            // If a pawn takes towards an empty square, assume it is doing a legal en passant capture
            else if piece_moved == Pawn && file_from != file_to &&
                captured_piece == Empty {
                self.board[rank_to as usize][file_to as usize]
                    = self.board[rank_from as usize][file_from as usize];

                self.board[rank_from as usize][file_to as usize] = Piece::empty();
                self.board[rank_from as usize][file_from as usize] = Piece::empty();


            }
                // If it is not a special move
                else {
                    // Does the move, depending on whether the move promotes or not
                    match c_move.prom {
                        Some(piece_type) => self.board[rank_to as usize][file_to as usize]
                            = Piece::from_type_color(piece_type, self.side_to_move()),
                        None => self.board[rank_to as usize][file_to as usize]
                            = self.board[rank_from as usize][file_from as usize],

                    }
                    self.board[rank_from as usize][file_from as usize] = Piece::empty();
                }

        // Remove any en passant square. If it was available to this player,
        // it has already been used. Any new en passant square is added below.

        self.set_en_passant_square(None);

        // If the pawn went two squares forward, add the square behind it as en passant square
        if piece_moved == Pawn &&
            (rank_from as i8 - rank_to as i8).abs() == 2
            {
                self.set_en_passant_square(
                    Some(Square::from_ints(file_from, (rank_from + rank_to) / 2)));
                debug_assert!(self.en_passant_square().is_some(), "Failed to set en passant square");
            }

        // Remove castling rights if necessary
        if self.castling_en_passant & 0b0000_1111 > 0 {
            // Remove castling rights on king/rook moves
            // Does not check when rooks are captured, it is assumed that the
            // function looking for moves checks that rooks are present
            if piece_moved == King {
                self.disable_castling(color);
            }
            // If a rook was moved, check if it came from a corner
            if piece_moved == Rook {
                match c_move.from {
                    Square(0) => self.disable_castling_queenside(Black),
                    Square(7) => self.disable_castling_kingside(Black),
                    Square(56) => self.disable_castling_queenside(White),
                    Square(63) => self.disable_castling_kingside(White),
                    _ => (),
                }
            }
            // For any piece, check if it goes into the corner
            match c_move.to {
                Square(0) => self.disable_castling_queenside(Black),
                Square(7) => self.disable_castling_kingside(Black),
                Square(56) => self.disable_castling_queenside(White),
                Square(63) => self.disable_castling_kingside(White),
                _ => (),
            }
        }

        self.to_move = !self.to_move;
        reverse_move
    }
    fn reverse_move(&mut self, c_move : Self::ReverseMove) {

        let (file_from, rank_from) = c_move.from.file_rank();
        let (file_to, rank_to) = c_move.to.file_rank();
        let piece_moved = self.piece_at(c_move.to).piece_type();
        let color = !self.to_move;

        if piece_moved == King &&
            (file_from as i8 - file_to as i8).abs() == 2
            {
                // Simple helper closure that moves a piece, emptying the square it came from
                // Checks nothing, not even if the square has a piece. Use carefully.
                let mut do_simple_move = |f_from, r_from, f_to, r_to| {
                    self.board[r_to as usize][f_to as usize]
                        = self.board[r_from as usize][f_from as usize];
                    self.board[r_from as usize][f_from as usize] = Piece::empty();
                };
                // Assume castling is legal, and move the king and rook to where they should go
                match (color, file_to) {
                    (White, 2) => { do_simple_move(2, 7, 4, 7);
                        do_simple_move(3, 7, 0, 7);
                    },
                    (White, 6) => { do_simple_move(6, 7, 4, 7);
                        do_simple_move(5, 7, 7, 7);
                    },
                    (Black, 2) => { do_simple_move(2, 0, 4, 0);
                        do_simple_move(3, 0, 0, 0);
                    },
                    (Black, 6) => { do_simple_move(6, 0, 4, 0);
                        do_simple_move(5, 0, 7, 0);
                    },
                    (_, _) => panic!(format!(
                        "Error: Tried to castle to the {}th file. ", file_to)),
                }

            }
            // Undo en passant capture
            else if piece_moved == Pawn && file_from != file_to && c_move.capture == Empty {
                self.board[rank_from as usize][file_from as usize] =
                    self.board[rank_to as usize][file_to as usize];
                self.board[rank_to as usize][file_to as usize] = Piece::empty();

                match color {
                    Black => self.board[rank_to as usize - 1][file_to as usize] = Piece::from_type_color(Pawn, White),
                    White => self.board[rank_to as usize + 1][file_to as usize] = Piece::from_type_color(Pawn, Black),
                }
            }
                else {
                    if c_move.prom {
                        self.board[rank_from as usize][file_from as usize] =
                            Piece::from_type_color(Pawn, color);
                    }
                        else {
                            self.board[rank_from as usize][file_from as usize] =
                                self.board[rank_to as usize][file_to as usize];
                        }

                    if c_move.capture != Empty
                        {
                            self.board[rank_to as usize][file_to as usize] =
                                Piece::from_type_color(c_move.capture, self.to_move);
                        }
                        else {
                            self.board[rank_to as usize][file_to as usize] = Piece::empty();
                        }
                }
        match c_move.old_past_move_hashes {
            Some(hashes) => self.past_board_hashes = hashes.clone(),
            None => { self.past_board_hashes.pop(); },
        };
        self.castling_en_passant = c_move.old_castling_en_passant;
        self.half_move_clock = c_move.old_half_move_clock;
        self.move_num -= 1;

        self.to_move = !self.to_move;
    }

    fn start_board() -> Self {
        Self::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap()
    }

    fn generate_moves(&self, moves: &mut Vec<Self::Move>) {
        debug_assert!(moves.is_empty());
        let (mut active_moves, mut quiet_moves) = move_gen::all_legal_moves(self);
        active_moves.append(&mut quiet_moves);
        moves.extend_from_slice(&active_moves[..]);
    }

    fn game_result(&self) -> Option<board::GameResult> {
        if self.half_move_clock >= 100 {
            return Some(board::GameResult::Draw);
        }

        // Check for repetitions
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        let hash = hasher.finish();

        if self.past_board_hashes.iter()
            .filter(|past_hash| **past_hash == hash)
            .count() >= 2 {
            return Some(GameResult::Draw)
        }

        // TODO: This shouldn't call generate_moves(), but instead store whether its mate or not
        let mut moves = vec![];
        self.generate_moves(&mut moves);
        if moves.is_empty() {
            if move_gen::is_attacked(self, self.king_pos(self.side_to_move())) {
                if self.to_move == White {
                    Some(board::GameResult::BlackWin)
                }
                    else {
                        Some(board::GameResult::WhiteWin)
                    }
            }
                else {
                    Some(board::GameResult::Draw)
                }
        }
            else {
                // Force a draw for unwinnable bishop/knight endgames
                let mut black_material = 0.0;
                let mut white_material = 0.0;
                for square in BoardIter::new() {
                    // Games with queens, rooks or pawns are always undecided
                    match self[square].piece_type() {
                        Queen | Rook | Pawn => return None,
                        Knight | Bishop | King | Empty => (),
                    };
                    match (self[square].piece_type(), self[square].color()) {
                        (piece_type, Some(Black)) => black_material += piece_type.value(),
                        (piece_type, Some(White)) => white_material += piece_type.value(),
                        (Empty, None) => (),
                        (_, _) => unreachable!(),
                    };
                }
                if black_material <= 3.5 && white_material <= 3.5 {
                    Some(board::GameResult::Draw)
                }
                    else {
                        None
                    }
            }
    }
}

impl EvalBoard for ChessBoard {
    #[inline(never)]
    fn static_eval (&self) -> f32 {
        const POS_VALS : [[u8; 8]; 8] = 
            [[0, 0, 0, 0, 0, 0, 0, 0],
             [0, 1, 1, 1, 1, 1, 1, 0],
             [0, 1, 2, 2, 2, 2, 1, 0],
             [0, 1, 2, 3, 3, 2, 1, 0],
             [0, 1, 2, 3, 3, 2, 1, 0],
             [0, 1, 2, 2, 2, 2, 1, 0],
             [0, 1, 1, 1, 1, 1, 1, 0],
             [0, 0, 0, 0, 0, 0, 0, 0]];
        let mut value = 0.0;
        for rank in 0..8 {
            for file in 0..8 {
                let piece = self.board[rank][file];
                let piece_val = piece.value();
                let pos_val = POS_VALS[rank][file] as f32 *
                    if piece.color() == None { 0.0 }
                else {
                    match (piece.piece_type(), piece.color().unwrap()) {
                        (Bishop, White) => 0.15,
                        (Bishop, Black) => -0.15,
                        (Knight, White) => 0.3,
                        (Knight, Black) => -0.3,
                        (Queen, White) => 0.3,
                        (Queen, Black) => -0.3,
                        (Pawn, White) => 0.00,
                        (Pawn, Black) => -0.00,
                        (Rook, Black) => -0.1,
                        (Rook, White) => 0.1,
                        _ => 0.0,
                    }
                };
                let pawn_val = match self.board[rank][file].piece_type() {
                    Pawn => (rank as f32 - 3.5) * -0.1,
                    _ => 0.0,
                };
                value += piece_val + pos_val + pawn_val;
            }
        }
        value
    }
}

impl ExtendedBoard for ChessBoard {
    type ReverseNullMove = ChessReverseNullMove;
    type HashBoard = Self;

    fn hash_board(&self) -> Self {
        self.clone()
    }

    fn move_is_legal(&self, mv: Self::Move) -> bool {

        if self[mv.from].color() != Some(self.side_to_move()) {
            return false;
        }

        let mut moves1 = vec![];
        let mut moves2 = vec![];
        let mut moves3 = vec![];
        let king_pos = move_gen::king_pos(&self);
        let is_in_check = move_gen::is_attacked(&self, king_pos);
        move_gen::legal_moves_for_piece(&mut self.clone(), mv.from,
                                        &mut moves1, &mut moves2, &mut moves3,
                                        is_in_check, king_pos);
        if moves1.contains(&mv) || moves2.contains(&mv) || moves3.contains(&mv) {
            let mut moves = vec![];
            self.generate_moves(&mut moves);
            debug_assert!(moves.contains(&mv),
                          "Illegal move {:?} marked as legal on \n{}True legal moves: {:?}\nPiece moves: {:?}, {:?}, {:?}", mv, self, moves, moves1, moves2, moves3);
        }
        moves1.contains(&mv) || moves2.contains(&mv) || moves3.contains(&mv)
    }

    fn active_moves(&self, moves: &mut Vec<Self::Move>) {
        let (mut active_moves, _) = move_gen::all_legal_moves(self);
        moves.append(&mut active_moves);
    }

    fn null_move_is_available(&self) -> bool {
        !move_gen::is_attacked(self, self.king_pos(self.to_move))
    }

    fn do_null_move(&mut self) -> Self::ReverseNullMove {
        debug_assert!(self.null_move_is_available());

        let reverse_move = ChessReverseNullMove {
            old_castling_en_passant: self.castling_en_passant,
        };
        self.set_en_passant_square(None);
        self.to_move = !self.to_move;

        reverse_move
    }

    fn reverse_null_move(&mut self, mv: <Self as ExtendedBoard>::ReverseNullMove) {
        self.castling_en_passant = mv.old_castling_en_passant;
        self.to_move = !self.to_move;
    }
}

impl fmt::Display for ChessBoard {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt.write_str("\n").unwrap();
        for rank in &self.board {
            for piece in rank.iter() {
                write!(fmt, "[{}]", piece).unwrap();
            }
            writeln!(fmt).unwrap();
        }
        writeln!(fmt, "To move: {}, move_number: {}, flags: {:b}, half_move_clock: {}, past_board_hashes: {:?}",
                 self.to_move, self.move_num, self.castling_en_passant, self.half_move_clock,
                 self.past_board_hashes)
            .unwrap();
        Ok(())   
    }
}

impl fmt::Debug for ChessBoard {
    fn fmt(&self, fmt : &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Display::fmt(self, fmt)
    }
}

impl ops::Index<Square> for ChessBoard {
    type Output = Piece;
    fn index(&self, square: Square) -> &Piece {
        let Square(i) = square;
        debug_assert!(i < 64, format!("Tried to find piece at pos {} on board{}!", i, self));
        &self.board[i as usize >> 3][i as usize & 0b0000_0111]
    }
}

impl ops::IndexMut<Square> for ChessBoard {
    fn index_mut(&mut self, square: Square) -> &mut Piece {
        let Square(i) = square;
        debug_assert!(i < 64, format!("Tried to find piece at pos {} on board{}!", i, self));
        &mut self.board[i as usize >> 3][i as usize & 0b0000_0111]
    }
}

impl ChessBoard {
    pub fn empty() -> Self {
        Self { board: [[Piece::Empty; 8]; 8], to_move: White, castling_en_passant: 0,
               half_move_clock: 0, move_num: 0, past_board_hashes: vec![]
        }
    }
    
    // TODO: Remove in favour of indexing operator
    pub fn piece_at(&self, square : Square) -> Piece {
        self[square]
    }

    pub fn king_pos(&self, color: Color) -> Square {
        match self.pos_of(Piece::from_type_color(King, color)) {
            Some(square) => square,
            None => panic!("Error: There is no king on the board:\n{}", self),
        }
    }
    
    pub fn pos_of(&self, piece : Piece) -> Option<Square> {
        for i in 0..64 {
            if self.piece_at(Square(i)) == piece {
                return Some(Square(i));
            }
        }
        None
    }
    
    pub fn disable_castling(&mut self, color: Color) {
        match color {
            White => self.castling_en_passant &= 0b1111_1100,
            Black => self.castling_en_passant &= 0b1111_0011,
        }
    }
    
    pub fn disable_castling_queenside(&mut self, color: Color) {
        match color {
            White => self.castling_en_passant &= 0b1111_1101,
            Black => self.castling_en_passant &= 0b1111_0111,
        }
    }
    
    pub fn disable_castling_kingside(&mut self, color: Color) {
        match color {
            White => self.castling_en_passant &= 0b1111_1110,
            Black => self.castling_en_passant &= 0b1111_1011,
        }
    }
    
    pub fn can_castle_kingside(&self, color: Color) -> bool {
        match color {
            White => self.castling_en_passant & 0b0000_0001 > 0,
            Black => self.castling_en_passant & 0b0000_0100 > 0,
        }
    }
    pub fn can_castle_queenside(&self, color: Color) -> bool {
        match color {
            White => self.castling_en_passant & 0b0000_0010 > 0,
            Black => self.castling_en_passant & 0b0000_1000 > 0,
        }
    }
    
    pub fn en_passant_square(&self) -> Option<Square> {
        if self.castling_en_passant & 0b1111_0000 != 0 {
            let rank = if self.to_move == Black { 5 } else { 2 };
            let file = (self.castling_en_passant & 0b0111_1111) >> 4;
            debug_assert!(file < 8);

            Some(Square::from_ints(file, rank))
        }
        else { None }
    }

    pub fn set_en_passant_square(&mut self, square: Option<Square>) {
        
        match square {
            Some(square) => {
                let (mut byte, _) = square.file_rank();
                byte |= 0b1000_0000;
                self.castling_en_passant |= (byte << 4) | 0b1000_0000;
            },
            None => self.castling_en_passant &= 0b0000_1111,
        }
    }
}



//! Data types for text representations of board positions and moves, which may be used for [Portable game notation][1].
//!
//! The terminology used in this module is specific to chess and chess variants, but it can be implemented for any game.
//!
//! [1]: https://en.wikipedia.org/wiki/Portable_Game_Notation

use search_algorithms::board;
use std::error;
use std::fmt;
use search_algorithms::board::GameResult;
use search_algorithms::board::Color;
use std::io::Write;
use std::io;

/// A list of general categories of errors related to pgn parsing.
///
/// This list is intended to grow over time and it is not recommended to exhaustively match against it.
///
/// It is used with the [`Error`] type.
///
/// [`Error`]: struct.Error.html
#[derive(Clone, Copy, Eq, PartialEq, Debug, PartialOrd, Ord)]
pub enum ErrorKind {
    ParseError,
    AmbiguousMove,
    IllegalMove,
    IllegalPosition,
    IOError,
    Other,
}

/// The error type for operations on a `PgnBoard`.
///
/// The error can be created with an arbitrary payload and optionally an underlying source error for error chaining.
#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    error: Box<dyn error::Error + Send + Sync>,
    source: Option<Box<dyn error::Error + Send + Sync>>,
}

impl Error {
    /// Returns a new error of the specific `ErrorKind` with an arbitrary payload.
    pub fn new<E>(kind: ErrorKind, error: E) -> Error
    where E: Into<Box<dyn error::Error + Send + Sync>> {
        Error { kind, error: error.into(), source: None}
    }

    /// Returns a new error of the specific `ErrorKind` with an arbitrary payload and source error.
    pub fn new_caused_by<E, F>(kind: ErrorKind, error: E, source: F) -> Error
    where E: Into<Box<dyn error::Error + Send + Sync>>,
          F: Into<Box<dyn error::Error + Send + Sync>> {
        Error { kind, error: error.into(), source: Some(source.into())}
    }
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.kind {
            ErrorKind::ParseError => write!(fmt, "Parse error. "),
            ErrorKind::AmbiguousMove => write!(fmt, "Ambiguous move. "),
            ErrorKind::IllegalMove => write!(fmt, "Illegal move. "),
            ErrorKind::IllegalPosition => write!(fmt, "Illegal position. "),
            ErrorKind::IOError => write!(fmt, "IO error. "),
            ErrorKind::Other => Ok(()),
        }?;
        write!(fmt, "{}", self.error)?;
        if let Some(ref source) = self.source {
            write!(fmt, "\nCaused by: {}", source)?;
        }
        Ok(())
    }
}

/// Trait for text representations of board positions and moves.
///
/// The terminology used in this trait is specific to chess and chess variants, but it can be implemented for any game.
pub trait PgnBoard: Sized + board::Board + PartialEq {
    /// Constructs a board from [Forsyth–Edwards Notation][1].
    ///
    /// Extensions to this notation exist for all large chess variants
    ///
    /// [1]: https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
    fn from_fen(fen: &str) -> Result<Self, Error>;

    /// Returns a string representation of the board in [Forsyth–Edwards Notation][1].
    ///
    /// Extensions to this notation exist for all large chess variants.
    ///
    /// [1]: https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
    fn to_fen(&self) -> String;

    /// Construct a game move from [Standard Algebraic Notation][1], specifically the format used in [pgn notation][2].
    ///
    /// Extensions to this notation exist for all large chess variants.
    ///
    /// [1]: https://en.wikipedia.org/wiki/Algebraic_notation_(chess)
    /// [2]: https://en.wikipedia.org/wiki/Portable_Game_Notation
    fn move_from_san(&self, input: &str) -> Result<Self::Move, Error>;

    /// Returns a string representation of the move in [Standard Algebraic Notation][1], specifically the format used in [pgn notation][2].
    ///
    /// Extensions to this notation exist for all large chess variants.
    ///
    /// [1]: https://en.wikipedia.org/wiki/Algebraic_notation_(chess)
    /// [2]: https://en.wikipedia.org/wiki/Portable_Game_Notation
    fn move_to_san(&self, mv: &Self::Move) -> String;

    /// Construct a move from an alternative, [long algebraic notation][1].
    ///
    /// This is mostly used for chess and chess variations in the uci interface, or for convenient debugging.
    /// Implementations may simply wrap this function around `move_from_san` where appropriate.
    ///
    /// [1]: https://en.wikipedia.org/wiki/Algebraic_notation_(chess)#Long_algebraic_notation
    fn move_from_lan(&self, input: &str) -> Result<Self::Move, Error>;

    /// Returns a string representation of the move in an alternative, [long algebraic notation][1].
    ///
    /// This is mostly used for chess and chess variations in the uci interface, or for convenient debugging.
    /// Implementations may simply wrap this function around `move_to_san` where appropriate.
    ///
    /// [1]: https://en.wikipedia.org/wiki/Algebraic_notation_(chess)#Long_algebraic_notation
    fn move_to_lan(&self, mv: &Self::Move) -> String;

    fn game_to_pgn<W: Write>(mut self, moves: &[Self::Move], event: &str, site: &str, date: &str,
                   round: &str, white: &str, black: &str, result: Option<GameResult>,
                   tags_pairs: &[(&str, &str)], f: &mut W) -> Result<(), io::Error> {
        writeln!(f, "[Event \"{}\"]", event)?;
        writeln!(f, "[Site \"{}\"]", site)?;
        writeln!(f, "[Date \"{}\"]", date)?;
        writeln!(f, "[Round \"{}\"]", round)?;
        writeln!(f, "[White \"{}\"]", white)?;
        writeln!(f, "[Black \"{}\"]", black)?;
        writeln!(f, "[Result \"{}\"]", match result {
            None => "*",
            Some(GameResult::WhiteWin) => "1-0",
            Some(GameResult::BlackWin) => "0-1",
            Some(GameResult::Draw) => "1/2-1/2",
        })?;

        if tags_pairs.iter().find(|(tag, _)| *tag == "FEN" ).is_none()
            && self != Self::start_board() {
            writeln!(f, "[FEN \"{}\"", self.to_fen())?;
        }

        for (i, mv) in moves.into_iter().enumerate() {
            if i % 12 == 0 {
                writeln!(f)?;
            }
            if i == 0 && self.side_to_move() == Color::Black {
                write!(f, "{}... {} ", 1, self.move_to_san(&mv))?;
            }
            else if self.side_to_move() == Color::White {
                write!(f, "{}. {} ", (i + 1) / 2 + 1, self.move_to_san(&mv))?;
            }
            else {
                write!(f, "{} ", self.move_to_san(&mv))?;
            }
            self.do_move(mv.clone());
        }

        write!(f, "{}", match result {
            None => "*",
            Some(GameResult::WhiteWin) => "1-0",
            Some(GameResult::BlackWin) => "0-1",
            Some(GameResult::Draw) => "1/2-1/2",
        })?;
        writeln!(f)?;
        writeln!(f)?;
        Ok(())
    }
}
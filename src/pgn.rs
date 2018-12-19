use search_algorithms::board;
use std::error;
use std::fmt;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum ErrorKind {
    ParseError,
    AmbiguousMove,
    IllegalMove,
    IllegalPosition,
    Other,
}

pub struct Error {
    kind: ErrorKind,
    error: Box<dyn error::Error + Send + Sync>,
}

impl Error {
    pub fn new<E>(kind: ErrorKind, error: E) -> Error
    where E: Into<Box<dyn error::Error + Send + Sync>> {
        Error { kind, error: error.into()}
    }
}

impl std::fmt::Debug for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "{:?}: {:?}", self.kind, self.error)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self.kind {
            ErrorKind::ParseError => write!(fmt, "Parse error. "),
            ErrorKind::AmbiguousMove => write!(fmt, "Ambiguous move. "),
            ErrorKind::IllegalMove => write!(fmt, "Illegal move. "),
            ErrorKind::IllegalPosition => write!(fmt, "Illegal position. "),
            ErrorKind::Other => Ok(()),
        }?;
        write!(fmt, "{}", self.error)
    }
}

impl error::Error for Error {}

pub trait UciBoard: Sized + board::Board {
    fn from_fen(fen: &str) -> Result<Self, Error>;
    fn to_fen(&self) -> String;

    fn move_to_san(&self, mv: &Self::Move) -> String;

    fn mv_from_san(&self, input: &str) -> Result<Self::Move, Error>;

    /// Converts a move to an alternative, long algebraic notation.
    /// This is mostly used for chess and chess variations in the uci interface, or for convenient debugging.
    /// Implementations may simply wrap this function around move_to_san
    fn move_to_lan(&self, mv: &Self::Move) -> String;

    /// Parses a move from an alternative, long algebraic notation.
    /// This is mostly used for chess and chess variations in the uci interface, or for convenient debugging.
    /// Implementations may simply wrap this function around move_from_san
    fn move_from_lan(&self, input: &str) -> Result<Self::Move, Error>;

}
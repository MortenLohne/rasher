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

    fn from_alg(&self, input: &str) -> Result<Self::Move, Error>; // Rename move_from_lan
    fn to_alg(&self, mv: &Self::Move) -> String; // lan_move_string
}
use std::marker;

pub trait Move : Sized + marker::Send + Clone {
    fn from_alg(&str) -> Result<Self, String>;

    fn to_alg(&self) -> String;
}

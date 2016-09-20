pub trait Move : Sized {
    fn from_alg(&str) -> Option<Self>;

    fn to_alg(&self) -> String;
}

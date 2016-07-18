pub trait Board : Sized {
    type Move : Copy;

    fn from_fen(&str) -> Result<Self, String>;

    fn do_move(&mut self, Self::Move);
    fn undo_move(&mut self, Self::Move);

    fn all_legal_moves(&mut self) -> Vec<Self::Move>;

    /// Returns a score which is either mate or stalemate. ASSUMES all_legal_moves.len() == 0
    fn is_mate_or_stalemate(&self) -> ::Score;

    fn score_board(&self) -> ::Score;
}

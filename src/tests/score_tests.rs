use search_algorithms::alpha_beta::Score;

#[test]
fn score_ordering_test() {
    assert!(Score::BlackWin(2) < Score::BlackWin(3));
    assert!(Score::WhiteWin(2) > Score::WhiteWin(3));
    assert!(Score::Val(1.0) < Score::Val(2.0));

    assert!(Score::BlackWin(2) < Score::Val(-200.0));
    assert!(Score::BlackWin(2) < Score::WhiteWin(3));
    assert!(Score::BlackWin(3) < Score::WhiteWin(2));
    
    assert!(Score::WhiteWin(2) > Score::Val(200.0));
    assert!(Score::WhiteWin(2) > Score::BlackWin(3));
    assert!(Score::WhiteWin(3) > Score::BlackWin(2));
}

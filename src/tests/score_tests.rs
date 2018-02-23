use search_algorithms::alpha_beta::Score;

#[test]
fn score_ordering_test() {
    assert!(Score::Loss(2) < Score::Loss(3));
    assert!(Score::Win(2) > Score::Win(3));
    assert!(Score::Val(1.0) < Score::Val(2.0));

    assert!(Score::Loss(2) < Score::Val(-200.0));
    assert!(Score::Loss(2) < Score::Win(3));
    assert!(Score::Loss(3) < Score::Win(2));
    
    assert!(Score::Win(2) > Score::Val(200.0));
    assert!(Score::Win(2) > Score::Loss(3));
    assert!(Score::Win(3) > Score::Loss(2));
}
